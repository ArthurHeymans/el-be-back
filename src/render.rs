use std::sync::OnceLock;

use emacs::{Env, GlobalRef, IntoLisp, Result, Value};
use wezterm_cell::color::{ColorAttribute, SrgbaTuple};
use wezterm_cell::CellAttributes;
use wezterm_escape_parser::csi::Intensity;
use wezterm_term::color::ColorPalette;

use crate::EbbTerminal;

// ---------------------------------------------------------------------------
// Pre-interned Emacs symbols -- allocated once at module init, reused every
// frame.  Eliminates all env.intern() and env.call("name", ..) FFI overhead
// during rendering.
// ---------------------------------------------------------------------------

pub(crate) struct Syms {
    // Functions
    insert: GlobalRef,
    propertize: GlobalRef,
    erase_buffer: GlobalRef,
    goto_char: GlobalRef,
    point_min: GlobalRef,
    forward_line: GlobalRef,
    line_beginning_position: GlobalRef,
    line_end_position: GlobalRef,
    delete_region: GlobalRef,
    symbol_value: GlobalRef,
    // Property name symbols
    face: GlobalRef,
    help_echo: GlobalRef,
    mouse_face: GlobalRef,
    highlight: GlobalRef,
    ebb_url: GlobalRef,
    keymap: GlobalRef,
    ebb_hyperlink_map: GlobalRef,
    // Face plist keywords and values
    kw_foreground: GlobalRef,
    kw_background: GlobalRef,
    kw_weight: GlobalRef,
    kw_slant: GlobalRef,
    kw_underline: GlobalRef,
    kw_strike_through: GlobalRef,
    kw_inverse_video: GlobalRef,
    bold: GlobalRef,
    light: GlobalRef,
    italic_val: GlobalRef,
    t_val: GlobalRef,
    // Property stickiness control
    rear_nonsticky: GlobalRef,
    // Additional functions for scrollback
    point: GlobalRef,
    point_max: GlobalRef,
}

static SYMS: OnceLock<Syms> = OnceLock::new();

pub(crate) fn init_syms(env: &Env) -> Result<()> {
    if SYMS.get().is_some() {
        return Ok(());
    }
    let syms = Syms {
        insert: env.intern("insert")?.make_global_ref(),
        propertize: env.intern("propertize")?.make_global_ref(),
        erase_buffer: env.intern("erase-buffer")?.make_global_ref(),
        goto_char: env.intern("goto-char")?.make_global_ref(),
        point_min: env.intern("point-min")?.make_global_ref(),
        forward_line: env.intern("forward-line")?.make_global_ref(),
        line_beginning_position: env.intern("line-beginning-position")?.make_global_ref(),
        line_end_position: env.intern("line-end-position")?.make_global_ref(),
        delete_region: env.intern("delete-region")?.make_global_ref(),
        symbol_value: env.intern("symbol-value")?.make_global_ref(),

        face: env.intern("face")?.make_global_ref(),
        help_echo: env.intern("help-echo")?.make_global_ref(),
        mouse_face: env.intern("mouse-face")?.make_global_ref(),
        highlight: env.intern("highlight")?.make_global_ref(),
        ebb_url: env.intern("ebb-url")?.make_global_ref(),
        keymap: env.intern("keymap")?.make_global_ref(),
        ebb_hyperlink_map: env.intern("ebb-hyperlink-map")?.make_global_ref(),

        kw_foreground: env.intern(":foreground")?.make_global_ref(),
        kw_background: env.intern(":background")?.make_global_ref(),
        kw_weight: env.intern(":weight")?.make_global_ref(),
        kw_slant: env.intern(":slant")?.make_global_ref(),
        kw_underline: env.intern(":underline")?.make_global_ref(),
        kw_strike_through: env.intern(":strike-through")?.make_global_ref(),
        kw_inverse_video: env.intern(":inverse-video")?.make_global_ref(),
        bold: env.intern("bold")?.make_global_ref(),
        light: env.intern("light")?.make_global_ref(),
        italic_val: env.intern("italic")?.make_global_ref(),
        t_val: env.intern("t")?.make_global_ref(),
        rear_nonsticky: env.intern("rear-nonsticky")?.make_global_ref(),
        point: env.intern("point")?.make_global_ref(),
        point_max: env.intern("point-max")?.make_global_ref(),
    };
    let _ = SYMS.set(syms);
    Ok(())
}

// ---------------------------------------------------------------------------
// Face key -- compact representation of the attributes that affect face
// construction.  Used to detect style changes between adjacent cells.
// ---------------------------------------------------------------------------

#[derive(Hash, Eq, PartialEq, Clone)]
pub(crate) struct FaceKey {
    fg: Option<[u8; 3]>,
    bg: Option<[u8; 3]>,
    intensity: u8, // 0=Normal 1=Bold 2=Half
    italic: bool,
    underline: bool,
    strikethrough: bool,
    reverse: bool,
}

impl FaceKey {
    fn default_unstyled() -> Self {
        FaceKey {
            fg: None,
            bg: None,
            intensity: 0,
            italic: false,
            underline: false,
            strikethrough: false,
            reverse: false,
        }
    }
}

/// Build a FaceKey from cell attributes, resolving palette colours to RGB.
/// Returns (key, has_style) where has_style=false means entirely default.
fn make_face_key(attrs: &CellAttributes, palette: &ColorPalette) -> (FaceKey, bool) {
    let fg = resolve_color_rgb(attrs.foreground(), palette);
    let bg = resolve_color_rgb(attrs.background(), palette);
    let intensity = match attrs.intensity() {
        Intensity::Normal => 0u8,
        Intensity::Bold => 1,
        Intensity::Half => 2,
    };
    let italic = attrs.italic();
    let underline = attrs.underline() != wezterm_escape_parser::csi::Underline::None;
    let strikethrough = attrs.strikethrough();
    let reverse = attrs.reverse();

    let has_style = fg.is_some()
        || bg.is_some()
        || intensity != 0
        || italic
        || underline
        || strikethrough
        || reverse;

    (
        FaceKey {
            fg,
            bg,
            intensity,
            italic,
            underline,
            strikethrough,
            reverse,
        },
        has_style,
    )
}

fn resolve_color_rgb(attr: ColorAttribute, palette: &ColorPalette) -> Option<[u8; 3]> {
    match attr {
        ColorAttribute::Default => None,
        ColorAttribute::PaletteIndex(idx) => {
            let c = palette.resolve_fg(ColorAttribute::PaletteIndex(idx));
            Some(srgba_to_rgb(c))
        }
        ColorAttribute::TrueColorWithPaletteFallback(c, _)
        | ColorAttribute::TrueColorWithDefaultFallback(c) => Some(srgba_to_rgb(c)),
    }
}

#[inline]
fn srgba_to_rgb(c: SrgbaTuple) -> [u8; 3] {
    [
        (c.0 * 255.0).round() as u8,
        (c.1 * 255.0).round() as u8,
        (c.2 * 255.0).round() as u8,
    ]
}

/// Build a face plist like (:foreground "#rrggbb" :weight bold …) directly
/// as a Lisp list -- no string formatting then (read …) round-trip.
fn build_face_plist<'e>(env: &'e Env, key: &FaceKey, syms: &'e Syms) -> Result<Value<'e>> {
    let mut p: Vec<Value<'e>> = Vec::with_capacity(14);

    if let Some(rgb) = key.fg {
        p.push(syms.kw_foreground.bind(env));
        p.push(format!("#{:02x}{:02x}{:02x}", rgb[0], rgb[1], rgb[2]).into_lisp(env)?);
    }
    if let Some(rgb) = key.bg {
        p.push(syms.kw_background.bind(env));
        p.push(format!("#{:02x}{:02x}{:02x}", rgb[0], rgb[1], rgb[2]).into_lisp(env)?);
    }
    match key.intensity {
        1 => {
            p.push(syms.kw_weight.bind(env));
            p.push(syms.bold.bind(env));
        }
        2 => {
            p.push(syms.kw_weight.bind(env));
            p.push(syms.light.bind(env));
        }
        _ => {}
    }
    if key.italic {
        p.push(syms.kw_slant.bind(env));
        p.push(syms.italic_val.bind(env));
    }
    if key.underline {
        p.push(syms.kw_underline.bind(env));
        p.push(syms.t_val.bind(env));
    }
    if key.strikethrough {
        p.push(syms.kw_strike_through.bind(env));
        p.push(syms.t_val.bind(env));
    }
    if key.reverse {
        p.push(syms.kw_inverse_video.bind(env));
        p.push(syms.t_val.bind(env));
    }
    env.list(&p[..])
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/// Render the terminal screen into the current Emacs buffer.
///
/// Buffer layout:
///   [scrollback lines]  -- permanent, above display region
///   [display region]    -- rows x cols, updated each frame
///
/// Uses incremental (dirty-row) rendering when possible.  Falls back to a
/// full erase+redraw when the terminal was resized or on the first render.
/// New scrollback lines are inserted above the display region as they appear.
pub fn render_to_buffer(env: &Env, term: &mut EbbTerminal) -> Result<()> {
    if term.freed {
        return Ok(());
    }

    let syms = SYMS.get().expect("ebb: symbols not initialised");

    let EbbTerminal {
        ref terminal,
        ref mut last_seqno,
        ref mut last_rows,
        ref mut last_first_vis_stable,
        ref mut scrollback_in_buffer,
        max_scrollback,
        ..
    } = *term;

    let screen = terminal.screen();
    let rows = screen.physical_rows;
    let cols = screen.physical_cols;
    let palette = terminal.palette();
    let cursor = terminal.cursor_pos();

    if rows == 0 {
        return Ok(());
    }

    let scrollback_count = screen.scrollback_rows().saturating_sub(rows);
    let cur_first_vis_stable = screen.visible_row_to_stable_row(0);
    let needs_full = *last_rows != rows || *last_seqno == 0;

    if needs_full {
        // ---- Full render: scrollback (styled) + display (styled) ----
        env.call(&syms.erase_buffer, [])?;

        // Render scrollback lines with full styling (colors, bold, etc.).
        if scrollback_count > 0 {
            for phys in 0..scrollback_count {
                let lines = screen.lines_in_phys_range(phys..phys + 1);
                if let Some(line) = lines.first() {
                    render_line(env, line, cols, &palette, syms)?;
                }
                env.call(&syms.insert, ("\n",))?;
            }
        }

        // Render visible rows with full styling.
        render_display_rows(env, screen, rows, cols, &palette, syms)?;

        *scrollback_in_buffer = scrollback_count;
    } else {
        // ---- Incremental render ----

        let new_sb = (cur_first_vis_stable - *last_first_vis_stable).max(0) as usize;

        if new_sb > 0 && new_sb < rows {
            // -- Scroll optimisation: promote + append --
            // The top `new_sb` display lines naturally become scrollback:
            // their content is already correct, just move the boundary.
            *scrollback_in_buffer += new_sb;

            // Append the `new_sb` new lines at the bottom of the display
            // region.  The middle lines stay untouched.
            env.call(&syms.goto_char, (env.call(&syms.point_max, [])?,))?;
            for i in 0..new_sb {
                let vis_row = rows - new_sb + i;
                env.call(&syms.insert, ("\n",))?;
                let stable = screen.visible_row_to_stable_row(vis_row as i64);
                if let Some(phys) = screen.stable_row_to_phys(stable) {
                    let lines = screen.lines_in_phys_range(phys..phys + 1);
                    if let Some(line) = lines.first() {
                        render_line(env, line, cols, &palette, syms)?;
                    }
                }
            }

            // Check for in-place changes in the kept (shifted) rows.
            let kept_rows = rows - new_sb;
            if kept_rows > 0 {
                let first_kept = cur_first_vis_stable;
                let last_kept = screen.visible_row_to_stable_row((kept_rows - 1) as i64);
                let dirty = screen.get_changed_stable_rows(first_kept..last_kept + 1, *last_seqno);
                if !dirty.is_empty() {
                    incremental_render_display(
                        env,
                        screen,
                        rows,
                        cols,
                        &palette,
                        syms,
                        &dirty,
                        first_kept,
                        *scrollback_in_buffer,
                    )?;
                }
            }
        } else if new_sb >= rows {
            // -- Scrolled past entire screen --
            // Old display lines become scrollback.
            *scrollback_in_buffer += rows;

            // Lines that scrolled through without ever being displayed
            // ("missed") need to be added as scrollback too.
            let missed = new_sb - rows;
            if missed > 0 {
                env.call(&syms.goto_char, (env.call(&syms.point_max, [])?,))?;
                let start_phys = scrollback_count.saturating_sub(missed);
                for phys in start_phys..scrollback_count {
                    let lines = screen.lines_in_phys_range(phys..phys + 1);
                    if let Some(line) = lines.first() {
                        env.call(&syms.insert, ("\n",))?;
                        render_line(env, line, cols, &palette, syms)?;
                    }
                }
                *scrollback_in_buffer += missed;
            }

            // Append a fresh display region.
            env.call(&syms.goto_char, (env.call(&syms.point_max, [])?,))?;
            env.call(&syms.insert, ("\n",))?;
            render_display_rows(env, screen, rows, cols, &palette, syms)?;
        } else {
            // -- No scrolling: incremental dirty-row updates --
            let first_stable = cur_first_vis_stable;
            let last_stable = screen.visible_row_to_stable_row((rows - 1) as i64);
            let dirty = screen.get_changed_stable_rows(first_stable..last_stable + 1, *last_seqno);

            if !dirty.is_empty() {
                if dirty.len() > rows / 2 {
                    redraw_display_region(
                        env,
                        screen,
                        rows,
                        cols,
                        &palette,
                        syms,
                        *scrollback_in_buffer,
                    )?;
                } else {
                    incremental_render_display(
                        env,
                        screen,
                        rows,
                        cols,
                        &palette,
                        syms,
                        &dirty,
                        first_stable,
                        *scrollback_in_buffer,
                    )?;
                }
            }
        }

        // Trim scrollback if over the limit.
        if *scrollback_in_buffer > max_scrollback {
            let excess = *scrollback_in_buffer - max_scrollback;
            env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
            env.call(&syms.forward_line, (excess as i64,))?;
            let cut = env.call(&syms.point, [])?;
            env.call(&syms.delete_region, (env.call(&syms.point_min, [])?, cut))?;
            *scrollback_in_buffer = max_scrollback;
        }
    }

    // ---- Position cursor (in display region) ----
    let cursor_buf_line = *scrollback_in_buffer as i64 + cursor.y;
    env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
    env.call(&syms.forward_line, (cursor_buf_line,))?;
    let char_off = cursor_char_offset(screen, cursor.y, cursor.x, cols);
    let line_start = env
        .call(&syms.line_beginning_position, [])?
        .into_rust::<i64>()?;
    let line_end = env.call(&syms.line_end_position, [])?.into_rust::<i64>()?;
    env.call(
        &syms.goto_char,
        ((line_start + char_off as i64).min(line_end).max(line_start),),
    )?;

    // ---- Update tracking ----
    *last_seqno = terminal.current_seqno();
    *last_rows = rows;
    *last_first_vis_stable = cur_first_vis_stable;

    Ok(())
}

/// Render all visible rows with full styling (no scrollback).
fn render_display_rows(
    env: &Env,
    screen: &wezterm_term::Screen,
    rows: usize,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
) -> Result<()> {
    for vis_row in 0..rows {
        let stable = screen.visible_row_to_stable_row(vis_row as i64);
        if let Some(phys) = screen.stable_row_to_phys(stable) {
            let lines = screen.lines_in_phys_range(phys..phys + 1);
            if let Some(line) = lines.first() {
                render_line(env, line, cols, palette, syms)?;
            }
        }
        if vis_row < rows - 1 {
            env.call(&syms.insert, ("\n",))?;
        }
    }
    Ok(())
}

/// Erase the display region (last `rows` lines) and redraw it, preserving
/// scrollback above.
fn redraw_display_region(
    env: &Env,
    screen: &wezterm_term::Screen,
    rows: usize,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
    scrollback_in_buffer: usize,
) -> Result<()> {
    // Navigate to start of display region.
    env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
    env.call(&syms.forward_line, (scrollback_in_buffer as i64,))?;
    let display_start = env.call(&syms.point, [])?;
    let buf_end = env.call(&syms.point_max, [])?;
    env.call(&syms.delete_region, (display_start, buf_end))?;

    // Re-render all visible rows.
    render_display_rows(env, screen, rows, cols, palette, syms)?;
    Ok(())
}

/// Update only the dirty rows in the display region.
fn incremental_render_display(
    env: &Env,
    screen: &wezterm_term::Screen,
    rows: usize,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
    dirty: &[isize],
    first_stable: isize,
    scrollback_in_buffer: usize,
) -> Result<()> {
    let mut vis_rows: Vec<usize> = dirty
        .iter()
        .map(|s| (s - first_stable) as usize)
        .filter(|&v| v < rows)
        .collect();
    vis_rows.sort_unstable();
    vis_rows.dedup();

    // Navigate to the start of the display region.
    env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
    env.call(&syms.forward_line, (scrollback_in_buffer as i64,))?;
    let mut current_line: i64 = 0;

    for &vis_row in &vis_rows {
        let delta = vis_row as i64 - current_line;
        env.call(&syms.forward_line, (delta,))?;
        current_line = vis_row as i64;

        let lbeg = env.call(&syms.line_beginning_position, [])?;
        let lend = env.call(&syms.line_end_position, [])?;
        env.call(&syms.delete_region, (lbeg, lend))?;

        let stable = screen.visible_row_to_stable_row(vis_row as i64);
        if let Some(phys) = screen.stable_row_to_phys(stable) {
            let lines = screen.lines_in_phys_range(phys..phys + 1);
            if let Some(line) = lines.first() {
                render_line(env, line, cols, palette, syms)?;
            }
        }
    }
    Ok(())
}

/// Compute the character offset within a rendered line for cursor placement.
/// Wide characters (width > 1 cell) produce a single rendered character, so
/// the character index may differ from the cell column.
fn cursor_char_offset(
    screen: &wezterm_term::Screen,
    cursor_y: i64,
    cursor_x: usize,
    cols: usize,
) -> usize {
    let stable = screen.visible_row_to_stable_row(cursor_y);
    let phys = match screen.stable_row_to_phys(stable) {
        Some(p) => p,
        None => return 0,
    };
    let lines = screen.lines_in_phys_range(phys..phys + 1);
    let line = match lines.first() {
        Some(l) => l,
        None => return 0,
    };

    let mut char_idx = 0usize;
    for col in 0..cols {
        if col == cursor_x {
            return char_idx;
        }
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue;
            }
        }
        char_idx += 1;
    }
    char_idx
}

// ---------------------------------------------------------------------------
// Per-line rendering
// ---------------------------------------------------------------------------

struct StyledRun {
    text: String,
    face_key: FaceKey,
    has_style: bool,
    hyperlink_uri: Option<String>,
}

/// Find the last column with non-space content or non-default background.
/// Trailing blanks past this point are rendered unstyled to prevent
/// underline / strikethrough from extending across the full width.
fn last_content_col(line: &wezterm_surface::Line, cols: usize) -> usize {
    let mut last = 0;
    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue;
            }
            let s = cell.str();
            let is_blank = s.is_empty() || s == " ";
            let has_bg = cell.attrs().background() != ColorAttribute::Default;
            if !is_blank || has_bg {
                last = col + 1;
            }
        }
    }
    last
}

/// Render a single line at the current buffer position.
///
/// Collects styled runs, builds propertized strings via the face cache,
/// and inserts everything with a single `(insert …)` call.
fn render_line(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
) -> Result<()> {
    let content_end = last_content_col(line, cols);
    let runs = collect_runs(line, cols, content_end, palette);

    if runs.is_empty() {
        let blanks = " ".repeat(cols);
        env.call(&syms.insert, (blanks.as_str(),))?;
        return Ok(());
    }

    // Resolve the hyperlink keymap once if any run carries a link.
    let hyperlink_km = if runs.iter().any(|r| r.hyperlink_uri.is_some()) {
        Some(env.call(&syms.symbol_value, (&syms.ebb_hyperlink_map,))?)
    } else {
        None
    };

    let mut insert_args: Vec<Value> = Vec::with_capacity(runs.len());

    for run in &runs {
        let needs_props = run.has_style || run.hyperlink_uri.is_some();

        if needs_props {
            // Build (propertize TEXT prop val …) argument vector.
            let mut pargs: Vec<Value> = Vec::with_capacity(14);
            pargs.push(run.text.as_str().into_lisp(env)?);

            if run.has_style {
                pargs.push(syms.face.bind(env));
                pargs.push(build_face_plist(env, &run.face_key, syms)?);
            }

            if let Some(ref uri) = run.hyperlink_uri {
                pargs.push(syms.ebb_url.bind(env));
                pargs.push(uri.as_str().into_lisp(env)?);
                pargs.push(syms.help_echo.bind(env));
                pargs.push(uri.as_str().into_lisp(env)?);
                pargs.push(syms.mouse_face.bind(env));
                pargs.push(syms.highlight.bind(env));
                if let Some(km) = hyperlink_km {
                    pargs.push(syms.keymap.bind(env));
                    pargs.push(km);
                }
            }

            // Prevent face properties from bleeding into adjacent
            // unstyled text via Emacs rear-stickiness.
            pargs.push(syms.rear_nonsticky.bind(env));
            pargs.push(syms.t_val.bind(env));

            insert_args.push(env.call(&syms.propertize, &pargs[..])?);
        } else {
            insert_args.push(run.text.as_str().into_lisp(env)?);
        }
    }

    // Single insert call for the whole line.
    env.call(&syms.insert, &insert_args[..])?;
    Ok(())
}

/// Collect styled runs from a terminal line (pure data, no FFI).
fn collect_runs(
    line: &wezterm_surface::Line,
    cols: usize,
    content_end: usize,
    palette: &ColorPalette,
) -> Vec<StyledRun> {
    let mut runs: Vec<StyledRun> = Vec::new();
    let mut text = String::new();
    let mut cur_key: Option<FaceKey> = None;
    let mut cur_style = false;
    let mut cur_link: Option<String> = None;

    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue;
            }

            // Past the last visible content: treat trailing blanks as unstyled
            // so underline/strikethrough don't extend to the edge.
            let (attrs, link_uri) = if col >= content_end {
                (CellAttributes::default(), None)
            } else {
                (
                    cell.attrs().clone(),
                    cell.attrs().hyperlink().map(|h| h.uri().to_string()),
                )
            };

            let (key, has_style) = make_face_key(&attrs, palette);
            let same = cur_key.as_ref().map_or(false, |k| *k == key) && cur_link == link_uri;

            if same {
                let s = cell.str();
                if s.is_empty() {
                    text.push(' ');
                } else {
                    text.push_str(s);
                }
            } else {
                // Flush previous run
                if !text.is_empty() {
                    runs.push(StyledRun {
                        text: std::mem::take(&mut text),
                        face_key: cur_key.take().unwrap_or_else(FaceKey::default_unstyled),
                        has_style: cur_style,
                        hyperlink_uri: cur_link.take(),
                    });
                }
                cur_key = Some(key);
                cur_style = has_style;
                cur_link = link_uri;
                let s = cell.str();
                if s.is_empty() {
                    text.push(' ');
                } else {
                    text.push_str(s);
                }
            }
        } else {
            // Cell doesn't exist (sparse line) — treat as unstyled space.
            // Must break the current run if it carries any styling,
            // otherwise underline/strikethrough bleeds into trailing blanks.
            let default_key = FaceKey::default_unstyled();
            let same = cur_key.as_ref().map_or(false, |k| *k == default_key) && cur_link.is_none();
            if same {
                text.push(' ');
            } else {
                if !text.is_empty() {
                    runs.push(StyledRun {
                        text: std::mem::take(&mut text),
                        face_key: cur_key.take().unwrap_or_else(FaceKey::default_unstyled),
                        has_style: cur_style,
                        hyperlink_uri: cur_link.take(),
                    });
                }
                cur_key = Some(default_key);
                cur_style = false;
                cur_link = None;
                text.push(' ');
            }
        }
    }

    if !text.is_empty() {
        runs.push(StyledRun {
            text,
            face_key: cur_key.unwrap_or_else(FaceKey::default_unstyled),
            has_style: cur_style,
            hyperlink_uri: cur_link,
        });
    }
    runs
}
