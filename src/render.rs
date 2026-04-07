use std::collections::HashMap;
use std::sync::{Mutex, OnceLock};

use emacs::raw::emacs_value;
use emacs::{Env, GlobalRef, IntoLisp, Result, Value};
use wezterm_cell::color::{ColorAttribute, SrgbaTuple};
use wezterm_cell::CellAttributes;
use wezterm_escape_parser::csi::Intensity;
use wezterm_term::color::ColorPalette;

use crate::EbbTerminal;

// ---------------------------------------------------------------------------
// Pre-interned Emacs symbols -- allocated once at module init, reused every
// frame.
// ---------------------------------------------------------------------------

pub(crate) struct Syms {
    // Functions
    insert: GlobalRef,
    erase_buffer: GlobalRef,
    goto_char: GlobalRef,
    point_min: GlobalRef,
    forward_line: GlobalRef,
    line_beginning_position: GlobalRef,
    line_end_position: GlobalRef,
    delete_region: GlobalRef,
    symbol_value: GlobalRef,
    put_text_property: GlobalRef,
    list: GlobalRef,
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
    // Additional functions for scrollback
    point: GlobalRef,
    point_max: GlobalRef,
}

static SYMS: OnceLock<Syms> = OnceLock::new();

// ---------------------------------------------------------------------------
// Pre-interned color strings (O10): the 256-color palette as "#rrggbb"
// GlobalRefs.  Built once at init, avoids all per-frame format!() heap allocs.
// ---------------------------------------------------------------------------

static COLOR_STRINGS: OnceLock<Vec<GlobalRef>> = OnceLock::new();

/// Pre-build GlobalRef strings for all 256 palette entries using the default
/// xterm palette.  These are reused every frame with zero allocation.
fn init_color_strings(env: &Env) -> Result<()> {
    if COLOR_STRINGS.get().is_some() {
        return Ok(());
    }
    let palette = ColorPalette::default();
    let mut strings = Vec::with_capacity(256);
    for idx in 0u16..256 {
        let c = palette.resolve_fg(ColorAttribute::PaletteIndex(idx as u8));
        let r = (c.0 * 255.0).round() as u8;
        let g = (c.1 * 255.0).round() as u8;
        let b = (c.2 * 255.0).round() as u8;
        let mut buf = [0u8; 7];
        buf[0] = b'#';
        hex_byte(r, &mut buf[1..3]);
        hex_byte(g, &mut buf[3..5]);
        hex_byte(b, &mut buf[5..7]);
        let s = std::str::from_utf8(&buf).unwrap();
        strings.push(s.into_lisp(env)?.make_global_ref());
    }
    let _ = COLOR_STRINGS.set(strings);
    Ok(())
}

pub(crate) fn init_syms(env: &Env) -> Result<()> {
    if SYMS.get().is_some() {
        return Ok(());
    }
    let syms = Syms {
        insert: env.intern("insert")?.make_global_ref(),
        erase_buffer: env.intern("erase-buffer")?.make_global_ref(),
        goto_char: env.intern("goto-char")?.make_global_ref(),
        point_min: env.intern("point-min")?.make_global_ref(),
        forward_line: env.intern("forward-line")?.make_global_ref(),
        line_beginning_position: env.intern("line-beginning-position")?.make_global_ref(),
        line_end_position: env.intern("line-end-position")?.make_global_ref(),
        delete_region: env.intern("delete-region")?.make_global_ref(),
        symbol_value: env.intern("symbol-value")?.make_global_ref(),
        put_text_property: env.intern("put-text-property")?.make_global_ref(),
        list: env.intern("list")?.make_global_ref(),

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
        point: env.intern("point")?.make_global_ref(),
        point_max: env.intern("point-max")?.make_global_ref(),
    };
    let _ = SYMS.set(syms);
    init_color_strings(env)?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Face key -- compact representation for cache lookup
// ---------------------------------------------------------------------------

#[derive(Hash, Eq, PartialEq, Clone, Copy)]
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

// ---------------------------------------------------------------------------
// Face cache (O6): static Mutex<HashMap> of GlobalRefs keyed by FaceKey.
// GlobalRefs are rooted and persist across frames, so identical face
// combinations (e.g. "green on black, bold") are built once and reused
// forever.  The Mutex is locked briefly per render_line call.
// ---------------------------------------------------------------------------

static FACE_CACHE: OnceLock<Mutex<HashMap<FaceKey, GlobalRef>>> = OnceLock::new();

fn face_cache() -> &'static Mutex<HashMap<FaceKey, GlobalRef>> {
    FACE_CACHE.get_or_init(|| Mutex::new(HashMap::with_capacity(64)))
}

/// Extract the raw emacs_value from a GlobalRef.
/// This is safe because GlobalRef is #[repr(transparent)] over emacs_value.
#[inline]
fn global_ref_raw(gref: &GlobalRef) -> emacs_value {
    // Safety: GlobalRef is #[repr(transparent)] wrapping emacs_value.
    unsafe { std::ptr::read(gref as *const GlobalRef as *const emacs_value) }
}

/// Look up or build a face plist for the given FaceKey.
/// Uses the global face cache to avoid rebuilding identical plists.
///
/// Safety: We extract the raw `emacs_value` from the GlobalRef while
/// holding the MutexGuard, then create a Value from it after dropping
/// the guard.  This is safe because:
/// 1. The GlobalRef keeps the Lisp object rooted (prevents GC)
/// 2. We never remove entries from FACE_CACHE
/// 3. The raw emacs_value is valid for the entire Emacs session
fn get_or_build_face<'e>(env: &'e Env, key: &FaceKey, syms: &'e Syms) -> Result<Value<'e>> {
    let cache = face_cache();
    // Fast path: look up cached face, extract raw pointer, drop lock.
    {
        let guard = cache.lock().unwrap();
        if let Some(gref) = guard.get(key) {
            let raw = global_ref_raw(gref);
            drop(guard);
            // Safety: The GlobalRef in FACE_CACHE keeps this value rooted.
            return Ok(unsafe { Value::new(raw, env) });
        }
    }
    // Slow path: build, cache, and return
    let face = build_face_plist(env, key, syms)?;
    let gref = face.make_global_ref();
    let raw = global_ref_raw(&gref);
    cache.lock().unwrap().entry(*key).or_insert(gref);
    // Safety: Same as above — the GlobalRef is now in FACE_CACHE.
    Ok(unsafe { Value::new(raw, env) })
}

/// Build a face plist like (:foreground "#rrggbb" :weight bold ...).
fn build_face_plist<'e>(env: &'e Env, key: &FaceKey, syms: &'e Syms) -> Result<Value<'e>> {
    let mut p: Vec<Value<'e>> = Vec::with_capacity(14);

    if let Some(rgb) = key.fg {
        p.push(syms.kw_foreground.bind(env));
        p.push(make_color_value(env, rgb)?);
    }
    if let Some(rgb) = key.bg {
        p.push(syms.kw_background.bind(env));
        p.push(make_color_value(env, rgb)?);
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
    env.call(&syms.list, &p[..])
}

/// Build an Emacs color string "#rrggbb" from RGB bytes.
/// Uses pre-interned palette strings (O10) when the color matches a
/// palette entry, otherwise formats on the stack (O3) — no heap alloc.
#[inline]
fn make_color_value<'e>(env: &'e Env, rgb: [u8; 3]) -> Result<Value<'e>> {
    // Check pre-interned palette strings first
    if let Some(colors) = COLOR_STRINGS.get() {
        let palette = ColorPalette::default();
        // Quick check for exact palette match (common for 16 standard colors)
        for idx in 0..16u8 {
            let c = palette.resolve_fg(ColorAttribute::PaletteIndex(idx));
            let pr = srgba_to_rgb(c);
            if pr == rgb {
                return Ok(colors[idx as usize].bind(env));
            }
        }
    }
    // Stack-format for non-palette colors (O3)
    let mut buf = [0u8; 7];
    buf[0] = b'#';
    hex_byte(rgb[0], &mut buf[1..3]);
    hex_byte(rgb[1], &mut buf[3..5]);
    hex_byte(rgb[2], &mut buf[5..7]);
    let s = std::str::from_utf8(&buf).unwrap();
    s.into_lisp(env)
}

#[inline]
fn hex_byte(b: u8, out: &mut [u8]) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    out[0] = HEX[(b >> 4) as usize];
    out[1] = HEX[(b & 0x0f) as usize];
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/// Feed PTY data and render in a single FFI call (O1).
/// This avoids the Elisp→Rust→Elisp→Rust round-trip of separate feed+render.
pub fn feed_and_render(env: &Env, term: &mut EbbTerminal, bytes: &str) -> Result<()> {
    if term.freed {
        return Ok(());
    }
    term.terminal.advance_bytes(bytes);
    render_to_buffer(env, term)
}

/// Render the terminal screen into the current Emacs buffer.
///
/// Uses insert-then-propertize strategy (O2): insert plain text first,
/// then apply face properties via put-text-property on ranges.
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

    // Face cache is now a global static (O6) — no per-frame allocation.

    let scrollback_count = screen.scrollback_rows().saturating_sub(rows);
    let cur_first_vis_stable = screen.visible_row_to_stable_row(0);
    let needs_full = *last_rows != rows || *last_seqno == 0;

    if needs_full {
        // ---- Full render ----
        env.call(&syms.erase_buffer, [])?;

        if scrollback_count > 0 {
            for phys in 0..scrollback_count {
                let lines = screen.lines_in_phys_range(phys..phys + 1);
                if let Some(line) = lines.first() {
                    render_line(env, line, cols, &palette, syms)?;
                }
                env.call(&syms.insert, ("\n",))?;
            }
        }

        render_display_rows(env, screen, rows, cols, &palette, syms)?;
        *scrollback_in_buffer = scrollback_count;
    } else {
        // ---- Incremental render ----
        let new_sb = (cur_first_vis_stable - *last_first_vis_stable).max(0) as usize;

        if new_sb > 0 && new_sb < rows {
            *scrollback_in_buffer += new_sb;

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
            *scrollback_in_buffer += rows;

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

            env.call(&syms.goto_char, (env.call(&syms.point_max, [])?,))?;
            env.call(&syms.insert, ("\n",))?;
            render_display_rows(env, screen, rows, cols, &palette, syms)?;
        } else {
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

        // Trim scrollback
        if *scrollback_in_buffer > max_scrollback {
            let excess = *scrollback_in_buffer - max_scrollback;
            env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
            env.call(&syms.forward_line, (excess as i64,))?;
            let cut = env.call(&syms.point, [])?;
            env.call(&syms.delete_region, (env.call(&syms.point_min, [])?, cut))?;
            *scrollback_in_buffer = max_scrollback;
        }
    }

    // ---- Position cursor ----
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

fn redraw_display_region(
    env: &Env,
    screen: &wezterm_term::Screen,
    rows: usize,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
    scrollback_in_buffer: usize,
) -> Result<()> {
    env.call(&syms.goto_char, (env.call(&syms.point_min, [])?,))?;
    env.call(&syms.forward_line, (scrollback_in_buffer as i64,))?;
    let display_start = env.call(&syms.point, [])?;
    let buf_end = env.call(&syms.point_max, [])?;
    env.call(&syms.delete_region, (display_start, buf_end))?;
    render_display_rows(env, screen, rows, cols, palette, syms)?;
    Ok(())
}

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
// Per-line rendering — insert-then-propertize strategy (O2)
//
// 1. Build the entire row text as a single String (one heap alloc)
// 2. Collect style runs as (byte_start, byte_end, FaceKey) tuples
// 3. Insert the text with a single env.call(insert, text)
// 4. Apply face properties via put-text-property on ranges
//
// This replaces the old propertize-per-run approach which required N
// funcalls to `propertize` plus N `into_lisp` string conversions.
// ---------------------------------------------------------------------------

struct StyleRun {
    /// Byte offset in the row text string (for Emacs char position calc)
    char_start: usize,
    char_end: usize,
    face_key: FaceKey,
    hyperlink_uri: Option<String>,
}

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

/// Render a single line using insert-then-propertize (O2).
fn render_line(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
    syms: &Syms,
) -> Result<()> {
    let content_end = last_content_col(line, cols);

    // Phase 1: Build row text and collect style runs (pure Rust, zero FFI)
    let mut text = String::with_capacity(cols + 16);
    let mut runs: Vec<StyleRun> = Vec::new();
    let mut char_pos: usize = 0;

    let mut cur_key = FaceKey::default_unstyled();
    let mut cur_styled = false;
    let mut cur_link: Option<String> = None;
    let mut run_start: usize = 0;

    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue;
            }

            let (attrs, link_uri) = if col >= content_end {
                (CellAttributes::default(), None)
            } else {
                (
                    cell.attrs().clone(),
                    cell.attrs().hyperlink().map(|h| h.uri().to_string()),
                )
            };

            let (key, has_style) = make_face_key(&attrs, palette);
            let same = key == cur_key && link_uri == cur_link;

            if !same {
                // Flush previous run
                if char_pos > run_start && (cur_styled || cur_link.is_some()) {
                    runs.push(StyleRun {
                        char_start: run_start,
                        char_end: char_pos,
                        face_key: cur_key,
                        hyperlink_uri: cur_link.take(),
                    });
                }
                cur_key = key;
                cur_styled = has_style;
                cur_link = link_uri;
                run_start = char_pos;
            }

            let s = cell.str();
            if s.is_empty() {
                text.push(' ');
            } else {
                text.push_str(s);
            }
            char_pos += 1;
        } else {
            // Sparse cell — unstyled space
            let default_key = FaceKey::default_unstyled();
            if cur_key != default_key || cur_link.is_some() {
                if char_pos > run_start && (cur_styled || cur_link.is_some()) {
                    runs.push(StyleRun {
                        char_start: run_start,
                        char_end: char_pos,
                        face_key: cur_key,
                        hyperlink_uri: cur_link.take(),
                    });
                }
                cur_key = default_key;
                cur_styled = false;
                cur_link = None;
                run_start = char_pos;
            }
            text.push(' ');
            char_pos += 1;
        }
    }

    // Flush final run
    if char_pos > run_start && (cur_styled || cur_link.is_some()) {
        runs.push(StyleRun {
            char_start: run_start,
            char_end: char_pos,
            face_key: cur_key,
            hyperlink_uri: cur_link,
        });
    }

    // Phase 2: Insert the plain text with a single funcall
    // Record point before insert so we can compute property ranges
    let insert_point = env.call(&syms.point, [])?.into_rust::<i64>()?;
    env.call(&syms.insert, (text.as_str(),))?;

    // Phase 3: Apply face properties via put-text-property on ranges (O2)
    if !runs.is_empty() {
        // Resolve hyperlink keymap once if needed
        let hyperlink_km = if runs.iter().any(|r| r.hyperlink_uri.is_some()) {
            Some(env.call(&syms.symbol_value, (&syms.ebb_hyperlink_map,))?)
        } else {
            None
        };

        for run in &runs {
            let start = insert_point + run.char_start as i64;
            let end = insert_point + run.char_end as i64;
            let start_v = start.into_lisp(env)?;
            let end_v = end.into_lisp(env)?;

            // Apply face
            let face = get_or_build_face(env, &run.face_key, syms)?;
            env.call(
                &syms.put_text_property,
                (start_v, end_v, syms.face.bind(env), face),
            )?;

            // Apply hyperlink properties
            if let Some(ref uri) = run.hyperlink_uri {
                let uri_v = uri.as_str().into_lisp(env)?;
                env.call(
                    &syms.put_text_property,
                    (start_v, end_v, syms.ebb_url.bind(env), uri_v),
                )?;
                env.call(
                    &syms.put_text_property,
                    (start_v, end_v, syms.help_echo.bind(env), uri_v),
                )?;
                env.call(
                    &syms.put_text_property,
                    (
                        start_v,
                        end_v,
                        syms.mouse_face.bind(env),
                        syms.highlight.bind(env),
                    ),
                )?;
                if let Some(km) = hyperlink_km {
                    env.call(
                        &syms.put_text_property,
                        (start_v, end_v, syms.keymap.bind(env), km),
                    )?;
                }
            }
        }
    }

    Ok(())
}
