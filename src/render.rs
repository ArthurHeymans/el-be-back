use emacs::{Env, Result};
use wezterm_cell::color::{ColorAttribute, SrgbaTuple};
use wezterm_cell::CellAttributes;
use wezterm_escape_parser::csi::Intensity;
use wezterm_term::color::ColorPalette;

use crate::EbbTerminal;

/// Render the terminal screen into the current Emacs buffer.
///
/// Buffer layout:
///   [scrollback lines with faces] ← permanent, above display region
///   [display line 0]              ← start of display region
///   [display line 1]
///   ...
///   [display line rows-1]         ← end of display region
///
/// On each call:
/// 1. Insert any new scrollback lines above the display region
/// 2. Erase the display region
/// 3. Re-render visible rows with faces
/// 4. Position cursor
pub fn render_to_buffer(env: &Env, term: &mut EbbTerminal) -> Result<()> {
    if term.freed {
        return Ok(());
    }

    let screen = term.terminal.screen();
    let rows = screen.physical_rows;
    let cols = screen.physical_cols;
    let palette = term.terminal.palette();
    let current_scrollback = screen.scrollback_rows();

    // --- Step 1: Insert new scrollback lines ---
    let new_scrollback = current_scrollback.saturating_sub(term.last_scrollback_count);
    if new_scrollback > 0 {
        // New scrollback lines are the lines just above the visible area.
        // They are at physical indices: (total - rows - new_scrollback) .. (total - rows)
        // But using scrollback_rows count: phys index
        //   old_scrollback_count .. current_scrollback
        let scroll_start = term.last_scrollback_count;
        let scroll_end = current_scrollback;

        // Go to the start of the display region (beginning of buffer for now,
        // or after existing scrollback)
        env.call("goto-char", (env.call("point-min", [])?,))?;
        // Skip past existing scrollback lines
        if term.last_scrollback_count > 0 {
            env.call("forward-line", (term.last_scrollback_count as i64,))?;
        }

        // Insert new scrollback lines at this position
        for sb_idx in scroll_start..scroll_end {
            let lines = screen.lines_in_phys_range(sb_idx..sb_idx + 1);
            if let Some(line) = lines.first() {
                render_line(env, line, cols, &palette)?;
                env.call("insert", ("\n",))?;
            }
        }

        term.last_scrollback_count = current_scrollback;
    }

    // --- Step 2: Erase display region ---
    // The display region starts after scrollback lines.
    env.call("goto-char", (env.call("point-min", [])?,))?;
    if term.last_scrollback_count > 0 {
        env.call("forward-line", (term.last_scrollback_count as i64,))?;
    }
    let display_start = env.call("point", [])?;
    let point_max = env.call("point-max", [])?;
    env.call("delete-region", (display_start, point_max))?;

    // --- Step 3: Render visible rows, tracking cursor position ---
    let cursor = term.terminal.cursor_pos();
    let mut cursor_buf_pos: Option<i64> = None;

    for vis_row in 0..rows {
        let is_cursor_row = vis_row as i64 == cursor.y;
        let stable = screen.visible_row_to_stable_row(vis_row as i64);
        if let Some(phys) = screen.stable_row_to_phys(stable) {
            let lines = screen.lines_in_phys_range(phys..phys + 1);
            if let Some(line) = lines.first() {
                if is_cursor_row {
                    cursor_buf_pos = Some(render_line_with_cursor(
                        env,
                        line,
                        cols,
                        &palette,
                        cursor.x as i64,
                    )?);
                } else {
                    render_line(env, line, cols, &palette)?;
                }
            }
        }
        if vis_row < rows - 1 {
            env.call("insert", ("\n",))?;
        }
    }

    // --- Step 4: Position cursor ---
    if let Some(pos) = cursor_buf_pos {
        env.call("goto-char", (pos,))?;
    } else {
        // Fallback: use display_start
        env.call("goto-char", (display_start,))?;
    }

    Ok(())
}

/// Render a line and return the buffer position corresponding to `cursor_col`.
/// This is used for the cursor row to get accurate cursor placement despite
/// character width mismatches between wezterm and Emacs.
fn render_line_with_cursor(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
    cursor_col: i64,
) -> Result<i64> {
    // First, build a mapping: wezterm cell column -> character index in the
    // rendered string. Then render the line and use the mapping to find the
    // buffer position.

    // Collect the rendered characters and their cell column origins
    let mut chars_with_cols: Vec<(usize, String)> = Vec::new(); // (cell_col, char_str)
    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue; // skip continuation cells
            }
            let s = cell.str();
            if s.is_empty() {
                chars_with_cols.push((col, " ".to_string()));
            } else {
                chars_with_cols.push((col, s.to_string()));
            }
        } else {
            chars_with_cols.push((col, " ".to_string()));
        }
    }

    // Find which character index the cursor falls on
    let mut cursor_char_idx = chars_with_cols.len(); // default: end of line
    for (i, (cell_col, _)) in chars_with_cols.iter().enumerate() {
        if *cell_col >= cursor_col as usize {
            cursor_char_idx = i;
            break;
        }
    }

    // Now render the line normally and record point at the cursor position
    let line_start = env.call("point", [])?.into_rust::<i64>()?;
    render_line(env, line, cols, palette)?;
    let line_end = env.call("point", [])?.into_rust::<i64>()?;

    // Calculate cursor buffer position: line_start + character offset
    // We need to count characters (not visual columns) up to cursor_char_idx
    let mut char_offset: i64 = 0;
    let _line_text_len = line_end - line_start;
    // Count actual characters inserted
    let total_chars: usize = chars_with_cols.iter().map(|(_, s)| s.chars().count()).sum();

    if total_chars > 0 && cursor_char_idx <= chars_with_cols.len() {
        let chars_before: usize = chars_with_cols[..cursor_char_idx]
            .iter()
            .map(|(_, s)| s.chars().count())
            .sum();
        char_offset = chars_before as i64;
    }

    // Clamp to line bounds
    let cursor_pos = (line_start + char_offset).min(line_end).max(line_start);
    Ok(cursor_pos)
}

/// A styled run of text: text content, cell attributes, and optional hyperlink URI.
struct StyledRun {
    text: String,
    attrs: CellAttributes,
    hyperlink_uri: Option<String>,
}

/// Render a single line by accumulating style runs and inserting with faces.
fn render_line(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
) -> Result<()> {
    // Collect runs: sequences of cells with identical attributes + hyperlink
    let mut runs: Vec<StyledRun> = Vec::new();
    let mut current_text = String::new();
    let mut current_attrs: Option<CellAttributes> = None;
    let mut current_link: Option<String> = None;

    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            // Skip continuation cells (width 0) -- the wide character from the
            // previous cell already occupies the right visual width in Emacs.
            if cell.width() == 0 {
                continue;
            }

            let attrs = cell.attrs().clone();
            let link_uri = attrs.hyperlink().map(|h| h.uri().to_string());

            let same_style = current_attrs.as_ref().map_or(false, |ca| {
                ca.attribute_bits_equal(&attrs)
                    && ca.foreground() == attrs.foreground()
                    && ca.background() == attrs.background()
            });
            let same_link = current_link == link_uri;

            if same_style && same_link {
                let s = cell.str();
                if s.is_empty() {
                    current_text.push(' ');
                } else {
                    current_text.push_str(s);
                }
            } else {
                // Flush previous run
                if !current_text.is_empty() {
                    if let Some(a) = current_attrs.take() {
                        runs.push(StyledRun {
                            text: std::mem::take(&mut current_text),
                            attrs: a,
                            hyperlink_uri: current_link.take(),
                        });
                    }
                }
                current_attrs = Some(attrs);
                current_link = link_uri;
                let s = cell.str();
                if s.is_empty() {
                    current_text.push(' ');
                } else {
                    current_text.push_str(s);
                }
            }
        } else {
            current_text.push(' ');
        }
    }

    // Flush final run
    if !current_text.is_empty() {
        runs.push(StyledRun {
            text: current_text,
            attrs: current_attrs.unwrap_or_default(),
            hyperlink_uri: current_link,
        });
    }

    // Insert each run with face properties and hyperlink buttons
    let face_sym = env.intern("face")?;
    let help_echo_sym = env.intern("help-echo")?;
    let mouse_face_sym = env.intern("mouse-face")?;
    let highlight_sym = env.intern("highlight")?;
    let ebb_url_sym = env.intern("ebb-url")?;
    let keymap_sym = env.intern("keymap")?;

    for run in &runs {
        let has_style = run.attrs.foreground() != ColorAttribute::Default
            || run.attrs.background() != ColorAttribute::Default
            || run.attrs.intensity() != Intensity::Normal
            || run.attrs.italic()
            || run.attrs.underline() != wezterm_escape_parser::csi::Underline::None
            || run.attrs.strikethrough()
            || run.attrs.reverse();

        let start = env.call("point", [])?;
        env.call("insert", (run.text.as_str(),))?;
        let end = env.call("point", [])?;

        // Apply face
        if has_style {
            let face_str = build_face_string(&run.attrs, palette);
            let face_val = env.call("read", (face_str.as_str(),))?;
            env.call("put-text-property", (start, end, face_sym, face_val))?;
        }

        // Apply hyperlink properties
        if let Some(uri) = &run.hyperlink_uri {
            // Store the URL as a text property
            env.call("put-text-property", (start, end, ebb_url_sym, uri.as_str()))?;
            // Add help-echo (tooltip on hover)
            env.call(
                "put-text-property",
                (start, end, help_echo_sym, uri.as_str()),
            )?;
            // Add mouse-face for hover highlight
            env.call(
                "put-text-property",
                (start, end, mouse_face_sym, highlight_sym),
            )?;
            // Add keymap for click action
            let km = env.call("symbol-value", (env.intern("ebb-hyperlink-map")?,))?;
            env.call("put-text-property", (start, end, keymap_sym, km))?;
        }
    }

    Ok(())
}

/// Build an Emacs face plist string like "(:foreground \"#ff0000\" :weight bold)"
fn build_face_string(attrs: &CellAttributes, palette: &ColorPalette) -> String {
    let mut parts = Vec::new();

    if let Some(hex) = resolve_color(attrs.foreground(), palette) {
        parts.push(format!(":foreground \"{}\"", hex));
    }

    if let Some(hex) = resolve_color(attrs.background(), palette) {
        parts.push(format!(":background \"{}\"", hex));
    }

    match attrs.intensity() {
        Intensity::Bold => parts.push(":weight bold".to_string()),
        Intensity::Half => parts.push(":weight light".to_string()),
        Intensity::Normal => {}
    }

    if attrs.italic() {
        parts.push(":slant italic".to_string());
    }

    if attrs.underline() != wezterm_escape_parser::csi::Underline::None {
        parts.push(":underline t".to_string());
    }

    if attrs.strikethrough() {
        parts.push(":strike-through t".to_string());
    }

    if attrs.reverse() {
        parts.push(":inverse-video t".to_string());
    }

    format!("({})", parts.join(" "))
}

/// Resolve a ColorAttribute to a hex string like "#rrggbb".
fn resolve_color(attr: ColorAttribute, palette: &ColorPalette) -> Option<String> {
    match attr {
        ColorAttribute::Default => None,
        ColorAttribute::PaletteIndex(idx) => {
            let color = palette.resolve_fg(ColorAttribute::PaletteIndex(idx));
            Some(srgba_to_hex(color))
        }
        ColorAttribute::TrueColorWithPaletteFallback(srgba, _) => Some(srgba_to_hex(srgba)),
        ColorAttribute::TrueColorWithDefaultFallback(srgba) => Some(srgba_to_hex(srgba)),
    }
}

/// Convert SrgbaTuple (0.0-1.0 floats) to "#rrggbb" hex string.
fn srgba_to_hex(color: SrgbaTuple) -> String {
    let r = (color.0 * 255.0).round() as u8;
    let g = (color.1 * 255.0).round() as u8;
    let b = (color.2 * 255.0).round() as u8;
    format!("#{:02x}{:02x}{:02x}", r, g, b)
}
