use emacs::{Env, Result};
use wezterm_cell::color::{ColorAttribute, SrgbaTuple};
use wezterm_cell::CellAttributes;
use wezterm_escape_parser::csi::Intensity;
use wezterm_term::color::ColorPalette;

use crate::EbbTerminal;

/// Render the terminal screen into the current Emacs buffer.
///
/// Erases the display region and re-renders all visible rows with faces.
/// Tracks cursor position during rendering for accurate placement
/// despite character width mismatches between wezterm and Emacs.
pub fn render_to_buffer(env: &Env, term: &mut EbbTerminal) -> Result<()> {
    if term.freed {
        return Ok(());
    }

    let screen = term.terminal.screen();
    let rows = screen.physical_rows;
    let cols = screen.physical_cols;
    let palette = term.terminal.palette();
    let cursor = term.terminal.cursor_pos();

    // Erase buffer and re-render visible rows
    env.call("erase-buffer", [])?;

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

    // Position cursor
    if let Some(pos) = cursor_buf_pos {
        env.call("goto-char", (pos,))?;
    } else {
        env.call("goto-char", (env.call("point-min", [])?,))?;
    }

    Ok(())
}

/// Render a line and return the buffer position corresponding to `cursor_col`.
fn render_line_with_cursor(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
    cursor_col: i64,
) -> Result<i64> {
    // Build mapping: cell column -> character index in rendered output
    let mut char_entries: Vec<usize> = Vec::new(); // cell_col for each rendered char
    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            if cell.width() == 0 {
                continue;
            }
            char_entries.push(col);
        } else {
            char_entries.push(col);
        }
    }

    // Find which character index the cursor falls on
    let cursor_char_idx = char_entries
        .iter()
        .position(|&col| col >= cursor_col as usize)
        .unwrap_or(char_entries.len());

    // Render the line
    let line_start = env.call("point", [])?.into_rust::<i64>()?;
    render_line(env, line, cols, palette)?;

    // Calculate cursor position: count characters up to cursor_char_idx
    let cursor_pos = line_start + cursor_char_idx as i64;
    let line_end = env.call("point", [])?.into_rust::<i64>()?;
    Ok(cursor_pos.min(line_end).max(line_start))
}

/// A styled run of text with optional hyperlink.
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
    let mut runs: Vec<StyledRun> = Vec::new();
    let mut current_text = String::new();
    let mut current_attrs: Option<CellAttributes> = None;
    let mut current_link: Option<String> = None;

    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
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

    if !current_text.is_empty() {
        runs.push(StyledRun {
            text: current_text,
            attrs: current_attrs.unwrap_or_default(),
            hyperlink_uri: current_link,
        });
    }

    // Insert runs with face properties
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

        if has_style {
            let face_str = build_face_string(&run.attrs, palette);
            let face_val = env.call("read", (face_str.as_str(),))?;
            env.call("put-text-property", (start, end, face_sym, face_val))?;
        }

        if let Some(uri) = &run.hyperlink_uri {
            env.call("put-text-property", (start, end, ebb_url_sym, uri.as_str()))?;
            env.call(
                "put-text-property",
                (start, end, help_echo_sym, uri.as_str()),
            )?;
            env.call(
                "put-text-property",
                (start, end, mouse_face_sym, highlight_sym),
            )?;
            let km = env.call("symbol-value", (env.intern("ebb-hyperlink-map")?,))?;
            env.call("put-text-property", (start, end, keymap_sym, km))?;
        }
    }

    Ok(())
}

/// Build an Emacs face plist string.
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

fn srgba_to_hex(color: SrgbaTuple) -> String {
    let r = (color.0 * 255.0).round() as u8;
    let g = (color.1 * 255.0).round() as u8;
    let b = (color.2 * 255.0).round() as u8;
    format!("#{:02x}{:02x}{:02x}", r, g, b)
}
