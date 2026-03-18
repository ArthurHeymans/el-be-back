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

    // --- Step 3: Render visible rows ---
    for vis_row in 0..rows {
        let stable = screen.visible_row_to_stable_row(vis_row as i64);
        if let Some(phys) = screen.stable_row_to_phys(stable) {
            let lines = screen.lines_in_phys_range(phys..phys + 1);
            if let Some(line) = lines.first() {
                render_line(env, line, cols, &palette)?;
            }
        }
        if vis_row < rows - 1 {
            env.call("insert", ("\n",))?;
        }
    }

    // --- Step 4: Position cursor ---
    let cursor = term.terminal.cursor_pos();
    // Cursor is relative to the display region
    env.call("goto-char", (display_start,))?;
    if cursor.y > 0 {
        env.call("forward-line", (cursor.y as i64,))?;
    }
    if cursor.x > 0 {
        env.call("move-to-column", (cursor.x as i64,))?;
    }

    Ok(())
}

/// Render a single line by accumulating style runs and inserting with faces.
fn render_line(
    env: &Env,
    line: &wezterm_surface::Line,
    cols: usize,
    palette: &ColorPalette,
) -> Result<()> {
    // Collect runs: sequences of cells with identical attributes
    let mut runs: Vec<(String, CellAttributes)> = Vec::new();
    let mut current_text = String::new();
    let mut current_attrs: Option<CellAttributes> = None;

    for col in 0..cols {
        if let Some(cell) = line.get_cell(col) {
            let attrs = cell.attrs().clone();

            let same = current_attrs.as_ref().map_or(false, |ca| {
                ca.attribute_bits_equal(&attrs)
                    && ca.foreground() == attrs.foreground()
                    && ca.background() == attrs.background()
            });

            if same {
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
                        runs.push((std::mem::take(&mut current_text), a));
                    }
                }
                current_attrs = Some(attrs);
                let s = cell.str();
                if s.is_empty() {
                    current_text.push(' ');
                } else {
                    current_text.push_str(s);
                }
            }
        } else {
            // Past end of line data -- space with current attrs
            current_text.push(' ');
        }
    }

    // Flush final run
    if !current_text.is_empty() {
        if let Some(a) = current_attrs.take() {
            runs.push((current_text, a));
        } else {
            runs.push((current_text, CellAttributes::default()));
        }
    }

    // Insert each run with face properties
    for (text, attrs) in &runs {
        let has_style = attrs.foreground() != ColorAttribute::Default
            || attrs.background() != ColorAttribute::Default
            || attrs.intensity() != Intensity::Normal
            || attrs.italic()
            || attrs.underline() != wezterm_escape_parser::csi::Underline::None
            || attrs.strikethrough()
            || attrs.reverse();

        if !has_style {
            // Default style - just insert
            env.call("insert", (text.as_str(),))?;
        } else {
            // Build face plist
            let face_str = build_face_string(attrs, palette);
            let start = env.call("point", [])?;
            env.call("insert", (text.as_str(),))?;
            let end = env.call("point", [])?;
            // Apply face via (put-text-property START END 'face FACE)
            let face_val = env.call("read", (face_str.as_str(),))?;
            let face_sym = env.intern("face")?;
            env.call("put-text-property", (start, end, face_sym, face_val))?;
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
