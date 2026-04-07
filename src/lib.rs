mod config;
mod input;
mod render;

use std::io;
use std::sync::{Arc, Mutex};

use emacs::{defun, Env, Result, Value};
use wezterm_surface::SequenceNo;
use wezterm_term::{Terminal, TerminalSize};

use config::{AlertQueue, EbbAlertSink, EbbConfig};

emacs::plugin_is_GPL_compatible!();

const VERSION: &str = "0.1.0";

#[emacs::module(name = "ebb-module", defun_prefix = "ebb", separator = "--")]
fn init(env: &Env) -> Result<()> {
    render::init_syms(env)?;
    env.message("[ebb] module loaded")?;
    Ok(())
}

// ---------------------------------------------------------------------------
// CapturingWriter: collects bytes written by wezterm-term's ThreadedWriter.
// ---------------------------------------------------------------------------

struct CapturingWriter {
    buf: Arc<Mutex<Vec<u8>>>,
}

impl io::Write for CapturingWriter {
    fn write(&mut self, data: &[u8]) -> io::Result<usize> {
        self.buf
            .lock()
            .map_err(|e| io::Error::new(io::ErrorKind::Other, e.to_string()))?
            .extend_from_slice(data);
        Ok(data.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// ---------------------------------------------------------------------------
// EbbTerminal: wraps wezterm-term::Terminal and associated state.
// ---------------------------------------------------------------------------

pub(crate) struct EbbTerminal {
    terminal: Terminal,
    output: Arc<Mutex<Vec<u8>>>,
    alerts: Arc<Mutex<AlertQueue>>,
    last_seqno: SequenceNo,
    last_rows: usize,
    /// StableRowIndex of the first visible line at last render.
    /// Used to detect how many new lines scrolled into scrollback.
    last_first_vis_stable: isize,
    /// Number of scrollback lines currently in the Emacs buffer.
    scrollback_in_buffer: usize,
    /// Maximum scrollback lines to keep in the buffer.
    max_scrollback: usize,
    freed: bool,
}

impl emacs::Transfer for EbbTerminal {
    fn type_name() -> &'static str {
        "EbbTerminal"
    }
}

impl EbbTerminal {
    fn new(rows: usize, cols: usize, scrollback: usize) -> Self {
        let output = Arc::new(Mutex::new(Vec::new()));
        let alerts = Arc::new(Mutex::new(AlertQueue::default()));

        let writer = CapturingWriter {
            buf: Arc::clone(&output),
        };

        let config = Arc::new(EbbConfig {
            scrollback_size: scrollback,
        });

        let size = TerminalSize {
            rows,
            cols,
            pixel_width: 0,
            pixel_height: 0,
            dpi: 0,
        };

        let mut terminal = Terminal::new(size, config, "el-be-back", VERSION, Box::new(writer));

        let alert_sink = EbbAlertSink {
            queue: Arc::clone(&alerts),
        };
        terminal.set_notification_handler(Box::new(alert_sink));

        EbbTerminal {
            terminal,
            output,
            alerts,
            last_seqno: 0,
            last_rows: 0,
            last_first_vis_stable: 0,
            scrollback_in_buffer: 0,
            max_scrollback: scrollback,
            freed: false,
        }
    }

    fn drain_output_bytes(&self) -> Vec<u8> {
        let mut buf = self.output.lock().unwrap();
        std::mem::take(&mut *buf)
    }

    fn screen_text(&self) -> String {
        let screen = self.terminal.screen();
        let rows = screen.physical_rows;
        let cols = screen.physical_cols;
        let mut text = String::with_capacity((cols + 1) * rows);

        for vis_row in 0..rows {
            let stable = screen.visible_row_to_stable_row(vis_row as i64);
            if let Some(phys) = screen.stable_row_to_phys(stable) {
                let lines = screen.lines_in_phys_range(phys..phys + 1);
                if let Some(line) = lines.first() {
                    for col in 0..cols {
                        if let Some(cell) = line.get_cell(col) {
                            // Skip continuation cells (wide char placeholders)
                            if cell.width() == 0 {
                                continue;
                            }
                            let s = cell.str();
                            if s.is_empty() {
                                text.push(' ');
                            } else {
                                text.push_str(s);
                            }
                        } else {
                            text.push(' ');
                        }
                    }
                } else {
                    for _ in 0..cols {
                        text.push(' ');
                    }
                }
            } else {
                for _ in 0..cols {
                    text.push(' ');
                }
            }
            text.push('\n');
        }
        text
    }
}

// ---------------------------------------------------------------------------
// Defuns: Lifecycle
// ---------------------------------------------------------------------------

/// Create a new terminal instance. Returns an opaque user-ptr.
#[defun(user_ptr)]
fn new(_env: &Env, rows: i64, cols: i64, scrollback: i64) -> Result<EbbTerminal> {
    Ok(EbbTerminal::new(
        rows.max(1) as usize,
        cols.max(1) as usize,
        scrollback.max(0) as usize,
    ))
}

/// Explicitly free a terminal instance.
#[defun]
fn free(term: &mut EbbTerminal) -> Result<()> {
    term.freed = true;
    Ok(())
}

// ---------------------------------------------------------------------------
// Defuns: I/O
// ---------------------------------------------------------------------------

/// Feed PTY output into the terminal's VT parser.
///
/// Takes a String because the `emacs` crate's FromLisp always goes through
/// copy_string_contents which UTF-8 encodes the Emacs string.  This is
/// correct for the common case: Emacs decodes UTF-8 PTY output to internal
/// representation, then re-encodes as UTF-8 for Rust, and advance_bytes
/// receives the original UTF-8 bytes.  The edge case of raw bytes 128-255
/// that aren't valid UTF-8 (extremely rare in modern terminals) would be
/// re-encoded as 2-byte UTF-8 sequences, but escape sequences are 7-bit
/// and binary payloads (sixel, kitty graphics) use base64.
#[defun]
fn feed(term: &mut EbbTerminal, bytes: String) -> Result<()> {
    if term.freed {
        return Ok(());
    }
    term.terminal.advance_bytes(bytes);
    Ok(())
}

/// Drain captured output bytes (terminal responses, key encodings).
/// Returns a string or nil if empty.
#[defun]
fn drain_output(term: &EbbTerminal) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    let bytes = term.drain_output_bytes();
    if bytes.is_empty() {
        Ok(None)
    } else {
        Ok(Some(String::from_utf8_lossy(&bytes).into_owned()))
    }
}

/// Return the visible screen content as a plain text string (debug/render).
#[defun]
fn content(term: &EbbTerminal) -> Result<String> {
    if term.freed {
        return Ok(String::new());
    }
    Ok(term.screen_text())
}

/// Debug: return info about cell colors on a given visible row.
/// Returns a string describing the first few non-default-colored cells.
#[defun]
fn debug_row_attrs(term: &EbbTerminal, vis_row: i64) -> Result<String> {
    if term.freed {
        return Ok("freed".to_string());
    }
    let screen = term.terminal.screen();
    let cols = screen.physical_cols;
    let stable = screen.visible_row_to_stable_row(vis_row);
    let phys = match screen.stable_row_to_phys(stable) {
        Some(p) => p,
        None => return Ok(format!("row {} not found", vis_row)),
    };
    let lines = screen.lines_in_phys_range(phys..phys + 1);
    let line = match lines.first() {
        Some(l) => l,
        None => return Ok("no line data".to_string()),
    };
    let mut result = String::new();
    let mut styled_count = 0;
    for col in 0..cols.min(120) {
        if let Some(cell) = line.get_cell(col) {
            use wezterm_cell::color::ColorAttribute;
            let fg = cell.attrs().foreground();
            let bg = cell.attrs().background();
            if fg != ColorAttribute::Default || bg != ColorAttribute::Default {
                styled_count += 1;
                if styled_count <= 5 {
                    result.push_str(&format!(
                        "col{}='{}' fg={:?} bg={:?}; ",
                        col,
                        cell.str(),
                        fg,
                        bg
                    ));
                }
            }
        }
    }
    result.push_str(&format!("total_styled={}/{}", styled_count, cols.min(120)));
    Ok(result)
}

// ---------------------------------------------------------------------------
// Defuns: State queries
// ---------------------------------------------------------------------------

/// Return terminal rows.
#[defun]
fn get_rows(term: &EbbTerminal) -> Result<i64> {
    if term.freed {
        return Ok(0);
    }
    Ok(term.terminal.get_size().rows as i64)
}

/// Return terminal cols.
#[defun]
fn get_cols(term: &EbbTerminal) -> Result<i64> {
    if term.freed {
        return Ok(0);
    }
    Ok(term.terminal.get_size().cols as i64)
}

/// Return cursor row (0-based).
#[defun]
fn cursor_row(term: &EbbTerminal) -> Result<i64> {
    if term.freed {
        return Ok(0);
    }
    Ok(term.terminal.cursor_pos().y as i64)
}

/// Return cursor column (0-based).
#[defun]
fn cursor_col(term: &EbbTerminal) -> Result<i64> {
    if term.freed {
        return Ok(0);
    }
    Ok(term.terminal.cursor_pos().x as i64)
}

/// Return the terminal title (from OSC 2) or nil.
#[defun]
fn get_title(term: &EbbTerminal) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    let title = term.terminal.get_title();
    if title.is_empty() {
        Ok(None)
    } else {
        Ok(Some(title.to_string()))
    }
}

/// Return the current working directory (from OSC 7) or nil.
#[defun]
fn get_cwd(term: &EbbTerminal) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    Ok(term.terminal.get_current_dir().map(|u| u.to_string()))
}

/// Check if alternate screen is active.
#[defun]
fn is_alt_screen(term: &EbbTerminal) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    Ok(term.terminal.is_alt_screen_active())
}

/// Check if the terminal has grabbed the mouse.
#[defun]
fn is_mouse_grabbed(term: &EbbTerminal) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    Ok(term.terminal.is_mouse_grabbed())
}

/// Check if bracketed paste mode is enabled.
#[defun]
fn bracketed_paste_enabled(term: &EbbTerminal) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    Ok(term.terminal.bracketed_paste_enabled())
}

/// Return the cursor style as an integer.
/// 0=bar, 1=block, 2=underline.
/// Maps DECSCUSR cursor shapes to a simple integer for Elisp.
#[defun]
fn cursor_style(term: &EbbTerminal) -> Result<i64> {
    if term.freed {
        return Ok(1); // default block
    }
    use wezterm_surface::CursorShape;
    let shape = term.terminal.cursor_pos().shape;
    Ok(match shape {
        CursorShape::Default | CursorShape::BlinkingBlock | CursorShape::SteadyBlock => 1,
        CursorShape::BlinkingBar | CursorShape::SteadyBar => 0,
        CursorShape::BlinkingUnderline | CursorShape::SteadyUnderline => 2,
    })
}

/// Return whether the cursor is visible.
#[defun]
fn cursor_visible(term: &EbbTerminal) -> Result<bool> {
    if term.freed {
        return Ok(true);
    }
    use wezterm_surface::CursorVisibility;
    Ok(term.terminal.cursor_pos().visibility == CursorVisibility::Visible)
}

/// Resize the terminal.
#[defun]
fn resize(term: &mut EbbTerminal, rows: i64, cols: i64) -> Result<()> {
    if term.freed {
        return Ok(());
    }
    let size = TerminalSize {
        rows: rows.max(1) as usize,
        cols: cols.max(1) as usize,
        pixel_width: 0,
        pixel_height: 0,
        dpi: 0,
    };
    term.terminal.resize(size);
    Ok(())
}

// ---------------------------------------------------------------------------
// Defuns: Input
// ---------------------------------------------------------------------------

/// Send a key press through the terminal, which reads all internal mode
/// state (DECCKM, newline mode, keyboard encoding) to produce correct
/// escape sequences.  Returns the encoded bytes to send to the PTY,
/// or nil if the key is unknown.
#[defun]
fn key_down(
    term: &mut EbbTerminal,
    key_name: String,
    shift: Option<i64>,
    ctrl: Option<i64>,
    meta: Option<i64>,
) -> Result<Option<String>> {
    input::key_down(
        term,
        &key_name,
        shift.is_some(),
        ctrl.is_some(),
        meta.is_some(),
    )
}

/// Send a mouse event through the terminal.  The terminal encodes it
/// based on the active mouse tracking mode (X10, VT200, SGR, etc.).
/// action: 0=press, 1=release, 2=motion.
/// button: 1=left, 2=right, 3=middle.
/// mods: bitmask (shift=1, meta=2, ctrl=4).
/// Returns encoded bytes or nil if mouse tracking is not active.
#[defun]
fn mouse_event(
    term: &mut EbbTerminal,
    action: i64,
    button: i64,
    row: i64,
    col: i64,
    mods: i64,
) -> Result<Option<String>> {
    input::mouse_event(term, action, button, row, col, mods)
}

/// Notify the terminal about a focus change.
/// The terminal handles mode 1004 (FocusTracking) internally --
/// if enabled, it writes \e[I (gained) or \e[O (lost) to the writer.
/// `gained` is non-nil (any integer) for focus gained, nil for focus lost.
/// Returns the encoded bytes (to send to the PTY) or nil.
#[defun]
fn focus_event(term: &mut EbbTerminal, gained: Option<i64>) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    term.terminal.focus_changed(gained.is_some());

    // The ThreadedWriter sends bytes through an mpsc channel to a
    // background thread which writes to our CapturingWriter.
    // Use the same wait pattern as key_down: yield first, then sleep.
    for i in 0..200 {
        if i < 50 {
            std::thread::yield_now();
        } else {
            std::thread::sleep(std::time::Duration::from_micros(100));
        }
        if let Ok(buf) = term.output.lock() {
            if !buf.is_empty() {
                break;
            }
        }
    }

    let bytes = term.drain_output_bytes();
    if bytes.is_empty() {
        Ok(None)
    } else {
        Ok(Some(String::from_utf8_lossy(&bytes).into_owned()))
    }
}

/// Send a paste to the terminal (with bracketed paste wrapping if enabled).
#[defun]
fn send_paste(term: &mut EbbTerminal, text: String) -> Result<()> {
    if term.freed {
        return Ok(());
    }
    term.terminal
        .send_paste(&text)
        .map_err(|e| anyhow::anyhow!("send_paste error: {}", e))?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Defuns: Rendering
// ---------------------------------------------------------------------------

/// Render the terminal screen into the current Emacs buffer with faces.
/// The buffer layout is:
///   [scrollback lines]  -- permanent, above display region
///   [display region]    -- rows x cols, updated each frame
/// New scrollback lines are inserted above the display region.
/// The display region is erased and rewritten each frame.
/// Must be called within inhibit-read-only / inhibit-modification-hooks.
#[defun]
fn render<'a>(env: &'a Env, term_val: Value<'a>) -> Result<()> {
    let mut term = term_val.into_ref_mut::<EbbTerminal>()?;
    render::render_to_buffer(env, &mut term)
}

// ---------------------------------------------------------------------------
// Defuns: Color palette (Theme integration)
// ---------------------------------------------------------------------------

/// Set the ANSI palette entries 0-15 from a concatenated hex string.
/// `colors_string` is 16 consecutive "#RRGGBB" entries (112 chars).
/// Returns t on success, nil on failure.
#[defun]
fn set_palette(term: &mut EbbTerminal, colors_string: String) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    if colors_string.len() < 7 * 16 {
        return Ok(false);
    }
    let palette = term.terminal.palette_mut();
    for i in 0..16 {
        let start = i * 7;
        let end = start + 7;
        if let Some(color) = config::parse_hex_color(&colors_string[start..end]) {
            palette.colors.0[i] = color;
        }
    }
    Ok(true)
}

/// Set the terminal's default foreground and background colors.
/// `fg_hex` and `bg_hex` are "#RRGGBB" strings.
#[defun]
fn set_default_colors(term: &mut EbbTerminal, fg_hex: String, bg_hex: String) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    let palette = term.terminal.palette_mut();
    if let Some(fg) = config::parse_hex_color(&fg_hex) {
        palette.foreground = fg;
    }
    if let Some(bg) = config::parse_hex_color(&bg_hex) {
        palette.background = bg;
    }
    Ok(true)
}

// ---------------------------------------------------------------------------
// Defuns: Alert queries
// ---------------------------------------------------------------------------

/// Check for and return a pending title change, or nil.
/// Calling this clears the pending title.
#[defun]
fn poll_title(term: &EbbTerminal) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    let mut q = term.alerts.lock().unwrap();
    Ok(q.title.take())
}

/// Check for and return a pending CWD change, or nil.
/// Calling this clears the pending CWD flag.
#[defun]
fn poll_cwd(term: &EbbTerminal) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }
    let mut q = term.alerts.lock().unwrap();
    if q.cwd_changed {
        q.cwd_changed = false;
        Ok(term.terminal.get_current_dir().map(|u| u.to_string()))
    } else {
        Ok(None)
    }
}

/// Check for and clear the bell flag.
#[defun]
fn poll_bell(term: &EbbTerminal) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }
    let mut q = term.alerts.lock().unwrap();
    let bell = q.bell;
    q.bell = false;
    Ok(bell)
}

// ---------------------------------------------------------------------------
// Defuns: Version
// ---------------------------------------------------------------------------

/// Return the el-be-back version string.
#[defun]
fn version(_env: &Env) -> Result<&'static str> {
    Ok(VERSION)
}
