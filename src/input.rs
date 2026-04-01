use emacs::Result;
use termwiz::input::{KeyCode, Modifiers};

use crate::EbbTerminal;

/// Translate an Emacs key name to a termwiz KeyCode.
fn translate_key(name: &str) -> Option<KeyCode> {
    match name {
        "return" | "RET" => Some(KeyCode::Enter),
        "backspace" | "DEL" => Some(KeyCode::Backspace),
        "tab" | "TAB" => Some(KeyCode::Tab),
        "escape" | "ESC" => Some(KeyCode::Escape),
        "up" => Some(KeyCode::UpArrow),
        "down" => Some(KeyCode::DownArrow),
        "left" => Some(KeyCode::LeftArrow),
        "right" => Some(KeyCode::RightArrow),
        "home" => Some(KeyCode::Home),
        "end" => Some(KeyCode::End),
        "prior" => Some(KeyCode::PageUp),
        "next" => Some(KeyCode::PageDown),
        "insert" => Some(KeyCode::Insert),
        "delete" | "deletechar" => Some(KeyCode::Delete),
        "SPC" | "space" => Some(KeyCode::Char(' ')),
        s if s.starts_with('f') || s.starts_with('F') => {
            let num_str = &s[1..];
            if let Ok(n) = num_str.parse::<u8>() {
                if (1..=24).contains(&n) {
                    return Some(KeyCode::Function(n));
                }
            }
            None
        }
        s if s.len() == 1 => {
            let ch = s.chars().next()?;
            Some(KeyCode::Char(ch))
        }
        _ => None,
    }
}

/// Build termwiz Modifiers from boolean flags.
fn build_modifiers(shift: bool, ctrl: bool, meta: bool) -> Modifiers {
    let mut mods = Modifiers::NONE;
    if shift {
        mods |= Modifiers::SHIFT;
    }
    if ctrl {
        mods |= Modifiers::CTRL;
    }
    if meta {
        mods |= Modifiers::ALT;
    }
    mods
}

/// Send a key press through the terminal's key_down() method, which
/// correctly reads all internal mode state (DECCKM, newline mode,
/// modify_other_keys, keyboard encoding) and writes the encoded bytes
/// to the terminal's writer (our CapturingWriter via ThreadedWriter).
///
/// After key_down(), we briefly yield to let the ThreadedWriter's
/// background thread deliver the bytes to our CapturingWriter, then
/// drain and return them.  The channel latency is microseconds.
///
/// Returns the encoded bytes as a string, or nil if the key is unknown.
pub fn key_down(
    term: &mut EbbTerminal,
    key_name: &str,
    shift: bool,
    ctrl: bool,
    meta: bool,
) -> Result<Option<String>> {
    if term.freed {
        return Ok(None);
    }

    let keycode = match translate_key(key_name) {
        Some(k) => k,
        None => return Ok(None),
    };

    let mods = build_modifiers(shift, ctrl, meta);

    term.terminal
        .key_down(keycode, mods)
        .map_err(|e| anyhow::anyhow!("key_down error: {}", e))?;

    // The ThreadedWriter sends bytes through an mpsc channel to a
    // background thread, which writes to our CapturingWriter.  Yield
    // briefly to let the background thread process the write.
    for _ in 0..1000 {
        std::thread::yield_now();
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
