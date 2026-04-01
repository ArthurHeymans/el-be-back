use emacs::Result;
use termwiz::input::{KeyCode, KeyCodeEncodeModes, KeyboardEncoding, Modifiers};

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

/// Encode a key press and return the bytes to send to the PTY.
///
/// This bypasses the terminal's async ThreadedWriter by encoding the key
/// directly using KeyCode::encode(). The bytes are returned synchronously
/// so Elisp can send them to the PTY immediately via process-send-string.
pub fn encode_key(
    term: &EbbTerminal,
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

    // Get the terminal's current keyboard encoding mode
    let encoding = term.terminal.get_keyboard_encoding();

    // Build encode modes from terminal state.
    // For modes we can't query directly, use safe defaults.
    let modes = KeyCodeEncodeModes {
        encoding,
        // Application cursor keys (DECCKM) - we can't easily query this
        // from the public API, but the default (false) produces standard
        // escape sequences which work for most cases.
        application_cursor_keys: false,
        newline_mode: false,
        modify_other_keys: None,
    };

    match keycode.encode(mods, modes, true) {
        Ok(encoded) => {
            if encoded.is_empty() {
                // KeyCode::encode() doesn't handle simple control characters.
                // Fall back to direct byte encoding for common keys.
                Ok(fallback_encode(keycode, mods))
            } else {
                Ok(Some(encoded))
            }
        }
        Err(_) => Ok(fallback_encode(keycode, mods)),
    }
}

/// Direct byte encoding for keys that KeyCode::encode() returns empty for.
fn fallback_encode(keycode: KeyCode, mods: Modifiers) -> Option<String> {
    let ctrl = mods.contains(Modifiers::CTRL);
    let meta = mods.contains(Modifiers::ALT);

    let byte = match keycode {
        KeyCode::Enter => Some("\r"),
        KeyCode::Backspace => Some("\x7f"),
        KeyCode::Tab => Some("\t"),
        KeyCode::Escape => Some("\x1b"),
        KeyCode::Char(c) if ctrl => {
            // Ctrl+letter -> control character (C-a = 0x01, C-z = 0x1a)
            let code = c.to_ascii_lowercase() as u8;
            if (b'a'..=b'z').contains(&code) {
                let ctrl_code = code - b'a' + 1;
                return Some(String::from(ctrl_code as char));
            }
            match c {
                '\\' => return Some(String::from(28 as char)), // C-\
                ']' => return Some(String::from(29 as char)),  // C-]
                '_' => return Some(String::from(31 as char)),  // C-_
                ' ' => return Some(String::from(0 as char)),   // C-SPC
                _ => None,
            }
        }
        KeyCode::Char(c) if !ctrl && !meta => {
            return Some(String::from(c));
        }
        _ => None,
    };

    match byte {
        Some(s) => {
            if meta {
                // Meta prefix: ESC + byte
                Some(format!("\x1b{}", s))
            } else {
                Some(s.to_string())
            }
        }
        None => None,
    }
}
