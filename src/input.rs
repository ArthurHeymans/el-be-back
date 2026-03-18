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
            // f1..f12
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

/// Send a key press to the terminal. Returns Ok(true) if the key was handled.
pub fn key_down(
    term: &mut EbbTerminal,
    key_name: &str,
    shift: bool,
    ctrl: bool,
    meta: bool,
) -> Result<bool> {
    if term.freed {
        return Ok(false);
    }

    let keycode = match translate_key(key_name) {
        Some(k) => k,
        None => return Ok(false),
    };

    let mods = build_modifiers(shift, ctrl, meta);

    term.terminal
        .key_down(keycode, mods)
        .map_err(|e| anyhow::anyhow!("key_down error: {}", e))?;

    Ok(true)
}
