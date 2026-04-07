use std::sync::{Arc, Mutex};

use wezterm_term::color::ColorPalette;
use wezterm_term::{Alert, AlertHandler, TerminalConfiguration};

/// Terminal configuration for el-be-back.
#[derive(Debug)]
pub struct EbbConfig {
    pub scrollback_size: usize,
}

impl TerminalConfiguration for EbbConfig {
    fn color_palette(&self) -> ColorPalette {
        ColorPalette::default()
    }

    fn scrollback_size(&self) -> usize {
        self.scrollback_size
    }

    fn enable_kitty_keyboard(&self) -> bool {
        true
    }

    fn enable_kitty_graphics(&self) -> bool {
        true
    }
}

/// Collects alerts (title changes, CWD changes, bell) from the terminal.
#[derive(Debug, Default)]
pub struct AlertQueue {
    pub title: Option<String>,
    pub bell: bool,
    pub cwd_changed: bool,
}

pub struct EbbAlertSink {
    pub queue: Arc<Mutex<AlertQueue>>,
}

impl AlertHandler for EbbAlertSink {
    fn alert(&mut self, alert: Alert) {
        if let Ok(mut q) = self.queue.lock() {
            match alert {
                Alert::WindowTitleChanged(t) => q.title = Some(t),
                Alert::CurrentWorkingDirectoryChanged => q.cwd_changed = true,
                Alert::Bell => q.bell = true,
                _ => {}
            }
        }
    }
}

/// Parse a "#RRGGBB" hex color string to an SrgbaTuple.
pub fn parse_hex_color(hex: &str) -> Option<wezterm_cell::color::SrgbaTuple> {
    if hex.len() >= 7 && hex.as_bytes()[0] == b'#' {
        let r = u8::from_str_radix(&hex[1..3], 16).ok()?;
        let g = u8::from_str_radix(&hex[3..5], 16).ok()?;
        let b = u8::from_str_radix(&hex[5..7], 16).ok()?;
        Some(wezterm_cell::color::SrgbaTuple(
            r as f32 / 255.0,
            g as f32 / 255.0,
            b as f32 / 255.0,
            1.0,
        ))
    } else {
        None
    }
}
