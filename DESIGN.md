# el-be-back: Design Document

A terminal emulator for Emacs built on wezterm's terminal emulation engine,
exposed as a Rust dynamic module.

## Prior Art

Three existing Emacs terminal emulators informed this design:

**emacs-libvterm (vterm)**: C dynamic module wrapping libvterm. Mature and
widely used. Key limitations: no mouse tracking forwarded to terminal
programs, "fake newline" rendering causes problems with Emacs text
operations (isearch, copy), no modern terminal features (kitty keyboard
protocol, kitty graphics, OSC 8 hyperlinks), libvterm is aging with
unclear maintenance. O(n) scrollback push due to memmove in ring buffer.

**emacs-eat (eat)**: Pure Elisp terminal emulator. ~8300 lines in a single
file. Zero build dependencies -- works on any Emacs. Sophisticated
latency-bounded output batching (8ms min / 33ms max). Full mouse tracking
(SGR protocol). Sixel graphics. Shell integration via custom OSC 51
protocol. Key limitations: ~1.5x slower than vterm for throughput
(inherent Elisp performance ceiling), hand-written parser is large and
hard to extend, basic resize reflow, single-file monolith.

**emacs-libgterm (gterm)**: Zig dynamic module wrapping ghostty-vt (from
the Ghostty terminal emulator). SIMD-optimized VT parsing, native text
reflow, OSC 8 hyperlinks. Key limitations: early prototype, no mouse
tracking, full buffer erase+rerender on every frame (incremental rendering
disabled), requires full Ghostty repo as build dependency, macOS-only
tested.

## Goals

1. **Maximum compatibility**: Programs that work in Alacritty/Kitty/Wezterm
   should work identically here.

2. **Deep Emacs integration**: Scrollback is searchable Emacs text. Shell
   prompts are navigable. Hyperlinks are clickable. Copy mode gives full
   Emacs navigation.

3. **Performance**: Heavy output (build logs, `find /`, large diffs) must
   not block Emacs. Rendering must be efficient enough for smooth
   interactive use.

4. **Modern terminal features**: OSC 8 hyperlinks, kitty keyboard protocol,
   kitty graphics protocol, synchronized output, sixel graphics.

## Architecture

```
Emacs process
+--------------------------------------------------+
|  el-be-back.el                                   |
|    - ebb-mode (major mode)                       |
|    - Keymaps: char / semi-char / emacs / line    |
|    - PTY management via make-process             |
|    - Latency-bounded output batching (8ms/33ms)  |
|    - Applies Rust-driven buffer changes          |
|              ^                                   |
|              | emacs-module FFI (#[defun])        |
|              v                                   |
|  ebb-module.so (Rust cdylib)                     |
|    - EbbTerminal wraps wezterm-term::Terminal     |
|    - CapturingWriter (Arc<Mutex<Vec<u8>>>)       |
|    - Feeds bytes -> VT state machine             |
|    - Renders dirty rows -> Emacs buffer via FFI  |
|    - Encodes key/mouse -> PTY escape sequences   |
|              ^                                   |
|              | PTY (Emacs-owned via make-process) |
|              v                                   |
|         Shell (bash/zsh/fish)                    |
+--------------------------------------------------+
```

### Why wezterm-term

We use two crates from the wezterm project:

- **wezterm-escape-parser** (aka termwiz escape module): VT escape sequence
  parser. Parses raw bytes into semantic `Action` enums (Print, CSI, OSC,
  Esc, KittyImage, Sixel, etc.). Streaming and stateful -- handles partial
  sequences across chunk boundaries.

- **wezterm-term**: Full VT terminal state machine. Takes `Action`s from
  the parser and applies them to a screen model (cursor movement, SGR
  attributes, scroll regions, mode changes, alternate screen, etc.).
  Includes scrollback, text reflow on resize, kitty graphics/keyboard
  protocol, OSC 8 hyperlinks, semantic zones, and clipboard support.

termwiz (the toolkit layer) provides the parser, cell model, color types,
input encoding, and surface diffing. But it does NOT contain a terminal
emulator -- the state machine that maps "CSI H" to "move cursor" lives in
the separate `term` crate.

Comparison of available VT engines:

| Library            | Lang | SIMD | Kitty Gfx | Kitty KB | Reflow | Embedding |
|--------------------|------|------|-----------|----------|--------|-----------|
| wezterm-term       | Rust | No   | Yes       | Yes      | Yes    | Usable    |
| alacritty_terminal | Rust | No   | No        | Yes      | Yes    | Somewhat  |
| ghostty-vt         | Zig  | Yes  | Yes       | Yes      | Yes    | Somewhat  |
| libvterm           | C    | No   | No        | No       | No     | Yes       |

wezterm-term wins on feature completeness. ghostty-vt has faster parsing
(SIMD) but is not a published standalone library. alacritty_terminal lacks
kitty graphics.

### Why Emacs-owned PTY

Emacs manages the PTY via `make-process`. The alternative (Rust-owned PTY
with a background event loop) was considered but rejected:

- Emacs process management is deeply integrated (sentinels, buffers,
  coding systems). Fighting it means reimplementing a lot.
- vterm and gterm both use this pattern successfully.
- The overhead of Emacs process filter + FFI is small compared to terminal
  output volume.

### The ThreadedWriter Problem

wezterm-term unconditionally spawns a background OS thread
(`ThreadedWriter`) to decouple terminal output writes from input
processing. This prevents deadlocks with programs like vim that
simultaneously produce output while consuming input.

The thread cannot be avoided through the public API. Our mitigation:

1. Pass a `CapturingWriter(Arc<Mutex<Vec<u8>>>)` as the writer.
2. The ThreadedWriter sends data through an mpsc channel to its thread,
   which writes to our CapturingWriter.
3. After `terminal.advance_bytes()` or `terminal.key_down()`, bytes are
   in-flight through the channel. They arrive at the CapturingWriter
   within microseconds.
4. Elisp drains captured bytes in its rendering timer (which fires at
   8-33ms) and sends them to the PTY via `process-send-string`.
5. For interactive key input, we drain immediately after key_down. The
   channel latency (~microseconds) is imperceptible.

### The CR Stripping Problem

Emacs `make-process` strips carriage returns from PTY output by default.
Real terminals expect `\r\n` for newlines; stripping `\r` breaks cursor
positioning.

Solution: set the process coding system to binary:
```elisp
(set-process-coding-system proc 'binary 'binary)
```

This preserves raw bytes. The VT parser in wezterm-term handles `\r` and
`\n` correctly.

## Data Flow

### Output path (shell -> display)

```
1. Shell writes to PTY
2. Emacs process filter receives bytes (binary coding)
3. Elisp pushes bytes onto ebb--pending-chunks
4. Elisp schedules render timer (8ms min, 33ms max since first chunk)
5. Timer fires:
   a. Feed all queued chunks to Rust: ebb--feed(term, bytes)
   b. Rust calls terminal.advance_bytes(bytes)
      - VT parser decodes escape sequences
      - State machine updates screen (cursor, cells, attributes)
      - Terminal may generate response bytes (DA, DECRQM, etc.)
        -> written to CapturingWriter via ThreadedWriter
   c. Rust marks terminal dirty (seqno incremented)
   d. Elisp calls ebb--render(term) in Rust
      - Rust queries screen.get_changed_stable_rows(range, last_seqno)
      - For each dirty row: iterate cells, accumulate style runs
      - Directly manipulate Emacs buffer via FFI:
        goto-char, delete-region, insert, put-text-property
      - Return cursor position
   e. Elisp calls ebb--drain-output(term)
      - Drains CapturingWriter buffer
      - Sends bytes to PTY via process-send-string (responses)
   f. Elisp positions cursor and syncs scroll
```

### Input path (user -> shell)

```
1. User presses key in Emacs
2. Keymap routes to ebb-self-input
3. Elisp calls ebb--key-down(term, key-name, shift, ctrl, meta)
4. Rust translates to termwiz KeyCode + Modifiers
5. Rust calls terminal.key_down(keycode, mods)
   - Terminal reads its mode state (DECCKM, keyboard encoding, etc.)
   - Calls KeyCode::encode() with the correct modes
   - Writes encoded bytes to internal writer -> CapturingWriter
6. Elisp calls ebb--drain-output(term)
7. Elisp sends bytes to PTY via process-send-string
```

## Rust Module API

### Lifecycle

```
ebb--new (rows cols scrollback-size) -> user-ptr
```
Create a new terminal. Returns an opaque user-ptr holding `EbbTerminal`.
The user-ptr has a GC finalizer that cleans up the Terminal and writer
thread.

```
ebb--free (term) -> nil
```
Explicit cleanup. Sets a `freed` guard to prevent use-after-free if GC
runs the finalizer later.

### I/O

```
ebb--feed (term bytes) -> nil
```
Feed raw PTY output bytes into the VT parser. Calls
`terminal.advance_bytes(bytes)`. Terminal state is updated. Any response
bytes are captured in the CapturingWriter.

```
ebb--drain-output (term) -> string-or-nil
```
Drain the CapturingWriter buffer. Returns captured bytes as a unibyte
string, or nil if empty. Elisp sends these to the PTY.

```
ebb--key-down (term key-name shift ctrl meta) -> nil
```
Send a key press. `key-name` is a string like `"a"`, `"return"`, `"up"`,
`"f1"`. Modifier booleans indicate shift/ctrl/meta state. Calls
`terminal.key_down()` which encodes using the terminal's current keyboard
mode and writes to the CapturingWriter.

```
ebb--mouse-event (term x y button mods is-press) -> nil
```
Send a mouse event. `x` and `y` are 0-based cell coordinates. `button`
is an integer (1=left, 2=middle, 3=right, 4=wheel-up, 5=wheel-down).
`mods` is a bitfield. `is-press` distinguishes press from release.

```
ebb--send-paste (term text) -> nil
```
Send text as a paste. If bracketed paste mode is enabled, wraps in
`ESC[200~` / `ESC[201~`.

### Rendering

```
ebb--render (term) -> (row . col)
```
Render dirty rows into the current Emacs buffer. Directly manipulates
the buffer via FFI (insert, delete-region, put-text-property). Returns
the cursor position as (row . col) for Elisp to position point.

This function is called from Elisp within the performance trinity:
```elisp
(let ((inhibit-read-only t)
      (inhibit-modification-hooks t)
      (inhibit-quit t)
      (buffer-undo-list t))
  (ebb--render term))
```

The Rust implementation:
1. Calls `screen.get_changed_stable_rows(visible_range, last_seqno)`
2. For each dirty row:
   a. Navigate to the row position in the buffer
   b. Delete the old line content
   c. Iterate cells: `screen.line_mut(phys_row).cells()`
   d. Accumulate style runs (consecutive cells with same CellAttributes)
   e. For each run: call `(insert text)` then `(put-text-property ...)`
      with a face plist built from the CellAttributes
3. Handle line wrapping: cells with `wrapped()` attribute get a newline
   with `ebb-wrap-line` text property
4. Update internal `last_seqno`
5. Return cursor position

### State Queries

```
ebb--cursor-info (term) -> (row col shape visible)
```
Get cursor state. `shape` is a symbol: `box`, `bar`, `underline`,
`hollow`. `visible` is t or nil.

```
ebb--get-title (term) -> string
```
Get the terminal title (set by OSC 2).

```
ebb--get-cwd (term) -> string-or-nil
```
Get the current working directory (set by OSC 7).

```
ebb--get-size (term) -> (rows . cols)
```
Get the current terminal dimensions.

```
ebb--is-alt-screen-p (term) -> bool
```
Check if the alternate screen buffer is active.

```
ebb--is-mouse-grabbed-p (term) -> bool
```
Check if the terminal has grabbed the mouse (any mouse tracking mode).

```
ebb--bracketed-paste-p (term) -> bool
```
Check if bracketed paste mode is enabled.

### Control

```
ebb--resize (term rows cols) -> nil
```
Resize the terminal. wezterm-term handles text reflow automatically.

```
ebb--scrollback-count (term) -> integer
```
Number of scrollback lines above the visible screen.

```
ebb--get-scrollback-line (term index) -> string
```
Get a scrollback line as a propertized string. Used when syncing
scrollback to the Emacs buffer.

## Rust Internal Design

### EbbTerminal struct

```rust
use std::sync::{Arc, Mutex};
use wezterm_term::{Terminal, TerminalSize};

struct EbbTerminal {
    terminal: Terminal,
    output: Arc<Mutex<Vec<u8>>>,  // shared with CapturingWriter
    last_seqno: SequenceNo,
    alerts: Arc<Mutex<AlertQueue>>,
    freed: bool,
}
```

### CapturingWriter

```rust
struct CapturingWriter {
    buf: Arc<Mutex<Vec<u8>>>,
}

impl std::io::Write for CapturingWriter {
    fn write(&mut self, data: &[u8]) -> std::io::Result<usize> {
        self.buf.lock().unwrap().extend_from_slice(data);
        Ok(data.len())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}
```

The `Arc<Mutex<Vec<u8>>>` is shared between EbbTerminal (for draining)
and the CapturingWriter (which lives inside the ThreadedWriter's
background thread). The Mutex ensures safe concurrent access.

### EbbConfig (TerminalConfiguration)

```rust
#[derive(Debug)]
struct EbbConfig {
    scrollback_size: usize,
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
```

### EbbAlertSink (AlertHandler)

```rust
struct AlertQueue {
    title: Option<String>,
    cwd: Option<Url>,
    bell: bool,
}

struct EbbAlertSink {
    queue: Arc<Mutex<AlertQueue>>,
}

impl AlertHandler for EbbAlertSink {
    fn alert(&mut self, alert: Alert) {
        let mut q = self.queue.lock().unwrap();
        match alert {
            Alert::WindowTitleChanged(t) => q.title = Some(t),
            Alert::CurrentWorkingDirectoryChanged => { /* read from terminal */ }
            Alert::Bell => q.bell = true,
            _ => {}
        }
    }
}
```

### Face Construction

Converting `CellAttributes` to an Emacs face plist:

```rust
fn build_face_plist(env: &Env, attrs: &CellAttributes) -> Result<Value<'_>> {
    // Start with an empty plist, add properties as needed
    let mut props: Vec<Value> = Vec::new();

    // Foreground
    match attrs.foreground() {
        ColorAttribute::Default => {}
        ColorAttribute::PaletteIndex(idx) => {
            let color = resolve_palette_color(idx);
            props.push(env.intern(":foreground")?);
            props.push(color.into_lisp(env)?);
        }
        ColorAttribute::TrueColorWithPaletteFallback(rgba, _)
        | ColorAttribute::TrueColorWithDefaultFallback(rgba) => {
            let hex = format!("#{:02x}{:02x}{:02x}",
                (rgba.0 * 255.0) as u8,
                (rgba.1 * 255.0) as u8,
                (rgba.2 * 255.0) as u8);
            props.push(env.intern(":foreground")?);
            props.push(hex.into_lisp(env)?);
        }
    }

    // Background (same pattern)
    // Bold: :weight bold
    // Italic: :slant italic
    // Underline: :underline t or (:style wave :color "#rrggbb")
    // Strikethrough: :strike-through t
    // Inverse: :inverse-video t

    env.list(&props)
}
```

Performance optimization: pre-intern all keyword symbols (`:foreground`,
`:background`, `:weight`, etc.) as `GlobalRef` values at module init time.
This avoids repeated `env.intern()` calls during rendering.

### Key Translation

Mapping Emacs key name strings to termwiz `KeyCode`:

```rust
fn translate_key(name: &str) -> Option<KeyCode> {
    match name {
        "return" => Some(KeyCode::Enter),
        "backspace" => Some(KeyCode::Backspace),
        "tab" => Some(KeyCode::Tab),
        "escape" => Some(KeyCode::Escape),
        "up" => Some(KeyCode::UpArrow),
        "down" => Some(KeyCode::DownArrow),
        "left" => Some(KeyCode::LeftArrow),
        "right" => Some(KeyCode::RightArrow),
        "home" => Some(KeyCode::Home),
        "end" => Some(KeyCode::End),
        "prior" => Some(KeyCode::PageUp),    // Emacs name for PageUp
        "next" => Some(KeyCode::PageDown),   // Emacs name for PageDown
        "insert" => Some(KeyCode::Insert),
        "delete" => Some(KeyCode::Delete),
        "f1" ..= "f12" => {
            let n: u8 = name[1..].parse().ok()?;
            Some(KeyCode::Function(n))
        }
        s if s.len() == 1 => {
            Some(KeyCode::Char(s.chars().next()?))
        }
        _ => None,
    }
}
```

## Elisp Layer Design

### Major Mode

```elisp
(define-derived-mode ebb-mode fundamental-mode "EBB"
  "Major mode for el-be-back terminal emulator."
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  ;; Disable font-lock (we manage faces ourselves)
  (setq-local font-lock-defaults '(nil t))
  ;; Track display region
  (setq-local ebb--display-begin (point-min-marker))
  (setq-local ebb--terminal nil)
  (setq-local ebb--process nil))
```

### Input Modes

Four input modes, each a minor mode with its own keymap:

**Emacs mode** (default): Standard Emacs navigation. Buffer is read-only.
Good for scrolling through output, searching, copying text.
- `C-c C-j` -> enter semi-char mode
- `C-c C-k` -> (no-op, already in emacs mode)

**Semi-char mode**: Most keys forwarded to terminal. Emacs prefix keys
preserved for discoverability.
- Non-forwarded keys (configurable via `ebb-semi-char-non-bound-keys`):
  `C-c`, `C-x`, `C-g`, `C-h`, `C-u`, `M-x`, `M-o`
- `C-c C-j` -> enter char mode
- `C-c C-k` -> enter emacs mode

**Char mode**: Nearly all keys forwarded to terminal. Maximum terminal
compatibility. Only escape hatch is:
- `C-c C-k` -> enter emacs mode

**Line mode**: Comint-like line editing. Type a command with full Emacs
editing, send on RET. History ring with `M-p`/`M-n`.

### Output Batching

Copied from eat's battle-tested approach:

```elisp
(defcustom ebb-minimum-latency 0.008
  "Minimum time in seconds between redraws."
  :type 'number :group 'el-be-back)

(defcustom ebb-maximum-latency 0.033
  "Maximum time in seconds before a forced redraw."
  :type 'number :group 'el-be-back)

(defun ebb--process-filter (process output)
  "Process filter: queue output and schedule rendering."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (push output ebb--pending-chunks)
      (unless ebb--first-chunk-time
        (setq ebb--first-chunk-time (current-time)))
      (when ebb--render-timer
        (cancel-timer ebb--render-timer))
      (let ((elapsed (float-time
                      (time-subtract nil ebb--first-chunk-time))))
        (if (>= elapsed ebb-maximum-latency)
            (ebb--flush-output)
          (setq ebb--render-timer
                (run-with-timer
                 (min (- ebb-maximum-latency elapsed)
                      ebb-minimum-latency)
                 nil #'ebb--flush-output-safe
                 (current-buffer))))))))

(defun ebb--flush-output ()
  "Process all pending output and render."
  (when ebb--render-timer
    (cancel-timer ebb--render-timer)
    (setq ebb--render-timer nil))
  (let ((chunks (nreverse ebb--pending-chunks)))
    (setq ebb--pending-chunks nil
          ebb--first-chunk-time nil)
    ;; Feed all chunks to the terminal
    (dolist (chunk chunks)
      (ebb--feed ebb--terminal chunk))
    ;; Render with the performance trinity
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (inhibit-quit t)
          (buffer-undo-list t))
      (save-restriction
        (widen)
        (let ((cursor-pos (ebb--render ebb--terminal)))
          ;; Position cursor
          (ebb--sync-cursor cursor-pos)
          ;; Sync scroll
          (ebb--sync-scroll))))
    ;; Drain terminal responses and send to PTY
    (let ((response (ebb--drain-output ebb--terminal)))
      (when response
        (process-send-string ebb--process response)))))
```

### Process Management

```elisp
(defcustom ebb-shell-name (or explicit-shell-file-name
                            (getenv "SHELL")
                            "/bin/sh")
  "Shell to run in the terminal."
  :type 'string :group 'el-be-back)

(defcustom ebb-term-environment-variable "xterm-256color"
  "TERM environment variable."
  :type 'string :group 'el-be-back)

(defcustom ebb-max-scrollback 10000
  "Maximum scrollback lines."
  :type 'integer :group 'el-be-back)

(defun ebb ()
  "Start a terminal."
  (interactive)
  (let* ((buf (generate-new-buffer "*ebb*"))
         (rows (window-body-height))
         (cols (window-body-width)))
    (with-current-buffer buf
      (ebb-mode)
      ;; Create terminal
      (setq ebb--terminal (ebb--new rows cols ebb-max-scrollback))
      ;; Initialize buffer with blank lines
      (ebb--init-buffer rows cols)
      ;; Start shell process
      (let ((process-environment
             (append
              (list (concat "TERM=" ebb-term-environment-variable)
                    "COLORTERM=truecolor"
                    (concat "INSIDE_EMACS=" emacs-version ",ebb"))
              process-environment)))
        (setq ebb--process
              (make-process
               :name "ebb"
               :buffer buf
               :command (list ebb-shell-name "-l")
               :coding 'binary
               :filter #'ebb--process-filter
               :sentinel #'ebb--process-sentinel
               :connection-type 'pty)))
      ;; Enter semi-char mode by default
      (ebb-semi-char-mode 1))
    (pop-to-buffer buf)))
```

### Resize Handling

```elisp
(defun ebb--window-size-change (frame)
  "Handle window resize."
  (dolist (window (window-list frame))
    (when (and (eq (buffer-local-value 'major-mode (window-buffer window))
                   'ebb-mode)
               (buffer-local-value 'ebb--terminal (window-buffer window)))
      (with-current-buffer (window-buffer window)
        (let ((rows (window-body-height window))
              (cols (window-body-width window)))
          (when (or (/= rows (car (ebb--get-size ebb--terminal)))
                    (/= cols (cdr (ebb--get-size ebb--terminal))))
            (ebb--resize ebb--terminal rows cols)
            ;; Signal the shell
            (when (process-live-p ebb--process)
              (set-process-window-size ebb--process rows cols))))))))

;; Hook installed in ebb-mode setup:
;; (add-hook 'window-size-change-functions #'ebb--window-size-change)
```

## Project Structure

```
el_be_back/
+-- Cargo.toml                  # Rust cdylib crate
+-- src/
|   +-- lib.rs                  # Module init, EbbTerminal, defuns
|   +-- render.rs               # Dirty row rendering, face construction
|   +-- input.rs                # Key/mouse translation
|   +-- config.rs               # TerminalConfiguration, AlertHandler
+-- el-be-back.el               # Elisp layer
+-- build.sh                    # Build script
+-- DESIGN.md                   # This file
+-- test/
    +-- el-be-back-tests.el     # ERT tests
```

## Implementation Phases

### Phase 1: Project Skeleton

**Goal**: Cargo project builds a .so that Emacs can load.

Rust:
- Cargo.toml with `crate-type = ["cdylib"]`, depend on `emacs = "0.20"`
- src/lib.rs: module init, `ebb--version` defun
- build.sh: cargo build + copy .so

Elisp:
- el-be-back.el: `(require 'ebb-module)`, auto-compilation,
  `ebb-version` wrapper

Verify:
```
emacs -Q -L . --eval '(require (quote el-be-back))'
      --eval '(message "%s" (ebb-version))'
=> "0.1.0"
```

### Phase 2: Terminal Core

**Goal**: Create a terminal, feed bytes, read screen content.

Rust:
- Add wezterm-term, wezterm-escape-parser dependencies
- EbbTerminal struct with Terminal + CapturingWriter + AlertSink
- EbbConfig implementing TerminalConfiguration
- Defuns: ebb--new, ebb--free, ebb--feed, ebb--drain-output, ebb--content

Elisp:
- ebb command: create buffer, create terminal, start shell process
- ebb--process-filter: feed bytes, drain responses
- ebb--process-sentinel: handle shell exit

Verify:
- `M-x ebb` opens a buffer
- Shell prompt appears (as raw text, no colors)
- Typing characters echoes back

### Phase 3: Rendering

**Goal**: Terminal output renders with correct colors and positioning.

Rust:
- render.rs: iterate dirty rows, accumulate style runs
- Face construction from CellAttributes -> Emacs face plist
- Pre-intern keyword symbols at module init
- Handle wide characters (CJK), grapheme clusters
- Handle line wrapping (wrapped cells -> newline with text property)
- ebb--render defun: modifies current buffer, returns cursor pos

Elisp:
- Latency-bounded output batching (8ms/33ms timers)
- Buffer setup: disable undo, read-only, truncate-lines
- Cursor positioning and scroll sync
- Display region tracking (ebb--display-begin marker)

Verify:
- `ls --color` shows colored output
- `htop` renders correctly (layout, colors)
- `clear` clears screen
- Cursor is positioned correctly

### Phase 4: Input

**Goal**: Full keyboard and mouse input.

Rust:
- input.rs: Emacs key name -> KeyCode translation table
- ebb--key-down: translate + terminal.key_down() + drain
- ebb--mouse-event: build MouseEvent, call terminal.mouse_event()
- ebb--send-paste: call terminal.send_paste()
- ebb--is-mouse-grabbed-p, ebb--bracketed-paste-p queries

Elisp:
- Three input modes: emacs / semi-char / char (minor modes)
- ebb-self-input command: extract event -> ebb--key-down -> drain -> send
- Programmatic keymap generation for char mode
- Mouse tracking: bind mouse events when terminal grabs mouse
- Paste: ebb-yank wraps in send-paste

Verify:
- Typing in bash works (echo, line editing, history)
- Arrow keys, Home/End, PgUp/PgDn work
- vim opens and responds to all keys
- C-c sends interrupt
- C-x C-f opens find-file in semi-char mode
- Mouse clicks work in htop/tmux (when mouse grabbed)

### Phase 5: Terminal Features

**Goal**: Complete terminal experience.

Scrollback:
- When rows scroll off visible area, Rust reads scrollback lines
  and Elisp inserts them above ebb--display-begin as permanent
  buffer text with text properties
- Scrollback truncated to ebb-max-scrollback lines
- In emacs mode, standard scroll commands work on scrollback

Alternate screen:
- Track via ebb--is-alt-screen-p
- Save/restore display content on alt screen switch
- No scrollback during alt screen

Resize:
- window-size-change-functions hook
- ebb--resize calls terminal.resize() (reflow handled by wezterm)
- set-process-window-size sends SIGWINCH

Title and CWD:
- AlertHandler captures title changes, CWD changes, bell
- ebb--get-title, ebb--get-cwd exposed to Elisp
- Elisp updates header-line-format, default-directory
- (visible-bell or (ding)) on bell

Verify:
- Scrollback: scroll up through shell history output
- C-s searches scrollback text
- vim uses alt screen; quitting restores output
- Resizing window resizes terminal; `tput cols` reflects new size
- cd /tmp updates default-directory

### Phase 6: Modern Features

OSC 8 Hyperlinks:
- During rendering, check cell.attrs().hyperlink()
- Add `button` text property + `help-echo` with URI
- Click invokes browse-url or find-file
- mouse-face property for hover highlighting

Synchronized Output (DEC mode 2026):
- Track BSU/ESU state in Rust
- When BSU active, skip rendering (queue but don't display)
- When ESU arrives, trigger immediate render
- Eliminates flicker for TUI applications

Kitty Keyboard Protocol:
- Already enabled via TerminalConfiguration
- wezterm-term negotiates CSI u mode automatically
- key_down() already uses the correct encoding
- No additional work beyond Phase 4

Sixel Graphics:
- Cells with ImageCell data: extract image bytes
- Create Emacs image descriptor: (image :type png :data ...)
- Apply as display text property
- Gate on (display-graphic-p)
- Fall back to Unicode half-blocks in terminal Emacs

Kitty Graphics Protocol:
- Enabled via TerminalConfiguration
- Image placements stored in CellAttributes
- Same rendering approach as Sixel
- Support z-ordering via attach_image

OSC 52 Clipboard:
- Implement Clipboard trait in Rust
- Elisp calls kill-new with clipboard data
- Gate on ebb-enable-osc52 (default nil for security)

### Phase 7: Emacs Integration

Shell Integration:
- Support OSC 133 (semantic zones from wezterm-term):
  - Prompt start/end markers as text properties
  - Command output boundaries
  - Exit code tracking
- Prompt navigation: M-p / M-n jump between prompts
- Command output narrowing

Copy Mode:
- C-c C-c enters copy mode:
  - Pause terminal (queue but don't feed)
  - Buffer becomes normal read-only Emacs buffer
  - isearch, mark, region, M-w all work
- C-c C-c exits: resume terminal, restore cursor

Line Mode:
- Comint-like line editing
- Standard Emacs editing + history ring
- Send on RET

Eshell Integration:
- ebb-eshell-mode: use ebb for terminal-capable Eshell programs
- Advise eshell-gather-process-output

### Phase 8: Polish

Terminfo:
- Create ebb.ti with ebb-truecolor, ebb-256color types
- Compile and ship in terminfo/ directory
- Set TERMINFO environment variable

Performance:
- Benchmark vs vterm/eat: `time cat large-file.txt`
- Profile FFI overhead per frame
- Face plist caching (reuse identical plists via hash map)
- Pre-intern all symbols at module init

Cross-platform:
- Linux x86_64 + aarch64
- macOS aarch64
- .so vs .dylib naming
- Emacs header detection

Packaging:
- MELPA recipe
- Auto-compilation on first load
- ebb-compile-module interactive command

## Risks and Mitigations

| Risk | Severity | Mitigation |
|------|----------|------------|
| ThreadedWriter adds latency | Low | Drain after ops; microsecond lag is fine |
| Char width mismatch (Emacs vs wezterm) | High | Track and pad during render (like gterm) |
| Heavy dependency tree (~20 crates) | Low | Accept; custom state machine is months |
| wezterm-term lacks some public getters | Medium | Use key_down() through writer; fork if needed |
| CR stripping from PTY | High | Binary coding system on process |
| wezterm internal API instability | Medium | Pin to specific git commit/tag |
| Emacs module can't be reloaded | Low | Require Emacs restart during dev |
