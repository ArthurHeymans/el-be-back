# el-be-back: Feature Parity Plan

Gap analysis based on comparison with [ghostel](https://github.com/dakra/ghostel) v0.7.1.
Items are grouped into implementation phases ordered by dependency and impact.
Each item includes the affected layer (Rust, Elisp, Shell, or Build) and
an estimate (S/M/L) of effort.

---

## Phase A: Core Terminal UX

These are table-stakes features for a usable terminal emulator. Without
them, TUI apps like htop/lazygit/fzf behave incorrectly.

### A1. Mouse tracking (SGR protocol) — Rust + Elisp, M

Forward mouse press, release, and drag events to the terminal via the
SGR mouse protocol so TUI apps receive full mouse input.

Rust:
- Add `ebb--mouse-event(term, action, button, row, col, mods) -> nil`.
  Build a `termwiz::input::MouseEvent`, call `terminal.mouse_event()`,
  drain CapturingWriter, return encoded bytes.
- Action: 0=press, 1=release, 2=motion. Button: 1=left, 2=right,
  3=middle. Mods: bitmask (shift=1, meta=2, ctrl=4).

Elisp:
- Bind `<down-mouse-1>`, `<mouse-1>`, `<drag-mouse-1>` (and buttons 2/3)
  in `ebb-semi-char-mode-map`.
- Handler extracts `(posn-col-row (event-start event))` to get cell
  coordinates, calls `ebb--mouse-event`.
- Gate forwarding on `(ebb--is-mouse-grabbed term)` — when the terminal
  has no mouse mode active, let Emacs handle clicks normally.

### A2. Focus events (DEC mode 1004) — Rust + Elisp, S

Apps like vim and tmux use focus events to refresh on window switch.

Rust:
- Add `ebb--focus-event(term, gained) -> nil`.
  Query mode 1004 from wezterm-term; if enabled, encode `\e[I` (gained)
  or `\e[O` (lost) and return via drain.
- Alternatively, check `terminal.get_mode(Mode::FocusTracking)`.

Elisp:
- Hook `focus-in-hook` / `focus-out-hook` (Emacs 27+). In each,
  iterate ghostel buffers and call `ebb--focus-event`.

### A3. Synchronized output (DEC mode 2026) — Rust + Elisp, S

Prevents half-drawn frames when TUI apps repaint the screen.

Rust:
- Add `ebb--sync-mode-active(term) -> bool`.
  Query BSU (Begin Synchronized Update) state from wezterm-term.
  wezterm-term tracks mode 2026 internally.

Elisp:
- In `ebb--flush-output`, after feeding bytes and before rendering,
  check `(ebb--sync-mode-active term)`. If active, skip the render
  call (let the timer re-fire). Render immediately when mode clears
  (ESU received).
- Add `ebb--force-next-redraw` flag so the next timer cycle always
  renders after sync mode ends.

### A4. Cursor style rendering — Rust + Elisp, S

Display the correct cursor style (block, bar, underline, hollow block)
based on what the terminal program requested via DECSCUSR.

Rust:
- Add `ebb--cursor-style(term) -> integer`.
  Read cursor shape from wezterm-term's `CursorShape` enum.
  Return 0=block, 1=bar, 2=underline, 3=hollow (or map to ghostel's
  convention).
- Also return cursor visibility via `ebb--cursor-visible(term) -> bool`.

Elisp:
- After `ebb--render`, call `ebb--cursor-style` and set buffer-local
  `cursor-type` accordingly: `box`, `(bar . 2)`, `(hbar . 2)`, or
  `hollow`.
- When cursor is invisible, set `cursor-type` to nil.

### A5. Immediate redraw for typing echo — Elisp, S

Eliminate the 8-33ms timer delay for interactive keystrokes.

Elisp:
- Record timestamp in `ebb--send-key` (`ebb--last-send-time`).
- In `ebb--process-filter`, if output arrives within 50ms of last
  keystroke and output size < 256 bytes, bypass the timer and call
  `ebb--flush-output` immediately.
- Add `ebb-immediate-redraw-threshold` (default 256) and
  `ebb-immediate-redraw-interval` (default 0.05) defcustoms.

### A6. Input coalescing — Elisp, S

Batch rapid keystrokes into a single PTY write to reduce syscall overhead.

Elisp:
- Add `ebb--input-buffer` (list of pending key strings) and
  `ebb--input-timer`.
- In `ebb--send-key`, for single-char keys, push to buffer and schedule
  a 3ms timer to flush. Multi-byte sequences flush immediately
  (including any buffered input first).
- Add `ebb-input-coalesce-delay` defcustom (default 0.003, 0 to disable).

---

## Phase B: Shell Integration

These features tie the terminal to the shell, enabling prompt navigation,
directory tracking without manual RC edits, and Elisp invocation from the
terminal.

### B1. Shell integration scripts — Shell + Elisp, M

Auto-inject shell integration for bash, zsh, and fish. No user RC edits.

Shell:
- Create `etc/ebb.bash`, `etc/ebb.zsh`, `etc/ebb.fish`.
- Each script provides:
  - `__ebb_osc7()` — report CWD via OSC 7.
  - OSC 133 markers: A (prompt start), B (command start), C (output
    start), D (command finished + exit status).
  - `ebb_cmd` helper — call whitelisted Elisp functions via OSC 51.
  - Idempotency guard (skip if already loaded).
  - bash: wrap PROMPT_COMMAND, install DEBUG trap for preexec.
  - zsh: use precmd/preexec hooks.
  - fish: use fish_prompt / fish_preexec / fish_postexec events.

Elisp:
- Add `ebb-shell-integration` defcustom (default t).
- When starting the shell, if `ebb-shell-integration` is non-nil:
  - Set `EMACS_EBB_PATH` env var to the package directory.
  - bash: use `--rcfile` that sources both user rc and ebb.bash.
    Or inject via `BASH_ENV` / `ENV`.
  - zsh: prepend `ZDOTDIR` trick or use `--rcs` with sourcing.
  - fish: use `--init-command` to source ebb.fish.
- Set `INSIDE_EMACS` to `<version>,ebb`.

### B2. OSC 133 semantic prompt markers — Rust + Elisp, M

Enable prompt-to-prompt navigation and exit status tracking.

Rust:
- wezterm-term parses OSC 133 internally and stores semantic zones
  in `SemanticZone` data. Add `ebb--get-semantic-zones(term) -> list`
  or handle OSC 133 in the Elisp process filter by scanning raw bytes
  (like ghostel does in Zig `extractOsc133`).
- Alternatively, since ebb already feeds raw bytes through Elisp, scan
  for `\e]133;` sequences in `ebb--process-filter` before feeding to
  Rust and call an Elisp handler.

Elisp:
- `ebb--osc133-marker(type param)` — called for each A/B/C/D marker.
- On marker A (prompt start): record buffer position in
  `ebb--prompt-positions` alist as `(line . nil)`.
- On marker D (command finished): record exit status in the most recent
  prompt entry: `(line . exit-code)`.
- During rendering, apply `ebb-prompt` text property to prompt regions
  (either from cell-level semantic data or from tracked positions).
- Re-apply prompt properties after full redraws using saved positions.

### B3. Prompt navigation — Elisp, S

Jump between shell prompts using OSC 133 markers.

Elisp:
- `ebb-next-prompt` / `ebb-previous-prompt` commands.
  Use `text-property-search-forward/backward` for the `ebb-prompt`
  text property. Skip the current prompt region when navigating
  backwards.
- Bind to `C-c C-n` / `C-c C-p` in both terminal and copy mode keymaps.
- Works in both copy mode (navigate freely) and terminal mode
  (scroll to prompt, optionally enter copy mode).

### B4. OSC 51 Elisp eval from shell — Elisp, S

Let shell scripts call whitelisted Emacs functions.

Elisp:
- Scan raw PTY output for `\e]51;E<payload>ST` sequences.
- Parse payload as space-separated quoted strings.
- First string is the command name; look up in `ebb-eval-cmds` alist.
- If found, funcall with remaining strings as arguments.
- Add `ebb-eval-cmds` defcustom:
  ```elisp
  (defcustom ebb-eval-cmds
    '(("find-file" find-file)
      ("find-file-other-window" find-file-other-window)
      ("dired" dired)
      ("dired-other-window" dired-other-window)
      ("message" message))
    ...)
  ```
- Shell scripts use `ebb_cmd find-file /path/to/file`.

### B5. OSC 52 clipboard — Elisp, S

Allow terminal programs to set the Emacs kill ring (opt-in for security).

Elisp:
- Scan raw PTY output for `\e]52;<selection>;<base64-data>ST`.
- Ignore query sequences (data = `?`).
- Base64-decode the data, call `kill-new` with the result.
- Gate on `ebb-enable-osc52` defcustom (default nil).
- When enabled, also set the system clipboard via
  `gui-set-selection 'CLIPBOARD`.

---

## Phase C: Copy Mode & Clipboard

Improvements to the existing copy mode and paste handling.

### C1. Soft-wrap newline filtering — Elisp, S

When copying text from the terminal, strip newlines that were inserted
by soft line wrapping so the copied text matches the original output.

Elisp:
- During rendering, when a row is soft-wrapped (wezterm marks wrapped
  lines), set an `ebb-wrap` text property on the newline character.
- `ebb--filter-soft-wraps(text)` — remove newlines with the `ebb-wrap`
  property from a string.
- Call this filter in copy mode's M-w / C-w handler before adding to
  the kill ring.

Rust:
- During rendering, check `line.is_wrapped()` for each line. If wrapped,
  inform Elisp (return a flag per row, or set the text property from
  Rust during insert).

### C2. Yank-pop (M-y) — Elisp, S

Cycle through the kill ring when pasting into the terminal.

Elisp:
- Track `ebb--yank-index` (buffer-local, reset to 0 on `ebb-yank`).
- `ebb-yank`: paste `(current-kill 0)`, set `this-command` to
  `ebb-yank`.
- `ebb-yank-pop`: check `last-command` is `ebb-yank` or `ebb-yank-pop`.
  Send backspaces to erase previous paste length, then paste next
  kill ring entry. Increment `ebb--yank-index`.
- Bind `M-y` in semi-char-mode-map.

### C3. Scroll-on-input — Elisp, S

Auto-scroll to the bottom of the terminal when the user types while
the viewport is scrolled into scrollback.

Elisp:
- Add `ebb-scroll-on-input` defcustom (default t).
- In `ebb--self-insert` and `ebb--send-event`, if the defcustom is
  non-nil and the viewport is not at the bottom, scroll to bottom
  before sending the key.
- "Not at bottom" can be detected by checking if point is in the
  scrollback region (before `ebb--display-begin`).

### C4. Enhanced copy mode — Elisp, S

Add page scrolling, scroll-to-top/bottom, and line-by-line navigation
with viewport scrolling at edges (matching ghostel's copy mode).

Elisp:
- `ebb-copy-mode-scroll-up` / `ebb-copy-mode-scroll-down` — page scroll.
- `ebb-copy-mode-beginning-of-buffer` / `ebb-copy-mode-end-of-buffer`.
- `ebb-copy-mode-previous-line` / `ebb-copy-mode-next-line` — scroll
  viewport when at top/bottom edge.
- `ebb-copy-mode-end-of-line` — move to last non-whitespace.
- Bind `M-v`/`C-v`, `M-<`/`M->`, `C-n`/`C-p`, `C-e` in copy mode map.
- Exit-and-send: normal letter keys exit copy mode and forward the key.

---

## Phase D: Link Detection & Navigation

### D1. Plain-text URL detection — Elisp, M

Auto-linkify `http://` and `https://` URLs in terminal output even
without OSC 8.

Elisp:
- `ebb--detect-urls` — called after each render.
- Regex scan the buffer for `https?://[^\s)>\]]+`.
- Strip trailing punctuation (`.`, `,`, `;`, `:`, `!`, `?`).
- Skip regions that already have `help-echo` (existing OSC 8 links).
- Apply `help-echo` (the URL), `mouse-face` (`highlight`), and
  `keymap` (`ebb-link-map`) text properties.
- Add `ebb-enable-url-detection` defcustom (default t).

### D2. File:line path detection — Elisp, M

Make compiler errors and stack traces clickable.

Elisp:
- `ebb--detect-file-refs` — called from `ebb--detect-urls`.
- Regex: absolute paths followed by `:<line>` —
  `\(/[^\s:]+\):\([0-9]+\)`.
- Verify file exists with `file-exists-p`.
- Apply `help-echo` as `fileref:/path/to/file:42`, `mouse-face`,
  `keymap`.
- `ebb--open-link` dispatcher: if help-echo starts with `fileref:`,
  parse path and line, call `find-file-other-window` and `goto-line`.
  Otherwise call `browse-url`.
- Add `ebb-enable-file-detection` defcustom (default t).

### D3. Link keymap and open handler — Elisp, S

Shared infrastructure for OSC 8, URL, and file links.

Elisp:
- Define `ebb-link-map` keymap with `mouse-1`, `mouse-2`, `RET` bound
  to `ebb-open-link-at-point`.
- `ebb-open-link-at-point` reads `help-echo` at point, dispatches to
  `ebb--open-link`.
- Refactor existing OSC 8 hyperlink handling to use this shared keymap
  instead of a separate one.

---

## Phase E: Theme & Color Integration

### E1. ANSI color faces — Elisp, S

Define 16 named faces that inherit from `term-color-*` so user themes
automatically apply.

Elisp:
- Define `ebb-color-black` through `ebb-color-bright-white` (16 faces).
- Each inherits from the corresponding `term-color-*` face.
- For bright variants, check if `term-color-bright-*` exists (Emacs 28+)
  and fall back to the non-bright variant.
- Store in `ebb-color-palette` vector for indexed lookup.

### E2. Color palette from Emacs faces — Rust + Elisp, M

Sync the terminal's ANSI palette with Emacs face colors so terminal
output respects the user's theme.

Rust:
- Add `ebb--set-palette(term, colors-string) -> t/nil`.
  Parse a concatenated `#RRGGBB` string (16 entries, 7 chars each).
  Set palette entries 0-15 on the wezterm Terminal.

Elisp:
- `ebb--apply-palette(term)` — extract foreground color from each
  `ebb-color-*` face, concatenate into a hex string, call
  `ebb--set-palette`.
- `ebb--face-hex-color(face property)` — resolve face color to
  `#RRGGBB` string using `face-attribute` and `color-values`.
- Call `ebb--apply-palette` during terminal creation and after theme
  changes.

### E3. Default FG/BG from theme — Rust + Elisp, S

Set the terminal's default foreground and background colors to match the
Emacs default face.

Rust:
- Add `ebb--set-default-colors(term, fg-hex, bg-hex) -> t/nil`.
  Parse `#RRGGBB` strings, call `terminal.set_color_palette()` or the
  appropriate wezterm-term API to set default fg/bg.

Elisp:
- Extract colors from `(face-attribute 'default :foreground)` and
  `:background`.
- Call during terminal creation and theme sync.

### E4. Buffer-level face remapping — Elisp, S

Set the buffer background color to match the terminal background so
empty regions don't show the Emacs default background.

Elisp:
- Use `face-remap-add-relative` to remap the `default` face in the
  terminal buffer with the correct `:background` and `:foreground`.
- Update the remapping whenever the theme changes.

### E5. Theme sync command — Elisp, S

Re-sync the terminal palette after an Emacs theme change.

Elisp:
- `ebb-sync-theme` interactive command.
- Iterates all live ebb buffers, calls `ebb--apply-palette`,
  `ebb--set-default-colors`, and `ebb--update-buffer-face`.
- Forces a full redraw of each terminal.
- Optionally hook into `after-load-theme-hook` or
  `enable-theme-functions` (Emacs 29+).

---

## Phase F: UX Polish

### F1. Send-next-key escape hatch — Elisp, S

Let the user send any key literally, even ones normally intercepted
by Emacs.

Elisp:
- `ebb-send-next-key` command: call `read-key-sequence`, extract the
  key event, route through the key encoder or send raw bytes.
- Bind to `C-c C-q` in semi-char mode.

### F2. Configurable keymap exceptions — Elisp, S

Let users customize which key prefixes pass through to Emacs.

Elisp:
- Add `ebb-keymap-exceptions` defcustom (default list matching
  ghostel: `C-c`, `C-x`, `C-u`, `C-h`, `C-g`, `M-x`, `M-o`,
  `M-:`, `C-\`).
- Generate the semi-char mode keymap dynamically from this list
  instead of hardcoding the non-forwarded keys.

### F3. Drag-and-drop — Elisp, S

Accept dropped files (as shell-quoted paths) and text (as bracketed
paste).

Elisp:
- Bind `[drag-n-drop]` in semi-char mode keymap.
- Handler: inspect event structure. If type is `file`, join paths
  with `shell-quote-argument` and send as key input. If text,
  send via `ebb--send-paste`.

### F4. Project integration — Elisp, S

Open a terminal rooted at the current project directory.

Elisp:
- `ebb-project` interactive command: use `project-root` to find
  the project root, set `default-directory`, call `ebb`.
  Buffer name includes project name.
- `ebb-other` command: switch to next ebb buffer or create one.
- Provide entry for `project-switch-commands`:
  `(add-to-list 'project-switch-commands '(ebb-project "EBB") t)`.

### F5. Raw key fallback — Elisp, S

When the Rust key encoder returns nil, fall back to CSI u encoding
for modified special keys.

Elisp:
- `ebb--raw-key-sequence(key-name mods)` — build escape sequences:
  - Ctrl+letter -> control character.
  - Meta+letter -> ESC + char.
  - Modified specials (return, tab, backspace, escape) -> CSI u format
    `\e[<code>;<1+mods>u`.
  - Cursor keys -> `\e[1;<1+mods><letter>`.
  - Function keys -> SS3 or tilde sequences with modifier parameter.
- `ebb--modifier-number(mods)` — convert modifier string to bitmask.
- Insert into the existing `ebb--send-key` fallback chain between
  the Rust encoder and the simple byte table.

---

## Phase G: Distribution & Quality

### G1. Test suite — Elisp, L

Comprehensive ERT tests covering both pure Elisp logic and native module
integration.

Elisp tests (no module required):
- Raw key sequence builder (all key types + modifiers).
- Modifier number calculation.
- Soft-wrap filter function.
- URL/file detection regex.
- OSC sequence scanning.
- Directory update helper.

Integration tests (require module):
- Terminal creation and basic properties.
- Write-input and render state.
- Backspace handling.
- Cursor movement sequences.
- Erase sequences.
- Resize.
- Scrollback.
- SGR styling.
- Title change (OSC 2).
- CWD tracking (OSC 7).
- CRLF handling.
- Incremental redraw correctness.
- Wide character rendering.
- Hyperlink properties.
- Shell process integration (echo, backspace via PTY).

Infrastructure:
- Create `test/ebb-test.el`.
- `ebb-test-run` (full suite) and `ebb-test-run-elisp` (pure Elisp).
- Add `make test`, `make all` targets.
- Helper functions: `ebb-test--row0`, `ebb-test--cursor`.

### G2. Benchmark suite — Elisp + Shell, M

Throughput and typing latency measurement.

Shell:
- `bench/run-bench.sh` — stream 1MB through `cat`, measure throughput.
- Compare against vterm, eat, term.

Elisp:
- `ebb-debug-typing-latency` — interactive measurement that sends keys
  and records per-keystroke PTY + render + total latency.
- Report min/median/p99/max statistics.

### G3. Auto-download prebuilt binaries — Elisp, M

Zero-friction installation for users who don't want to build from source.

Elisp:
- `ebb--module-platform-tag` — detect arch + OS
  (`x86_64-linux`, `aarch64-linux`, `x86_64-macos`, `aarch64-macos`).
- `ebb--module-download-url` — construct GitHub release URL.
- `ebb-download-module` interactive command — download and load.
- `ebb-module-auto-install` defcustom (`ask` / `download` / `compile` /
  nil).
- On `(require 'el-be-back)`, if module is missing, dispatch based on
  the defcustom.

Build:
- GitHub Actions workflow to build for all 4 platforms on each release
  tag and upload artifacts.

### G4. Module version checking — Elisp, S

Detect stale native modules and offer to update.

Elisp:
- `ebb--minimum-module-version` constant — bump when Elisp requires a
  newer module.
- On load, call `ebb--version`, compare with minimum. If older, warn
  and offer to download/compile via `ebb-module-auto-install`.

### G5. MELPA packaging — Build, M

Publish to MELPA for standard `package-install`.

Build:
- Create MELPA recipe.
- Ensure `el-be-back.el` has correct Package-Requires header.
- Test that `ebb-download-module` works from a MELPA install (no
  build.sh available).
- Document the MELPA install path in README.

### G6. Adaptive FPS — Elisp, S

Stop the redraw timer when idle to save CPU; use a shorter initial
delay for responsive interactive feedback.

Elisp:
- Add `ebb-adaptive-fps` defcustom (default t).
- When enabled: first frame after idle uses a shorter delay (e.g. 16ms).
  Subsequent frames use the standard `ebb-maximum-latency`.
  When no output arrives for 2x the timer delay, cancel the timer
  entirely (restart on next process filter call).

---

## Phase H: Advanced Features (Lower Priority)

These are nice-to-have features that go beyond ghostel parity.

### H1. Line mode (comint-like editing) — Elisp, L

Type a command with full Emacs editing (completion, history), send on
RET. Already described in DESIGN.md Phase 7.

### H2. Sixel graphics rendering — Rust + Elisp, L

Render inline images from sixel escape sequences. wezterm-term already
parses sixel data. Extract image bytes, create Emacs image descriptors,
apply as `display` text properties.

### H3. Kitty graphics rendering — Rust + Elisp, L

Same approach as sixel but for the kitty graphics protocol. wezterm-term
stores image placements in cell attributes.

### H4. Custom terminfo — Build, S

Ship an `ebb.ti` terminfo entry with correct capability descriptions.
Set `TERMINFO` env var to the package's terminfo directory.

### H5. Eshell integration — Elisp, M

Use ebb as the terminal backend for Eshell's terminal-capable programs.

---

## Dependency Graph

```
Phase A (core terminal UX)
  A1 Mouse tracking
  A2 Focus events
  A3 Synchronized output
  A4 Cursor style
  A5 Immediate redraw
  A6 Input coalescing

Phase B (shell integration) — depends on rendering being solid
  B1 Shell integration scripts
  B2 OSC 133 markers ← depends on B1 (scripts emit the markers)
  B3 Prompt navigation ← depends on B2
  B4 OSC 51 eval ← depends on B1 (scripts provide ebb_cmd helper)
  B5 OSC 52 clipboard

Phase C (copy mode) — independent
  C1 Soft-wrap filtering
  C2 Yank-pop
  C3 Scroll-on-input
  C4 Enhanced copy mode

Phase D (links) — independent
  D1 URL detection
  D2 File:line detection ← depends on D1 (shared infra)
  D3 Link keymap ← D1 and D2 use this; refactors existing OSC 8

Phase E (theme) — independent
  E1 ANSI color faces
  E2 Palette from faces ← depends on E1
  E3 Default FG/BG from theme
  E4 Buffer face remapping
  E5 Theme sync ← depends on E1-E4

Phase F (polish) — independent
  F1 Send-next-key
  F2 Configurable exceptions
  F3 Drag-and-drop
  F4 Project integration
  F5 Raw key fallback

Phase G (quality) — can start anytime but benefits from features existing
  G1 Test suite
  G2 Benchmark suite
  G3 Auto-download binaries
  G4 Module version check
  G5 MELPA packaging ← depends on G3
  G6 Adaptive FPS
```

## Suggested Implementation Order

Phases A through G can be interleaved. A reasonable order that maximizes
usability at each step:

1. **A5, A6** — Immediate redraw + input coalescing (pure Elisp, quick wins)
2. **A1** — Mouse tracking (unlocks TUI apps)
3. **A3** — Synchronized output (fixes flicker in TUI apps)
4. **A4, A2** — Cursor style + focus events (small Rust additions)
5. **D3, D1, D2** — Link detection infra + URL + file refs
6. **E1, E2, E3, E4, E5** — Full theme integration
7. **B1** — Shell integration scripts
8. **B2, B3** — OSC 133 + prompt navigation
9. **B4, B5** — OSC 51 eval + OSC 52 clipboard
10. **C1, C2, C3, C4** — Copy mode improvements
11. **F1-F5** — Polish features
12. **G1** — Test suite (write tests alongside or after each phase)
13. **G2, G6** — Benchmarks + adaptive FPS
14. **G3, G4, G5** — Distribution (auto-download, version check, MELPA)

## Where ebb Is Already Ahead

These features exist in ebb but not in ghostel — preserve them:

- **TRAMP integration** — remote directory SSH sessions, CWD tracking
  via TRAMP paths.
- **Nix flake** — reproducible builds with `flake.nix`.
- **Kitty graphics protocol** — enabled in config (ghostel does not
  mention it).
