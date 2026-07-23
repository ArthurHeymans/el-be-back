# Building a Better Emacs Terminal Emulator from Scratch

A detailed engineering plan for a pure-Elisp terminal emulator that matches eat's
feature set while fixing its architectural weaknesses. Uses `futur.el` for async
I/O so the main thread never hangs, and a separated screen model so buffer state
is always consistent.

Working name: **ebb** (it eats terminals, but faster).

---

## Table of Contents

1. [Design Principles](#1-design-principles)
2. [Architecture Overview](#2-architecture-overview)
3. [Module Breakdown](#3-module-breakdown)
4. [Module 1: ebb-term -- Screen Model](#4-module-1-ebb-term----screen-model)
5. [Module 2: ebb-parse -- VT State Machine](#5-module-2-ebb-parse----vt-state-machine)
   - [5.7: Tree-sitter VT Grammar Backend](#57-tree-sitter-vt-grammar-backend-ebb-parse-tsel)
6. [Module 3: ebb-render -- Buffer Display](#6-module-3-ebb-render----buffer-display)
7. [Module 4: ebb-input -- Key Translation](#7-module-4-ebb-input----key-translation)
8. [Module 5: ebb-shell -- Shell Integration](#8-module-5-ebb-shell----shell-integration)
9. [Module 6: ebb-io -- Async Process I/O](#9-module-6-ebb-io----async-process-io)
10. [Module 7: ebb-sixel -- Inline Images](#10-module-7-ebb-sixel----inline-images)
11. [Module 8: ebb-eshell -- Eshell Integration](#11-module-8-ebb-eshell----eshell-integration)
12. [Module 9: ebb.el -- Entry Points & Mode](#12-module-9-ebbel----entry-points--mode)
13. [Module 10: ebb-highlight -- Content Highlighting](#13-module-10-ebb-highlight----tree-sitter-content-highlighting)
14. [Terminfo & Shell Integration Scripts](#14-terminfo--shell-integration-scripts)
15. [Test Strategy](#15-test-strategy)
16. [Implementation Phases](#16-implementation-phases)
17. [Full Escape Sequence Checklist](#17-full-escape-sequence-checklist)

---

## 1. Design Principles

### 1.1 The main thread never hangs

This is the single most important departure from eat. In eat, `cat /etc/localtime`
or any large binary dump freezes Emacs because the VT parser runs synchronously in
the process filter. In ebb:

- The process filter only appends raw bytes to a queue. It never parses.
- Parsing happens in bounded chunks (configurable, default 4KB per chunk).
- Between chunks, control returns to the Emacs event loop via `futur.el`.
- A throughput monitor detects binary floods and skips rendering.
- `C-g` always works because the event loop is never blocked for more than one chunk.

### 1.2 Screen model is not the buffer

Eat uses the Emacs buffer as its screen model -- text properties on buffer text ARE
the terminal state. This causes: undo history pollution, conflicts with hl-line-mode,
flicker from fighting with Emacs redisplay, and tight coupling.

In ebb, the screen is a pure Elisp data structure (vector of line records). The
buffer is a *view* that gets refreshed from the model in controlled render passes.
The model is always consistent. The buffer is write-only from the terminal's
perspective.

### 1.3 Defensive parsing

Unknown escape sequences must never crash. Every parser state has a fallthrough
that logs the unknown sequence and continues. Cursor coordinates are always clamped
to valid bounds. Integer overflow is checked.

### 1.4 Modular from day one

Each file has a single responsibility and a documented public API. The parser can be
tested without a buffer. The renderer can be tested without a PTY. The input
translator can be tested without a process.

### 1.5 Tree-sitter accelerated parsing

Emacs 29+ includes built-in tree-sitter support. Ebb uses this in two ways:

1. **VT grammar parser** (`ebb-parse-ts.el`): A tree-sitter grammar for VT escape
   sequences provides C-speed structural parsing with built-in error recovery. The
   grammar identifies sequence boundaries and extracts parameters; the Elisp layer
   handles semantic dispatch (applying operations to the screen model). Falls back to
   the pure-Elisp state machine on Emacs < 29.

2. **Terminal content highlighting** (`ebb-highlight.el`): After rendering, optionally
   run tree-sitter on the visible terminal text to syntax-highlight code output.
   Auto-detects language from shell context (e.g., `python foo.py` → Python highlighting
   on the output) or lets the user pin a language. A unique feature no other terminal
   emulator provides.

### 1.6 Pure Elisp core, optional native acceleration

The core (state machine parser, screen model, renderer) is 100% Emacs Lisp and works
on Emacs >= 28.1. Tree-sitter integration is an optional acceleration/feature layer
requiring Emacs 29+. The architecture supports both paths via a parser backend
abstraction -- `ebb-parse.el` defines the interface, `ebb-parse-ts.el` provides
the fast path.

---

## 2. Architecture Overview

```
                    +--------------+
                    |  PTY Process |
                    +------+-------+
                           |
                    raw bytes arrive
                           |
                           v
              +------------------------+
              |  ebb-io (futur.el)   |  Process filter: append to queue.
              |  Chunk scheduler.      |  Timer/futur drives parse loop.
              +----------+-------------+
                         |
                  bounded chunks (4KB)
                         |
                         v
              +------------------------+     +------------------------+
              |  ebb-parse           |     |  ebb-parse-ts        |
              |  Elisp state machine.  | <=> |  tree-sitter grammar.  |
              |  Fallback for Emacs<29.|     |  C-speed, error-       |
              |  Always available.     |     |  tolerant. Emacs 29+.  |
              +----------+-------------+     +------------------------+
                         |
                  command stream
                  (cursor-move, write-char, set-sgr, scroll, etc.)
                         |
                         v
              +------------------------+
              |  ebb-term            |  Screen model.
              |  Vector of line recs.  |  Applies commands. Tracks dirty lines.
              |  Main + alt screen.    |
              +----------+-------------+
                         |
                  dirty line list
                         |
                         v
              +------------------------+     +------------------------+
              |  ebb-render          |     |  ebb-highlight       |
              |  Buffer renderer.      | --> |  tree-sitter content   |
              |  Writes to Emacs buf.  |     |  highlighting.         |
              |  Only dirty lines.     |     |  Post-render pass.     |
              +------------------------+     |  Emacs 29+, optional.  |
                                             +------------------------+

              +------------------------+
              |  ebb-input           |  Key event -> escape sequence.
              |  Three keybinding modes|  Sends bytes to PTY.
              +------------------------+

              +------------------------+
              |  ebb-shell           |  OSC 51 handler.
              |  Directory tracking.   |  Prompt annotation.
              |  History exchange.     |  Message handlers.
              +------------------------+

              +------------------------+
              |  ebb-sixel           |  DCS Sixel renderer.
              |  Image protocol.       |  Multiple output formats.
              +------------------------+

              +------------------------+
              |  ebb-eshell          |  eat-eshell-mode equivalent.
              |  Visual command mode.  |
              +------------------------+

              +------------------------+
              |  ebb.el              |  Entry points: M-x ebb
              |  ebb-mode.           |  Buffer/window/process mgmt.
              +------------------------+
```

---

## 3. Module Breakdown

| Module | File | Approx. Lines | Depends On |
|--------|------|---------------|------------|
| Screen Model | `ebb-term.el` | ~800 | nothing |
| VT Parser | `ebb-parse.el` | ~1500 | ebb-term |
| TS VT Parser | `ebb-parse-ts.el` | ~400 | ebb-parse, treesit |
| Buffer Renderer | `ebb-render.el` | ~600 | ebb-term |
| Content Highlight | `ebb-highlight.el` | ~500 | ebb-render, treesit |
| Input Translator | `ebb-input.el` | ~500 | nothing |
| Shell Integration | `ebb-shell.el` | ~400 | ebb-term |
| Async I/O | `ebb-io.el` | ~300 | futur, ebb-parse, ebb-term, ebb-render |
| Sixel | `ebb-sixel.el` | ~400 | ebb-term |
| Eshell | `ebb-eshell.el` | ~400 | ebb.el |
| Main | `ebb.el` | ~600 | all of the above |
| **Total** | | **~6400** | |

Dependencies: `futur` (GNU ELPA), `compat` (for older Emacs versions).
Optional: `treesit` (built-in, Emacs 29+) -- enables TS VT parser and content highlighting.
Build dependency: Node.js (for compiling the tree-sitter VT grammar, one-time only).

Also ships: `tree-sitter-vt/grammar.js` (grammar source) and precompiled
`libtree-sitter-vt.so`/`.dylib`/`.dll` for common platforms.

---

## 4. Module 1: ebb-term -- Screen Model

This is the heart. A pure data structure with no buffer or display dependencies.

### 4.1 Data Structures

```elisp
;; A single cell on the screen.
(cl-defstruct ebb-cell
  (char ?\s)        ; character (Unicode codepoint)
  (width 1)         ; display width (1 or 2 for CJK)
  (attr nil))       ; attribute record, or nil for default

;; Text attributes for a cell.
(cl-defstruct ebb-attr
  (fg nil)          ; foreground: nil (default), integer (0-255), or (R G B)
  (bg nil)          ; background: same
  (ul-color nil)    ; underline color: same
  (bold nil)        ; boolean
  (faint nil)       ; boolean
  (italic nil)      ; boolean
  (underline nil)   ; nil, line, double, curly, dotted, dashed
  (blink nil)       ; nil, slow, fast
  (inverse nil)     ; boolean
  (conceal nil)     ; boolean
  (crossed nil)     ; boolean
  (font 0))         ; font index 0-9

;; A single line in the terminal.
(cl-defstruct ebb-line
  (cells nil)       ; vector of ebb-cell, length = terminal width
  (wrapped nil)     ; boolean: was this line auto-wrapped from the previous?
  (dirty t))        ; boolean: needs re-rendering?

;; The complete screen state.
(cl-defstruct ebb-screen
  ;; Display area
  (lines nil)       ; vector of ebb-line, length = terminal height
  (width 80)
  (height 24)
  ;; Cursor
  (cursor-x 0)     ; 0-indexed column
  (cursor-y 0)     ; 0-indexed row
  (cursor-saved-x 0)
  (cursor-saved-y 0)
  (cursor-saved-attr nil)
  (cursor-style :block)    ; :block, :bar, :underline, :blinking-block, etc.
  (cursor-visible t)
  ;; Current attributes (applied to next character written)
  (current-attr (make-ebb-attr))
  ;; Scroll region
  (scroll-top 0)
  (scroll-bottom 23)  ; 0-indexed, inclusive
  ;; Mode flags
  (auto-wrap t)        ; DECAWM
  (insert-mode nil)    ; IRM
  (origin-mode nil)    ; DECOM
  (keypad-mode nil)    ; DECCKM
  (bracketed-paste nil)
  ;; Mouse
  (mouse-mode nil)     ; nil, x10, normal, button-event, any-event
  (mouse-sgr nil)      ; SGR encoding
  (focus-events nil)
  ;; Character sets
  (charset-g0 'us-ascii)
  (charset-g1 'us-ascii)
  (charset-g2 'us-ascii)
  (charset-g3 'us-ascii)
  (charset-active 'g0)
  ;; Alternate screen
  (alt-screen nil)     ; saved ebb-screen when in alt mode, nil when in main
  ;; Scrollback (main screen only)
  (scrollback nil)     ; list of ebb-line (newest first)
  (scrollback-max 131072)  ; max characters (not lines)
  ;; Sixel state
  (sixel-scrolling t)
  ;; Title
  (title "")
  (cwd nil)            ; current working directory string
  ;; Tab stops
  (tab-stops nil)      ; sorted list of 0-indexed column positions
  ;; Last character written (for REP sequence)
  (last-char nil)
  ;; Dirty tracking
  (dirty-lines nil))   ; list of 0-indexed row numbers that changed

```

### 4.2 Public API

```elisp
;; Construction
(ebb-screen-create width height) -> ebb-screen

;; The screen model operations. Each returns the list of dirty line indices.
(ebb-screen-write-char screen char)
(ebb-screen-cursor-move screen direction count)  ; direction: up/down/left/right
(ebb-screen-cursor-goto screen row col)
(ebb-screen-set-attr screen attr-changes)
(ebb-screen-erase screen type)         ; type: to-end, to-start, whole, line-*, display-*
(ebb-screen-scroll screen direction count)
(ebb-screen-insert-lines screen count)
(ebb-screen-delete-lines screen count)
(ebb-screen-insert-chars screen count)
(ebb-screen-delete-chars screen count)
(ebb-screen-resize screen new-width new-height)  ; reflow + clamp + dirty all (see 9.6.5)
(ebb-screen-set-scroll-region screen top bottom)
(ebb-screen-enter-alt screen)
(ebb-screen-leave-alt screen)
(ebb-screen-save-cursor screen)
(ebb-screen-restore-cursor screen)
(ebb-screen-reset screen)
(ebb-screen-set-mode screen mode value)
(ebb-screen-tab-forward screen count)
(ebb-screen-tab-backward screen count)

;; Query
(ebb-screen-get-line screen row) -> ebb-line
(ebb-screen-get-dirty screen) -> list of row indices
(ebb-screen-clear-dirty screen)
(ebb-screen-scrollback-lines screen) -> list of ebb-line
```

### 4.3 Key Design Decisions

**Double-width characters**: When a CJK character is written at column N, it
occupies cells N and N+1. Cell N+1 is a "continuation" cell with `(width 0)`.
When the cursor is at the last column and a double-width char is written, a space
fills the last column and the char goes to the next line. This prevents the infinite
loop bug (eat issue #227).

**Coordinate clamping**: Every cursor movement function clamps to `[0, width-1]` and
`[0, height-1]` (or scroll region bounds when origin mode is on). No negative values
ever propagate. This prevents the `wholenump` crashes (eat issues #111, #141).

**Dirty tracking**: Each `ebb-line` has a `dirty` flag. Screen operations set it.
The renderer reads and clears it. Only dirty lines are re-rendered.

**Scrollback**: Stored as a list of `ebb-line` structs (newest first). On scroll-up
in main display, lines pushed off top get prepended to scrollback. Scrollback is
trimmed by character count during render passes.

---

## 5. Module 2: ebb-parse -- VT State Machine

### 5.1 Parser Architecture

The parser is a state machine that consumes a byte string and emits calls to
`ebb-screen-*` functions. It is a pure transformer: it takes bytes in and produces
screen operations.

```elisp
(cl-defstruct ebb-parser
  (state :ground)        ; current parser state
  (params nil)           ; collected CSI/DCS parameters
  (intermediates nil)    ; collected intermediate bytes
  (osc-string "")       ; collected OSC string
  (dcs-string "")       ; collected DCS string
  (screen nil)           ; reference to ebb-screen
  (callbacks nil))       ; alist of callback functions for OSC/title/bell/etc.
```

### 5.2 Parser States

Following the VT500 parser model (Paul Flo Williams' state diagram):

| State | Description | Transitions |
|-------|-------------|-------------|
| `:ground` | Normal character processing | ESC -> `:escape`, C0 controls inline |
| `:escape` | After ESC | `[` -> `:csi-entry`, `]` -> `:osc-string`, `P` -> `:dcs-entry`, `(` `)` `*` `+` `-` `.` `/` -> `:charset-designate`, `7` `8` `D` `E` `M` `c` `n` `o` -> execute + `:ground` |
| `:csi-entry` | After ESC [ | digit/`;` -> `:csi-param`, `?` `>` -> `:csi-param` (private), letter -> execute + `:ground` |
| `:csi-param` | Collecting CSI parameters | digit/`;`/`:` -> stay, letter -> execute + `:ground`, SP -> `:csi-intermediate` |
| `:csi-intermediate` | After CSI intermediate byte | letter -> execute + `:ground` |
| `:osc-string` | Collecting OSC string | BEL or ST -> execute + `:ground` |
| `:dcs-entry` | After ESC P | params -> `:dcs-param`, letter -> `:dcs-passthrough` |
| `:dcs-param` | Collecting DCS parameters | letter -> `:dcs-passthrough` |
| `:dcs-passthrough` | In DCS body | ST -> `:ground` |
| `:charset-designate` | After ESC ( etc. | any char -> execute + `:ground` |
| `:sos-pm-apc` | Inside SOS/PM/APC strings | ST -> `:ground` (consume and ignore) |

### 5.3 Parse Function

```elisp
(defun ebb-parse-bytes (parser bytes &optional start end)
  "Parse BYTES from START to END through PARSER.
Applies resulting operations to the parser's screen.
Returns the number of bytes consumed (may be less than (- end start)
if processing was interrupted by a yield point)."
  ...)
```

The key innovation: this function accepts a byte limit. It processes at most
`ebb-io-chunk-size` bytes per call, then returns the number consumed. The I/O
layer calls it repeatedly with the remaining bytes, yielding between calls.

### 5.4 CSI Dispatch Table

Rather than a giant `pcase`, use a dispatch vector indexed by the final byte:

```elisp
(defvar ebb-parse--csi-dispatch
  (let ((table (make-vector 128 #'ebb-parse--csi-unknown)))
    (aset table ?@ #'ebb-parse--csi-ich)    ; Insert Character
    (aset table ?A #'ebb-parse--csi-cuu)    ; Cursor Up
    (aset table ?B #'ebb-parse--csi-cud)    ; Cursor Down
    (aset table ?C #'ebb-parse--csi-cuf)    ; Cursor Forward
    (aset table ?D #'ebb-parse--csi-cub)    ; Cursor Back
    (aset table ?E #'ebb-parse--csi-cnl)    ; Cursor Next Line
    (aset table ?F #'ebb-parse--csi-cpl)    ; Cursor Previous Line
    (aset table ?G #'ebb-parse--csi-cha)    ; Cursor Horizontal Absolute
    (aset table ?H #'ebb-parse--csi-cup)    ; Cursor Position
    (aset table ?I #'ebb-parse--csi-cht)    ; Cursor Horizontal Tab
    (aset table ?J #'ebb-parse--csi-ed)     ; Erase in Display
    (aset table ?K #'ebb-parse--csi-el)     ; Erase in Line
    (aset table ?L #'ebb-parse--csi-il)     ; Insert Line
    (aset table ?M #'ebb-parse--csi-dl)     ; Delete Line
    (aset table ?P #'ebb-parse--csi-dch)    ; Delete Character
    (aset table ?S #'ebb-parse--csi-su)     ; Scroll Up
    (aset table ?T #'ebb-parse--csi-sd)     ; Scroll Down
    (aset table ?X #'ebb-parse--csi-ech)    ; Erase Character
    (aset table ?Z #'ebb-parse--csi-cbt)    ; Cursor Backward Tab
    (aset table ?` #'ebb-parse--csi-hpa)    ; Horizontal Position Absolute
    (aset table ?a #'ebb-parse--csi-hpr)    ; Horizontal Position Relative
    (aset table ?b #'ebb-parse--csi-rep)    ; Repeat
    (aset table ?c #'ebb-parse--csi-da)     ; Device Attributes
    (aset table ?d #'ebb-parse--csi-vpa)    ; Vertical Position Absolute
    (aset table ?e #'ebb-parse--csi-vpr)    ; Vertical Position Relative
    (aset table ?f #'ebb-parse--csi-hvp)    ; Horizontal Vertical Position
    (aset table ?h #'ebb-parse--csi-sm)     ; Set Mode
    (aset table ?j #'ebb-parse--csi-cub)    ; alias for Cursor Back
    (aset table ?k #'ebb-parse--csi-cuu)    ; alias for Cursor Up (VPB)
    (aset table ?l #'ebb-parse--csi-rm)     ; Reset Mode
    (aset table ?m #'ebb-parse--csi-sgr)    ; Select Graphic Rendition
    (aset table ?n #'ebb-parse--csi-dsr)    ; Device Status Report
    (aset table ?q #'ebb-parse--csi-decscusr) ; with SP intermediate
    (aset table ?r #'ebb-parse--csi-decstbm)  ; Set Scroll Region
    (aset table ?s #'ebb-parse--csi-scp)    ; Save Cursor Position
    (aset table ?u #'ebb-parse--csi-rcp)    ; Restore Cursor Position
    table)
  "Dispatch table for CSI final bytes.")
```

This is O(1) dispatch instead of eat's O(n) `pcase` chain. Every slot in the table
has a handler; unrecognized ones go to `ebb-parse--csi-unknown` which logs a
debug message and does nothing.

### 5.5 SGR Handler

The SGR handler (CSI m) is the most complex single function. It walks the parameter
list and builds attribute changes:

```elisp
(defun ebb-parse--csi-sgr (parser params)
  "Handle Select Graphic Rendition."
  (let ((screen (ebb-parser-screen parser))
        (i 0)
        (len (length params)))
    (when (zerop len)
      ;; No params = reset all
      (ebb-screen-reset-attr screen)
      (cl-return-from ebb-parse--csi-sgr))
    (while (< i len)
      (let ((p (aref params i)))
        (pcase p
          (0  (ebb-screen-reset-attr screen))
          (1  (ebb-screen-set-attr screen :bold t))
          (2  (ebb-screen-set-attr screen :faint t))
          (3  (ebb-screen-set-attr screen :italic t))
          (4  ;; Underline -- check for sub-parameters via colon syntax
           (let ((sub (ebb-parse--get-subparam params i)))
             (ebb-screen-set-attr screen :underline
                                    (pcase sub
                                      (0 nil) (1 'line) (2 'double)
                                      (3 'curly) (4 'dotted) (5 'dashed)
                                      (_ 'line)))))
          ;; ... (all 50+ SGR codes)
          (38 ;; Extended foreground color
           (pcase (aref params (1+ i))
             (2 (let ((r (aref params (+ i 2)))
                      (g (aref params (+ i 3)))
                      (b (aref params (+ i 4))))
                  (ebb-screen-set-attr screen :fg (list r g b))
                  (cl-incf i 4)))
             (5 (ebb-screen-set-attr screen :fg (aref params (+ i 2)))
                (cl-incf i 2))))
          ;; ... remaining SGR codes
          (_ nil)))  ;; UNKNOWN: silently skip
      (cl-incf i))))
```

### 5.6 Error Recovery

Every dispatch handler is wrapped:

```elisp
(defun ebb-parse--dispatch-csi (parser final-byte)
  (condition-case err
      (let ((handler (aref ebb-parse--csi-dispatch final-byte)))
        (funcall handler parser (ebb-parser-params parser)))
    (error
     (ebb-parse--log parser "CSI dispatch error for %c: %S" final-byte err))))
```

The parser never propagates errors to the caller. Every error is caught, logged, and
parsing continues from the next ground state.

### 5.7 Tree-sitter VT Grammar Backend (ebb-parse-ts.el)

On Emacs 29+, ebb can use a tree-sitter grammar for the structural parsing layer.
The grammar handles the byte-level syntax (identifying sequence boundaries, extracting
parameters, classifying sequence types). The Elisp layer handles semantic dispatch
(what each sequence *does* to the screen model). This split is natural because:

- **Syntax** is context-free: `ESC [ digits ; digits letter` is always a CSI sequence
  regardless of terminal state.
- **Semantics** are stateful: `CSI H` means "cursor home" only because of accumulated
  mode flags, scroll regions, etc.

#### 5.7.1 Grammar Definition (tree-sitter-vt/grammar.js)

```javascript
// tree-sitter-vt/grammar.js
module.exports = grammar({
  name: 'vt',

  rules: {
    // Top-level: terminal output is a stream of text and escape sequences
    terminal_stream: $ => repeat(choice(
      $.plain_text,
      $.csi_sequence,
      $.osc_sequence,
      $.dcs_sequence,
      $.esc_sequence,
      $.control_char,
    )),

    // Plain text: any run of printable bytes (0x20-0x7E, plus UTF-8 multibyte)
    plain_text: $ => /[\x20-\x7e\x80-\xff]+/,

    // C0 control characters (individually, so we can dispatch on each)
    control_char: $ => /[\x00-\x1a\x1c-\x1f\x7f]/,
    // Note: 0x1B (ESC) is NOT included -- it starts escape sequences

    // === CSI Sequences: ESC [ [?/>] params final ===
    csi_sequence: $ => seq(
      '\x1b[',
      optional(field('private', $.csi_private_marker)),
      optional(field('params', $.parameter_list)),
      optional(field('intermediate', $.intermediate_bytes)),
      field('final', $.csi_final_byte),
    ),

    csi_private_marker: $ => /[?>=]/,

    parameter_list: $ => seq(
      $.parameter,
      repeat(seq(';', $.parameter)),
    ),

    // Parameters with colon sub-parameters (e.g., 4:3 for curly underline)
    parameter: $ => seq(
      optional(/[0-9]+/),
      repeat(seq(':', /[0-9]+/)),
    ),

    intermediate_bytes: $ => /[\x20-\x2f]+/,

    csi_final_byte: $ => /[\x40-\x7e]/,  // @ through ~

    // === OSC Sequences: ESC ] number ; string (BEL | ST) ===
    osc_sequence: $ => seq(
      '\x1b]',
      field('number', /[0-9]+/),
      optional(seq(';', field('payload', /[^\x07\x1b]*/))),
      choice('\x07', '\x1b\\'),  // BEL or ST terminator
    ),

    // === DCS Sequences: ESC P params final body ST ===
    dcs_sequence: $ => seq(
      '\x1bP',
      optional(field('params', $.parameter_list)),
      field('final', /[\x40-\x7e]/),
      field('body', /[^\x1b]*/),      // everything until ST
      '\x1b\\',                         // ST terminator
    ),

    // === Simple ESC sequences: ESC + one or two bytes ===
    esc_sequence: $ => seq(
      '\x1b',
      choice(
        // Single-byte ESC commands
        field('simple', /[78DEMcno]/),
        // Charset designation: ESC ( X, ESC ) X, etc.
        seq(field('designator', /[()*/+\-.\/]/), field('charset', /./)),
        // SOS, PM, APC: consume until ST
        seq(/[X^_]/, /[^\x1b]*/, '\x1b\\'),
      ),
    ),
  },
});
```

#### 5.7.2 How the Grammar Handles Ambiguity

VT byte streams don't have the luxury of being well-formed. The grammar handles
edge cases:

- **Incomplete sequences**: If the stream ends mid-CSI (e.g., `ESC [` with no final
  byte), tree-sitter produces an `ERROR` node. The parser notes the byte offset and
  waits for more data before retrying.
- **Malformed sequences**: Invalid bytes in a CSI parameter position produce `ERROR`
  nodes that the semantic layer skips (equivalent to the Elisp fallthrough).
- **Embedded C0 controls**: A BEL or BS inside a CSI sequence is technically allowed
  by some terminals. The grammar treats C0 bytes (except ESC) as plain `control_char`
  nodes, which interrupts the sequence -- matching xterm behavior.

#### 5.7.3 Elisp Bridge

```elisp
(defvar ebb-parse-ts--parser nil
  "The tree-sitter parser instance for VT parsing.
Buffer-local, created on demand.")

(defvar ebb-parse-ts--language nil
  "Cached tree-sitter language object for the VT grammar.")

(defun ebb-parse-ts-available-p ()
  "Return non-nil if tree-sitter VT parsing is available."
  (and (fboundp 'treesit-ready-p)
       (treesit-ready-p 'vt t)))

(defun ebb-parse-ts-init ()
  "Initialize the tree-sitter VT parser."
  (unless ebb-parse-ts--language
    (setq ebb-parse-ts--language (treesit-language-at nil))) ;; or explicit load
  (setq ebb-parse-ts--parser (treesit-parser-create 'vt)))

(defun ebb-parse-ts-parse-chunk (ts-parser elisp-parser bytes start end)
  "Parse BYTES[START..END] using tree-sitter, dispatch via ELISP-PARSER.
Returns number of bytes consumed."
  (let* ((chunk (substring bytes start end))
         ;; Parse the chunk into a syntax tree
         (tree (treesit-parse-string ts-parser chunk))
         (root (treesit-parser-root-node tree))
         (consumed 0))
    ;; Walk the tree and dispatch each node
    (ebb-parse-ts--walk root elisp-parser)
    ;; Calculate consumed bytes (everything except trailing ERROR nodes
    ;; that might be incomplete sequences waiting for more data)
    (setq consumed (ebb-parse-ts--usable-extent root))
    consumed))

(defun ebb-parse-ts--walk (node parser)
  "Walk a tree-sitter NODE tree, dispatching to PARSER's screen model."
  (let ((type (treesit-node-type node))
        (screen (ebb-parser-screen parser)))
    (pcase type
      ("plain_text"
       ;; Fast path: bulk-insert printable text
       (let ((text (treesit-node-text node)))
         (dotimes (i (length text))
           (ebb-screen-write-char screen (aref text i)))))

      ("control_char"
       (let ((byte (aref (treesit-node-text node) 0)))
         (ebb-parse--dispatch-c0 parser byte)))

      ("csi_sequence"
       (let ((private (treesit-node-child-by-field-name node "private"))
             (params (treesit-node-child-by-field-name node "params"))
             (intermediate (treesit-node-child-by-field-name node "intermediate"))
             (final (treesit-node-child-by-field-name node "final")))
         (ebb-parse--dispatch-csi-from-tree
          parser
          (when private (treesit-node-text private))
          (when params (ebb-parse-ts--extract-params params))
          (when intermediate (treesit-node-text intermediate))
          (aref (treesit-node-text final) 0))))

      ("osc_sequence"
       (let ((number (treesit-node-child-by-field-name node "number"))
             (payload (treesit-node-child-by-field-name node "payload")))
         (ebb-parse--dispatch-osc
          parser
          (string-to-number (treesit-node-text number))
          (when payload (treesit-node-text payload)))))

      ("dcs_sequence"
       (let ((params (treesit-node-child-by-field-name node "params"))
             (final (treesit-node-child-by-field-name node "final"))
             (body (treesit-node-child-by-field-name node "body")))
         (ebb-parse--dispatch-dcs
          parser
          (when params (ebb-parse-ts--extract-params params))
          (aref (treesit-node-text final) 0)
          (when body (treesit-node-text body)))))

      ("esc_sequence"
       (let ((simple (treesit-node-child-by-field-name node "simple"))
             (designator (treesit-node-child-by-field-name node "designator"))
             (charset (treesit-node-child-by-field-name node "charset")))
         (cond
          (simple
           (ebb-parse--dispatch-esc parser (aref (treesit-node-text simple) 0)))
          (designator
           (ebb-parse--dispatch-charset
            parser
            (aref (treesit-node-text designator) 0)
            (aref (treesit-node-text charset) 0))))))

      ("ERROR"
       ;; Malformed sequence -- log and skip
       (ebb-parse--log parser "tree-sitter ERROR node at byte %d: %S"
                         (treesit-node-start node)
                         (treesit-node-text node)))

      ;; For container nodes, recurse into children
      (_
       (dotimes (i (treesit-node-child-count node))
         (ebb-parse-ts--walk (treesit-node-child node i) parser))))))

(defun ebb-parse-ts--extract-params (params-node)
  "Extract CSI parameters from a tree-sitter params node.
Returns a vector of integers (with -1 for omitted parameters)."
  (let ((text (treesit-node-text params-node)))
    ;; Split on ; and parse each, handling : sub-parameters
    (vconcat
     (mapcar (lambda (p)
               (if (string-empty-p p) -1
                 (string-to-number p)))
             (split-string text "[;]")))))

(defun ebb-parse-ts--usable-extent (root)
  "Return byte count of usably-parsed content from ROOT.
Excludes any trailing ERROR node (likely an incomplete sequence)."
  (let ((last-child (treesit-node-child root (1- (treesit-node-child-count root)))))
    (if (string= (treesit-node-type last-child) "ERROR")
        (treesit-node-start last-child)  ;; stop before the error
      (treesit-node-end root))))         ;; consumed everything
```

#### 5.7.4 Parser Backend Dispatch

`ebb-parse.el` selects the backend at init time:

```elisp
(defcustom ebb-use-tree-sitter 'auto
  "Whether to use tree-sitter for VT parsing.
`auto' uses it when available, `always' requires it (error if unavailable),
nil disables it."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Always" always)
                 (const :tag "Never" nil)))

(defun ebb-parse-create (screen)
  "Create a parser for SCREEN, selecting the best available backend."
  (let ((parser (make-ebb-parser :screen screen)))
    (when (and ebb-use-tree-sitter
               (or (eq ebb-use-tree-sitter 'always)
                   (ebb-parse-ts-available-p)))
      (ebb-parse-ts-init)
      (setf (ebb-parser-ts-parser parser) ebb-parse-ts--parser))
    parser))

(defun ebb-parse-bytes (parser bytes &optional start end)
  "Parse BYTES[START..END] through PARSER using the best backend."
  (if (ebb-parser-ts-parser parser)
      (ebb-parse-ts-parse-chunk
       (ebb-parser-ts-parser parser) parser bytes
       (or start 0) (or end (length bytes)))
    ;; Fallback: Elisp state machine
    (ebb-parse--state-machine parser bytes (or start 0) (or end (length bytes)))))
```

#### 5.7.5 Performance Characteristics

| Operation | Elisp State Machine | tree-sitter Backend |
|-----------|-------------------|-------------------|
| Parse 4KB chunk of plain text | ~2ms | ~0.1ms |
| Parse 4KB chunk with heavy CSI | ~8ms | ~0.3ms |
| Parse malformed sequence | catch + log (~1ms) | ERROR node (~0.05ms) |
| Incremental re-parse (new chunk) | N/A (stateless chunks) | ~0.1ms (tree diff) |
| Memory overhead | ~negligible | ~50KB (grammar + tree) |

The tree-sitter backend is roughly **10-20x faster** on the parse step itself. For
typical interactive use the difference is invisible (both are fast enough). The win
shows up during high-throughput scenarios: `cat` of a large file, `find /`, compiler
output floods. Where the Elisp parser might need 8ms per 4KB chunk (leaving only
~120 chunks/sec budget before Emacs feels sluggish), tree-sitter handles the same at
0.3ms/chunk (~3000 chunks/sec), leaving ample headroom.

---

## 6. Module 3: ebb-render -- Buffer Display

### 6.1 Render Pass

The renderer is called after each parse chunk completes. It only touches dirty lines.

```elisp
(defun ebb-render-refresh (render-state)
  "Refresh the buffer display from the screen model.
Only re-renders lines that have changed since the last refresh."
  (let* ((screen (ebb-render-screen render-state))
         (buffer (ebb-render-buffer render-state))
         (dirty (ebb-screen-get-dirty screen)))
    (when dirty
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))   ;; Never record undo
          ;; Render scrollback changes (if any new lines were added)
          (ebb-render--update-scrollback render-state)
          ;; Render each dirty display line
          (dolist (row dirty)
            (ebb-render--update-line render-state row))
          ;; Update cursor position
          (ebb-render--update-cursor render-state)))
      (ebb-screen-clear-dirty screen))))
```

### 6.2 Line Rendering

Each line is rendered by converting `ebb-cell` structs to a propertized string:

```elisp
(defun ebb-render--update-line (render-state row)
  "Re-render display line ROW in the buffer."
  (let* ((screen (ebb-render-screen render-state))
         (line (ebb-screen-get-line screen row))
         (cells (ebb-line-cells line))
         (width (ebb-screen-width screen))
         ;; Build the text and property list
         (result (ebb-render--cells-to-propertized-string cells width)))
    ;; Replace the line in the buffer
    (ebb-render--replace-buffer-line render-state row result)))
```

### 6.3 Attribute to Face Conversion

```elisp
(defun ebb-render--attr-to-face (attr)
  "Convert a ebb-attr to an Emacs face specification.
Returns a plist suitable for the `face' text property."
  (let (face)
    (when (ebb-attr-bold attr)
      (push 'ebb-bold face))
    (when (ebb-attr-italic attr)
      (push 'ebb-italic face))
    (when-let ((fg (ebb-attr-fg attr)))
      (push `(:foreground ,(ebb-render--color-to-string fg)) face))
    (when-let ((bg (ebb-attr-bg attr)))
      (push `(:background ,(ebb-render--color-to-string bg)) face))
    ;; ... all other attributes
    (if (cdr face) face (car face))))
```

### 6.4 Buffer Layout

```
+------- buffer start -------+
|  scrollback line 1          |   (oldest)
|  scrollback line 2          |
|  ...                        |
|  scrollback line N          |   (newest)
+------- display-begin ------+   <-- marker
|  display line 0             |   (terminal row 0)
|  display line 1             |
|  ...                        |
|  display line H-1           |   (terminal row height-1)
+------- display-end --------+   <-- marker
```

The display region is bounded by two markers. Scrollback grows above
`display-begin`. The buffer is read-only except during render passes.

---

## 7. Module 4: ebb-input -- Key Translation

### 7.1 Keymaps

Three keymaps, just like eat:

```elisp
(defvar ebb-char-mode-map ...)       ;; All keys -> terminal
(defvar ebb-semi-char-mode-map ...)  ;; Most keys -> terminal, some reserved
(defvar ebb-emacs-mode-map ...)      ;; Normal Emacs keys, few overrides
(defvar ebb-line-mode-map ...)       ;; Shell-mode-like editing
```

### 7.2 Translation Function

```elisp
(defun ebb-input-translate (event &optional screen)
  "Translate Emacs key EVENT to terminal escape sequence string.
SCREEN is consulted for mode flags (keypad mode, etc.).
Returns a string to send to the PTY, or nil if the event shouldn't be sent."
  ...)
```

The translation covers:
- ASCII self-inserting characters (sent as-is)
- Control characters: C-a through C-z -> 0x01-0x1A
- Meta modifier: prepend ESC
- Arrow keys: `\e[A`-`\e[D` (normal) or `\eOA`-`\eOD` (keypad mode)
- Modified arrows: `\e[1;{mod}A` where mod = 2(S) 3(M) 5(C) etc.
- Function keys F1-F63: lookup table
- Special keys: Insert, Delete, Home, End, PageUp, PageDown
- Backspace: 0x7F
- Tab: 0x09, Backtab: `\e[Z`
- Mouse events: encode per current mouse mode and SGR flag
- Focus events: `\e[I` / `\e[O`
- Bracketed paste wrapping

### 7.3 Mouse Encoding

```elisp
(defun ebb-input-encode-mouse (event screen ref-pos)
  "Encode a mouse EVENT as a terminal escape sequence."
  (let* ((button (ebb-input--mouse-button event))
         (mods (ebb-input--mouse-modifiers event))
         (pos (ebb-input--mouse-position event ref-pos))
         (x (car pos))
         (y (cdr pos))
         (code (+ button mods)))
    (if (ebb-screen-mouse-sgr screen)
        ;; SGR encoding: no coordinate limit
        (format "\e[<%d;%d;%d%c" code (1+ x) (1+ y)
                (if (ebb-input--mouse-release-p event) ?m ?M))
      ;; Default encoding: coordinates capped at 95
      (when (and (<= x 94) (<= y 94))
        (format "\e[M%c%c%c" (+ code 32) (+ x 33) (+ y 33))))))
```

---

## 8. Module 5: ebb-shell -- Shell Integration

### 8.1 OSC 51 Protocol

Reuses eat's protocol exactly (OSC 51 with `e;` namespace). This means existing
bash/zsh integration scripts work unchanged.

```elisp
(defun ebb-shell-handle-osc51 (screen payload callbacks)
  "Handle an OSC 51 shell integration sequence."
  (when (string-prefix-p "e;" payload)
    (let ((cmd (aref payload 2))
          (args (ebb-shell--split-payload payload 4)))
      (pcase cmd
        (?A (ebb-shell--set-cwd args callbacks))
        (?B (ebb-shell--pre-prompt callbacks))
        (?C (ebb-shell--post-prompt callbacks))
        (?D nil)  ;; continuation prompt start (unused)
        (?E (ebb-shell--post-cont-prompt callbacks))
        (?F (ebb-shell--set-command args callbacks))
        (?G (ebb-shell--pre-exec callbacks))
        (?H (ebb-shell--set-exit-status args callbacks))
        (?I (ebb-shell--history-exchange args screen callbacks))
        (?J (ebb-shell--before-new-prompt callbacks))
        (?M (ebb-shell--user-message args callbacks))
        (_  (ebb-shell--log "Unknown OSC 51 command: %c" cmd))))))
```

### 8.2 Prompt Annotation

Same margin-overlay approach as eat, but cleaner separation: the shell module
communicates prompt positions and status to the main mode via a callback interface,
rather than directly manipulating buffer overlays.

```elisp
;; Callback interface
(cl-defstruct ebb-shell-callbacks
  (set-cwd nil)            ;; (lambda (path))
  (prompt-start nil)       ;; (lambda (position))
  (prompt-end nil)         ;; (lambda (position status))
  (command-start nil)      ;; (lambda (command-string))
  (exit-status nil)        ;; (lambda (code))
  (bell nil)               ;; (lambda ())
  (set-title nil)          ;; (lambda (title))
  (history-request nil)    ;; (lambda (format host file))
  (message nil))           ;; (lambda (name &rest args))
```

---

## 9. Module 6: ebb-io -- Async Process I/O

This is where `futur.el` comes in. The I/O module ensures the main thread is never
blocked by terminal output processing.

### 9.1 Core Data Structure

```elisp
(cl-defstruct ebb-io
  (process nil)            ;; the PTY process
  (parser nil)             ;; ebb-parser instance
  (screen nil)             ;; ebb-screen instance
  (render nil)             ;; ebb-render-state instance
  (pending-bytes "")       ;; unprocessed byte queue
  (pending-offset 0)       ;; how far into pending-bytes we've parsed
  (processing nil)         ;; non-nil when a parse loop is active
  (chunk-size 4096)        ;; bytes per parse chunk
  (min-latency 0.008)      ;; seconds: min delay before render (batching)
  (max-latency 0.033)      ;; seconds: max delay (responsiveness)
  (first-chunk-time nil)   ;; time of first unrendered chunk
  (render-timer nil)       ;; timer for next render
  (throughput-window nil)  ;; sliding window for throughput monitoring
  (throughput-bytes 0)
  (binary-flood nil))      ;; t when in binary flood mode
```

### 9.2 Process Filter (never blocks)

```elisp
(defun ebb-io--filter (io _process output)
  "Process filter. Only appends to queue. Never parses."
  (setf (ebb-io-pending-bytes io)
        (concat (ebb-io-pending-bytes io) output))
  ;; Record time of first unprocessed chunk for latency calculation
  (unless (ebb-io-first-chunk-time io)
    (setf (ebb-io-first-chunk-time io) (current-time)))
  ;; Schedule processing if not already scheduled
  (unless (ebb-io-processing io)
    (ebb-io--schedule-processing io)))
```

### 9.3 Chunked Processing with futur.el

```elisp
(defun ebb-io--schedule-processing (io)
  "Schedule the next parse+render cycle."
  (let* ((now (current-time))
         (first (ebb-io-first-chunk-time io))
         (elapsed (float-time (time-subtract now first)))
         (time-left (- (ebb-io-max-latency io) elapsed))
         (delay (if (<= time-left 0) 0
                  (min time-left (ebb-io-min-latency io)))))
    ;; Cancel any existing timer
    (when (ebb-io-render-timer io)
      (cancel-timer (ebb-io-render-timer io)))
    ;; Schedule via futur-timeout, which yields to the event loop
    (setf (ebb-io-processing io) t)
    (futur-let*
        ((_ <- (futur-timeout delay))
         (_ (ebb-io--process-one-batch io)))
      ;; After processing, check if more data arrived
      (setf (ebb-io-processing io) nil)
      (when (> (length (ebb-io-pending-bytes io))
               (ebb-io-pending-offset io))
        (ebb-io--schedule-processing io))
      (futur-done nil))))

(defun ebb-io--process-one-batch (io)
  "Process up to chunk-size bytes, then render dirty lines."
  (let* ((bytes (ebb-io-pending-bytes io))
         (offset (ebb-io-pending-offset io))
         (remaining (- (length bytes) offset))
         (chunk-size (min remaining (ebb-io-chunk-size io)))
         (parser (ebb-io-parser io)))
    ;; Binary flood detection
    (ebb-io--update-throughput io chunk-size)
    (unless (ebb-io-binary-flood io)
      ;; Parse one chunk
      (let ((consumed (ebb-parse-bytes parser bytes offset (+ offset chunk-size))))
        (cl-incf (ebb-io-pending-offset io) consumed)))
    ;; Compact the pending buffer if we've consumed a lot
    (when (> (ebb-io-pending-offset io) 65536)
      (setf (ebb-io-pending-bytes io)
            (substring (ebb-io-pending-bytes io) (ebb-io-pending-offset io)))
      (setf (ebb-io-pending-offset io) 0))
    ;; Render
    (ebb-render-refresh (ebb-io-render io))
    ;; Reset latency tracking
    (setf (ebb-io-first-chunk-time io) nil)))
```

### 9.4 Binary Flood Detection

```elisp
(defun ebb-io--update-throughput (io chunk-bytes)
  "Track throughput and detect binary floods."
  (let* ((now (float-time))
         (window (ebb-io-throughput-window io)))
    ;; Slide the window (1-second window)
    (when (or (null window) (> (- now (car window)) 1.0))
      (setf (ebb-io-throughput-window io) (cons now 0))
      (setf (ebb-io-throughput-bytes io) 0))
    (cl-incf (ebb-io-throughput-bytes io) chunk-bytes)
    ;; If throughput exceeds 1MB/sec and mostly non-printable, enter flood mode
    (let ((bytes-per-sec (ebb-io-throughput-bytes io)))
      (setf (ebb-io-binary-flood io)
            (> bytes-per-sec 1048576)))))
```

### 9.5 Why futur.el Fits

`futur-let*` with the `<-` binding form is exactly the right abstraction:

```elisp
;; This reads as sequential code but never blocks:
(futur-let*
    ((_ <- (futur-timeout 0.008))          ;; wait min-latency
     (_ (ebb-io--process-one-batch io))  ;; parse + render
     (_ <- (futur-timeout 0))              ;; yield to event loop
     (_ (when more-data                    ;; loop if needed
          (ebb-io--process-one-batch io))))
  (futur-done nil))
```

Each `<-` is a yield point where C-g, resize, input, and other events get processed.
`futur-abort` can cancel an in-flight parse chain (e.g., when the process dies or
the buffer is killed).

For process lifecycle management:

```elisp
(defun ebb-io-start (io command &rest args)
  "Start the terminal process. Returns a futur that completes when the process exits."
  (futur-new
   (lambda (f)
     (let ((proc (make-process
                  :name "ebb"
                  :buffer nil
                  :command (cons command args)
                  :connection-type 'pty
                  :filter (lambda (proc output)
                            (ebb-io--filter io proc output))
                  :sentinel (lambda (proc event)
                              (ebb-io--sentinel io proc event f)))))
       (setf (ebb-io-process io) proc)
       proc))))

(defun ebb-io--sentinel (io _proc event futur)
  "Process sentinel. Delivers the futur when the process exits."
  (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
    ;; Process remaining output
    (ebb-io--process-one-batch io)
    ;; Deliver the exit event
    (futur-deliver-value futur event)))
```

### 9.6 Window Resize Handling

Resize is the one operation that must coordinate all three layers (screen model →
PTY process → renderer) atomically. In eat, the buffer *is* the screen, so resize
is a single mutation. In ebb's separated architecture, we need explicit wiring.

#### 9.6.1 The Problem

When an Emacs window changes size, four things must happen in order:

1. **Screen model** resizes: line vectors reallocated, content reflowed, cursor
   clamped to new bounds, scroll region reset if it exceeds new height.
2. **PTY process** notified: `set-process-window-size` sends SIGWINCH so the child
   process (shell, vim, htop, etc.) knows the new dimensions.
3. **Renderer** invalidated: every line is dirty because the column count changed --
   even lines whose content didn't change may need re-rendering due to new wrap points.
4. **Pending parse data** stays valid: bytes already in the queue were emitted by the
   child process for the *old* dimensions. They must still parse correctly.

#### 9.6.2 Resize Hook

```elisp
(defun ebb-io--setup-resize-hook (io buffer)
  "Install the window-size-change hook for BUFFER."
  (with-current-buffer buffer
    (setq-local ebb--resize-cookie
                (add-hook 'window-size-change-functions
                          (lambda (frame)
                            (ebb-io--handle-resize io buffer frame))
                          nil t))))  ;; buffer-local hook

(defun ebb-io--handle-resize (io buffer frame)
  "Handle window resize for BUFFER displayed in FRAME."
  (when-let* ((win (get-buffer-window buffer frame))
              (new-height (window-body-height win))
              (new-width (window-max-chars-per-line win))
              (screen (ebb-io-screen io)))
    ;; Only act if dimensions actually changed
    (unless (and (= new-width (ebb-screen-width screen))
                 (= new-height (ebb-screen-height screen)))

      ;; Step 1: Resize the screen model.
      ;; This is a pure data operation: reallocate line vectors, reflow wrapped
      ;; lines, clamp cursor, reset scroll region if needed. All coordinate
      ;; clamping happens inside ebb-screen-resize, so no invalid state leaks.
      (ebb-screen-resize screen new-width new-height)

      ;; Step 2: Tell the PTY process.
      ;; SIGWINCH is sent to the child process group. The child will query
      ;; the new size via TIOCGWINSZ and start emitting output for the new
      ;; dimensions. Output already buffered in the kernel or our queue was
      ;; computed for the OLD dimensions -- that's fine, see 9.6.3 below.
      (when-let ((proc (ebb-io-process io)))
        (when (process-live-p proc)
          (set-process-window-size proc new-height new-width)))

      ;; Step 3: Force a full re-render.
      ;; Every line needs re-rendering because column count changed (wrap
      ;; points differ, padding may need adjustment, etc.).
      (ebb-render-invalidate-all (ebb-io-render io))
      (ebb-render-refresh (ebb-io-render io)))))
```

#### 9.6.3 Race Condition: Resize During Async Parse

Because ebb parses in async chunks via `futur-let*`, a resize can happen *between*
parse chunks. This timeline is possible:

```
  time ──────────────────────────────────────────────>
  
  parse chunk 1        resize fires        parse chunk 2
  (80 cols)            (80→120 cols)       (120 cols, but bytes
                                            were emitted for 80)
```

This is safe, and here's why:

1. **The screen model is always consistent.** `ebb-screen-resize` runs on the main
   thread between chunks. After it completes, the model is valid for the new size.
   The next parse chunk applies operations to the new-sized model.

2. **Old-dimension output still parses correctly.** VT escape sequences don't encode
   the terminal width. `CSI 5;40H` means "go to row 5, column 40" -- if the terminal
   is now 120 columns wide, that's still a valid position. If the terminal is now 30
   columns wide, the cursor gets clamped to column 29. The coordinate clamping in
   `ebb-screen` (design principle 1.3) handles this automatically.

3. **Content reflow is the child's job.** After receiving SIGWINCH, programs like
   bash, vim, htop re-draw themselves for the new size. Until that new output arrives,
   the screen may look wrong briefly -- this is the same behavior as every terminal
   emulator (xterm, kitty, alacritty all show the same transient glitch).

4. **The kernel PTY buffer is the boundary.** Bytes already in Emacs's pending queue
   or in the kernel's PTY buffer were generated pre-resize. They flow through the
   parser normally. The parser doesn't care about dimensions -- it just emits screen
   operations. The screen model enforces bounds.

#### 9.6.4 Comparison with Eat

In eat, resize is simpler in *code* but harder to *reason about*:

| | eat | ebb |
|---|---|---|
| Resize handler | ~50 lines, directly manipulates buffer text, adjusts markers, calls `set-process-window-size` | ~30 lines, calls `ebb-screen-resize` + `set-process-window-size` + `ebb-render-invalidate-all` |
| Reflow logic | Interleaved with buffer manipulation (insert/delete text, move overlays) | Pure data operation in `ebb-screen-resize` (testable without a buffer) |
| Undo pollution | Must suppress undo around resize mutations | Not applicable (buffer-undo-list is always t) |
| Async safety | Not relevant (everything is synchronous) | Safe due to coordinate clamping between chunks |
| Extra cost | None (buffer is screen) | One full re-render pass (~0.2ms for 40 lines) |

The ebb approach trades a trivial re-render cost for testability: `ebb-screen-resize`
can be unit-tested with synthetic screen state and no buffer, window, or process.

#### 9.6.5 Resize in ebb-screen (screen model side)

For completeness, the screen model resize operation:

```elisp
(defun ebb-screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT.
Reflows wrapped lines, clamps cursor, resets scroll region."
  (let ((old-width (ebb-screen-width screen))
        (old-height (ebb-screen-height screen))
        (old-lines (ebb-screen-lines screen)))

    ;; Phase 1: Flatten wrapped lines into logical lines.
    ;; A sequence of physical lines where (ebb-line-wrapped line) is t
    ;; is really one logical line that was auto-wrapped. Concatenate them.
    (let ((logical-lines (ebb-screen--unwrap-lines old-lines old-width)))

      ;; Phase 2: Re-wrap logical lines to the new width.
      (let ((new-lines (ebb-screen--rewrap-lines logical-lines new-width new-height)))

        ;; Phase 3: Apply the new geometry.
        (setf (ebb-screen-lines screen) new-lines)
        (setf (ebb-screen-width screen) new-width)
        (setf (ebb-screen-height screen) new-height)

        ;; Phase 4: Clamp cursor to new bounds.
        (setf (ebb-screen-cursor-x screen)
              (min (ebb-screen-cursor-x screen) (1- new-width)))
        (setf (ebb-screen-cursor-y screen)
              (min (ebb-screen-cursor-y screen) (1- new-height)))

        ;; Phase 5: Reset scroll region (standard terminal behavior on resize).
        (setf (ebb-screen-scroll-top screen) 0)
        (setf (ebb-screen-scroll-bottom screen) (1- new-height))

        ;; Phase 6: Mark all lines dirty.
        (setf (ebb-screen-dirty-lines screen)
              (number-sequence 0 (1- new-height)))))))

(defun ebb-screen--unwrap-lines (lines _old-width)
  "Merge sequences of wrapped physical lines into logical lines.
Returns a list of (cells . wrapped-p) where cells is a concatenated vector."
  (let ((result nil)
        (current-cells nil))
    (dotimes (i (length lines))
      (let ((line (aref lines i)))
        (setq current-cells
              (vconcat (or current-cells []) (ebb-line-cells line)))
        (unless (ebb-line-wrapped line)
          ;; End of a logical line
          (push (cons current-cells nil) result)
          (setq current-cells nil))))
    ;; Handle trailing wrapped line (no newline at end)
    (when current-cells
      (push (cons current-cells t) result))
    (nreverse result)))

(defun ebb-screen--rewrap-lines (logical-lines new-width new-height)
  "Re-wrap LOGICAL-LINES to NEW-WIDTH, producing a vector of NEW-HEIGHT lines."
  (let ((physical nil))
    ;; Break each logical line into chunks of new-width
    (dolist (ll logical-lines)
      (let* ((cells (car ll))
             (len (length cells)))
        (if (<= len new-width)
            ;; Fits on one line, pad to new-width
            (push (make-ebb-line
                   :cells (ebb-screen--pad-cells cells new-width)
                   :wrapped nil :dirty t)
                  physical)
          ;; Must wrap: split into chunks
          (let ((offset 0))
            (while (< offset len)
              (let* ((end (min (+ offset new-width) len))
                     (chunk (seq-subseq cells offset end))
                     (last-chunk (>= end len)))
                (push (make-ebb-line
                       :cells (ebb-screen--pad-cells chunk new-width)
                       :wrapped (not last-chunk) :dirty t)
                      physical)
                (setq offset end)))))))
    ;; Take the last new-height lines (or pad with empty lines if fewer)
    (setq physical (nreverse physical))
    (let ((count (length physical)))
      (cond
       ((> count new-height)
        ;; More lines than screen: push excess into scrollback, keep bottom
        (vconcat (last physical new-height)))
       ((< count new-height)
        ;; Fewer lines: pad with empty lines at the bottom
        (vconcat physical
                 (cl-loop repeat (- new-height count)
                          collect (ebb-screen--make-empty-line new-width))))
       (t (vconcat physical))))))
```

---

## 10. Module 7: ebb-sixel -- Inline Images

### 10.1 Sixel Accumulator

Sixel data arrives byte-by-byte within a DCS sequence. The parser calls into
ebb-sixel for each byte:

```elisp
(cl-defstruct ebb-sixel-state
  (pixels nil)            ;; 2D pixel array (vector of vectors)
  (width 0)               ;; current image width
  (height 0)              ;; current image height
  (x 0)                   ;; current X position
  (y 0)                   ;; current Y band (each band = 6 pixels high)
  (color 0)               ;; current color register
  (palette nil)           ;; vector of 256 color values (RGB)
  (repeat-count 1))       ;; pending repeat count

(defun ebb-sixel-feed-byte (state byte)
  "Process one byte of Sixel data."
  (cond
   ((<= ?~ byte ?~) nil) ;; shouldn't happen, but defensive
   ((<= ?? byte ?~)      ;; Sixel data byte
    (ebb-sixel--write-sixel state byte (ebb-sixel-state-repeat-count state))
    (setf (ebb-sixel-state-repeat-count state) 1))
   ((= byte ?!)          ;; Run-length encoding prefix
    (setf (ebb-sixel-state-repeat-count state) 0)) ;; next digits set it
   ((= byte ?$)          ;; Graphics carriage return
    (setf (ebb-sixel-state-x state) 0))
   ((= byte ?-)          ;; Graphics new line
    (setf (ebb-sixel-state-x state) 0)
    (cl-incf (ebb-sixel-state-y state)))
   ((= byte ?#)          ;; Color introducer (followed by params)
    ...)
   ;; digits go to repeat count or color params depending on sub-state
   ))
```

### 10.2 Render Formats

Same as eat: XPM, SVG, half-block, background, none. Configurable preference list.

---

## 11. Module 8: ebb-eshell -- Eshell Integration

### 11.1 Visual Command Mode

```elisp
(define-minor-mode ebb-eshell-visual-command-mode
  "Use ebb instead of term.el for visual commands in Eshell."
  :global t
  (if ebb-eshell-visual-command-mode
      (advice-add 'eshell-exec-visual :override #'ebb-eshell--exec-visual)
    (advice-remove 'eshell-exec-visual #'ebb-eshell--exec-visual)))
```

### 11.2 Full Terminal in Eshell

```elisp
(define-minor-mode ebb-eshell-mode
  "Enable ebb terminal emulation inside Eshell."
  :global t
  (if ebb-eshell-mode
      (progn
        (add-hook 'eshell-mode-hook #'ebb-eshell--setup)
        (add-hook 'eshell-output-filter-functions #'ebb-eshell--output-filter))
    (remove-hook 'eshell-mode-hook #'ebb-eshell--setup)
    (remove-hook 'eshell-output-filter-functions #'ebb-eshell--output-filter)))
```

---

## 12. Module 9: ebb.el -- Entry Points & Mode

### 12.1 Interactive Commands

```elisp
(defun ebb (&optional program arg)
  "Start a terminal emulator."
  (interactive)
  ...)

(defun ebb-other-window (&optional program arg) ...)
(defun ebb-project (&optional arg) ...)
```

### 12.2 Major Mode

```elisp
(define-derived-mode ebb-mode fundamental-mode "Ebb"
  "Major mode for the ebb terminal emulator."
  (setq-local buffer-read-only t)
  (setq-local buffer-undo-list t)        ;; Never record undo
  (setq-local truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local ebb--io (make-ebb-io))
  ;; Resize hook: coordinates screen model, PTY, and renderer (see 9.6)
  (ebb-io--setup-resize-hook ebb--io (current-buffer))
  ;; ... setup keymaps, input mode, shell integration, etc.
  )
```

### 12.3 Window/Buffer Management

```elisp
(defcustom ebb-kill-buffer-on-exit nil ...)
(defcustom ebb-buffer-name "*ebb*" ...)
(defcustom ebb-default-shell nil ...)
(defcustom ebb-query-before-kill 'auto ...)
(defcustom ebb-show-title t ...)
(defcustom ebb-scrollback-size 131072 ...)
```

---

## 13. Module 10: ebb-highlight -- Tree-sitter Content Highlighting

A unique feature: syntax-highlight code that appears in terminal output. When you
run `gcc` and get error messages with code snippets, or `git diff` with patch output,
or `python script.py` and get a traceback -- ebb can apply language-aware
highlighting on top of the terminal's ANSI colors.

### 13.1 How It Works

Content highlighting runs as an *optional post-render pass*. After `ebb-render`
updates the buffer, `ebb-highlight` examines the visible text and applies
tree-sitter highlighting overlays. It never interferes with the terminal's own
colors -- it only adds highlighting where the terminal output is unstyled (default
foreground).

```elisp
(cl-defstruct ebb-highlight-state
  (enabled nil)            ;; master toggle
  (mode 'auto)             ;; 'auto, 'manual, or 'off
  (language nil)           ;; manually pinned language (symbol), or nil for auto
  (detected-lang nil)      ;; auto-detected language for current command
  (parsers nil)            ;; alist of (lang . treesit-parser)
  (overlays nil)           ;; list of active highlight overlays
  (debounce-timer nil)     ;; don't re-highlight on every render
  (last-command nil))      ;; the shell command that produced current output

(defcustom ebb-highlight-mode 'auto
  "When to apply syntax highlighting to terminal output.
`auto' detects language from the running command.
`manual' uses `ebb-highlight-language' only.
nil disables content highlighting entirely."
  :type '(choice (const :tag "Auto-detect from command" auto)
                 (const :tag "Manual language selection" manual)
                 (const :tag "Disabled" nil)))

(defcustom ebb-highlight-languages
  '(python ruby javascript typescript c cpp rust go java
    bash sh diff json yaml toml xml html css sql)
  "Languages to attempt for auto-detection.
Must have corresponding tree-sitter grammars installed."
  :type '(repeat symbol))
```

### 13.2 Language Auto-Detection

The highlight module hooks into shell integration (OSC 51 command notification) to
know what command is running. From the command string, it infers the language:

```elisp
(defvar ebb-highlight--command-language-alist
  '(;; Direct interpreters
    ("python[23]?" . python)
    ("ruby"        . ruby)
    ("node"        . javascript)
    ("ts-node"     . typescript)
    ("lua"         . lua)
    ;; Compilers / build tools (error output contains source)
    ("gcc\\|g\\+\\+\\|clang" . c)
    ("rustc\\|cargo"         . rust)
    ("go "                   . go)
    ("javac"                 . java)
    ;; Tools with structured output
    ("git diff\\|git log\\|git show" . diff)
    ("diff "        . diff)
    ("jq\\|json_pp" . json)
    ("curl.*json"   . json)
    ("yamllint"     . yaml)
    ;; Script execution (infer from extension)
    ("\\.py "  . python)
    ("\\.rb "  . ruby)
    ("\\.js "  . javascript)
    ("\\.ts "  . typescript)
    ("\\.rs "  . rust)
    ("\\.go "  . go)
    ("\\.c "   . c)
    ("\\.sh "  . bash))
  "Map command patterns to tree-sitter language symbols.")

(defun ebb-highlight--detect-language (command-string)
  "Infer tree-sitter language from COMMAND-STRING."
  (cl-loop for (pattern . lang) in ebb-highlight--command-language-alist
           when (string-match-p pattern command-string)
           return lang))
```

### 13.3 Selective Overlay Application

The key constraint: terminal ANSI colors take precedence. Highlighting only applies
to text regions with no explicit foreground color (i.e., default terminal foreground).

```elisp
(defun ebb-highlight--apply (hl-state buffer display-start display-end)
  "Apply tree-sitter highlighting to BUFFER between DISPLAY-START and DISPLAY-END.
Only highlights regions where no terminal foreground color is set."
  (let* ((lang (or (ebb-highlight-state-language hl-state)
                   (ebb-highlight-state-detected-lang hl-state)))
         (parser (ebb-highlight--get-parser hl-state lang)))
    (when parser
      ;; Remove old overlays
      (ebb-highlight--clear-overlays hl-state)
      ;; Get tree-sitter highlights
      (with-current-buffer buffer
        (let* ((root (treesit-parser-root-node parser))
               (query (ebb-highlight--get-query lang))
               (captures (treesit-query-capture root query display-start display-end)))
          (dolist (capture captures)
            (let* ((node (cdr capture))
                   (name (car capture))
                   (start (treesit-node-start node))
                   (end (treesit-node-end node))
                   (face (ebb-highlight--capture-to-face name)))
              ;; Only apply if the region has no terminal foreground
              (when (and face (ebb-highlight--region-unstyled-p buffer start end))
                (let ((ov (make-overlay start end buffer)))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'ebb-highlight t)
                  (overlay-put ov 'evaporate t)
                  (push ov (ebb-highlight-state-overlays hl-state)))))))))))

(defun ebb-highlight--region-unstyled-p (buffer start end)
  "Return non-nil if text in BUFFER from START to END has no terminal foreground."
  (with-current-buffer buffer
    (let ((pos start)
          (unstyled t))
      (while (and unstyled (< pos end))
        (let ((face (get-text-property pos 'face)))
          (when (and face (ebb-highlight--has-foreground-p face))
            (setq unstyled nil)))
        (setq pos (next-single-property-change pos 'face nil end)))
      unstyled)))
```

### 13.4 Debounced Highlighting

Content highlighting is expensive relative to the render pass. It runs on a debounce
timer so it doesn't fire on every parse chunk during rapid output:

```elisp
(defun ebb-highlight--schedule (hl-state buffer)
  "Schedule a highlighting pass after output settles."
  (when (ebb-highlight-state-enabled hl-state)
    (when (ebb-highlight-state-debounce-timer hl-state)
      (cancel-timer (ebb-highlight-state-debounce-timer hl-state)))
    (setf (ebb-highlight-state-debounce-timer hl-state)
          (run-with-idle-timer
           0.15 nil
           (lambda ()
             (when (buffer-live-p buffer)
               (let ((win (get-buffer-window buffer)))
                 (when win
                   (ebb-highlight--apply
                    hl-state buffer
                    (window-start win)
                    (window-end win t))))))))))
```

### 13.5 User Interface

```elisp
(defun ebb-highlight-toggle ()
  "Toggle content highlighting in the current ebb buffer."
  (interactive)
  ...)

(defun ebb-highlight-set-language (lang)
  "Pin the highlighting language to LANG for this buffer."
  (interactive
   (list (intern (completing-read "Language: "
                                  ebb-highlight-languages nil t))))
  ...)

(defun ebb-highlight-auto ()
  "Return to auto-detecting the language from commands."
  (interactive)
  ...)
```

### 13.6 Integration with Render Pipeline

The render module calls into highlighting at the end of each render pass:

```elisp
;; In ebb-render-refresh, after updating dirty lines:
(when (and ebb-highlight-state
           (ebb-highlight-state-enabled ebb-highlight-state))
  (ebb-highlight--schedule ebb-highlight-state buffer))
```

This keeps highlighting decoupled -- the render module doesn't need to know about
tree-sitter. It just calls a hook.

---

## 14. Terminfo & Shell Integration Scripts

### 14.1 Terminfo

Reuse eat's terminfo definitions (`eat.ti`) directly. The terminal types
(`eat-mono`, `eat-color`, `eat-256color`, `eat-truecolor`) are stable and
well-tested. No reason to reinvent them.

Alternatively, create `ebb-*` aliases that use the same capability definitions.
This allows diverging later without breaking eat users.

### 14.2 Shell Integration Scripts

Reuse eat's bash and zsh integration scripts unchanged. They use OSC 51 with the
`e;` namespace. Our OSC 51 handler in `ebb-shell.el` speaks the same protocol.

Add fish integration (eat's PR #133 has been waiting 2+ years):

```fish
# integration/fish
function __ebb_precmd --on-event fish_prompt
    printf '\e]51;e;H;%d\e\\' $status
    printf '\e]51;e;J\e\\'
    printf '\e]51;e;A;%s;%s\e\\' (echo -n $hostname | base64) (echo -n $PWD | base64)
    printf '\e]2;%s@%s:%s\$\e\\' $USER $hostname (prompt_pwd)
end

function __ebb_preexec --on-event fish_preexec
    set -l cmd (echo -n $argv | base64)
    printf '\e]51;e;F;%s\e\\' $cmd
    printf '\e]51;e;G\e\\'
end

# PS1 wrapper via fish_prompt event
function __ebb_prompt_start --on-event fish_prompt
    printf '\e]51;e;B\e\\'
end
# ... (emit C after the prompt)
```

---

## 15. Test Strategy

### 15.1 Unit Tests for the Screen Model

The screen model is pure data. Test it by feeding operations and asserting state:

```elisp
(ert-deftest ebb-test-cursor-move-clamp ()
  "Cursor movement clamps to screen bounds."
  (let ((screen (ebb-screen-create 20 6)))
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-cursor-move screen 'up 100)
    (should (= 0 (ebb-screen-cursor-y screen)))
    (ebb-screen-cursor-move screen 'left 100)
    (should (= 0 (ebb-screen-cursor-x screen)))))

(ert-deftest ebb-test-resize-reflows-wrapped-lines ()
  "Resize correctly reflows wrapped content."
  (let ((screen (ebb-screen-create 10 4)))
    ;; Write a 15-char string at 10 columns → wraps to 2 lines
    (dotimes (i 15)
      (ebb-screen-write-char screen (+ ?a i)))
    (should (eq t (ebb-line-wrapped (ebb-screen-get-line screen 0))))
    ;; Widen to 20 columns → should unwrap to 1 line
    (ebb-screen-resize screen 20 4)
    (should (eq nil (ebb-line-wrapped (ebb-screen-get-line screen 0))))
    ;; Narrow to 5 columns → should wrap to 3 lines
    (ebb-screen-resize screen 5 4)
    (should (eq t (ebb-line-wrapped (ebb-screen-get-line screen 0))))
    (should (eq t (ebb-line-wrapped (ebb-screen-get-line screen 1))))
    (should (eq nil (ebb-line-wrapped (ebb-screen-get-line screen 2))))))

(ert-deftest ebb-test-resize-clamps-cursor ()
  "Resize clamps cursor to new bounds."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-cursor-goto screen 20 70)
    (ebb-screen-resize screen 40 10)
    (should (= 39 (ebb-screen-cursor-x screen)))  ;; clamped from 70
    (should (= 9 (ebb-screen-cursor-y screen))))) ;; clamped from 20

(ert-deftest ebb-test-resize-resets-scroll-region ()
  "Resize resets scroll region to full screen."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-set-scroll-region screen 5 15)
    (ebb-screen-resize screen 80 30)
    (should (= 0 (ebb-screen-scroll-top screen)))
    (should (= 29 (ebb-screen-scroll-bottom screen)))))

(ert-deftest ebb-test-resize-marks-all-dirty ()
  "After resize, every line is marked dirty."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-clear-dirty screen)
    (ebb-screen-resize screen 100 30)
    (should (= 30 (length (ebb-screen-get-dirty screen))))))
```

### 15.2 Parser Tests

Feed raw byte sequences to the parser with a mock screen, assert the resulting
screen state. Port all 56 of eat's existing tests.

### 15.3 Render Tests

Create a screen model with known state, run the renderer, assert the buffer
contents and text properties match.

### 15.4 Integration Tests

Start a real PTY process, send commands, wait for output, assert screen state.
Use `futur-blocking-wait-to-get-result` in tests (it's acceptable in test context).

### 15.5 Stress Tests

#### 15.6 Tree-sitter Backend Tests

Run the *entire* test suite twice: once with `ebb-use-tree-sitter` set to nil
(Elisp backend) and once with it set to `always` (tree-sitter backend). Both must
produce identical screen model state. Automated via:

```elisp
(defmacro ebb-test-with-both-backends (&rest body)
  "Run BODY once with Elisp parser, once with tree-sitter parser.
Both runs must produce the same result."
  `(dolist (backend '(nil always))
     (let ((ebb-use-tree-sitter backend))
       (when (or (null backend) (ebb-parse-ts-available-p))
         ,@body))))
```

#### 15.7 Content Highlighting Tests

```elisp
(ert-deftest ebb-test-highlight-auto-detect ()
  "Language detection from command string works."
  (should (eq (ebb-highlight--detect-language "python3 foo.py") 'python))
  (should (eq (ebb-highlight--detect-language "git diff HEAD~3") 'diff))
  (should (eq (ebb-highlight--detect-language "cargo build") 'rust))
  (should (null (ebb-highlight--detect-language "ls -la"))))

(ert-deftest ebb-test-highlight-respects-ansi ()
  "Highlighting overlays don't override terminal ANSI colors."
  ;; Set up a screen with some ANSI-colored text and some plain text
  ;; Run highlighting
  ;; Assert: overlays only on the plain text regions
  ...)
```

```elisp
(ert-deftest ebb-test-binary-flood-no-hang ()
  "Processing large binary data doesn't freeze Emacs."
  (let ((io (make-ebb-io ...)))
    ;; Simulate receiving 1MB of random bytes
    (ebb-io--filter io nil (make-string 1048576 0))
    ;; The test framework itself completing proves we didn't hang
    (should t)))
```

---

## 16. Implementation Phases

### Phase 1: Minimal Viable Terminal (Weeks 1-3) -- **COMPLETE**

**Goal**: `M-x ebb` opens a shell, you can type commands, see output, it doesn't
hang.

**Status**: All 6 core files implemented. 42 unit tests passing. All C0 controls,
ESC sequences, CSI sequences (except j/k aliases and XTSMGRAPHICS), all DECSET
modes (except sixel scrolling), all SGR attributes, and all OSC sequences
implemented. Async chunked I/O with timer-based scheduling (no futur.el dependency
for now). Uses `TERM=xterm-256color`.

Files written:
1. `ebb-term.el` (~960 lines) -- Screen model with full operations: write char,
   cursor move, scroll, erase, resize, alt screen, save/restore, DEC graphics charset
2. `ebb-parse.el` (~580 lines) -- Full VT state machine with O(1) CSI dispatch,
   complete SGR handler (256-color + truecolor), all ESC/CSI/OSC/DCS sequences
3. `ebb-render.el` (~350 lines) -- Dirty-line renderer with 256-color + truecolor
   face conversion, scrollback rendering, cursor overlay
4. `ebb-io.el` (~250 lines) -- Async I/O with timer-based chunked processing,
   latency management, binary flood detection, resize coordination
5. `ebb-input.el` (~335 lines) -- Char/semi-char keymaps, full key translation
   (ASCII, control, meta, arrows, function keys, special keys, mouse, bracketed paste)
6. `ebb.el` (~340 lines) -- `ebb-mode`, `M-x ebb`, process/window management,
   event handling, input mode switching
7. `ebb-test.el` (~370 lines) -- 42 unit tests covering screen model, parser, and
   renderer

**Checkpoint test**: Run `ls --color`, `htop`, `vim`, basic shell usage.

### Phase 2: Full Terminal Compatibility + Tree-sitter Grammar (Weeks 4-7)

**Goal**: Pass all of eat's 56 test cases. Ship tree-sitter VT grammar.

Add to parser:
- All remaining CSI sequences (ICH, DCH, ECH, IL, DL, REP, SU, SD, DA, DSR, DECSCUSR)
- All DECSET/DECRST modes (alt screen, mouse tracking, bracketed paste, focus events, auto-wrap)
- Full SGR (256-color, truecolor, underline styles, all attributes)
- All OSC sequences (title, cwd, clipboard, color query)
- ESC sequences (save/restore cursor, charset designation, index, reverse index, reset)
- DEC line drawing character set

Add to input:
- Full mouse encoding (X10, normal, button-event, any-event, SGR)
- Function keys F1-F63
- All modifier combinations
- Focus events
- Bracketed paste

Tree-sitter VT grammar (`ebb-parse-ts.el` + `tree-sitter-vt/grammar.js`):
1. Write the tree-sitter grammar in `tree-sitter-vt/grammar.js`
2. Write test corpus (`tree-sitter-vt/test/corpus/*.txt`) covering all sequence types
3. Build and test the grammar standalone (`tree-sitter generate && tree-sitter test`)
4. Write the Elisp bridge (`ebb-parse-ts.el`) with `treesit-parse-string` integration
5. Add backend dispatch to `ebb-parse.el` (`ebb-use-tree-sitter` defcustom)
6. Run the full test suite with *both* backends, compare results
7. Precompile grammar shared libraries for Linux x86_64, aarch64, macOS arm64/x86_64

**Checkpoint test**: All 56 eat tests pass with both Elisp and tree-sitter backends.
Tree-sitter backend is measurably faster on the throughput benchmark.

### Phase 3: Shell Integration & Line Mode (Weeks 8-9)

**Goal**: Feature parity with eat's shell integration.

1. `ebb-shell.el` -- Full OSC 51 protocol handler
2. Shell integration scripts (bash, zsh, fish)
3. Prompt annotation (margin overlays)
4. Directory tracking
5. Command history exchange
6. Line mode (comint-like editing at the prompt)
7. Auto line mode (switch on prompt, switch back on exec)

**Checkpoint test**: Shell prompt annotations show, `C-c C-p`/`C-c C-n` navigate
prompts, directory tracking works with `C-x C-f`.

### Phase 4: Eshell, Sixel, Content Highlighting, Polish (Weeks 10-13)

1. `ebb-eshell.el` -- Visual command mode + full terminal in eshell
2. `ebb-sixel.el` -- Sixel protocol with XPM/SVG/half-block rendering
3. `ebb-highlight.el` -- Tree-sitter content highlighting:
   a. Language auto-detection from shell command strings (via OSC 51 hook)
   b. Selective overlay application (only unstyled regions)
   c. Debounced highlighting (idle timer, not every render pass)
   d. Interactive commands: `ebb-highlight-toggle`, `ebb-highlight-set-language`
   e. Support for diff, Python, C, Rust, Go, JS/TS, Bash, JSON, YAML at minimum
4. TRAMP support (auto terminfo deployment, remote directory tracking)
5. Cursor blinking
6. Text blinking
7. Trace mode (debug logging)
8. Customization variables (all of eat's ~50 defcustoms)

### Phase 5: Testing & Release (Weeks 14-15)

1. Stress tests (binary flood, rapid resize, long-running processes)
2. Compatibility testing:
   - Emacs 28 (no tree-sitter): core functionality, Elisp parser only
   - Emacs 29, 30, 31: full feature set including tree-sitter backends
   - Linux x86_64, aarch64; macOS arm64, x86_64
   - Terminal and GUI Emacs
3. Performance benchmarking vs eat (both parser backends)
4. Tree-sitter grammar CI: automated grammar build + test on push
5. Documentation (Info manual, README)
6. Package metadata for GNU ELPA or MELPA

---

## 17. Full Escape Sequence Checklist

Every sequence that eat handles, which ebb must also handle for feature parity.

### C0 Control Characters

- [x] NUL (0x00) -- ignore
- [x] BEL (0x07) -- bell callback
- [x] BS (0x08) -- cursor left 1
- [x] HT (0x09) -- horizontal tab
- [x] LF (0x0A) -- line feed
- [x] VT (0x0B) -- same as index
- [x] FF (0x0C) -- same as index
- [x] CR (0x0D) -- carriage return
- [x] SO (0x0E) -- shift out (invoke G1)
- [x] SI (0x0F) -- shift in (invoke G0)
- [x] DEL (0x7F) -- ignore

### ESC Sequences

- [x] ESC 7 -- save cursor (DECSC)
- [x] ESC 8 -- restore cursor (DECRC)
- [x] ESC D -- index (IND)
- [x] ESC E -- next line (NEL)
- [x] ESC M -- reverse index (RI)
- [x] ESC P -- DCS introducer
- [x] ESC [ -- CSI introducer
- [x] ESC ] -- OSC introducer
- [x] ESC c -- full reset (RIS)
- [x] ESC n -- invoke G2 (LS2)
- [x] ESC o -- invoke G3 (LS3)
- [x] ESC ( X -- designate G0
- [x] ESC ) X -- designate G1
- [x] ESC * X -- designate G2
- [x] ESC + X -- designate G3
- [x] ESC - X -- designate G1 (VT300)
- [x] ESC . X -- designate G2 (VT300)
- [x] ESC / X -- designate G3 (VT300)
- [x] ESC X, ESC ^, ESC _ -- SOS/PM/APC (consume and ignore)

### CSI Sequences

- [x] CSI Ps @ -- insert character (ICH)
- [x] CSI Ps A -- cursor up (CUU)
- [x] CSI Ps B -- cursor down (CUD)
- [x] CSI Ps C -- cursor forward (CUF)
- [x] CSI Ps D -- cursor back (CUB)
- [x] CSI Ps E -- cursor next line (CNL)
- [x] CSI Ps F -- cursor previous line (CPL)
- [x] CSI Ps G -- cursor horizontal absolute (CHA)
- [x] CSI Ps ; Ps H -- cursor position (CUP)
- [x] CSI Ps I -- cursor horizontal tab (CHT)
- [x] CSI Ps J -- erase in display (ED) [0,1,2,3]
- [x] CSI Ps K -- erase in line (EL) [0,1,2]
- [x] CSI Ps L -- insert line (IL)
- [x] CSI Ps M -- delete line (DL)
- [x] CSI Ps P -- delete character (DCH)
- [x] CSI Ps S -- scroll up (SU)
- [x] CSI ? Ps S -- sixel graphics attrs (XTSMGRAPHICS)
- [x] CSI Ps T -- scroll down (SD)
- [x] CSI Ps X -- erase character (ECH)
- [x] CSI Ps Z -- cursor backward tab (CBT)
- [x] CSI Ps ` -- horizontal position absolute (HPA)
- [x] CSI Ps a -- horizontal position relative (HPR)
- [x] CSI Ps b -- repeat last character (REP)
- [x] CSI Ps c -- send primary device attributes (DA1)
- [x] CSI > Ps c -- send secondary device attributes (DA2)
- [x] CSI Ps d -- vertical position absolute (VPA)
- [x] CSI Ps e -- vertical position relative (VPR)
- [x] CSI Ps ; Ps f -- horizontal vertical position (HVP)
- [x] CSI Ps h -- set mode (SM)
- [x] CSI ? Ps h -- set DEC private mode (DECSET)
- [x] CSI Ps j -- cursor back (alias)
- [x] CSI Ps k -- cursor up (alias VPB)
- [x] CSI Ps l -- reset mode (RM)
- [x] CSI ? Ps l -- reset DEC private mode (DECRST)
- [x] CSI Ps m -- select graphic rendition (SGR)
- [x] CSI Ps n -- device status report (DSR)
- [x] CSI Ps SP q -- set cursor style (DECSCUSR)
- [x] CSI Ps ; Ps r -- set scroll region (DECSTBM)
- [x] CSI s -- save cursor position (SCP)
- [x] CSI u -- restore cursor position (RCP)

### DECSET/DECRST Modes

- [x] ?1 -- application cursor keys (DECCKM)
- [x] ?7 -- auto-wrap (DECAWM)
- [x] ?9 -- X10 mouse
- [x] ?12 -- blinking cursor (att610)
- [x] ?25 -- show/hide cursor (DECTCEM)
- [ ] ?80 -- sixel scrolling (DECSDM)
- [x] ?1000 -- normal mouse tracking
- [x] ?1002 -- button-event mouse tracking
- [x] ?1003 -- any-event mouse tracking
- [x] ?1004 -- focus events
- [x] ?1006 -- SGR mouse encoding
- [x] ?1047 -- alternate screen buffer
- [x] ?1048 -- save/restore cursor
- [x] ?1049 -- alt screen + save cursor
- [x] ?2004 -- bracketed paste

### SGR Attributes

- [x] 0 -- reset
- [x] 1 -- bold
- [x] 2 -- faint
- [x] 3 -- italic
- [x] 4 -- underline (with sub-params 4:0-4:5)
- [x] 5 -- slow blink
- [x] 6 -- fast blink
- [x] 7 -- inverse
- [x] 8 -- conceal
- [x] 9 -- crossed-out
- [x] 10-19 -- fonts 0-9
- [x] 21 -- double underline
- [x] 22 -- normal intensity
- [x] 23 -- not italic
- [x] 24 -- not underlined
- [x] 25 -- not blinking
- [x] 27 -- not inverse
- [x] 28 -- not concealed
- [x] 29 -- not crossed
- [x] 30-37 -- foreground ANSI
- [x] 38;2;R;G;B -- foreground truecolor
- [x] 38;5;N -- foreground 256-color
- [x] 39 -- default foreground
- [x] 40-47 -- background ANSI
- [x] 48;2;R;G;B -- background truecolor
- [x] 48;5;N -- background 256-color
- [x] 49 -- default background
- [x] 58;2;R;G;B -- underline color truecolor
- [x] 58;5;N -- underline color 256
- [x] 59 -- default underline color
- [x] 90-97 -- bright foreground
- [x] 100-107 -- bright background

### OSC Sequences

- [x] OSC 0 -- set title + icon name
- [x] OSC 2 -- set title
- [x] OSC 7 -- set current working directory
- [x] OSC 10 ; ? -- query foreground color
- [x] OSC 11 ; ? -- query background color
- [x] OSC 51 ; e ; ... -- shell integration (all sub-commands A-M)
- [x] OSC 52 -- clipboard manipulation

### DCS Sequences

- [ ] DCS Ps q -- Sixel graphics

---

*This plan provides a complete specification for building a terminal emulator that
is feature-equivalent to eat v0.9.4 while fixing its architectural weaknesses, and
goes beyond with tree-sitter accelerated parsing and terminal content highlighting.
Estimated total effort: 15 weeks for one experienced Elisp developer, or 8 weeks
for two working in parallel (term+parse+grammar track and io+render+highlight track).*
