;;; ebb-input.el --- Key translation for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Translates Emacs key events to terminal escape sequences.
;; Provides three keybinding modes: char, semi-char, and emacs.

;;; Code:

(require 'cl-lib)
(require 'ebb-term)

;; Forward declarations for functions defined in ebb.el
(declare-function ebb-self-input "ebb")
(declare-function ebb-quoted-input "ebb")
(declare-function ebb-yank "ebb")
(declare-function ebb-yank-pop "ebb")
(declare-function ebb-mouse-input "ebb")
(declare-function ebb-char-mode "ebb")
(declare-function ebb-semi-char-mode "ebb")
(declare-function ebb-emacs-mode "ebb")
(declare-function ebb-kill-process "ebb")
(declare-function ebb-previous-prompt "ebb")
(declare-function ebb-next-prompt "ebb")
(declare-function ebb-next-hyperlink "ebb")
(declare-function ebb-previous-hyperlink "ebb")
(declare-function ebb-open-link-at-point "ebb")
(declare-function ebb-scroll-up "ebb")
(declare-function ebb-scroll-down "ebb")
(declare-function ebb-copy-region "ebb")

;;;; ---- Function Key Tables --------------------------------------------

(defconst ebb-input--function-keys
  '((f1  . "\eOP")   (f2  . "\eOQ")   (f3  . "\eOR")   (f4  . "\eOS")
    (f5  . "\e[15~")  (f6  . "\e[17~")  (f7  . "\e[18~")  (f8  . "\e[19~")
    (f9  . "\e[20~")  (f10 . "\e[21~")  (f11 . "\e[23~")  (f12 . "\e[24~")
    (f13 . "\e[25~")  (f14 . "\e[26~")  (f15 . "\e[28~")  (f16 . "\e[29~")
    (f17 . "\e[31~")  (f18 . "\e[32~")  (f19 . "\e[33~")  (f20 . "\e[34~")
    ;; F21-F63: modified function keys (Shift/Ctrl/Meta variants)
    ;; F21-F24 = Shift+F9-F12, F25-F36 = Ctrl+F1-F12, etc.
    ;; These use the same base sequences as F1-F12 with modifier encoding.
    (f21 . "\e[20;2~")  (f22 . "\e[21;2~")  (f23 . "\e[23;2~")  (f24 . "\e[24;2~")
    (f25 . "\e[1;5P")   (f26 . "\e[1;5Q")   (f27 . "\e[1;5R")   (f28 . "\e[1;5S")
    (f29 . "\e[15;5~")  (f30 . "\e[17;5~")  (f31 . "\e[18;5~")  (f32 . "\e[19;5~")
    (f33 . "\e[20;5~")  (f34 . "\e[21;5~")  (f35 . "\e[23;5~")  (f36 . "\e[24;5~")
    (f37 . "\e[1;6P")   (f38 . "\e[1;6Q")   (f39 . "\e[1;6R")   (f40 . "\e[1;6S")
    (f41 . "\e[15;6~")  (f42 . "\e[17;6~")  (f43 . "\e[18;6~")  (f44 . "\e[19;6~")
    (f45 . "\e[20;6~")  (f46 . "\e[21;6~")  (f47 . "\e[23;6~")  (f48 . "\e[24;6~")
    (f49 . "\e[1;3P")   (f50 . "\e[1;3Q")   (f51 . "\e[1;3R")   (f52 . "\e[1;3S")
    (f53 . "\e[15;3~")  (f54 . "\e[17;3~")  (f55 . "\e[18;3~")  (f56 . "\e[19;3~")
    (f57 . "\e[20;3~")  (f58 . "\e[21;3~")  (f59 . "\e[23;3~")  (f60 . "\e[24;3~")
    (f61 . "\e[1;4P")   (f62 . "\e[1;4Q")   (f63 . "\e[1;4R"))
  "Escape sequences for function keys (F1-F63).")

(defconst ebb-input--special-keys
  '((insert   . "\e[2~")
    (delete   . "\e[3~")
    (home     . "\e[H")
    (end      . "\e[F")
    (prior    . "\e[5~")   ; PageUp
    (next     . "\e[6~"))  ; PageDown
  "Escape sequences for special keys.")

(defconst ebb-input--arrow-keys-normal
  '((up . "\e[A") (down . "\e[B") (right . "\e[C") (left . "\e[D"))
  "Arrow key sequences in normal mode.")

(defconst ebb-input--arrow-keys-app
  '((up . "\eOA") (down . "\eOB") (right . "\eOC") (left . "\eOD"))
  "Arrow key sequences in application cursor mode.")

;;;; ---- Modifier Encoding ----------------------------------------------

(defun ebb-input--modifier-code (mods)
  "Return the xterm modifier code for MODS (list of symbols).
Returns 1 + sum of: shift=1, meta/alt=2, control=4."
  (let ((code 1))
    (when (memq 'shift mods) (cl-incf code 1))
    (when (memq 'meta mods) (cl-incf code 2))
    (when (memq 'control mods) (cl-incf code 4))
    code))

;;;; ---- Main Translation -----------------------------------------------

(defun ebb-input-translate (key &optional screen)
  "Translate Emacs key event KEY to a terminal escape sequence string.
SCREEN is consulted for mode flags.  Returns a string or nil.
A string KEY is committed input text from an input method."
  (if (stringp key)
      key
    (let* ((mods (event-modifiers key))
         (basic (event-basic-type key))
         (keypad (and screen (ebb-screen-keypad-mode screen))))
    (cond
     ;; ---- Self-inserting ASCII character ----
     ((and (characterp key) (>= key ?\s) (<= key ?~) (null mods))
      (string key))

     ;; ---- Multibyte / Unicode character (no modifiers) ----
     ((and (characterp key) (> key ?~) (null mods))
      (string key))

     ;; ---- Control character (C-a through C-z) ----
     ((and (characterp basic) (memq 'control mods)
           (>= basic ?a) (<= basic ?z)
           (not (memq 'meta mods)))
      (string (- basic ?a -1)))

     ;; ---- C-space -> NUL ----
     ((and (eq basic ?\s) (memq 'control mods))
      "\x00")

     ;; ---- C-@ -> NUL ----
     ((and (eq basic ?@) (memq 'control mods))
      "\x00")

     ;; ---- Meta + character -> ESC + char ----
     ((and (characterp basic) (memq 'meta mods)
           (not (memq 'control mods)))
      (concat "\e" (string basic)))

     ;; ---- C-M-char -> ESC + control ----
     ((and (characterp basic) (memq 'meta mods) (memq 'control mods)
           (>= basic ?a) (<= basic ?z))
      (concat "\e" (string (- basic ?a -1))))

     ;; ---- Arrow keys ----
     ((memq basic '(up down left right))
      (let* ((arrows (if keypad
                         ebb-input--arrow-keys-app
                       ebb-input--arrow-keys-normal))
             (base-seq (cdr (assq basic arrows))))
        (if (and mods (not (equal mods nil)))
            ;; Modified arrow: ESC [ 1 ; mod A
            (let ((mod-code (ebb-input--modifier-code mods))
                  (final (aref base-seq (1- (length base-seq)))))
              (format "\e[1;%d%c" mod-code final))
          base-seq)))

     ;; ---- Function keys ----
     ((assq basic ebb-input--function-keys)
      (let ((base-seq (cdr (assq basic ebb-input--function-keys))))
        (if (and mods (not (equal mods nil)))
            ;; Modified: insert modifier before final char/~
            (ebb-input--add-modifier base-seq mods)
          base-seq)))

     ;; ---- Special keys (Insert, Delete, Home, End, Page*) ----
     ((assq basic ebb-input--special-keys)
      (let ((base-seq (cdr (assq basic ebb-input--special-keys))))
        (if (and mods (not (equal mods nil)))
            (ebb-input--add-modifier base-seq mods)
          base-seq)))

     ;; ---- Backspace (all modifier variants) ----
     ((eq basic 'backspace)
      (cond
       ((and (memq 'control mods) (memq 'meta mods)) "\e\x08")  ; C-M-backspace
       ((memq 'control mods) "\x08")                              ; C-backspace
       ((memq 'meta mods) "\e\x7f")                               ; M-backspace
       (t "\x7f")))                                                ; backspace

     ;; ---- Delete / Deletechar ----
     ((memq basic '(delete deletechar))
      (let ((base "\e[3~"))
        (if (and mods (not (equal mods nil)))
            (ebb-input--add-modifier base mods)
          base)))

     ;; ---- Return ----
     ((eq basic 'return)
      (cond
       ((memq 'meta mods) "\e\r")
       (t "\r")))

     ;; ---- Tab / Backtab ----
     ((eq basic 'tab)
      (if (memq 'meta mods) "\e\t" "\t"))

     ((eq basic 'backtab)
      "\e[Z")

     ;; ---- Escape ----
     ((eq basic 'escape)
      "\e")

     ;; ---- Literal char with no special handling ----
     ((characterp key)
      (string key))

     ;; ---- Unknown ----
     (t nil)))))

;;;; ---- Modifier Insertion Helper --------------------------------------

(defun ebb-input--add-modifier (seq mods)
  "Add modifier encoding to an escape SEQ for MODS."
  (let ((mod-code (ebb-input--modifier-code mods)))
    (cond
     ;; Tilde-terminated: ESC[N~ -> ESC[N;mod~
     ((string-suffix-p "~" seq)
      (concat (substring seq 0 (1- (length seq)))
              (format ";%d~" mod-code)))
     ;; Letter-terminated CSI: ESC[X -> ESC[1;modX
     ((string-match "\\`\e\\[\\([A-Z]\\)\\'" seq)
      (format "\e[1;%d%s" mod-code (match-string 1 seq)))
     ;; SS3: ESC O X -> ESC[1;modX
     ((string-match "\\`\eO\\(.)\\'" seq)
      (format "\e[1;%d%s" mod-code (match-string 1 seq)))
     (t seq))))

;;;; ---- Mouse Encoding -------------------------------------------------

(defun ebb-input-encode-mouse (event screen pos-offset)
  "Encode mouse EVENT as terminal escape sequence.
POS-OFFSET is the buffer position of display line 0, col 0.
Returns a string or nil."
  (when-let ((mouse-mode (ebb-screen-mouse-mode screen)))
    (when (ebb-input--mouse-event-allowed-p event mouse-mode screen)
      (when-let* ((coords (ebb-input--mouse-coordinates event screen pos-offset))
                  (x (car coords))
                  (y (cdr coords))
                  (button (ebb-input--mouse-button event screen)))
        (when (ebb-input--mouse-press-p event)
          (ebb-input--mouse-remember-press event screen button))
        (let ((code (+ button (ebb-input--mouse-mod-bits event))))
          (when (ebb-input--mouse-release-p event)
            (ebb-input--mouse-forget-press event screen))
          (if (ebb-screen-mouse-sgr screen)
              ;; SGR encoding
              (format "\e[<%d;%d;%d%c"
                      code (1+ x) (1+ y)
                      (if (ebb-input--mouse-release-p event) ?m ?M))
            ;; Legacy encoding (coordinates limited to 223)
            (when (and (<= x 222) (<= y 222))
              (format "\e[M%c%c%c"
                      (+ code 32)
                      (+ x 33)
                      (+ y 33)))))))))

(defun ebb-input--mouse-posn (event)
  "Return the position object for mouse EVENT."
  (or (and (memq 'drag (event-modifiers event))
           (ignore-errors (event-end event)))
      (event-start event)))

(defun ebb-input--mouse-coordinates (event screen pos-offset)
  "Return zero-based terminal coordinates for mouse EVENT on SCREEN."
  (let* ((posn (ebb-input--mouse-posn event))
         (pt (posn-point posn))
         coords)
    (setq coords
          (cond
           ((and pos-offset (integer-or-marker-p pt))
            (save-excursion
              (goto-char pt)
              (let ((row (- (line-number-at-pos pt)
                            (line-number-at-pos pos-offset)))
                    (col (current-column)))
                (cons col row))))
           (t
            (posn-col-row posn))))
    (when (and coords
               (<= 0 (car coords))
               (< (car coords) (ebb-screen-width screen))
               (<= 0 (cdr coords))
               (< (cdr coords) (ebb-screen-height screen)))
      coords)))

(defun ebb-input--mouse-event-allowed-p (event mode screen)
  "Return non-nil if EVENT should be reported in DEC mouse MODE."
  (let ((mods (event-modifiers event))
        (basic (event-basic-type event)))
    (pcase mode
      ('x10
       (and (memq 'down mods) (memq basic '(mouse-1 mouse-2 mouse-3))))
      ('normal
       (not (or (mouse-movement-p event) (memq 'drag mods))))
      ('button-event
       (or (not (mouse-movement-p event))
           (ebb-screen-mouse-pressed screen)))
      ('any-event t)
      (_ nil))))

(defun ebb-input--mouse-button (event screen)
  "Return xterm mouse button code for EVENT on SCREEN, or nil."
  (let ((basic (event-basic-type event)))
    (cond
     ((mouse-movement-p event)
      (if-let ((pressed (car (ebb-screen-mouse-pressed screen))))
          (+ pressed 32)
        35))
     ((eq basic 'mouse-1) 0)
     ((eq basic 'mouse-2) 1)
     ((eq basic 'mouse-3) 2)
     ((memq basic '(mouse-4 wheel-up)) 64)
     ((memq basic '(mouse-5 wheel-down)) 65)
     ((memq basic '(mouse-6 wheel-left)) 66)
     ((memq basic '(mouse-7 wheel-right)) 67)
     ((eq basic 'mouse-8) 128)
     ((eq basic 'mouse-9) 129)
     ((eq basic 'mouse-10) 130)
     ((eq basic 'mouse-11) 131)
     (t nil))))

(defun ebb-input--mouse-press-p (event)
  "Return non-nil if EVENT is a button press."
  (memq 'down (event-modifiers event)))

(defun ebb-input--mouse-release-p (event)
  "Return non-nil if EVENT is a button release."
  (let ((basic (event-basic-type event))
        (mods (event-modifiers event)))
    (and (memq basic '(mouse-1 mouse-2 mouse-3))
         (or (memq 'click mods) (memq 'drag mods)))))

(defun ebb-input--mouse-remember-press (event screen button)
  "Remember pressed mouse BUTTON from EVENT on SCREEN."
  (when (and (memq (event-basic-type event) '(mouse-1 mouse-2 mouse-3))
             (< button 3))
    (setf (ebb-screen-mouse-pressed screen)
          (sort (cons button (ebb-screen-mouse-pressed screen)) #'<))))

(defun ebb-input--mouse-forget-press (event screen)
  "Forget the released button from EVENT on SCREEN."
  (let ((button (pcase (event-basic-type event)
                  ('mouse-1 0) ('mouse-2 1) ('mouse-3 2))))
    (when button
      (setf (ebb-screen-mouse-pressed screen)
            (cl-delete-if (lambda (b) (= b button))
                          (ebb-screen-mouse-pressed screen))))))

(defun ebb-input--mouse-mod-bits (event)
  "Return modifier bits for mouse EVENT."
  (let ((mods (event-modifiers event))
        (bits 0))
    (when (memq 'shift mods) (setq bits (+ bits 4)))
    (when (memq 'meta mods) (setq bits (+ bits 8)))
    (when (memq 'control mods) (setq bits (+ bits 16)))
    bits))

;;;; ---- Focus Events ---------------------------------------------------

(defun ebb-input-focus-in ()
  "Return the focus-in escape sequence."
  "\e[I")

(defun ebb-input-focus-out ()
  "Return the focus-out escape sequence."
  "\e[O")

;;;; ---- Bracketed Paste ------------------------------------------------

(defun ebb-input-bracketed-paste-start ()
  "Return the bracketed paste start sequence."
  "\e[200~")

(defun ebb-input-bracketed-paste-end ()
  "Return the bracketed paste end sequence."
  "\e[201~")

;;;; ---- Keymaps --------------------------------------------------------

(defcustom ebb-semi-char-non-bound-keys
  '([?\C-x] [?\C-\\] [?\C-q] [?\C-g] [?\C-h] [?\e ?\C-c] [?\C-u]
    [?\e ?x] [?\e ?:] [?\e ?!] [?\e ?&]
    [C-insert] [M-insert] [S-insert] [C-M-insert]
    [C-S-insert] [M-S-insert] [C-M-S-insert]
    [C-delete] [M-delete] [S-delete] [C-M-delete]
    [C-S-delete] [M-S-delete] [C-M-S-delete]
    [C-deletechar] [M-deletechar]
    [S-deletechar] [C-M-deletechar] [C-S-deletechar]
    [M-S-deletechar] [C-M-S-deletechar]
    [C-up] [C-down] [C-right] [C-left]
    [M-up] [M-down] [M-right] [M-left]
    [S-up] [S-down] [S-right] [S-left]
    [C-M-up] [C-M-down] [C-M-right] [C-M-left]
    [C-S-up] [C-S-down] [C-S-right] [C-S-left]
    [M-S-up] [M-S-down] [M-S-right] [M-S-left]
    [C-M-S-up] [C-M-S-down] [C-M-S-right] [C-M-S-left]
    [C-home] [M-home] [S-home] [C-M-home] [C-S-home]
    [M-S-home] [C-M-S-home]
    [C-end] [M-end] [S-end] [C-M-end] [C-S-end]
    [M-S-end] [C-M-S-end]
    [C-prior] [M-prior] [S-prior] [C-M-prior]
    [C-S-prior] [M-S-prior] [C-M-S-prior]
    [C-next] [M-next] [S-next] [C-M-next] [C-S-next]
    [M-S-next] [C-M-S-next])
  "Keys that should not be bound in semi-char mode.
These fall through to normal Emacs bindings.
Use ESC notation for Meta keys, not M-."
  :type '(repeat key-sequence)
  :group 'ebb
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'ebb-update-semi-char-mode-map)
           (ebb-update-semi-char-mode-map))))

(defun ebb-input-make-keymap (input-command categories exceptions)
  "Build a keymap binding INPUT-COMMAND to events in CATEGORIES.
CATEGORIES is a list of keywords: `:ascii', `:arrow', `:navigation',
`:function'.  EXCEPTIONS is a list of key sequences to exclude."
  (let ((map (make-sparse-keymap)))
    (cl-flet ((bind (key)
                (unless (member key exceptions)
                  (define-key map key input-command))))
      (when (memq :ascii categories)
        ;; All self-inserting characters
        (bind [remap self-insert-command])
        ;; Control characters C-@ through C-? (except ESC = meta-prefix)
        (cl-loop for i from ?\C-@ to ?\C-?
                 do (unless (= i meta-prefix-char)
                      (bind (vector i))))
        ;; Special keys with all modifier variants
        (dolist (key '(tab backtab backspace C-backspace
                       M-backspace C-M-backspace
                       insert C-insert M-insert S-insert C-M-insert
                       C-S-insert M-S-insert C-M-S-insert
                       delete C-delete M-delete S-delete C-M-delete
                       C-S-delete M-S-delete C-M-S-delete
                       deletechar C-deletechar M-deletechar
                       S-deletechar C-M-deletechar C-S-deletechar
                       M-S-deletechar C-M-S-deletechar))
          (bind (vector key)))
        ;; Non-encodable control keys
        (dolist (key '(?\C-- ?\C-? ?\C-\s))
          (bind (vector key)))
        ;; Meta + ASCII via ESC prefix
        (unless (member (vector meta-prefix-char) exceptions)
          (define-key map (vector meta-prefix-char) (make-sparse-keymap))
          (cl-loop for i from ?\C-@ to ?\C-?
                   do (unless (memq i '(?O ?\[))
                        (bind (vector meta-prefix-char i))))
          (bind (vector meta-prefix-char meta-prefix-char))))
      (when (memq :arrow categories)
        (dolist (key '(up down right left
                       C-up C-down C-right C-left
                       M-up M-down M-right M-left
                       S-up S-down S-right S-left
                       C-M-up C-M-down C-M-right C-M-left
                       C-S-up C-S-down C-S-right C-S-left
                       M-S-up M-S-down M-S-right M-S-left
                       C-M-S-up C-M-S-down C-M-S-right C-M-S-left))
          (bind (vector key))))
      (when (memq :navigation categories)
        (dolist (key '(home C-home M-home S-home C-M-home C-S-home
                       M-S-home C-M-S-home
                       end C-end M-end S-end C-M-end C-S-end
                       M-S-end C-M-S-end
                       prior C-prior M-prior S-prior C-M-prior
                       C-S-prior M-S-prior C-M-S-prior
                       next C-next M-next S-next C-M-next C-S-next
                       M-S-next C-M-S-next))
          (bind (vector key))))
      (when (memq :function categories)
        (cl-loop for i from 1 to 63
                 do (bind (vector (intern (format "f%d" i)))))))
    map))

;;;; ---- Mouse Keymap ---------------------------------------------------

(defconst ebb-input--mouse-mod-prefixes
  '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-")
  "Modifier prefixes used for mouse event symbols.")

(defun ebb-input--mouse-event-symbols ()
  "Return mouse event symbols that Ebb should pass through to TUIs."
  (let (events)
    (dolist (prefix ebb-input--mouse-mod-prefixes)
      (dotimes (i 11)
        (push (intern (format "%smouse-%d" prefix (1+ i))) events))
      (dotimes (i 3)
        (push (intern (format "%sdown-mouse-%d" prefix (1+ i))) events)
        (push (intern (format "%sdrag-mouse-%d" prefix (1+ i))) events))
      (dolist (wheel '(wheel-up wheel-down wheel-left wheel-right))
        (push (intern (concat prefix (symbol-name wheel))) events)))
    (push 'mouse-movement events)
    events))

(defun ebb--prepare-mouse-mode-map ()
  "Build the transient DEC mouse tracking keymap."
  (let ((map (make-sparse-keymap)))
    (dolist (key (ebb-input--mouse-event-symbols))
      (define-key map (vector key) #'ebb-mouse-input))
    ;; Mouse events can arrive through window decoration prefixes while dragging.
    (dolist (prefix '(mode-line header-line tab-line vertical-line
                      right-divider bottom-divider))
      (define-key map (vector prefix) map))
    map))

(defvar ebb-mouse-mode-map
  (ignore-errors (ebb--prepare-mouse-mode-map))
  "Keymap active while a child program has enabled DEC mouse tracking.")

;;;; ---- Semi-Char Keymap -----------------------------------------------

(defun ebb--prepare-semi-char-mode-map ()
  "Build the semi-char mode keymap."
  (let ((map (ebb-input-make-keymap
              #'ebb-self-input '(:ascii :arrow :navigation :function)
              `([?\C-c] [?\C-q] [?\C-y] [?\e ?y]
                ,@ebb-semi-char-non-bound-keys))))
    ;; Overrides
    (define-key map [?\C-q] #'ebb-quoted-input)
    (define-key map [?\C-y] #'ebb-yank)
    (define-key map [?\M-y] #'ebb-yank-pop)
    ;; C-c C-c sends literal C-c to terminal
    (define-key map [?\C-c ?\C-c] #'ebb-self-input)
    map))

(defvar ebb-semi-char-mode-map
  (ignore-errors (ebb--prepare-semi-char-mode-map))
  "Keymap for semi-char mode.")

(defun ebb-update-semi-char-mode-map ()
  "Rebuild the semi-char keymap after customization changes."
  (setq ebb-semi-char-mode-map (ebb--prepare-semi-char-mode-map))
  ;; `define-minor-mode' records the original map object in this alist.
  (when-let ((entry (assq 'ebb--semi-char-mode minor-mode-map-alist)))
    (setcdr entry ebb-semi-char-mode-map)))

;;;; ---- Char Keymap ----------------------------------------------------

(defun ebb--prepare-char-mode-map ()
  "Build the char mode keymap."
  (let ((map (ebb-input-make-keymap
              #'ebb-self-input
              '(:ascii :arrow :navigation :function)
              nil)))
    ;; Escape back to semi-char
    (define-key map [?\C-\M-m] #'ebb-semi-char-mode)
    map))

(defvar ebb-char-mode-map
  (ignore-errors (ebb--prepare-char-mode-map))
  "Keymap for char mode.")

;;;; ---- Other Keymaps --------------------------------------------------

(defvar ebb-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Mode switching
    (define-key map (kbd "C-c M-d") #'ebb-char-mode)
    (define-key map (kbd "C-c C-j") #'ebb-semi-char-mode)
    (define-key map (kbd "C-c C-e") #'ebb-emacs-mode)
    (define-key map (kbd "C-c C-k") #'ebb-kill-process)
    ;; Prompt navigation
    (define-key map (kbd "C-c C-p") #'ebb-previous-prompt)
    (define-key map (kbd "C-c C-n") #'ebb-next-prompt)
    ;; Hyperlink navigation
    (define-key map (kbd "C-c M-p") #'ebb-previous-hyperlink)
    (define-key map (kbd "C-c M-n") #'ebb-next-hyperlink)
    ;; The history buffer is a bounded materialized slab.
    (define-key map [remap scroll-up-command] #'ebb-scroll-up)
    (define-key map [remap scroll-down-command] #'ebb-scroll-down)
    (define-key map [wheel-up] #'ebb-scroll-down)
    (define-key map [wheel-down] #'ebb-scroll-up)
    (define-key map [remap kill-ring-save] #'ebb-copy-region)
    map)
  "Base keymap for `ebb-mode'.")

(defvar ebb-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    ;; In copy/Emacs mode RET follows a link at point.
    (define-key map (kbd "RET") #'ebb-open-link-at-point)
    (define-key map (kbd "<return>") #'ebb-open-link-at-point)
    map)
  "Keymap for emacs mode.")

(provide 'ebb-input)
;;; ebb-input.el ends here
