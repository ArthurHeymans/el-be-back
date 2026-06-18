;;; chomp-render.el --- Buffer renderer for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders the chomp screen model into an Emacs buffer.
;; Only dirty lines are re-rendered.  The buffer is write-only from the
;; terminal's perspective -- undo is always disabled.
;;
;; Buffer layout:
;;   [scrollback lines...] display-begin [display lines...] display-end

;;; Code:

(require 'cl-lib)
(require 'chomp-term)

;;;; ---- Color Tables ---------------------------------------------------

(defconst chomp-render--ansi-colors
  ["#000000" "#cd0000" "#00cd00" "#cdcd00"
   "#0000ee" "#cd00cd" "#00cdcd" "#e5e5e5"
   "#7f7f7f" "#ff0000" "#00ff00" "#ffff00"
   "#5c5cff" "#ff00ff" "#00ffff" "#ffffff"]
  "Standard 16 ANSI color values.")

;;;; ---- Per-color Named Faces (like eat) --------------------------------

;; Generate 256 named faces so users can customize individual colors
;; via their theme or custom settings.
(defgroup chomp-faces nil
  "Faces used by chomp terminal emulator."
  :group 'chomp)

(defun chomp-render--256color-hex (n)
  "Compute default hex color for 256-color palette index N."
  (cond
   ((< n 16)
    (aref chomp-render--ansi-colors n))
   ((< n 232)
    (let* ((idx (- n 16))
           (b-idx (% idx 6))
           (g-idx (% (/ idx 6) 6))
           (r-idx (/ idx 36))
           (vals [0 95 135 175 215 255]))
      (format "#%02x%02x%02x"
              (aref vals r-idx) (aref vals g-idx) (aref vals b-idx))))
   (t
    (let ((v (chomp--clamp (+ 8 (* 10 (- n 232))) 0 255)))
      (format "#%02x%02x%02x" v v v)))))

;; Define 256 named faces at compile/load time
(defvar chomp-render--color-faces (make-vector 256 nil)
  "Vector of face symbols for palette indices 0-255.")

(defvar chomp-render--indexed-color-cache (make-vector 256 nil)
  "Cached face-resolved color strings for indexed palette entries.")

(defvar chomp-render--attr-face-cache (make-hash-table :test #'equal)
  "Cache from immutable `chomp-attr' values to rendered face plists.")

(defconst chomp-render--attr-face-cache-limit 4096
  "Maximum number of cached attribute face specs before clearing the cache.")

(dotimes (i 256)
  (let* ((sym (intern (format "chomp-color-%d" i)))
         (hex (chomp-render--256color-hex i)))
    (eval `(defface ,sym
             '((t :foreground ,hex :background ,hex))
             ,(format "Chomp color %d." i)
             :group 'chomp-faces))
    (aset chomp-render--color-faces i sym)))

;; Font faces (10 fonts, 0-9)
(defvar chomp-render--font-faces (make-vector 10 nil)
  "Vector of face symbols for font indices 0-9.")

(dotimes (i 10)
  (let ((sym (intern (format "chomp-font-%d" i))))
    (eval `(defface ,sym
             '((t :family nil))
             ,(format "Chomp font %d." i)
             :group 'chomp-faces))
    (aset chomp-render--font-faces i sym)))

(defun chomp-render--color-to-string (color)
  "Convert a color value to a hex string.
COLOR is nil (default), integer 0-255, or (R G B) list.
For indexed colors, uses the corresponding named face's foreground,
allowing theme/user customization."
  (cond
   ((null color) nil)
   ((listp color)
    (format "#%02x%02x%02x"
            (chomp--clamp (car color) 0 255)
            (chomp--clamp (cadr color) 0 255)
            (chomp--clamp (caddr color) 0 255)))
   ((and (integerp color) (<= 0 color 255))
    ;; Use the named face's foreground for customizability
    (or (aref chomp-render--indexed-color-cache color)
        (let* ((face-sym (aref chomp-render--color-faces color))
               (value (or (face-foreground face-sym nil t)
                          (chomp-render--256color-hex color))))
          (aset chomp-render--indexed-color-cache color value)
          value)))
   (t nil)))

;;;; ---- Faces ----------------------------------------------------------

(defface chomp-bold    '((t :weight bold))       "Bold text."    :group 'chomp)
(defface chomp-faint   '((t :weight light))      "Faint text."   :group 'chomp)
(defface chomp-italic  '((t :slant italic))      "Italic text."  :group 'chomp)
(defface chomp-crossed '((t :strike-through t))  "Struck text."  :group 'chomp)

(defface chomp-cursor
  '((t :inverse-video t))
  "Face for the terminal cursor."
  :group 'chomp)

;;;; ---- Render State ---------------------------------------------------

(cl-defstruct (chomp-render-state (:copier nil))
  "State for the buffer renderer."
  (screen nil)           ; chomp-screen
  (buffer nil)           ; Emacs buffer
  (display-begin nil)    ; marker at start of display area
  (cursor-overlay nil)   ; overlay for cursor
  (scrollback-count 0))  ; scrollback lines rendered in buffer

;;;; ---- Constructor ----------------------------------------------------

(defun chomp-render-create (screen buffer)
  "Create a render state for SCREEN displayed in BUFFER."
  (let ((render (make-chomp-render-state :screen screen :buffer buffer))
        (w (chomp-screen-width screen))
        (h (chomp-screen-height screen)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        ;; Insert display lines (H lines of W spaces, newline-separated)
        (dotimes (i h)
          (insert (make-string w ?\s))
          (when (< i (1- h))
            (insert "\n")))
        ;; Place display-begin marker at buffer start
        (let ((m (copy-marker (point-min))))
          (set-marker-insertion-type m t)  ; advances when text inserted at it
          (setf (chomp-render-state-display-begin render) m))
        ;; Create cursor overlay (1 char wide at origin)
        (let ((ov (make-overlay (point-min) (1+ (point-min)) buffer)))
          (overlay-put ov 'face 'chomp-cursor)
          (overlay-put ov 'priority 100)
          (setf (chomp-render-state-cursor-overlay render) ov))))
    render))

;;;; ---- Main Refresh ---------------------------------------------------

(defun chomp-render-refresh (render)
  "Refresh the buffer from the screen model.
Only dirty display lines are re-rendered, but scrollback and cursor
state are reconciled independently so metadata-only updates are visible."
  (let* ((screen (chomp-render-state-screen render))
         (buffer (chomp-render-state-buffer render))
         (dirty (chomp-screen-get-dirty screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          ;; Reconcile scrollback independently of dirty display rows.
          (chomp-render--update-scrollback render)
          ;; Render dirty display lines.
          (dolist (row dirty)
            (chomp-render--update-line render row))
          ;; Cursor movement/visibility/style can change without dirtying a row.
          (chomp-render--update-cursor render)))
      (chomp-screen-clear-dirty screen))))

;;;; ---- Scrollback Rendering -------------------------------------------

(defun chomp-render--update-scrollback (render)
  "Reconcile rendered scrollback with the screen model."
  (let* ((screen (chomp-render-state-screen render))
         (model-count (chomp-screen-scrollback-length screen))
         (rendered-count (chomp-render-state-scrollback-count render))
         (new-count (- model-count rendered-count))
         (appended (chomp-screen-scrollback-appended-count screen))
         (trimmed (chomp-screen-scrollback-trimmed-count screen)))
    (cond
     ;; Clears, alternate-screen switches, and other non-append mutations
     ;; require a full scrollback-region rewrite.
     ((or (chomp-screen-scrollback-dirty screen)
          (< new-count 0))
      (chomp-render--rebuild-scrollback render))
     ;; When scrollback is already full, new model lines are balanced by trims:
     ;; mirror that in the buffer by deleting oldest rendered rows and appending
     ;; the new rows, instead of rebuilding the entire history region.
     ((or (> new-count 0) (> appended 0) (> trimmed 0))
      (chomp-render--append-scrollback render
                                       (max appended new-count)
                                       trimmed
                                       model-count)))
    (chomp-screen-clear-scrollback-dirty screen)))

(defun chomp-render--append-scrollback (render append-count trim-count model-count)
  "Append APPEND-COUNT newest scrollback lines and delete TRIM-COUNT old rows."
  (let* ((screen (chomp-render-state-screen render))
         (width (chomp-screen-width screen))
         (display-begin (chomp-render-state-display-begin render))
         (count (min append-count model-count)))
    (save-excursion
      (when (> trim-count 0)
        (goto-char (point-min))
        (let ((beg (point))
              (n (min trim-count (chomp-render-state-scrollback-count render))))
          (forward-line n)
          (delete-region beg (point))))
      (when (> count 0)
        ;; New scrollback lines are at the front of the list (newest first).
        ;; Insert them in chronological order (oldest of the new batch first).
        (goto-char display-begin)
        (insert (chomp-render--lines-to-string
                 (chomp-render--newest-lines-oldest-first
                  (chomp-screen-scrollback screen) count)
                 width))))
    (setf (chomp-render-state-scrollback-count render) model-count)))

(defun chomp-render--rebuild-scrollback (render)
  "Replace the rendered scrollback region from the screen model."
  (let* ((screen (chomp-render-state-screen render))
         (width (chomp-screen-width screen))
         (display-begin (chomp-render-state-display-begin render))
         (lines (chomp-screen-scrollback-lines-raw screen)))
    (save-excursion
      (delete-region (point-min) display-begin)
      (goto-char (point-min))
      (insert (chomp-render--lines-to-string lines width)))
    (setf (chomp-render-state-scrollback-count render) (length lines))))

(defun chomp-render--newest-lines-oldest-first (lines count)
  "Return the first COUNT newest-first LINES in oldest-first order."
  (let ((taken nil)
        (n 0))
    (while (and lines (< n count))
      (push (pop lines) taken)
      (cl-incf n))
    taken))

(defun chomp-render--lines-to-string (lines width)
  "Return scrollback LINES as one string with trailing newlines.
Unlike display rows, scrollback rows do not need invisible spacer characters for
cursor column addressing, so default wide-character rows can be emitted more
compactly."
  (mapconcat (lambda (line)
               (concat (chomp-render--line-to-string-scrollback line width) "\n"))
             lines ""))

(defun chomp-render--line-to-string-scrollback (line width)
  "Convert LINE for scrollback rendering."
  (cond
   ((and (chomp-line-text line)
         (chomp-line-attr-runs line)
         (= (length (chomp-line-text line)) width))
    (setf (chomp-line-dirty line) nil)
    (chomp-render--text-runs-to-string line width))
   ((and (chomp-line-text line)
         (= (length (chomp-line-text line)) width))
    (setf (chomp-line-dirty line) nil)
    (if-let ((attr (chomp-line-uniform-attr line)))
        (let ((s (copy-sequence (chomp-line-text line)))
              (face (chomp-render--attr-to-face attr)))
          (when face
            (put-text-property 0 width 'face face s))
          s)
      (chomp-line-text line)))
   ((chomp-render--cells-to-string-scrollback-fast (chomp-line-cells line) width))
   (t
    (chomp-render--line-to-string line width))))

(defun chomp-render--cells-to-string-scrollback-fast (cells width)
  "Return unstyled CELLS as visible scrollback text, or nil if styled."
  (catch 'styled
    (let ((s (make-string width ?\s))
          (i 0)
          (pos 0)
          (cols 0))
      (while (< i width)
        (let* ((cell (aref cells i))
               (cw (chomp-cell-width cell)))
          (when (chomp-cell-attr cell)
            (throw 'styled nil))
          (cond
           ((zerop cw)
            (cl-incf i))
           (t
            (aset s pos (chomp-cell-char cell))
            (cl-incf pos)
            (cl-incf cols cw)
            (cl-incf i cw)))))
      (substring s 0 (+ pos (max 0 (- width cols)))))))

(defun chomp-render--line-to-string (line width)
  "Convert LINE to a string of WIDTH terminal columns."
  (cond
   ((and (chomp-line-text line)
         (chomp-line-attr-runs line)
         (= (length (chomp-line-text line)) width))
    (let ((s (chomp-render--text-runs-to-string line width)))
      (setf (chomp-line-rendered line) s)
      (setf (chomp-line-dirty line) nil)
      s))
   ((and (chomp-line-text line)
         (chomp-line-uniform-attr line)
         (= (length (chomp-line-text line)) width))
    (let ((s (copy-sequence (chomp-line-text line)))
          (face (chomp-render--attr-to-face (chomp-line-uniform-attr line))))
      (when face
        (put-text-property 0 width 'face face s))
      (setf (chomp-line-rendered line) s)
      (setf (chomp-line-dirty line) nil)
      s))
   ((and (chomp-line-text line)
         (= (length (chomp-line-text line)) width))
    (setf (chomp-line-dirty line) nil)
    (chomp-line-text line))
   ((and (not (chomp-line-dirty line))
         (chomp-line-rendered line)
         (= (length (chomp-line-rendered line)) width))
    (chomp-line-rendered line))
   (t
    (let ((s (chomp-render--cells-to-string (chomp-line-cells line) width)))
      (setf (chomp-line-rendered line) s)
      (setf (chomp-line-dirty line) nil)
      s))))

(defun chomp-render--text-runs-to-string (line width)
  "Render LINE's text plus attribute runs into a propertized string."
  (let ((s (copy-sequence (chomp-line-text line))))
    (dolist (run (chomp-line-attr-runs line))
      (let ((face (chomp-render--attr-to-face (nth 2 run))))
        (when face
          (put-text-property (nth 0 run) (min width (nth 1 run)) 'face face s))))
    s))

;;;; ---- Display Line Rendering -----------------------------------------

(defun chomp-render--update-line (render row)
  "Re-render display line ROW in the buffer."
  (let* ((screen (chomp-render-state-screen render))
         (line (chomp--line-at screen row))
         (width (chomp-screen-width screen))
         (display-begin (chomp-render-state-display-begin render)))
    (when line
      (save-excursion
        (goto-char display-begin)
        (forward-line row)
        (let* ((bol (point))
               (eol (line-end-position))
               (new (chomp-render--line-to-string line width)))
          ;; TUI programs often repaint rows with identical content.  Avoid a
          ;; buffer modification when the rendered text/properties are already
          ;; correct; cursor overlay movement is handled separately.
          (unless (and (= (- eol bol) (length new))
                       (equal-including-properties
                        new (buffer-substring bol eol)))
            (delete-region bol eol)
            (insert new)))))))

;;;; ---- Cell-to-String Conversion --------------------------------------

(defun chomp-render--cells-to-string (cells width)
  "Convert a vector of chomp-cells to a propertized string.
Handles double-width characters by inserting invisible spacers."
  (or (chomp-render--cells-to-string-fast cells width)
      (chomp-render--cells-to-string-uniform cells width)
      (chomp-render--cells-to-string-general cells width)))

(defun chomp-render--cells-to-string-fast (cells width)
  "Fast path for default-attribute CELLS, or nil if styled.
Handles both single-width and wide characters without consing per-cell run
lists; falls back only when a styled cell is present."
  (catch 'styled
    (let ((s (make-string width ?\s))
          (i 0)
          (pos 0))
      (while (< i width)
        (let* ((cell (aref cells i))
               (cw (chomp-cell-width cell)))
          (when (chomp-cell-attr cell)
            (throw 'styled nil))
          (cond
           ((zerop cw)
            (cl-incf i))
           ((> cw 1)
            (aset s pos (chomp-cell-char cell))
            (let ((end (min width (+ pos cw))))
              (when (< (1+ pos) end)
                (put-text-property (1+ pos) end 'invisible t s)
                (put-text-property (1+ pos) end 'chomp-wide-spacer t s)))
            (cl-incf pos cw)
            (cl-incf i cw))
           (t
            (aset s pos (chomp-cell-char cell))
            (cl-incf pos)
            (cl-incf i)))))
      s)))

(defun chomp-render--cells-to-string-uniform (cells width)
  "Fast path for single-width rows with one shared/equal attribute."
  (let ((attr (chomp-cell-attr (aref cells 0)))
        (i 0))
    (when attr
      (while (and (< i width)
                  (let ((cell (aref cells i)))
                    (and (= (chomp-cell-width cell) 1)
                         (or (eq (chomp-cell-attr cell) attr)
                             (equal (chomp-cell-attr cell) attr)))))
        (cl-incf i))
      (when (= i width)
        (let ((s (make-string width ?\s))
              (face (chomp-render--attr-to-face attr)))
          (dotimes (j width)
            (aset s j (chomp-cell-char (aref cells j))))
          (when face
            (put-text-property 0 width 'face face s))
          s)))))

(defun chomp-render--cells-to-string-general (cells width)
  "General propertized conversion for CELLS."
  (let ((parts nil)
        (i 0))
    ;; Process cells one at a time, grouping single-width same-attr runs
    (while (< i width)
      (let* ((cell (aref cells i))
             (cw (chomp-cell-width cell)))
        (cond
         ;; Zero-width continuation cell: skip (already handled by wide char)
         ((zerop cw)
          (cl-incf i))
         ;; Double-width character: emit char + invisible spacer
         ((> cw 1)
          (let* ((ch (chomp-cell-char cell))
                 (attr (chomp-cell-attr cell))
                 (face (chomp-render--attr-to-face attr))
                 (s (string ch))
                 ;; Invisible spacers for the extra columns
                 (spacer (propertize (make-string (1- cw) ?\s)
                                     'invisible t
                                     'chomp-wide-spacer t)))
            (when face
              (put-text-property 0 (length s) 'face face s)
              (put-text-property 0 (length spacer) 'face face spacer))
            (push spacer parts)
            (push s parts))
          ;; Skip the char cell and its continuation cells
          (cl-incf i cw))
         ;; Normal single-width: group consecutive same-attr cells
         (t
          (let ((attr (chomp-cell-attr cell))
                (chars nil))
            (while (and (< i width)
                        (let ((c (aref cells i)))
                          (and (= (chomp-cell-width c) 1)
                               (equal (chomp-cell-attr c) attr))))
              (push (chomp-cell-char (aref cells i)) chars)
              (cl-incf i))
            (when chars
              (let ((s (apply #'string (nreverse chars)))
                    (face (chomp-render--attr-to-face attr)))
                (when face
                  (put-text-property 0 (length s) 'face face s))
                (push s parts))))))))
    ;; Build result and ensure it is exactly width display columns
    (let ((result (apply #'concat (nreverse parts))))
      ;; Pad with spaces if display width is short
      ;; Note: `string-width' accounts for wide chars
      (let ((dw (string-width result)))
        (if (< dw width)
            (concat result (make-string (- width dw) ?\s))
          result)))))

;;;; ---- Attribute to Face ----------------------------------------------

(defun chomp-render--attr-to-face (attr)
  "Convert chomp-attr ATTR to an Emacs face spec (plist or list)."
  (when (and attr (chomp--attr-non-default-p attr))
    (or (gethash attr chomp-render--attr-face-cache)
        (let ((face nil)
              (fg (chomp-attr-fg attr))
              (bg (chomp-attr-bg attr))
              (inv (chomp-attr-inverse attr)))
      ;; Handle inverse: swap fg/bg, using actual Emacs default colors
      ;; when fg or bg is nil (matches eat's behavior)
      (when inv
        (let* ((default-fg (or (chomp-render--color-to-string fg)
                               (face-foreground 'default nil t)
                               "#ffffff"))
               (default-bg (or (chomp-render--color-to-string bg)
                               (face-background 'default nil t)
                               "#000000")))
          ;; Swap: what was foreground becomes background and vice versa
          (setq face (plist-put face :foreground default-bg))
          (setq face (plist-put face :background default-fg))))
      ;; Foreground (non-inverse)
      (unless inv
        (when-let ((fg-str (chomp-render--color-to-string fg)))
          (setq face (plist-put face :foreground fg-str))))
      ;; Background (non-inverse)
      (unless inv
        (when-let ((bg-str (chomp-render--color-to-string bg)))
          (setq face (plist-put face :background bg-str))))
      ;; Bold
      (when (chomp-attr-bold attr)
        (setq face (plist-put face :weight 'bold)))
      ;; Faint
      (when (chomp-attr-faint attr)
        (setq face (plist-put face :weight 'light)))
      ;; Italic
      (when (chomp-attr-italic attr)
        (setq face (plist-put face :slant 'italic)))
      ;; Underline
      (when-let ((ul (chomp-attr-underline attr)))
        (let ((ul-color (chomp-render--color-to-string
                         (chomp-attr-ul-color attr))))
          (setq face
                (plist-put
                 face :underline
                 (pcase ul
                   ('line    (if ul-color `(:color ,ul-color :style line) t))
                   ('double  (if ul-color `(:color ,ul-color :style line) t))
                   ('curly   (if ul-color
                                 `(:color ,ul-color :style wave)
                               '(:style wave)))
                   ('dotted  (if ul-color
                                 `(:color ,ul-color :style wave)
                               '(:style wave)))
                   ('dashed  (if ul-color
                                 `(:color ,ul-color :style wave)
                               '(:style wave)))
                   (_ t))))))
      ;; Strikethrough
      (when (chomp-attr-crossed attr)
        (setq face (plist-put face :strike-through t)))
      ;; Conceal (make invisible by setting fg = bg)
      (when (chomp-attr-conceal attr)
        (let ((bg-str (or (plist-get face :background) "black")))
          (setq face (plist-put face :foreground bg-str))))
          ;; Return as anonymous face (a plist), caching because styled terminal
          ;; output tends to reuse a small set of attributes across many cells.
          (when (> (hash-table-count chomp-render--attr-face-cache)
                   chomp-render--attr-face-cache-limit)
            (clrhash chomp-render--attr-face-cache))
          (puthash attr face chomp-render--attr-face-cache)
          face))))

;;;; ---- Cursor Rendering -----------------------------------------------

(defun chomp-render--cursor-type-for-style (style)
  "Map cursor STYLE keyword to Emacs `cursor-type' value."
  (pcase style
    (:block 'box)
    (:blinking-block 'box)
    (:underline '(hbar . 2))
    (:blinking-underline '(hbar . 2))
    (:bar '(bar . 2))
    (:blinking-bar '(bar . 2))
    (_ 'box)))

(defun chomp-render--cursor-blink-p (style)
  "Return non-nil if cursor STYLE should blink."
  (memq style '(:blinking-block :blinking-underline :blinking-bar)))

(defun chomp-render--update-cursor (render)
  "Update the cursor overlay position and visibility."
  (let* ((screen (chomp-render-state-screen render))
         (ov (chomp-render-state-cursor-overlay render))
         (display-begin (chomp-render-state-display-begin render))
         (cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (visible (chomp-screen-cursor-visible screen))
         (style (chomp-screen-cursor-style screen)))
    (when ov
      (if (not visible)
          ;; Hide cursor
          (progn
            (overlay-put ov 'face nil)
            (setq-local cursor-type nil))
        ;; Position cursor
        (save-excursion
          (goto-char display-begin)
          (forward-line cy)
          (let* ((bol (point))
                 (eol (line-end-position))
                 (pos (min (+ bol cx) eol))
                 (end (min (1+ pos) (point-max))))
            (move-overlay ov pos end)
            (overlay-put ov 'face 'chomp-cursor)))
        ;; Set Emacs cursor-type based on terminal cursor style
        (setq-local cursor-type (chomp-render--cursor-type-for-style style))
        ;; Enable/disable blink
        (if (chomp-render--cursor-blink-p style)
            (blink-cursor-mode 1)
          (blink-cursor-mode -1))
        ;; Move point to cursor position for scrolling
        (goto-char (overlay-start ov))))))

;;;; ---- Invalidation ---------------------------------------------------

(defun chomp-render-invalidate-all (render)
  "Mark all display lines as needing re-render."
  (let* ((screen (chomp-render-state-screen render))
         (h (chomp-screen-height screen)))
    (setf (chomp-screen-dirty-lines screen)
          (number-sequence 0 (1- h)))))

;;;; ---- Full Re-render (for resize) ------------------------------------

(defun chomp-render-full-reset (render)
  "Completely rebuild the buffer contents from the screen model.
Used after resize when the display area size has changed."
  (let* ((screen (chomp-render-state-screen render))
         (buffer (chomp-render-state-buffer render))
         (w (chomp-screen-width screen))
         (h (chomp-screen-height screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (erase-buffer)
          ;; Re-render scrollback
          (let ((sb-lines (chomp-screen-scrollback-lines screen)))
            (insert (chomp-render--lines-to-string sb-lines w))
            (setf (chomp-render-state-scrollback-count render) (length sb-lines)))
          ;; Place display-begin marker
          (let ((m (chomp-render-state-display-begin render)))
            (set-marker m (point))
            ;; Temporarily disable advance-on-insert so display lines
            ;; go AFTER the marker, not before it
            (set-marker-insertion-type m nil))
          ;; Insert display lines
          (dotimes (i h)
            (let ((line (chomp-screen-get-line screen i)))
              (insert (if line
                          (chomp-render--line-to-string line w)
                        (make-string w ?\s)))
              (when (< i (1- h))
                (insert "\n"))))
          ;; Now re-enable advance-on-insert so future scrollback
          ;; insertions push the marker forward
          (set-marker-insertion-type
           (chomp-render-state-display-begin render) t)
          ;; Reset cursor overlay
          (let ((ov (chomp-render-state-cursor-overlay render)))
            (when ov
              (move-overlay ov (point-min) (1+ (point-min)))))
          ;; Update cursor
          (chomp-render--update-cursor render)
          ;; Clear dirty since we just rendered everything
          (chomp-screen-clear-dirty screen)
          (chomp-screen-clear-scrollback-dirty screen))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun chomp-render-destroy (render)
  "Clean up render state."
  (when-let ((ov (chomp-render-state-cursor-overlay render)))
    (delete-overlay ov))
  (when-let ((m (chomp-render-state-display-begin render)))
    (set-marker m nil)))

(provide 'chomp-render)
;;; chomp-render.el ends here
