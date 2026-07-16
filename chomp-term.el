;;; chomp-term.el --- Terminal screen model for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure data structure representing the terminal screen state.
;; No buffer, display, or process dependencies.
;;
;; The screen is a vector of line records.  Each line contains a vector
;; of cells.  Every cursor coordinate is clamped to valid bounds.
;; Dirty tracking lets the renderer know which lines changed.

;;; Code:

(require 'cl-lib)

;;;; ---- Data Structures ------------------------------------------------

(cl-defstruct (chomp-cell (:copier nil))
  "A single cell on the terminal screen."
  (char ?\s)
  (combining nil)       ; zero-width suffix, including ZWJ sequences
  (width 1)
  (attr nil))

(cl-defstruct (chomp-attr (:copier chomp-attr-copy))
  "Text attributes for a cell."
  (fg nil)          ; nil, integer 0-255, or (R G B)
  (bg nil)
  (ul-color nil)    ; underline color
  (bold nil)
  (faint nil)
  (italic nil)
  (underline nil)   ; nil, line, double, curly, dotted, dashed
  (blink nil)       ; nil, slow, fast
  (inverse nil)
  (conceal nil)
  (crossed nil)
  (font 0))

(cl-defstruct (chomp-line (:copier nil))
  "A single line in the terminal."
  (cells nil)       ; vector of chomp-cell
  (cells-valid t)   ; nil when CELLS must be materialized from TEXT
  (text nil)        ; single-width text cache string, or nil
  (attr-runs nil)   ; list of (START END ATTR) for styled TEXT ranges
  (uniform-attr nil) ; non-nil when TEXT has one shared non-default attr
  (rendered nil)    ; cached rendered/propertized string for non-plain lines
  (prompt-begins nil) ; OSC shell prompt start columns
  (prompt-ends nil)   ; OSC shell prompt end columns
  (wrapped nil)     ; auto-wrapped from previous line?
  (dirty t))

(cl-defstruct (chomp-alt-save (:copier nil))
  "Saved main-screen state when in alternate screen."
  (lines nil)
  (line-start 0)
  (cursor-x 0) (cursor-y 0)
  (cursor-saved-x 0) (cursor-saved-y 0) (cursor-saved-attr nil)
  (current-attr nil)
  (scroll-top 0) (scroll-bottom 23)
  (scrollback nil)
  (scrollback-length 0)
  (auto-wrap t)
  (origin-mode nil)
  (insert-mode nil))

(cl-defstruct (chomp-screen (:constructor chomp-screen--make) (:copier nil))
  "The complete terminal screen state."
  ;; Display
  (lines nil) (width 80) (height 24)
  (line-start 0)       ; physical index of logical display row 0
  ;; Cursor
  (cursor-x 0) (cursor-y 0)
  (cursor-saved-x 0) (cursor-saved-y 0) (cursor-saved-attr nil)
  (cursor-style :block) (cursor-visible t) (cursor-blink nil)
  ;; Attributes
  (current-attr nil)    ; chomp-attr for next char written
  ;; Scroll region (0-indexed, inclusive)
  (scroll-top 0) (scroll-bottom 23)
  ;; Mode flags
  (auto-wrap t) (insert-mode nil) (origin-mode nil)
  (keypad-mode nil) (bracketed-paste nil)
  ;; Mouse
  (mouse-mode nil) (mouse-sgr nil) (mouse-pressed nil) (focus-events nil)
  ;; Character sets
  (charset-g0 'us-ascii) (charset-g1 'us-ascii)
  (charset-g2 'us-ascii) (charset-g3 'us-ascii)
  (charset-active 'g0)
  ;; Alternate screen
  (alt-screen nil)      ; chomp-alt-save when in alt mode, nil when main
  ;; Scrollback (main only)
  (scrollback nil)      ; list of chomp-line, newest first
  (scrollback-length 0) ; cached length of scrollback
  (scrollback-max 10000) ; max lines
  (scrollback-trim-batch 256) ; amortize list trimming after this overflow
  (scrollback-dirty nil) ; non-nil when renderer must fully reconcile it
  (scrollback-appended-count 0) ; lines appended since last render refresh
  (scrollback-trimmed-count 0)  ; oldest lines trimmed since last render refresh
  ;; Title / CWD
  (title "") (cwd nil)
  ;; Tab stops
  (tab-stops nil)
  ;; Last char written (for REP)
  (last-char nil)
  ;; Dirty tracking
  (dirty-lines nil)
  (dirty-map nil)        ; bool vector indexed by row, avoids hot-path `pushnew'
  (dirty-count 0)
  ;; Pending wrap
  (pending-wrap nil))

;;;; ---- DEC Special Graphics Character Set ------------------------------

(defconst chomp--dec-graphics-map
  (let ((m (make-vector 128 nil)))
    (aset m ?` #x25c6)  ; diamond
    (aset m ?a #x2592)  ; medium shade
    (aset m ?b #x2409)  ; HT
    (aset m ?c #x240c)  ; FF
    (aset m ?d #x240d)  ; CR
    (aset m ?e #x240a)  ; LF
    (aset m ?f #xb0)    ; degree
    (aset m ?g #xb1)    ; plus-minus
    (aset m ?h #x2424)  ; NL
    (aset m ?i #x240b)  ; VT
    (aset m ?j #x2518)  ; box up-left
    (aset m ?k #x2510)  ; box down-left
    (aset m ?l #x250c)  ; box down-right
    (aset m ?m #x2514)  ; box up-right
    (aset m ?n #x253c)  ; box cross
    (aset m ?o #x23ba)  ; scan 1
    (aset m ?p #x23bb)  ; scan 3
    (aset m ?q #x2500)  ; box horizontal
    (aset m ?r #x23bc)  ; scan 7
    (aset m ?s #x23bd)  ; scan 9
    (aset m ?t #x251c)  ; box right-tee
    (aset m ?u #x2524)  ; box left-tee
    (aset m ?v #x2534)  ; box up-tee
    (aset m ?w #x252c)  ; box down-tee
    (aset m ?x #x2502)  ; box vertical
    (aset m ?y #x2264)  ; <=
    (aset m ?z #x2265)  ; >=
    (aset m ?{ #x03c0)  ; pi
    (aset m ?| #x2260)  ; not equal
    (aset m ?} #xa3)    ; pound
    (aset m ?~ #xb7)    ; middle dot
    m)
  "DEC Special Graphics character translation table.")

;;;; ---- Internal Helpers -----------------------------------------------

(defsubst chomp--clamp (val lo hi)
  "Clamp VAL to [LO, HI]."
  (min (max val lo) hi))

(defun chomp--make-empty-cells (width)
  "Return a vector of WIDTH default cells."
  (let ((v (make-vector width nil)))
    (dotimes (i width)
      (aset v i (make-chomp-cell)))
    v))

(defun chomp--make-empty-line (width)
  "Return a new empty chomp-line of WIDTH columns.
Cells are materialized lazily from the plain text cache; this keeps scrolling
from allocating a full vector of cell structs for every blank bottom row."
  (make-chomp-line :cells nil
                   :cells-valid nil
                   :text (make-string width ?\s)
                   :dirty t))

(defsubst chomp--line-index (screen row)
  "Return physical line-vector index for logical ROW on SCREEN."
  (% (+ (chomp-screen-line-start screen) row)
     (chomp-screen-height screen)))

(defsubst chomp--line-at (screen row)
  "Return logical ROW from SCREEN."
  (aref (chomp-screen-lines screen) (chomp--line-index screen row)))

(defun chomp--line-ensure-cells (line width)
  "Ensure LINE's cell vector reflects its plain text cache."
  (unless (chomp-line-cells-valid line)
    (let ((text (or (chomp-line-text line) (make-string width ?\s)))
          (cells (chomp-line-cells line)))
      (unless (and cells (= (length cells) width))
        (setq cells (chomp--make-empty-cells width))
        (setf (chomp-line-cells line) cells))
      (let ((i 0))
        (while (< i width)
          (let ((cell (aref cells i)))
            (setf (chomp-cell-char cell)
                  (if (< i (length text)) (aref text i) ?\s))
            (setf (chomp-cell-width cell) 1)
            (setf (chomp-cell-attr cell) (chomp-line-uniform-attr line)))
          (cl-incf i)))
      (dolist (run (chomp-line-attr-runs line))
        (let ((i (nth 0 run))
              (end (min width (nth 1 run)))
              (attr (nth 2 run)))
          (while (< i end)
            (setf (chomp-cell-attr (aref cells i)) attr)
            (cl-incf i))))
      (setf (chomp-line-cells-valid line) t)))
  (chomp-line-cells line))

(defun chomp--line-set-attr-run (line start end attr)
  "Set LINE's attribute over [START, END) to ATTR in its text run cache."
  (when (< start end)
    (let ((runs (chomp-line-attr-runs line))
          (out nil)
          (inserted nil))
      (dolist (run runs)
        (let ((rs (nth 0 run))
              (re (nth 1 run))
              (ra (nth 2 run)))
          (cond
           ((<= re start)
            (push run out))
           ((>= rs end)
            (unless inserted
              (when attr (push (list start end attr) out))
              (setq inserted t))
            (push run out))
           (t
            (when (< rs start)
              (push (list rs start ra) out))
            (unless inserted
              (when attr (push (list start end attr) out))
              (setq inserted t))
            (when (> re end)
              (push (list end re ra) out))))))
      (unless inserted
        (when attr (push (list start end attr) out)))
      (setf (chomp-line-attr-runs line)
            (chomp--merge-attr-runs (nreverse out))))))

(defun chomp--merge-attr-runs (runs)
  "Merge adjacent equal attribute RUNS."
  (let ((merged nil))
    (dolist (run runs)
      (let ((prev (car merged)))
        (if (and prev
                 (= (nth 1 prev) (nth 0 run))
                 (equal (nth 2 prev) (nth 2 run)))
            (setcar (cdr prev) (nth 1 run))
          (push run merged))))
    (nreverse merged)))

(defsubst chomp--set-line-at (screen row line)
  "Set logical ROW on SCREEN to LINE."
  (aset (chomp-screen-lines screen) (chomp--line-index screen row) line))

(defun chomp--ordered-lines-vector (screen)
  "Return SCREEN's display lines in logical row order."
  (let* ((height (chomp-screen-height screen))
         (v (make-vector height nil)))
    (dotimes (i height)
      (aset v i (chomp--line-at screen i)))
    v))

(defun chomp--default-tab-stops (width)
  "Return default tab stop list (every 8 columns) for WIDTH."
  (cl-loop for i from 8 below width by 8 collect i))

(defsubst chomp--attr-non-default-p (attr)
  "Return non-nil if ATTR differs from the all-defaults attribute."
  (and attr
       (or (chomp-attr-fg attr) (chomp-attr-bg attr)
           (chomp-attr-ul-color attr)
           (chomp-attr-bold attr) (chomp-attr-faint attr)
           (chomp-attr-italic attr) (chomp-attr-underline attr)
           (chomp-attr-blink attr) (chomp-attr-inverse attr)
           (chomp-attr-conceal attr) (chomp-attr-crossed attr)
           (/= 0 (chomp-attr-font attr)))))

(defsubst chomp--cell-attr-for-write (screen)
  "Return attr to store on a new cell, or nil for default.
Current attributes are copy-on-write, so non-default cells may share this
immutable attr object instead of allocating a copy per cell."
  (let ((a (chomp-screen-current-attr screen)))
    (and (chomp--attr-non-default-p a) a)))

(defun chomp--make-erase-cell (screen)
  "Return a cell for erased positions (space with current bg via BCE)."
  (let ((bg (and (chomp-screen-current-attr screen)
                 (chomp-attr-bg (chomp-screen-current-attr screen)))))
    (if bg
        (make-chomp-cell :char ?\s :width 1
                         :attr (make-chomp-attr :bg bg))
      (make-chomp-cell))))

(defsubst chomp--char-display-width (char)
  "Return the display width of CHAR (1 for normal, 2 for CJK, 0 for zero-width)."
  (char-width char))

(defun chomp--clear-wide-char-at (screen row col)
  "If COL in ROW is part of a wide character, replace it and its pair with spaces.
This handles both the case where COL is the start of a wide char (width > 1)
and where COL is a continuation cell (width = 0)."
  (when (and (>= col 0) (< col (chomp-screen-width screen)))
    (let* ((line (chomp--line-at screen row))
           (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
           (cell (aref cells col))
           (w (chomp-cell-width cell)))
      (unless (= w 1)
        (setf (chomp-line-text line) nil)
        (setf (chomp-line-uniform-attr line) nil))
      (cond
       ;; This is a wide char start: clear it and its continuation cells
       ((> w 1)
        (aset cells col (make-chomp-cell))
        (cl-loop for i from 1 below w
                 when (< (+ col i) (chomp-screen-width screen))
                 do (aset cells (+ col i) (make-chomp-cell))))
       ;; This is a continuation cell (width=0): find the start and clear all
       ((zerop w)
        ;; Scan backwards to find the wide char that owns this cell
        (let ((start col))
          (while (and (> start 0)
                      (zerop (chomp-cell-width (aref cells start))))
            (cl-decf start))
          (let ((sw (chomp-cell-width (aref cells start))))
            (when (> sw 1)
              (aset cells start (make-chomp-cell))
              (cl-loop for i from 1 below sw
                       when (< (+ start i) (chomp-screen-width screen))
                       do (aset cells (+ start i) (make-chomp-cell)))))))))))

(defsubst chomp--mark-dirty (screen row)
  "Mark ROW as dirty on SCREEN."
  (let ((map (chomp-screen-dirty-map screen)))
    (unless (aref map row)
      (aset map row t)
      (push row (chomp-screen-dirty-lines screen))
      (cl-incf (chomp-screen-dirty-count screen)))))

(defun chomp--mark-region-dirty (screen top bot)
  "Mark logical rows TOP through BOT dirty."
  (unless (= (chomp-screen-dirty-count screen) (chomp-screen-height screen))
    (let ((i top))
      (while (<= i bot)
        (chomp--mark-dirty screen i)
        (cl-incf i)))))

(defsubst chomp--translate-charset (screen char)
  "Translate CHAR through the active character set on SCREEN."
  (let* ((slot (chomp-screen-charset-active screen))
         (cs (pcase slot
               ('g0 (chomp-screen-charset-g0 screen))
               ('g1 (chomp-screen-charset-g1 screen))
               ('g2 (chomp-screen-charset-g2 screen))
               ('g3 (chomp-screen-charset-g3 screen))
               (_   'us-ascii))))
    (if (and (eq cs 'dec-graphics)
             (< char 128))
        (or (aref chomp--dec-graphics-map char) char)
      char)))

;;;; ---- Constructor ----------------------------------------------------

(defun chomp-screen-create (width height)
  "Create a new screen of WIDTH columns and HEIGHT rows."
  (let ((lines (make-vector height nil)))
    (dotimes (i height)
      (aset lines i (chomp--make-empty-line width)))
    (chomp-screen--make
     :lines lines
     :width width
     :height height
     :scroll-bottom (1- height)
     :current-attr (make-chomp-attr)
     :tab-stops (chomp--default-tab-stops width)
     :dirty-lines (number-sequence 0 (1- height))
     :dirty-map (make-vector height t)
     :dirty-count height)))

;;;; ---- Dirty Tracking -------------------------------------------------

(defun chomp-screen-get-dirty (screen)
  "Return sorted list of dirty row indices."
  (sort (copy-sequence (chomp-screen-dirty-lines screen)) #'<))

(defun chomp-screen-clear-dirty (screen)
  "Clear the dirty line list."
  (dolist (row (chomp-screen-dirty-lines screen))
    (aset (chomp-screen-dirty-map screen) row nil))
  (setf (chomp-screen-dirty-lines screen) nil)
  (setf (chomp-screen-dirty-count screen) 0))

(defun chomp-screen-clear-scrollback-dirty (screen)
  "Clear SCREEN's scrollback reconciliation flags."
  (setf (chomp-screen-scrollback-dirty screen) nil)
  (setf (chomp-screen-scrollback-appended-count screen) 0)
  (setf (chomp-screen-scrollback-trimmed-count screen) 0))

;;;; ---- Internal Scrolling ---------------------------------------------

(defun chomp--scroll-region-up (screen count)
  "Scroll the scroll region up by COUNT lines.
Top lines go to scrollback (if on main screen)."
  (let* ((top (chomp-screen-scroll-top screen))
         (bot (chomp-screen-scroll-bottom screen))
         (height (chomp-screen-height screen))
         (lines (chomp-screen-lines screen))
         (width (chomp-screen-width screen))
         (n (min count (1+ (- bot top)))))
    ;; Push lines scrolling off the terminal's top edge into scrollback.
    ;; Inner scroll regions do not leave the display and must not become
    ;; history lines.
    (when (and (not (chomp-screen-alt-screen screen))
               (zerop top))
      (dotimes (i n)
        (push (chomp--line-at screen (+ top i))
              (chomp-screen-scrollback screen))
        (cl-incf (chomp-screen-scrollback-length screen))
        (cl-incf (chomp-screen-scrollback-appended-count screen))))
    (if (and (zerop top) (= bot (1- height)))
        ;; Full-screen scrolling is the dominant bulk-output path.  Advance a
        ;; logical top index instead of moving every row object in the vector.
        (let ((old-start (chomp-screen-line-start screen))
              (i 0))
          (setf (chomp-screen-line-start screen) (% (+ old-start n) height))
          ;; The physical slots that used to hold the top rows are now the
          ;; bottom logical rows; replace them with fresh blank rows.  Do not
          ;; reuse the scrolled-off line objects because scrollback references
          ;; them.
          (while (< i n)
            (aset lines (% (+ old-start i) height)
                  (chomp--make-empty-line width))
            (cl-incf i)))
      ;; Inner/partial regions are uncommon; keep logical row order correct via
      ;; accessors rather than physically normalizing the whole display.
      (let ((i top)
            (end (- bot n)))
        (while (<= i end)
          (chomp--set-line-at screen i (chomp--line-at screen (+ i n)))
          (cl-incf i)))
      (let ((i (1+ (- bot n))))
        (while (<= i bot)
          (chomp--set-line-at screen i (chomp--make-empty-line width))
          (cl-incf i))))
    ;; Mark all lines in region dirty.
    (chomp--mark-region-dirty screen top bot)
    ;; Trim scrollback
    (chomp--trim-scrollback screen)))

(defun chomp--scroll-region-down (screen count)
  "Scroll the scroll region down by COUNT lines.
Bottom lines are discarded."
  (let* ((top (chomp-screen-scroll-top screen))
         (bot (chomp-screen-scroll-bottom screen))
         (height (chomp-screen-height screen))
         (width (chomp-screen-width screen))
         (n (min count (1+ (- bot top)))))
    (if (and (zerop top) (= bot (1- height)))
        (let ((new-start (% (+ (chomp-screen-line-start screen) height (- n))
                            height))
              (i 0))
          (setf (chomp-screen-line-start screen) new-start)
          (while (< i n)
            (chomp--set-line-at screen i (chomp--make-empty-line width))
            (cl-incf i)))
      ;; Shift lines down.
      (let ((i bot)
            (end (+ top n)))
        (while (>= i end)
          (chomp--set-line-at screen i (chomp--line-at screen (- i n)))
          (cl-decf i)))
      ;; Fill top with empty lines.
      (let ((i top)
            (end (+ top n -1)))
        (while (<= i end)
          (chomp--set-line-at screen i (chomp--make-empty-line width))
          (cl-incf i))))
    ;; Mark dirty.
    (chomp--mark-region-dirty screen top bot)))

(defun chomp--trim-scrollback (screen)
  "Trim scrollback to max lines."
  (let ((max (chomp-screen-scrollback-max screen))
        (len (chomp-screen-scrollback-length screen))
        (batch (chomp-screen-scrollback-trim-batch screen)))
    (when (> len (+ max batch))
      ;; Keep the newest MAX entries (the list is newest first) without
      ;; rebuilding the whole list on every scroll after the limit.  Trimming is
      ;; intentionally batched because otherwise every new line after a full
      ;; scrollback performs an O(max) `nthcdr'.  The renderer mirrors the whole
      ;; batch as one delete-oldest + append-newest update.
      (let ((tail (nthcdr (1- max) (chomp-screen-scrollback screen)))
            (trimmed (- len max)))
        (when tail
          (setcdr tail nil))
        (setf (chomp-screen-scrollback-length screen) max)
        (cl-incf (chomp-screen-scrollback-trimmed-count screen) trimmed)))))

;;;; ---- Character Writing ----------------------------------------------

(defun chomp--previous-cell (screen)
  "Return the cell immediately before SCREEN's cursor, or nil.
A pending wrap leaves the cursor on the final cell.  Across a line boundary,
only join with a line that was auto-wrapped."
  (let* ((y (chomp-screen-cursor-y screen))
         (x (chomp-screen-cursor-x screen))
         (width (chomp-screen-width screen))
         (line (chomp--line-at screen y)))
    (cond
     ((chomp-screen-pending-wrap screen)
      (cons line x))
     ((> x 0)
      (let* ((cells (chomp--line-ensure-cells line width))
             (column (1- x)))
        (while (and (> column 0)
                    (zerop (chomp-cell-width (aref cells column))))
          (cl-decf column))
        (cons line column)))
     ((and (> y 0) (chomp-line-wrapped (chomp--line-at screen (1- y))))
      (let* ((previous (chomp--line-at screen (1- y)))
             (cells (chomp--line-ensure-cells previous width))
             (column (1- width)))
        (while (and (> column 0)
                    (zerop (chomp-cell-width (aref cells column))))
          (cl-decf column))
        (cons previous column))))))

(defun chomp--append-to-previous-cell (screen char)
  "Append zero-width CHAR to the cell before SCREEN's cursor."
  (when-let* ((target (chomp--previous-cell screen))
              (cell (aref (chomp-line-cells (car target)) (cdr target))))
    (setf (chomp-cell-combining cell)
          (concat (chomp-cell-combining cell) (string char)))
    (setf (chomp-line-text (car target)) nil
          (chomp-line-attr-runs (car target)) nil
          (chomp-line-uniform-attr (car target)) nil
          (chomp-line-rendered (car target)) nil
          (chomp-line-dirty (car target)) t)
    (chomp--mark-dirty screen
                        (if (eq (car target)
                                (chomp--line-at screen
                                                (chomp-screen-cursor-y screen)))
                            (chomp-screen-cursor-y screen)
                          (1- (chomp-screen-cursor-y screen))))
    t))

(defun chomp--append-joined-char (screen char)
  "Append CHAR when it continues a preceding zero-width joiner sequence."
  (when-let* ((target (chomp--previous-cell screen))
              (cell (aref (chomp-line-cells (car target)) (cdr target)))
              (suffix (chomp-cell-combining cell)))
    (when (eq (aref suffix (1- (length suffix))) #x200d)
      (chomp--append-to-previous-cell screen char))))

(defun chomp-screen-write-char (screen char)
  "Write CHAR at the current cursor position.
Handles double-width (CJK) characters by occupying two cells."
  (let* ((scrn-width (chomp-screen-width screen))
         (char-w (chomp--char-display-width char)))
    (catch 'chomp-screen-write-char
      (when (zerop char-w)
        (chomp--append-to-previous-cell screen char)
        (throw 'chomp-screen-write-char nil))
      (when (chomp--append-joined-char screen char)
        (throw 'chomp-screen-write-char nil))
    (unless (zerop char-w)

    ;; Handle pending wrap
    (when (chomp-screen-pending-wrap screen)
      (setf (chomp-screen-pending-wrap screen) nil)
      (when (chomp-screen-auto-wrap screen)
        ;; Mark current line as wrapped
        (let ((line (chomp--line-at screen (chomp-screen-cursor-y screen))))
          (setf (chomp-line-wrapped line) t))
        ;; Move to next line
        (setf (chomp-screen-cursor-x screen) 0)
        (if (= (chomp-screen-cursor-y screen)
                (chomp-screen-scroll-bottom screen))
            (chomp--scroll-region-up screen 1)
          (cl-incf (chomp-screen-cursor-y screen)))))

    (let* ((cx (chomp-screen-cursor-x screen))
           (cy (chomp-screen-cursor-y screen))
           (translated (chomp--translate-charset screen char))
           (line (chomp--line-at screen cy))
           (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
           (attr (chomp--cell-attr-for-write screen))
           (cell (aref cells cx)))

      (if (and (= char-w 1)
               (not (chomp-screen-insert-mode screen))
               (= (chomp-cell-width cell) 1))
          ;; Hot path: ordinary single-width overwrite.  Mutate the existing
          ;; cell instead of allocating a new struct and running wide-char
          ;; cleanup.  This is the dominant path for plain command output.
          (progn
            (setf (chomp-cell-char cell) translated)
            (setf (chomp-cell-combining cell) nil)
            (setf (chomp-cell-width cell) 1)
            (setf (chomp-cell-attr cell) attr)
            (let ((uniform (chomp-line-uniform-attr line)))
              (if (and (chomp-line-text line)
                       (null (chomp-line-attr-runs line))
                       (or (and (null attr) (null uniform))
                           (and attr uniform
                                (or (eq attr uniform) (equal attr uniform)))))
                  (aset (chomp-line-text line) cx translated)
                (setf (chomp-line-text line) nil)
                (setf (chomp-line-attr-runs line) nil)
                (setf (chomp-line-uniform-attr line) nil)))
            (setf (chomp-line-dirty line) t)
            (chomp--mark-dirty screen cy)
            (setf (chomp-screen-last-char screen) translated)
            (let ((new-cx (1+ cx)))
              (if (>= new-cx scrn-width)
                  (progn
                    (setf (chomp-screen-cursor-x screen) (1- scrn-width))
                    (when (chomp-screen-auto-wrap screen)
                      (setf (chomp-screen-pending-wrap screen) t)))
                (setf (chomp-screen-cursor-x screen) new-cx))))

        ;; General path: wide chars, insert mode, or overwriting existing wide
        ;; cells.
        (progn
          (setf (chomp-line-text line) nil)
          (setf (chomp-line-attr-runs line) nil)
        (setf (chomp-line-uniform-attr line) nil)
          ;; Double-width char at last column: fill with space and wrap
          (when (and (> char-w 1) (>= cx (1- scrn-width)))
            ;; Fill last column with space
            (aset cells cx (make-chomp-cell :char ?\s :width 1 :attr nil))
            (setf (chomp-line-dirty line) t)
            (chomp--mark-dirty screen cy)
            ;; Wrap to next line
            (if (chomp-screen-auto-wrap screen)
                (progn
                  (setf (chomp-line-wrapped line) t)
                  (setf (chomp-screen-cursor-x screen) 0)
                  (if (= cy (chomp-screen-scroll-bottom screen))
                      (chomp--scroll-region-up screen 1)
                    (cl-incf (chomp-screen-cursor-y screen)))
                  (setq cx 0)
                  (setq cy (chomp-screen-cursor-y screen))
                  (setq line (chomp--line-at screen cy))
                  (setq cells (chomp--line-ensure-cells line (chomp-screen-width screen))))
              ;; No auto-wrap: just stay at last column
              (setq cx (1- scrn-width))))

          ;; Clear existing multi-width cells only when needed.  Sequential
          ;; mixed Unicode output usually writes into fresh single-width blank
          ;; cells, so avoid two helper calls per wide character.
          (unless (= (chomp-cell-width (aref cells cx)) 1)
            (chomp--clear-wide-char-at screen cy cx))
          (when (and (> char-w 1)
                     (< (1+ cx) scrn-width)
                     (/= (chomp-cell-width (aref cells (1+ cx))) 1))
            (chomp--clear-wide-char-at screen cy (1+ cx)))

          ;; Insert mode: shift chars right
          (when (chomp-screen-insert-mode screen)
            (cl-loop for i from (1- scrn-width) above (+ cx char-w -1)
                     do (aset cells i (aref cells (- i char-w)))))

          ;; Write the character by mutating existing cells instead of
          ;; allocating new cell structs for every Unicode character.
          (let ((cell (aref cells cx)))
            (setf (chomp-cell-char cell) translated)
            (setf (chomp-cell-combining cell) nil)
            (setf (chomp-cell-width cell) char-w)
            (setf (chomp-cell-attr cell) attr))
          ;; For double-width: mark continuation cell(s).
          (when (> char-w 1)
            (let ((i 1))
              (while (and (< i char-w) (< (+ cx i) scrn-width))
                (let ((cell (aref cells (+ cx i))))
                  (setf (chomp-cell-char cell) ?\s)
                  (setf (chomp-cell-combining cell) nil)
                  (setf (chomp-cell-width cell) 0)
                  (setf (chomp-cell-attr cell) attr))
                (cl-incf i))))

          (setf (chomp-line-dirty line) t)
          (chomp--mark-dirty screen cy)

          ;; Record for REP
          (setf (chomp-screen-last-char screen) translated)

          ;; Advance cursor
          (let ((new-cx (+ cx char-w)))
            (if (>= new-cx scrn-width)
                (progn
                  (setf (chomp-screen-cursor-x screen) (1- scrn-width))
                  (when (chomp-screen-auto-wrap screen)
                    (setf (chomp-screen-pending-wrap screen) t)))
              (setf (chomp-screen-cursor-x screen) new-cx))))))))))

(defun chomp-screen-write-string (screen string start end)
  "Write printable STRING bytes from START to END to SCREEN.
This is the hot path for ground-state text.  It writes contiguous ASCII
runs directly into existing cells and falls back to `chomp-screen-write-char'
for wide/non-ASCII/insert-mode cases."
  (let ((i start)
        (width (chomp-screen-width screen)))
    (while (< i end)
      (if (or (chomp-screen-insert-mode screen)
              (not (eq (chomp-screen-charset-active screen) 'g0))
              (not (eq (chomp-screen-charset-g0 screen) 'us-ascii))
              (>= (aref string i) 128))
          (progn
            (chomp-screen-write-char screen (aref string i))
            (cl-incf i))
        ;; Honor pending wrap before writing the next byte.
        (when (chomp-screen-pending-wrap screen)
          (setf (chomp-screen-pending-wrap screen) nil)
          (when (chomp-screen-auto-wrap screen)
            (let ((line (chomp--line-at screen (chomp-screen-cursor-y screen))))
              (setf (chomp-line-wrapped line) t))
            (setf (chomp-screen-cursor-x screen) 0)
            (if (= (chomp-screen-cursor-y screen)
                   (chomp-screen-scroll-bottom screen))
                (chomp--scroll-region-up screen 1)
              (cl-incf (chomp-screen-cursor-y screen)))))
        (let* ((cx (chomp-screen-cursor-x screen))
               (cy (chomp-screen-cursor-y screen))
               (line (chomp--line-at screen cy))
               (attr-template (chomp-screen-current-attr screen))
               (default-attr (not (chomp--attr-non-default-p attr-template)))
               (line-text (chomp-line-text line))
               (limit (min end (+ i (- width cx))))
               (start-i i)
               (ascii-end
                (let ((j i))
                  (while (and (< j limit) (< (aref string j) 128))
                    (cl-incf j))
                  j)))
          (cond
           ;; Text-first hot path: use it only when this row segment is entirely
           ;; ASCII.  Mixed Unicode lines would otherwise pay to materialize the
           ;; lazy prefix before every wide/non-ASCII character.
           ((and default-attr line-text (= ascii-end limit))
            (store-substring line-text cx (substring string i limit))
            (when (chomp-line-attr-runs line)
              (chomp--line-set-attr-run line cx (+ cx (- limit start-i)) nil))
            (setq i limit)
            (setf (chomp-line-uniform-attr line) nil)
            (setf (chomp-line-cells-valid line) nil))
           ((and (not default-attr) line-text (= ascii-end limit))
            (store-substring line-text cx (substring string i limit))
            (chomp--line-set-attr-run line cx (+ cx (- limit start-i)) attr-template)
            (setq i limit)
            (setf (chomp-line-uniform-attr line) nil)
            (setf (chomp-line-cells-valid line) nil))
           (t
            (let ((cells (chomp--line-ensure-cells line width)))
              (unless default-attr
                (setf (chomp-line-text line) nil)
                (setf (chomp-line-attr-runs line) nil)
        (setf (chomp-line-uniform-attr line) nil))
              ;; Stop before cells that need wide-char cleanup or non-ASCII chars.
              (if default-attr
                  (while (and (< i ascii-end)
                              (= (chomp-cell-width (aref cells (+ cx (- i start-i)))) 1))
                    (let* ((col (+ cx (- i start-i)))
                           (cell (aref cells col))
                           (ch (aref string i)))
                      (setf (chomp-cell-char cell) ch)
                      (setf (chomp-cell-combining cell) nil)
                      (setf (chomp-cell-width cell) 1)
                      (setf (chomp-cell-attr cell) nil))
                    (cl-incf i))
                (while (and (< i ascii-end)
                            (= (chomp-cell-width (aref cells (+ cx (- i start-i)))) 1))
                  (let* ((cell (aref cells (+ cx (- i start-i))))
                         (ch (aref string i)))
                    (setf (chomp-cell-char cell) ch)
                    (setf (chomp-cell-combining cell) nil)
                    (setf (chomp-cell-width cell) 1)
                    (setf (chomp-cell-attr cell) attr-template))
                  (cl-incf i))))))
          (if (= i start-i)
              ;; Could not use the fast row writer for this byte.
              (progn
                (chomp-screen-write-char screen (aref string i))
                (cl-incf i))
            (setf (chomp-line-rendered line) nil)
            (setf (chomp-line-dirty line) t)
            (chomp--mark-dirty screen cy)
            (setf (chomp-screen-last-char screen) (aref string (1- i)))
            (let ((new-cx (+ cx (- i start-i))))
              (if (>= new-cx width)
                  (progn
                    (setf (chomp-screen-cursor-x screen) (1- width))
                    (when (chomp-screen-auto-wrap screen)
                      (setf (chomp-screen-pending-wrap screen) t)))
                (setf (chomp-screen-cursor-x screen) new-cx)))))))))

;;;; ---- Cursor Movement ------------------------------------------------

(defun chomp-screen-cursor-move (screen direction count)
  "Move cursor in DIRECTION by COUNT.  DIRECTION: up, down, left, right."
  (setf (chomp-screen-pending-wrap screen) nil)
  (pcase direction
    ('up
     (let ((min-y (if (chomp-screen-origin-mode screen)
                      (chomp-screen-scroll-top screen) 0)))
       (setf (chomp-screen-cursor-y screen)
             (max min-y (- (chomp-screen-cursor-y screen) count)))))
    ('down
     (let ((max-y (if (chomp-screen-origin-mode screen)
                      (chomp-screen-scroll-bottom screen)
                    (1- (chomp-screen-height screen)))))
       (setf (chomp-screen-cursor-y screen)
             (min max-y (+ (chomp-screen-cursor-y screen) count)))))
    ('left
     (setf (chomp-screen-cursor-x screen)
           (max 0 (- (chomp-screen-cursor-x screen) count))))
    ('right
     (setf (chomp-screen-cursor-x screen)
           (min (1- (chomp-screen-width screen))
                (+ (chomp-screen-cursor-x screen) count))))))

(defun chomp-screen-cursor-goto (screen row col)
  "Move cursor to ROW, COL (0-indexed, origin-mode aware)."
  (setf (chomp-screen-pending-wrap screen) nil)
  (let* ((min-y (if (chomp-screen-origin-mode screen)
                    (chomp-screen-scroll-top screen) 0))
         (max-y (if (chomp-screen-origin-mode screen)
                    (chomp-screen-scroll-bottom screen)
                  (1- (chomp-screen-height screen))))
         (actual-row (+ min-y row)))
    (setf (chomp-screen-cursor-y screen)
          (chomp--clamp actual-row min-y max-y))
    (setf (chomp-screen-cursor-x screen)
          (chomp--clamp col 0 (1- (chomp-screen-width screen))))))

(defun chomp-screen-cursor-next-line (screen count)
  "Move cursor to beginning of line COUNT lines down."
  (setf (chomp-screen-pending-wrap screen) nil)
  (setf (chomp-screen-cursor-x screen) 0)
  (chomp-screen-cursor-move screen 'down count))

(defun chomp-screen-cursor-prev-line (screen count)
  "Move cursor to beginning of line COUNT lines up."
  (setf (chomp-screen-pending-wrap screen) nil)
  (setf (chomp-screen-cursor-x screen) 0)
  (chomp-screen-cursor-move screen 'up count))

;;;; ---- Index / Reverse Index / CR / BS --------------------------------

(defun chomp-screen-index (screen)
  "Index: move cursor down, scrolling if at scroll bottom.
Handles LF, VT, FF."
  (setf (chomp-screen-pending-wrap screen) nil)
  (if (= (chomp-screen-cursor-y screen)
          (chomp-screen-scroll-bottom screen))
      (chomp--scroll-region-up screen 1)
    (cl-incf (chomp-screen-cursor-y screen))))

(defun chomp-screen-reverse-index (screen)
  "Reverse index: move cursor up, scrolling down if at scroll top."
  (setf (chomp-screen-pending-wrap screen) nil)
  (if (= (chomp-screen-cursor-y screen)
          (chomp-screen-scroll-top screen))
      (chomp--scroll-region-down screen 1)
    (cl-decf (chomp-screen-cursor-y screen))))

(defun chomp-screen-next-line (screen)
  "NEL: carriage return + index."
  (setf (chomp-screen-cursor-x screen) 0)
  (chomp-screen-index screen))

(defun chomp-screen-carriage-return (screen)
  "Move cursor to column 0."
  (setf (chomp-screen-pending-wrap screen) nil)
  (setf (chomp-screen-cursor-x screen) 0))

(defun chomp-screen-backspace (screen)
  "Move cursor left by 1 (minimum 0)."
  (setf (chomp-screen-pending-wrap screen) nil)
  (when (> (chomp-screen-cursor-x screen) 0)
    (cl-decf (chomp-screen-cursor-x screen))))

;;;; ---- SGR Attributes -------------------------------------------------

(defun chomp-screen-set-attr (screen prop value)
  "Set attribute PROP to VALUE on SCREEN's current-attr."
  (let ((attr (chomp-attr-copy (chomp-screen-current-attr screen))))
    (setf (chomp-screen-current-attr screen) attr)
    (pcase prop
      (:fg        (setf (chomp-attr-fg attr) value))
      (:bg        (setf (chomp-attr-bg attr) value))
      (:ul-color  (setf (chomp-attr-ul-color attr) value))
      (:bold      (setf (chomp-attr-bold attr) value))
      (:faint     (setf (chomp-attr-faint attr) value))
      (:italic    (setf (chomp-attr-italic attr) value))
      (:underline (setf (chomp-attr-underline attr) value))
      (:blink     (setf (chomp-attr-blink attr) value))
      (:inverse   (setf (chomp-attr-inverse attr) value))
      (:conceal   (setf (chomp-attr-conceal attr) value))
      (:crossed   (setf (chomp-attr-crossed attr) value))
      (:font      (setf (chomp-attr-font attr) value)))))

(defun chomp-screen-reset-attr (screen)
  "Reset all attributes to defaults."
  (setf (chomp-screen-current-attr screen) (make-chomp-attr)))

;;;; ---- Erasing --------------------------------------------------------

(defun chomp-screen-erase-in-display (screen mode)
  "Erase in display.  MODE: 0=to-end, 1=to-start, 2=whole, 3=scrollback."
  (let ((cy (chomp-screen-cursor-y screen))
        (height (chomp-screen-height screen)))
    (pcase mode
      (0 ;; Erase from cursor to end of display
       (chomp-screen-erase-in-line screen 0)
       (cl-loop for r from (1+ cy) below height
                do (chomp--erase-whole-line screen r)))
      (1 ;; Erase from start to cursor
       (chomp-screen-erase-in-line screen 1)
       (cl-loop for r from 0 below cy
                do (chomp--erase-whole-line screen r)))
      (2 ;; Erase whole display
       (let ((bg (and (chomp-screen-current-attr screen)
                      (chomp-attr-bg (chomp-screen-current-attr screen)))))
         (if (null bg)
             ;; Common full-frame path: clear by replacing row objects with
             ;; lazy blank lines instead of materializing and clearing every
             ;; cell.  ED 2 does not affect scrollback.
             (let ((r 0)
                   (width (chomp-screen-width screen)))
               (while (< r height)
                 (chomp--set-line-at screen r (chomp--make-empty-line width))
                 (cl-incf r))
               (chomp--mark-region-dirty screen 0 (1- height)))
           (cl-loop for r from 0 below height
                    do (chomp--erase-whole-line screen r)))))
      (3 ;; Erase scrollback
       (setf (chomp-screen-scrollback screen) nil)
       (setf (chomp-screen-scrollback-length screen) 0)
       (setf (chomp-screen-scrollback-dirty screen) t)))))

(defun chomp-screen-erase-in-line (screen mode)
  "Erase in line.  MODE: 0=to-end, 1=to-start, 2=whole."
  (let* ((cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (width (chomp-screen-width screen))
         (line (chomp--line-at screen cy))
         (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
         (ecell (chomp--make-erase-cell screen)))
    (pcase mode
      (0 ;; cursor to end
       (cl-loop for i from cx below width
                do (aset cells i (copy-chomp-cell ecell)))
       (if (and (null (chomp-cell-attr ecell))
                (chomp-line-text line)
                (null (chomp-line-uniform-attr line)))
           (cl-loop for i from cx below width
                    do (aset (chomp-line-text line) i ?\s))
         (setf (chomp-line-text line) nil)
         (setf (chomp-line-uniform-attr line) nil)))
      (1 ;; start to cursor
       (cl-loop for i from 0 to cx
                do (aset cells i (copy-chomp-cell ecell)))
       (if (and (null (chomp-cell-attr ecell))
                (chomp-line-text line)
                (null (chomp-line-uniform-attr line)))
           (cl-loop for i from 0 to cx
                    do (aset (chomp-line-text line) i ?\s))
         (setf (chomp-line-text line) nil)
         (setf (chomp-line-uniform-attr line) nil)))
      (2 ;; whole line
       (cl-loop for i from 0 below width
                do (aset cells i (copy-chomp-cell ecell)))
       (setf (chomp-line-prompt-begins line) nil)
       (setf (chomp-line-prompt-ends line) nil)
       (if (null (chomp-cell-attr ecell))
           (progn
             (setf (chomp-line-text line) (make-string width ?\s))
             (setf (chomp-line-uniform-attr line) nil))
         (setf (chomp-line-text line) nil)
         (setf (chomp-line-uniform-attr line) nil))))
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen cy)))

(defun chomp--erase-whole-line (screen row)
  "Erase entire line ROW with BCE."
  (let* ((width (chomp-screen-width screen))
         (line (chomp--line-at screen row))
         (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
         (ecell (chomp--make-erase-cell screen)))
    (cl-loop for i from 0 below width
             do (aset cells i (copy-chomp-cell ecell)))
    (if (null (chomp-cell-attr ecell))
        (progn
          (setf (chomp-line-text line) (make-string width ?\s))
          (setf (chomp-line-uniform-attr line) nil))
      (setf (chomp-line-text line) nil)
      (setf (chomp-line-uniform-attr line) nil))
    (setf (chomp-line-prompt-begins line) nil)
    (setf (chomp-line-prompt-ends line) nil)
    (setf (chomp-line-wrapped line) nil)
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen row)))

(defun chomp-screen-erase-chars (screen count)
  "Erase COUNT characters starting at cursor (ECH)."
  (let* ((cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (width (chomp-screen-width screen))
         (line (chomp--line-at screen cy))
         (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
         (ecell (chomp--make-erase-cell screen))
         (end (min (+ cx count) width)))
    (cl-loop for i from cx below end
             do (aset cells i (copy-chomp-cell ecell)))
    (if (and (null (chomp-cell-attr ecell))
             (chomp-line-text line)
             (null (chomp-line-uniform-attr line)))
        (cl-loop for i from cx below end
                 do (aset (chomp-line-text line) i ?\s))
      (setf (chomp-line-text line) nil)
      (setf (chomp-line-uniform-attr line) nil))
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen cy)))

;;;; ---- Scrolling (public) ---------------------------------------------

(defun chomp-screen-scroll (screen direction count)
  "Scroll COUNT lines.  DIRECTION: up or down."
  (pcase direction
    ('up   (chomp--scroll-region-up screen count))
    ('down (chomp--scroll-region-down screen count))))

;;;; ---- Line Operations ------------------------------------------------

(defun chomp-screen-insert-lines (screen count)
  "Insert COUNT blank lines at cursor row, within scroll region."
  (setf (chomp-screen-pending-wrap screen) nil)
  (let* ((cy (chomp-screen-cursor-y screen))
         (top (chomp-screen-scroll-top screen))
         (bot (chomp-screen-scroll-bottom screen))
         (width (chomp-screen-width screen)))
    ;; Only operates within scroll region and when cursor is in it
    (when (and (>= cy top) (<= cy bot))
      (let ((n (min count (1+ (- bot cy)))))
        ;; Shift lines down from cy
        (cl-loop for i from bot downto (+ cy n)
                 do (chomp--set-line-at screen i (chomp--line-at screen (- i n))))
        ;; Insert blank lines at cy
        (cl-loop for i from cy below (+ cy n)
                 do (chomp--set-line-at screen i (chomp--make-empty-line width)))
        ;; Mark dirty
        (chomp--mark-region-dirty screen cy bot)))))

(defun chomp-screen-delete-lines (screen count)
  "Delete COUNT lines at cursor row, within scroll region."
  (setf (chomp-screen-pending-wrap screen) nil)
  (let* ((cy (chomp-screen-cursor-y screen))
         (top (chomp-screen-scroll-top screen))
         (bot (chomp-screen-scroll-bottom screen))
         (width (chomp-screen-width screen)))
    (when (and (>= cy top) (<= cy bot))
      (let ((n (min count (1+ (- bot cy)))))
        ;; Shift lines up
        (cl-loop for i from cy to (- bot n)
                 do (chomp--set-line-at screen i (chomp--line-at screen (+ i n))))
        ;; Fill bottom with empty
        (cl-loop for i from (1+ (- bot n)) to bot
                 do (chomp--set-line-at screen i (chomp--make-empty-line width)))
        ;; Mark dirty
        (chomp--mark-region-dirty screen cy bot)))))

;;;; ---- Character Operations -------------------------------------------

(defun chomp-screen-insert-chars (screen count)
  "Insert COUNT blank characters at cursor, shifting right."
  (let* ((cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (width (chomp-screen-width screen))
         (line (chomp--line-at screen cy))
         (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
         (n (min count (- width cx))))
    (setf (chomp-line-text line) nil)
        (setf (chomp-line-uniform-attr line) nil)
    ;; Shift right
    (cl-loop for i from (1- width) downto (+ cx n)
             do (aset cells i (aref cells (- i n))))
    ;; Insert blanks
    (cl-loop for i from cx below (+ cx n)
             do (aset cells i (make-chomp-cell)))
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen cy)))

(defun chomp-screen-delete-chars (screen count)
  "Delete COUNT characters at cursor, shifting left."
  (let* ((cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (width (chomp-screen-width screen))
         (line (chomp--line-at screen cy))
         (cells (chomp--line-ensure-cells line (chomp-screen-width screen)))
         (n (min count (- width cx))))
    (setf (chomp-line-text line) nil)
        (setf (chomp-line-uniform-attr line) nil)
    ;; Shift left
    (cl-loop for i from cx below (- width n)
             do (aset cells i (aref cells (+ i n))))
    ;; Fill end with blanks
    (cl-loop for i from (- width n) below width
             do (aset cells i (make-chomp-cell)))
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen cy)))

(defun chomp-screen-repeat-char (screen count)
  "Repeat the last written character COUNT times."
  (when-let ((ch (chomp-screen-last-char screen)))
    (dotimes (_ count)
      (chomp-screen-write-char screen ch))))

;;;; ---- Tab Stops ------------------------------------------------------

(defun chomp-screen-tab-forward (screen count)
  "Move cursor forward to the next tab stop, COUNT times."
  (setf (chomp-screen-pending-wrap screen) nil)
  (let ((cx (chomp-screen-cursor-x screen))
        (max-x (1- (chomp-screen-width screen)))
        (stops (chomp-screen-tab-stops screen)))
    (dotimes (_ count)
      (let ((next (cl-find-if (lambda (s) (> s cx)) stops)))
        (setq cx (if next (min next max-x) max-x))))
    (setf (chomp-screen-cursor-x screen) cx)))

(defun chomp-screen-tab-backward (screen count)
  "Move cursor backward to the previous tab stop, COUNT times."
  (setf (chomp-screen-pending-wrap screen) nil)
  (let ((cx (chomp-screen-cursor-x screen))
        (stops (reverse (chomp-screen-tab-stops screen))))
    (dotimes (_ count)
      (let ((prev (cl-find-if (lambda (s) (< s cx)) stops)))
        (setq cx (or prev 0))))
    (setf (chomp-screen-cursor-x screen) cx)))

(defun chomp-screen-set-tab-stop (screen)
  "Set a tab stop at the current cursor column."
  (let ((cx (chomp-screen-cursor-x screen)))
    (unless (member cx (chomp-screen-tab-stops screen))
      (setf (chomp-screen-tab-stops screen)
            (sort (cons cx (chomp-screen-tab-stops screen)) #'<)))))

(defun chomp-screen-clear-tab-stop (screen mode)
  "Clear tab stops.  MODE: 0=current, 3=all."
  (pcase mode
    (0 (setf (chomp-screen-tab-stops screen)
             (delq (chomp-screen-cursor-x screen)
                   (chomp-screen-tab-stops screen))))
    (3 (setf (chomp-screen-tab-stops screen) nil))))

;;;; ---- Scroll Region --------------------------------------------------

(defun chomp-screen-set-scroll-region (screen top bottom)
  "Set scroll region to [TOP, BOTTOM] (0-indexed, inclusive)."
  (let ((max-row (1- (chomp-screen-height screen))))
    (setq top (chomp--clamp top 0 max-row))
    (setq bottom (chomp--clamp bottom 0 max-row))
    (when (< top bottom)
      (setf (chomp-screen-scroll-top screen) top)
      (setf (chomp-screen-scroll-bottom screen) bottom)
      ;; DECSTBM homes cursor
      (chomp-screen-cursor-goto screen 0 0))))

;;;; ---- Alternate Screen -----------------------------------------------

(defun chomp-screen-enter-alt (screen)
  "Enter alternate screen buffer."
  (unless (chomp-screen-alt-screen screen)
    ;; Save main screen state
    (setf (chomp-screen-alt-screen screen)
          (make-chomp-alt-save
           :lines (chomp-screen-lines screen)
           :line-start (chomp-screen-line-start screen)
           :cursor-x (chomp-screen-cursor-x screen)
           :cursor-y (chomp-screen-cursor-y screen)
           :cursor-saved-x (chomp-screen-cursor-saved-x screen)
           :cursor-saved-y (chomp-screen-cursor-saved-y screen)
           :cursor-saved-attr (chomp-screen-cursor-saved-attr screen)
           :current-attr (chomp-attr-copy (chomp-screen-current-attr screen))
           :scroll-top (chomp-screen-scroll-top screen)
           :scroll-bottom (chomp-screen-scroll-bottom screen)
           :scrollback (chomp-screen-scrollback screen)
           :scrollback-length (chomp-screen-scrollback-length screen)
           :auto-wrap (chomp-screen-auto-wrap screen)
           :origin-mode (chomp-screen-origin-mode screen)
           :insert-mode (chomp-screen-insert-mode screen)))
    ;; Create fresh alt screen
    (let ((w (chomp-screen-width screen))
          (h (chomp-screen-height screen)))
      (let ((lines (make-vector h nil)))
        (dotimes (i h)
          (aset lines i (chomp--make-empty-line w)))
        (setf (chomp-screen-lines screen) lines))
      (setf (chomp-screen-line-start screen) 0)
      (setf (chomp-screen-cursor-x screen) 0)
      (setf (chomp-screen-cursor-y screen) 0)
      (setf (chomp-screen-scroll-top screen) 0)
      (setf (chomp-screen-scroll-bottom screen) (1- h))
      (setf (chomp-screen-scrollback screen) nil)
      (setf (chomp-screen-scrollback-length screen) 0)
      (setf (chomp-screen-scrollback-dirty screen) t)
      (setf (chomp-screen-dirty-lines screen)
            (number-sequence 0 (1- h)))
      (setf (chomp-screen-dirty-map screen) (make-vector h t))
      (setf (chomp-screen-dirty-count screen) h))))

(defun chomp-screen-leave-alt (screen)
  "Leave alternate screen buffer, restoring main screen."
  (when-let ((saved (chomp-screen-alt-screen screen)))
    (setf (chomp-screen-lines screen) (chomp-alt-save-lines saved))
    (setf (chomp-screen-line-start screen) (chomp-alt-save-line-start saved))
    (setf (chomp-screen-cursor-x screen) (chomp-alt-save-cursor-x saved))
    (setf (chomp-screen-cursor-y screen) (chomp-alt-save-cursor-y saved))
    (setf (chomp-screen-cursor-saved-x screen) (chomp-alt-save-cursor-saved-x saved))
    (setf (chomp-screen-cursor-saved-y screen) (chomp-alt-save-cursor-saved-y saved))
    (setf (chomp-screen-cursor-saved-attr screen) (chomp-alt-save-cursor-saved-attr saved))
    (setf (chomp-screen-current-attr screen) (chomp-alt-save-current-attr saved))
    (setf (chomp-screen-scroll-top screen) (chomp-alt-save-scroll-top saved))
    (setf (chomp-screen-scroll-bottom screen) (chomp-alt-save-scroll-bottom saved))
    (setf (chomp-screen-scrollback screen) (chomp-alt-save-scrollback saved))
    (setf (chomp-screen-scrollback-length screen)
          (chomp-alt-save-scrollback-length saved))
    (setf (chomp-screen-scrollback-dirty screen) t)
    (setf (chomp-screen-auto-wrap screen) (chomp-alt-save-auto-wrap saved))
    (setf (chomp-screen-origin-mode screen) (chomp-alt-save-origin-mode saved))
    (setf (chomp-screen-insert-mode screen) (chomp-alt-save-insert-mode saved))
    (setf (chomp-screen-alt-screen screen) nil)
    ;; Everything is dirty
    (setf (chomp-screen-dirty-lines screen)
          (number-sequence 0 (1- (chomp-screen-height screen))))
    (setf (chomp-screen-dirty-map screen)
          (make-vector (chomp-screen-height screen) t))
    (setf (chomp-screen-dirty-count screen) (chomp-screen-height screen))))

;;;; ---- Save / Restore Cursor ------------------------------------------

(defun chomp-screen-save-cursor (screen)
  "Save cursor position and attributes (DECSC)."
  (setf (chomp-screen-cursor-saved-x screen) (chomp-screen-cursor-x screen))
  (setf (chomp-screen-cursor-saved-y screen) (chomp-screen-cursor-y screen))
  (setf (chomp-screen-cursor-saved-attr screen)
        (chomp-attr-copy (chomp-screen-current-attr screen))))

(defun chomp-screen-restore-cursor (screen)
  "Restore cursor position and attributes (DECRC)."
  (setf (chomp-screen-pending-wrap screen) nil)
  (setf (chomp-screen-cursor-x screen)
        (chomp--clamp (chomp-screen-cursor-saved-x screen)
                      0 (1- (chomp-screen-width screen))))
  (setf (chomp-screen-cursor-y screen)
        (chomp--clamp (chomp-screen-cursor-saved-y screen)
                      0 (1- (chomp-screen-height screen))))
  (when (chomp-screen-cursor-saved-attr screen)
    (setf (chomp-screen-current-attr screen)
          (chomp-attr-copy (chomp-screen-cursor-saved-attr screen)))))

;;;; ---- Mode Setting ---------------------------------------------------

(defun chomp-screen-set-mode (screen mode value)
  "Set a DECSET/DECRST MODE to VALUE (t or nil)."
  (pcase mode
    (1    (setf (chomp-screen-keypad-mode screen) value))
    (7    (setf (chomp-screen-auto-wrap screen) value))
    (9    (setf (chomp-screen-mouse-mode screen) (and value 'x10)))
    (12   (setf (chomp-screen-cursor-blink screen) value))
    (25   (setf (chomp-screen-cursor-visible screen) value))
    (80   nil) ;; sixel scrolling - TODO
    (1000 (setf (chomp-screen-mouse-mode screen) (and value 'normal)))
    (1002 (setf (chomp-screen-mouse-mode screen) (and value 'button-event)))
    (1003 (setf (chomp-screen-mouse-mode screen) (and value 'any-event)))
    (1004 (setf (chomp-screen-focus-events screen) value))
    (1006 (setf (chomp-screen-mouse-sgr screen) value))
    (1047 ;; Alt screen only (no cursor save)
     (if value
         (chomp-screen-enter-alt screen)
       (chomp-screen-leave-alt screen)))
    (1048 ;; Cursor save only
     (if value
         (chomp-screen-save-cursor screen)
       (chomp-screen-restore-cursor screen)))
    (1049 ;; Alt screen + cursor save
     (if value
         (progn
           (chomp-screen-save-cursor screen)
           (chomp-screen-enter-alt screen))
       (chomp-screen-leave-alt screen)
       (chomp-screen-restore-cursor screen)))
    (2004 (setf (chomp-screen-bracketed-paste screen) value))
    (4    (setf (chomp-screen-insert-mode screen) value))))

(defun chomp-screen-set-cursor-style (screen style)
  "Set the cursor style.  STYLE: 0-6."
  (setf (chomp-screen-cursor-style screen)
        (pcase style
          ((or 0 1) :blinking-block)
          (2 :block)
          (3 :blinking-underline)
          (4 :underline)
          (5 :blinking-bar)
          (6 :bar)
          (_ :block))))

;;;; ---- Resize ---------------------------------------------------------

(defun chomp-screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT.
Reflows wrapped lines, clamps cursor, resets scroll region."
  (when (and (> new-width 0) (> new-height 0)
             (or (/= new-width (chomp-screen-width screen))
                 (/= new-height (chomp-screen-height screen))))
    (let ((old-lines (chomp--ordered-lines-vector screen))
          (old-width (chomp-screen-width screen)))
      ;; Phase 1: Unwrap lines into logical lines
      (let ((logical (chomp--unwrap-lines old-lines old-width)))
        ;; Phase 2: Re-wrap to new width
        (let ((new-lines (chomp--rewrap-lines logical new-width new-height)))
          ;; Phase 3: Apply
          (setf (chomp-screen-lines screen) new-lines)
          (setf (chomp-screen-line-start screen) 0)
          (setf (chomp-screen-width screen) new-width)
          (setf (chomp-screen-height screen) new-height)
          ;; Phase 4: Clamp cursor
          (setf (chomp-screen-cursor-x screen)
                (min (chomp-screen-cursor-x screen) (1- new-width)))
          (setf (chomp-screen-cursor-y screen)
                (min (chomp-screen-cursor-y screen) (1- new-height)))
          ;; Phase 5: Reset scroll region
          (setf (chomp-screen-scroll-top screen) 0)
          (setf (chomp-screen-scroll-bottom screen) (1- new-height))
          ;; Phase 6: All dirty
          (setf (chomp-screen-dirty-lines screen)
                (number-sequence 0 (1- new-height)))
          (setf (chomp-screen-dirty-map screen) (make-vector new-height t))
          (setf (chomp-screen-dirty-count screen) new-height)
          ;; Reset tab stops for new width
          (setf (chomp-screen-tab-stops screen)
                (chomp--default-tab-stops new-width)))))))

(defun chomp--unwrap-lines (lines old-width)
  "Merge wrapped physical lines into logical lines.
Returns list of (CELLS . TRAILING-WRAP-P)."
  (let ((result nil)
        (current-cells nil))
    (dotimes (i (length lines))
      (let ((line (aref lines i)))
        (setq current-cells
              (vconcat (or current-cells [])
                       (chomp--line-ensure-cells line old-width)))
        (unless (chomp-line-wrapped line)
          ;; A non-wrapped line's right-padding is presentation, not logical
          ;; content.  Keeping it would make resize/reflow invent large runs of
          ;; blanks and split real content unexpectedly.
          (setq current-cells (chomp--trim-trailing-blank-cells current-cells))
          (push (cons current-cells nil) result)
          (setq current-cells nil))))
    (when current-cells
      (push (cons current-cells t) result))
    (nreverse result)))

(defun chomp--blank-cell-p (cell)
  "Return non-nil when CELL is a default blank cell."
  (and (= (chomp-cell-char cell) ?\s)
       (= (chomp-cell-width cell) 1)
       (null (chomp-cell-attr cell))))

(defun chomp--trim-trailing-blank-cells (cells)
  "Return CELLS without trailing default blanks."
  (let ((end (length cells)))
    (while (and (> end 0)
                (chomp--blank-cell-p (aref cells (1- end))))
      (cl-decf end))
    (cl-subseq cells 0 end)))

(defun chomp--rewrap-lines (logical-lines new-width new-height)
  "Re-wrap LOGICAL-LINES to NEW-WIDTH, returning vector of NEW-HEIGHT lines."
  (let ((physical nil))
    (dolist (ll logical-lines)
      (let* ((cells (car ll))
             (len (length cells)))
        (if (<= len new-width)
            (push (make-chomp-line
                   :cells (chomp--pad-cells cells new-width)
                   :wrapped nil :dirty t)
                  physical)
          (let ((offset 0))
            (while (< offset len)
              ;; Avoid cutting a wide character away from its continuation
              ;; cell.  The model stores wide chars as a start cell followed by
              ;; width=0 continuation cells, so chunk boundaries must not fall
              ;; immediately before a continuation.
              (let* ((end (chomp--wrap-end cells offset new-width))
                     (chunk (cl-subseq cells offset end))
                     (last-chunk (>= end len)))
                (push (make-chomp-line
                       :cells (chomp--pad-cells chunk new-width)
                       :wrapped (not last-chunk) :dirty t)
                      physical)
                (setq offset end)))))))
    (setq physical (nreverse physical))
    (let ((count (length physical)))
      (cond
       ((> count new-height)
        (vconcat (last physical new-height)))
       ((< count new-height)
        (vconcat physical
                 (cl-loop repeat (- new-height count)
                          collect (chomp--make-empty-line new-width))))
       (t (vconcat physical))))))

(defun chomp--wrap-end (cells offset width)
  "Return a safe wrap end for CELLS starting at OFFSET and WIDTH columns."
  (let* ((len (length cells))
         (end (min (+ offset width) len)))
    ;; If the next cell is a wide-char continuation, this boundary would split
    ;; the wide char; back up before its start.  Ensure forward progress even on
    ;; a one-column terminal.
    (while (and (< end len)
                (> end offset)
                (zerop (chomp-cell-width (aref cells end))))
      (cl-decf end))
    (if (= end offset)
        (min len (1+ offset))
      end)))

(defun chomp--pad-cells (cells target-width)
  "Pad or truncate CELLS vector to TARGET-WIDTH."
  (let ((len (length cells)))
    (cond
     ((= len target-width) cells)
     ((< len target-width)
      (vconcat cells
               (cl-loop repeat (- target-width len)
                        collect (make-chomp-cell))))
     (t (cl-subseq cells 0 target-width)))))

;;;; ---- Reset ----------------------------------------------------------

(defun chomp-screen-reset (screen)
  "Full terminal reset (RIS)."
  (let ((w (chomp-screen-width screen))
        (h (chomp-screen-height screen)))
    ;; Leave alt screen if active
    (when (chomp-screen-alt-screen screen)
      (setf (chomp-screen-alt-screen screen) nil))
    ;; Reset lines
    (let ((lines (make-vector h nil)))
      (dotimes (i h)
        (aset lines i (chomp--make-empty-line w)))
      (setf (chomp-screen-lines screen) lines))
    (setf (chomp-screen-line-start screen) 0)
    ;; Reset cursor
    (setf (chomp-screen-cursor-x screen) 0)
    (setf (chomp-screen-cursor-y screen) 0)
    (setf (chomp-screen-cursor-saved-x screen) 0)
    (setf (chomp-screen-cursor-saved-y screen) 0)
    (setf (chomp-screen-cursor-saved-attr screen) nil)
    (setf (chomp-screen-cursor-style screen) :block)
    (setf (chomp-screen-cursor-visible screen) t)
    (setf (chomp-screen-pending-wrap screen) nil)
    ;; Reset attrs
    (setf (chomp-screen-current-attr screen) (make-chomp-attr))
    ;; Reset scroll region
    (setf (chomp-screen-scroll-top screen) 0)
    (setf (chomp-screen-scroll-bottom screen) (1- h))
    ;; Reset modes
    (setf (chomp-screen-auto-wrap screen) t)
    (setf (chomp-screen-insert-mode screen) nil)
    (setf (chomp-screen-origin-mode screen) nil)
    (setf (chomp-screen-keypad-mode screen) nil)
    (setf (chomp-screen-bracketed-paste screen) nil)
    (setf (chomp-screen-mouse-mode screen) nil)
    (setf (chomp-screen-mouse-sgr screen) nil)
    (setf (chomp-screen-focus-events screen) nil)
    ;; Reset charsets
    (setf (chomp-screen-charset-g0 screen) 'us-ascii)
    (setf (chomp-screen-charset-g1 screen) 'us-ascii)
    (setf (chomp-screen-charset-g2 screen) 'us-ascii)
    (setf (chomp-screen-charset-g3 screen) 'us-ascii)
    (setf (chomp-screen-charset-active screen) 'g0)
    ;; Reset scrollback
    (setf (chomp-screen-scrollback screen) nil)
    (setf (chomp-screen-scrollback-length screen) 0)
    (setf (chomp-screen-scrollback-dirty screen) t)
    ;; Reset tab stops
    (setf (chomp-screen-tab-stops screen)
          (chomp--default-tab-stops w))
    ;; Reset last char
    (setf (chomp-screen-last-char screen) nil)
    ;; Everything is dirty
    (setf (chomp-screen-dirty-lines screen)
          (number-sequence 0 (1- h)))
    (setf (chomp-screen-dirty-map screen) (make-vector h t))
    (setf (chomp-screen-dirty-count screen) h)))

;;;; ---- Character Set Designation --------------------------------------

(defun chomp-screen-designate-charset (screen slot charset-char)
  "Designate character set for SLOT (g0-g3) from CHARSET-CHAR."
  (let ((cs (pcase charset-char
              (?0 'dec-graphics)
              (?B 'us-ascii)
              (?A 'uk)
              (_  'us-ascii))))
    (pcase slot
      (?\( (setf (chomp-screen-charset-g0 screen) cs))
      ((or ?\) ?-) (setf (chomp-screen-charset-g1 screen) cs))
      ((or ?* ?.) (setf (chomp-screen-charset-g2 screen) cs))
      ((or ?+ ?/) (setf (chomp-screen-charset-g3 screen) cs)))))

(defun chomp-screen-shift-out (screen)
  "Invoke G1 character set (SO)."
  (setf (chomp-screen-charset-active screen) 'g1))

(defun chomp-screen-shift-in (screen)
  "Invoke G0 character set (SI)."
  (setf (chomp-screen-charset-active screen) 'g0))

;;;; ---- Plain Text -----------------------------------------------------

(defun chomp--line-plain-text (line width)
  "Return LINE as plain text with terminal padding removed."
  (let ((cells (chomp--line-ensure-cells line width))
        chars)
    (dotimes (i width)
      (let ((cell (aref cells i)))
        (unless (zerop (chomp-cell-width cell))
          (push (chomp-cell-char cell) chars))))
    (string-trim-right (apply #'string (nreverse chars)))))

(defun chomp-screen-plain-text (screen)
  "Return scrollback and viewport text from SCREEN.
Rows joined by a soft wrap have no intervening newline."
  (let* ((width (chomp-screen-width screen))
         (lines (append (chomp-screen-scrollback-lines-raw screen)
                        (append (chomp--ordered-lines-vector screen) nil)))
         parts)
    (dolist (line lines)
      (push (chomp--line-plain-text line width) parts)
      (unless (chomp-line-wrapped line)
        (push "\n" parts)))
    (string-trim-right (apply #'concat (nreverse parts)))))

;;;; ---- Query ----------------------------------------------------------

(defun chomp-screen-get-line (screen row)
  "Return the chomp-line at ROW."
  (when (and (>= row 0) (< row (chomp-screen-height screen)))
    (let ((line (chomp--line-at screen row)))
      (chomp--line-ensure-cells line (chomp-screen-width screen))
      line)))

(defun chomp-screen-scrollback-lines-raw (screen)
  "Return scrollback lines, oldest first, without materializing lazy cells."
  (reverse (chomp-screen-scrollback screen)))

(defun chomp-screen-scrollback-lines (screen)
  "Return scrollback lines, oldest first."
  (let ((lines (chomp-screen-scrollback-lines-raw screen)))
    (dolist (line lines)
      (chomp--line-ensure-cells line (chomp-screen-width screen)))
    lines))

;;;; ---- Cell copying helper (used by erase) ----------------------------

(defun copy-chomp-cell (cell)
  "Return a shallow copy of CELL."
  (make-chomp-cell :char (chomp-cell-char cell)
                   :combining (chomp-cell-combining cell)
                   :width (chomp-cell-width cell)
                   :attr (chomp-cell-attr cell)))

(provide 'chomp-term)
;;; chomp-term.el ends here
