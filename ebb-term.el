;;; ebb-term.el --- Terminal screen model for ebb -*- lexical-binding: t; -*-

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

(defgroup ebb nil
  "Terminal emulator."
  :group 'processes
  :prefix "ebb-")

;;;; ---- Data Structures ------------------------------------------------

(cl-defstruct (ebb-cell (:copier nil))
  "A single cell on the terminal screen."
  (char ?\s)
  (combining nil)       ; zero-width suffix, including ZWJ sequences
  (width 1)
  (attr nil))

(cl-defstruct (ebb-attr (:copier ebb-attr-copy))
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
  (font 0)
  (hyperlink nil)
  (hyperlink-id nil))

(cl-defstruct (ebb-line (:copier nil))
  "A single line in the terminal."
  (cells nil)       ; vector of ebb-cell
  (cells-valid t)   ; nil when CELLS must be materialized from TEXT
  (text nil)        ; single-width text cache string, or nil
  (attr-runs nil)   ; list of (START END ATTR) for styled TEXT ranges
  (uniform-attr nil) ; non-nil when TEXT has one shared non-default attr
  (rendered nil)    ; cached rendered/propertized string for non-plain lines
  (prompt-begins nil) ; OSC shell prompt start columns
  (prompt-ends nil)   ; OSC shell prompt end columns
  (wrapped nil)     ; auto-wrapped from previous line?
  (dirty t))

(cl-defstruct (ebb-history-line (:copier nil))
  "One unwrapped main-screen history line."
  (id 0)
  (cells [])
  (text nil)          ; lazy single-width text, instead of CELLS
  (text-length 0)     ; logical prefix of TEXT, excluding trailing blanks
  (attr-runs nil)     ; logical (START END ATTR) ranges over TEXT
  (rendition 'normal) ; DEC width/height rendition for reflowed rows
  (prompt-begins nil)
  (prompt-ends nil)
  (open nil)
  (generation 0))

(cl-defstruct (ebb-history-chunk (:copier nil))
  "A batch of closed plain ASCII history lines.
TEXT either stores fixed-width source rows consecutively or uses STARTS to
index retained CRLF input.  LENGTHS records each logical line's unpadded
length.  FIRST permits bounded trimming without copying the surviving suffix."
  (id-start 0)
  (text "")
  (source-width 0)
  (starts nil)
  (lengths [])
  (first 0)
  (row-ends nil)
  (row-map-width nil))

(defvar ebb--history-batch-screen nil
  "Screen currently collecting plain history rows into one chunk.")

(defvar ebb--history-batch-rows nil
  "Reverse-ordered plain rows collected for `ebb--history-batch-screen'.")

(cl-defstruct (ebb-alt-save (:copier nil))
  "Saved main-screen state when in alternate screen."
  (lines nil)
  (width 80) (height 24)
  (line-start 0)
  (cursor-x 0) (cursor-y 0)
  (cursor-saved-x 0) (cursor-saved-y 0) (cursor-saved-attr nil)
  (pending-wrap nil)
  (current-attr nil)
  (scroll-top 0) (scroll-bottom 23)
  (scrollback nil)
  (scrollback-length 0)
  (history-next-id 0)
  (history-generation 0)
  (auto-wrap t)
  (origin-mode nil)
  (insert-mode nil))

(cl-defstruct (ebb-screen (:constructor ebb-screen--make) (:copier nil))
  "The complete terminal screen state."
  ;; Display
  (lines nil) (width 80) (height 24)
  (line-start 0)       ; physical index of logical display row 0
  ;; Cursor
  (cursor-x 0) (cursor-y 0)
  (cursor-saved-x 0) (cursor-saved-y 0) (cursor-saved-attr nil)
  (cursor-style :block) (cursor-visible t) (cursor-blink nil)
  ;; Attributes
  (current-attr nil)    ; ebb-attr for next char written
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
  (alt-screen nil)      ; ebb-alt-save when in alt mode, nil when main
  ;; Scrollback (main only)
  (scrollback nil)      ; list of ebb-history-line, newest first
  (scrollback-length 0) ; cached logical-line count
  (scrollback-max 10000) ; max lines
  (scrollback-trim-batch 256) ; amortize list trimming after this overflow
  (scrollback-dirty nil) ; non-nil when renderer must reconcile history
  (history-next-id 0)
  (history-generation 0)
  (history-logical-p nil)
  (history-row-ends nil)
  (history-lines-vector nil)
  (history-map-count 0)
  (history-row-map-width nil)
  (history-row-map-generation nil)
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

(defvar ebb--viewport-reset-screens
  (make-hash-table :test #'eq :weakness 'key)
  "Screens whose next render should show the live viewport from its top.")

(defvar ebb--saved-cursor-renditions
  (make-hash-table :test #'eq :weakness 'key)
  "Extended DECSC state keyed by screen without changing the screen layout.")

(defvar ebb--alt-saved-cursor-renditions
  (make-hash-table :test #'eq :weakness 'key)
  "Main-screen extended DECSC state saved while an alternate screen is active.")

(defvar ebb--line-renditions
  (make-hash-table :test #'eq :weakness 'key)
  "DEC width/height rendition keyed by screen line.")

(defvar ebb--column-mode-screens
  (make-hash-table :test #'eq :weakness 'key)
  "Reload-safe DECCOLM state keyed by terminal screen.")

(defvar ebb--horizontal-margins
  (make-hash-table :test #'eq :weakness 'key)
  "Enabled left/right margins keyed by terminal screen.")

(defvar ebb--reverse-wrap-modes
  (make-hash-table :test #'eq :weakness 'key)
  "Enabled reverse-wrap modes keyed by screen as (INLINE . EXTENDED).")

(defun ebb--reverse-wrap-mode (screen)
  "Return SCREEN's effective reverse-wrap mode."
  (when-let* ((state (gethash screen ebb--reverse-wrap-modes)))
    (cond ((cdr state) 'extended)
          ((car state) 'inline))))

(defun ebb--set-reverse-wrap-mode (screen extended enabled)
  "Set SCREEN's inline or EXTENDED reverse-wrap mode to ENABLED."
  (let ((state (or (gethash screen ebb--reverse-wrap-modes)
                   (cons nil nil))))
    (if extended
        (setcdr state (and enabled t))
      (setcar state (and enabled t)))
    (if (or (car state) (cdr state))
        (puthash screen state ebb--reverse-wrap-modes)
      (remhash screen ebb--reverse-wrap-modes))))

(defvar ebb--reverse-wrap-barriers
  (make-hash-table :test #'eq :weakness 'key)
  "Lines entered by explicit NEL, which mode 45 must not cross.")

(defvar ebb--initialized-cells
  (make-hash-table :test #'eq :weakness 'key)
  "Initialization bits used to distinguish empty cells from erased blanks.")

(defvar ebb--dec-protected-cells
  (make-hash-table :test #'eq :weakness 'key)
  "DEC-protected cell bits keyed by terminal line.")

(defvar ebb--iso-protected-cells
  (make-hash-table :test #'eq :weakness 'key)
  "ISO-protected cell bits keyed by terminal line.")

(defvar ebb--dec-protection-mode-screens
  (make-hash-table :test #'eq :weakness 'key)
  "Screens currently writing DEC-protected cells.")

(defvar ebb--iso-protection-mode-screens
  (make-hash-table :test #'eq :weakness 'key)
  "Screens currently writing ISO-protected cells.")

(defsubst ebb--clamp (val lo hi)
  "Clamp VAL to [LO, HI]."
  (min (max val lo) hi))

(defun ebb--line-protection-bits (table line width)
  "Return LINE's WIDTH protection bit vector from TABLE."
  (let ((bits (gethash line table)))
    (unless (and bits (= (length bits) width))
      (setq bits (make-bool-vector width nil))
      (puthash line bits table))
    bits))

(defun ebb--mark-written-protection (screen line start end)
  "Record protection modes for cells in LINE from START through END."
  (let* ((width (ebb-screen-width screen))
         (dec (and (gethash screen ebb--dec-protection-mode-screens) t))
         (iso (and (gethash screen ebb--iso-protection-mode-screens) t))
         (dec-bits (gethash line ebb--dec-protected-cells))
         (iso-bits (gethash line ebb--iso-protected-cells)))
    (when (or dec dec-bits)
      (setq dec-bits (ebb--line-protection-bits
                      ebb--dec-protected-cells line width))
      (cl-loop for column from start below (min end width)
               do (aset dec-bits column dec)))
    (when (or iso iso-bits)
      (setq iso-bits (ebb--line-protection-bits
                      ebb--iso-protected-cells line width))
      (cl-loop for column from start below (min end width)
               do (aset iso-bits column iso)))))

(defun ebb-screen-dec-protection-enabled-p (screen)
  "Return non-nil when SCREEN writes DEC-protected cells."
  (and (gethash screen ebb--dec-protection-mode-screens) t))

(defun ebb-screen-set-dec-protection (screen enabled)
  "Set whether subsequent writes on SCREEN are DEC-protected."
  (if enabled
      (puthash screen t ebb--dec-protection-mode-screens)
    (remhash screen ebb--dec-protection-mode-screens)))

(defun ebb-screen-set-iso-protection (screen enabled)
  "Set whether subsequent writes on SCREEN are ISO-protected."
  (if enabled
      (puthash screen t ebb--iso-protection-mode-screens)
    (remhash screen ebb--iso-protection-mode-screens)))

(defun ebb--line-initialized-cells (line cells width)
  "Return LINE's WIDTH initialization bits, refreshing them from CELLS."
  (let ((bits (gethash line ebb--initialized-cells)))
    (unless (and bits (= (length bits) width))
      (setq bits (make-bool-vector width nil))
      (puthash line bits ebb--initialized-cells))
    (dotimes (column width)
      (unless (= (ebb-cell-char (aref cells column)) ?\s)
        (aset bits column t)))
    bits))

(defun ebb-screen-horizontal-margins-enabled-p (screen)
  "Return non-nil when SCREEN uses left/right margins (DECLRMM)."
  (and (gethash screen ebb--horizontal-margins) t))

(defun ebb-screen-left-margin (screen)
  "Return SCREEN's zero-based left margin."
  (if-let* ((margins (gethash screen ebb--horizontal-margins)))
      (car margins)
    0))

(defun ebb-screen-right-margin (screen &optional row)
  "Return SCREEN's zero-based right margin at ROW."
  (let ((line-right (1- (ebb-screen-line-width screen row))))
    (if-let* ((margins (gethash screen ebb--horizontal-margins)))
        (min line-right (cdr margins))
      line-right)))

(defun ebb-screen-set-horizontal-margin-mode (screen enabled)
  "Enable or disable left/right margin mode on SCREEN."
  (if enabled
      (puthash screen (cons 0 (1- (ebb-screen-width screen)))
               ebb--horizontal-margins)
    (remhash screen ebb--horizontal-margins)))

(defun ebb-screen-set-horizontal-margins (screen left right)
  "Set SCREEN's inclusive zero-based LEFT and RIGHT margins."
  (when (ebb-screen-horizontal-margins-enabled-p screen)
    (let ((max-column (1- (ebb-screen-width screen))))
      (setq left (ebb--clamp left 0 max-column)
            right (ebb--clamp right 0 max-column))
      (when (< left right)
        (puthash screen (cons left right) ebb--horizontal-margins)
        (ebb-screen-cursor-goto screen 0 0)))))

(defun ebb-line-rendition (line)
  "Return LINE's DEC rendition, or `normal'."
  (or (gethash line ebb--line-renditions) 'normal))

(defun ebb-screen-line-width (screen &optional row)
  "Return the logical column count for SCREEN at ROW.
ROW defaults to the cursor row.  DEC double-width and double-height lines use
half of the physical screen columns."
  (let* ((row (or row (ebb-screen-cursor-y screen)))
         (line (ebb--line-at screen row)))
    (if (eq (ebb-line-rendition line) 'normal)
        (ebb-screen-width screen)
      (max 1 (/ (ebb-screen-width screen) 2)))))

(defun ebb-screen-set-line-rendition (screen rendition)
  "Set the current line's DEC RENDITION."
  (let* ((row (ebb-screen-cursor-y screen))
         (line (ebb--line-at screen row)))
    (if (eq rendition 'normal)
        (remhash line ebb--line-renditions)
      (puthash line rendition ebb--line-renditions))
    (setf (ebb-screen-cursor-x screen)
          (min (ebb-screen-cursor-x screen)
               (1- (ebb-screen-line-width screen row)))
          (ebb-screen-pending-wrap screen) nil
          (ebb-line-rendered line) nil
          (ebb-line-dirty line) t)
    (ebb--mark-dirty screen row)))

(defun ebb-screen-mark-viewport-reset (screen)
  "Request that SCREEN's next render reset its visible viewport."
  (puthash screen t ebb--viewport-reset-screens))

(defun ebb-screen-take-viewport-reset (screen)
  "Return and clear SCREEN's pending visible viewport reset."
  (prog1 (gethash screen ebb--viewport-reset-screens)
    (remhash screen ebb--viewport-reset-screens)))

;;;; ---- DEC Special Graphics Character Set ------------------------------

(defconst ebb--dec-graphics-map
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

(defun ebb--make-empty-cells (width)
  "Return a vector of WIDTH default cells."
  (let ((v (make-vector width nil)))
    (dotimes (i width)
      (aset v i (make-ebb-cell)))
    v))

(defun ebb--make-empty-line (width)
  "Return a new empty ebb-line of WIDTH columns.
Cells are materialized lazily from the plain text cache; this keeps scrolling
from allocating a full vector of cell structs for every blank bottom row."
  (make-ebb-line :cells nil
                   :cells-valid nil
                   :text (make-string width ?\s)
                   :dirty t))

(defsubst ebb--line-index (screen row)
  "Return physical line-vector index for logical ROW on SCREEN."
  (% (+ (ebb-screen-line-start screen) row)
     (ebb-screen-height screen)))

(defsubst ebb--line-at (screen row)
  "Return logical ROW from SCREEN."
  (aref (ebb-screen-lines screen) (ebb--line-index screen row)))

(defun ebb--line-ensure-cells (line width)
  "Ensure LINE has WIDTH cells, materializing its text cache when needed."
  (let ((cells (ebb-line-cells line)))
    (unless (and (ebb-line-cells-valid line)
                 cells
                 (= (length cells) width))
      (if (ebb-line-cells-valid line)
          ;; A resize can leave a saved/scrollback row at its old width.
          ;; Keep its already-materialized contents rather than blanking it.
          (let ((resized (ebb--make-empty-cells width)))
            (when cells
              (cl-replace resized cells :end2 (min width (length cells))))
            (setq cells resized)
            (setf (ebb-line-cells line) cells))
        (let ((text (or (ebb-line-text line) (make-string width ?\s))))
          (unless (and cells (= (length cells) width))
            (setq cells (ebb--make-empty-cells width))
            (setf (ebb-line-cells line) cells))
          (let ((i 0))
            (while (< i width)
              (let ((cell (aref cells i)))
                (setf (ebb-cell-char cell)
                      (if (< i (length text)) (aref text i) ?\s))
                (setf (ebb-cell-width cell) 1)
                (setf (ebb-cell-attr cell) (ebb-line-uniform-attr line)))
              (cl-incf i)))
          (dolist (run (ebb-line-attr-runs line))
            (let ((i (nth 0 run))
                  (end (min width (nth 1 run)))
                  (attr (nth 2 run)))
              (while (< i end)
                (setf (ebb-cell-attr (aref cells i)) attr)
                (cl-incf i))))))
      (setf (ebb-line-cells-valid line) t))
    cells))

(defun ebb--line-set-attr-run (line start end attr)
  "Set LINE's attribute over [START, END) to ATTR in its text run cache."
  (when (< start end)
    (let ((runs (ebb-line-attr-runs line))
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
      (setf (ebb-line-attr-runs line)
            (ebb--merge-attr-runs (nreverse out))))))

(defun ebb--merge-attr-runs (runs)
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

(defsubst ebb--set-line-at (screen row line)
  "Set logical ROW on SCREEN to LINE."
  (aset (ebb-screen-lines screen) (ebb--line-index screen row) line))

(defun ebb--ordered-lines-vector (screen)
  "Return SCREEN's display lines in logical row order."
  (let* ((height (ebb-screen-height screen))
         (v (make-vector height nil)))
    (dotimes (i height)
      (aset v i (ebb--line-at screen i)))
    v))

(defun ebb--default-tab-stops (width)
  "Return default tab stop list (every 8 columns) for WIDTH."
  (cl-loop for i from 8 below width by 8 collect i))

(defsubst ebb--attr-non-default-p (attr)
  "Return non-nil if ATTR differs from the all-defaults attribute."
  (and attr
       (or (ebb-attr-fg attr) (ebb-attr-bg attr)
           (ebb-attr-ul-color attr)
           (ebb-attr-bold attr) (ebb-attr-faint attr)
           (ebb-attr-italic attr) (ebb-attr-underline attr)
           (ebb-attr-blink attr) (ebb-attr-inverse attr)
           (ebb-attr-conceal attr) (ebb-attr-crossed attr)
           (/= 0 (ebb-attr-font attr))
           (ebb-attr-hyperlink attr))))

(defsubst ebb--cell-attr-for-write (screen)
  "Return attr to store on a new cell, or nil for default.
Current attributes are copy-on-write, so non-default cells may share this
immutable attr object instead of allocating a copy per cell."
  (let ((a (ebb-screen-current-attr screen)))
    (and (ebb--attr-non-default-p a) a)))

(defun ebb--make-erase-cell (screen)
  "Return a cell for erased positions (space with current bg via BCE)."
  (let ((bg (and (ebb-screen-current-attr screen)
                 (ebb-attr-bg (ebb-screen-current-attr screen)))))
    (if bg
        (make-ebb-cell :char ?\s :width 1
                         :attr (make-ebb-attr :bg bg))
      (make-ebb-cell))))

(defsubst ebb--normalize-display-char (char)
  "Return CHAR when it is Unicode, or a replacement for Emacs raw bytes."
  (if (> char #x10ffff) #xfffd char))

(defsubst ebb--char-display-width (char)
  "Return the display width of CHAR (1 for normal, 2 for CJK, 0 for zero-width)."
  (char-width char))

(defun ebb--clear-wide-char-at (screen row col)
  "If COL in ROW is part of a wide character, replace it and its pair with spaces.
This handles both the case where COL is the start of a wide char (width > 1)
and where COL is a continuation cell (width = 0)."
  (when (and (>= col 0) (< col (ebb-screen-width screen)))
    (let* ((line (ebb--line-at screen row))
           (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
           (cell (aref cells col))
           (w (ebb-cell-width cell)))
      (unless (= w 1)
        (setf (ebb-line-text line) nil)
        (setf (ebb-line-uniform-attr line) nil))
      (cond
       ;; This is a wide char start: clear it and its continuation cells
       ((> w 1)
        (aset cells col (make-ebb-cell))
        (cl-loop for i from 1 below w
                 when (< (+ col i) (ebb-screen-width screen))
                 do (aset cells (+ col i) (make-ebb-cell))))
       ;; This is a continuation cell (width=0): find the start and clear all
       ((zerop w)
        ;; Scan backwards to find the wide char that owns this cell
        (let ((start col))
          (while (and (> start 0)
                      (zerop (ebb-cell-width (aref cells start))))
            (cl-decf start))
          (let ((sw (ebb-cell-width (aref cells start))))
            (when (> sw 1)
              (aset cells start (make-ebb-cell))
              (cl-loop for i from 1 below sw
                       when (< (+ start i) (ebb-screen-width screen))
                       do (aset cells (+ start i) (make-ebb-cell)))))))))))

(defsubst ebb--mark-dirty (screen row)
  "Mark ROW as dirty on SCREEN."
  (let ((map (ebb-screen-dirty-map screen)))
    (unless (aref map row)
      (aset map row t)
      (push row (ebb-screen-dirty-lines screen))
      (cl-incf (ebb-screen-dirty-count screen)))))

(defun ebb--mark-region-dirty (screen top bot)
  "Mark logical rows TOP through BOT dirty."
  (unless (= (ebb-screen-dirty-count screen) (ebb-screen-height screen))
    (let ((i top))
      (while (<= i bot)
        (ebb--mark-dirty screen i)
        (cl-incf i)))))

(defsubst ebb--translate-charset (screen char)
  "Translate CHAR through the active character set on SCREEN."
  (let* ((slot (ebb-screen-charset-active screen))
         (cs (pcase slot
               ('g0 (ebb-screen-charset-g0 screen))
               ('g1 (ebb-screen-charset-g1 screen))
               ('g2 (ebb-screen-charset-g2 screen))
               ('g3 (ebb-screen-charset-g3 screen))
               (_   'us-ascii))))
    (if (and (eq cs 'dec-graphics)
             (< char 128))
        (or (aref ebb--dec-graphics-map char) char)
      char)))

;;;; ---- Constructor ----------------------------------------------------

(defun ebb-screen-create (width height)
  "Create a new screen of WIDTH columns and HEIGHT rows."
  (let ((lines (make-vector height nil)))
    (dotimes (i height)
      (aset lines i (ebb--make-empty-line width)))
    (ebb-screen--make
     :lines lines
     :width width
     :height height
     :scroll-bottom (1- height)
     :current-attr (make-ebb-attr)
     :tab-stops (ebb--default-tab-stops width)
     :dirty-lines (number-sequence 0 (1- height))
     :dirty-map (make-vector height t)
     :dirty-count height)))

;;;; ---- Dirty Tracking -------------------------------------------------

(defun ebb-screen-get-dirty (screen)
  "Return sorted list of dirty row indices."
  (sort (copy-sequence (ebb-screen-dirty-lines screen)) #'<))

(defun ebb-screen-clear-dirty (screen)
  "Clear the dirty line list."
  (dolist (row (ebb-screen-dirty-lines screen))
    (aset (ebb-screen-dirty-map screen) row nil))
  (setf (ebb-screen-dirty-lines screen) nil)
  (setf (ebb-screen-dirty-count screen) 0))

(defun ebb-screen-clear-scrollback-dirty (screen)
  "Clear SCREEN's scrollback reconciliation flags."
  (setf (ebb-screen-scrollback-dirty screen) nil))

(defun ebb--history-map-current-p (screen)
  "Return non-nil when SCREEN's history map matches its current model."
  (and (ebb-screen-history-row-ends screen)
       (= (ebb-screen-width screen)
          (ebb-screen-history-row-map-width screen))
       (= (ebb-screen-history-generation screen)
          (ebb-screen-history-row-map-generation screen))))

(defun ebb--history-grow-map (screen)
  "Ensure SCREEN's history vectors have room for one more entry."
  (let* ((count (ebb-screen-history-map-count screen))
         (old-lines (ebb-screen-history-lines-vector screen))
         (capacity (length old-lines)))
    (when (= count capacity)
      (let ((new-capacity (max 16 (* 2 (max 1 capacity)))))
        (setf (ebb-screen-history-lines-vector screen)
              (vconcat old-lines (make-vector (- new-capacity capacity) nil))
              (ebb-screen-history-row-ends screen)
              (vconcat (ebb-screen-history-row-ends screen)
                       (make-vector (- new-capacity capacity) 0)))))))

(defun ebb--history-entry-line-count (entry)
  "Return the number of logical lines represented by history ENTRY."
  (if (ebb-history-chunk-p entry)
      (- (length (ebb-history-chunk-lengths entry))
         (ebb-history-chunk-first entry))
    1))

(defun ebb--history-chunk-line-length (chunk index)
  "Return CHUNK logical line INDEX's unpadded length."
  (aref (ebb-history-chunk-lengths chunk) index))

(defun ebb--history-chunk-line-text (chunk index)
  "Materialize CHUNK logical line INDEX as plain text."
  (let* ((starts (ebb-history-chunk-starts chunk))
         (start (if starts (aref starts index)
                  (* index (ebb-history-chunk-source-width chunk))))
         (length (ebb--history-chunk-line-length chunk index)))
    (substring (ebb-history-chunk-text chunk) start (+ start length))))

(defun ebb--history-chunk-compact (chunk)
  "Discard CHUNK storage before its retained FIRST logical line."
  (let ((first (ebb-history-chunk-first chunk)))
    (when (> first 0)
      (let* ((lengths (cl-subseq (ebb-history-chunk-lengths chunk) first))
             (count (length lengths))
             (starts (ebb-history-chunk-starts chunk)))
        (if starts
            (let* ((selected (cl-subseq starts first))
                   (base (aref selected 0))
                   (last (1- count))
                   (limit (+ (aref selected last) (aref lengths last)))
                   (text (substring (ebb-history-chunk-text chunk) base limit))
                   (i 0))
              (while (< i count)
                (aset selected i (- (aref selected i) base))
                (cl-incf i))
              (setf (ebb-history-chunk-text chunk) text
                    (ebb-history-chunk-starts chunk) selected))
          (let* ((width (ebb-history-chunk-source-width chunk))
                 (start (* first width)))
            (setf (ebb-history-chunk-text chunk)
                  (substring (ebb-history-chunk-text chunk) start))))
        (cl-incf (ebb-history-chunk-id-start chunk) first)
        (setf (ebb-history-chunk-lengths chunk) lengths
              (ebb-history-chunk-first chunk) 0
              (ebb-history-chunk-row-ends chunk) nil
              (ebb-history-chunk-row-map-width chunk) nil)))))

(defun ebb--history-entry-row-count (entry width)
  "Return physical WIDTH-column rows represented by history ENTRY."
  (if (ebb-history-chunk-p entry)
      (let* ((map (ebb--history-chunk-row-map entry width))
             (count (length map)))
        (if (zerop count) 0 (aref map (1- count))))
    (ebb-history-line-row-count entry width)))

(defun ebb--history-chunk-row-map (chunk width)
  "Return CHUNK's cumulative physical-row map at WIDTH."
  (unless (and (ebb-history-chunk-row-ends chunk)
               (= width (ebb-history-chunk-row-map-width chunk)))
    (let* ((first (ebb-history-chunk-first chunk))
           (count (length (ebb-history-chunk-lengths chunk)))
           (map (make-vector (- count first) 0))
           (rows 0)
           (i first))
      (while (< i count)
        (let ((length (ebb--history-chunk-line-length chunk i)))
          (cl-incf rows (max 1 (/ (+ length width -1) width))))
        (aset map (- i first) rows)
        (cl-incf i))
      (setf (ebb-history-chunk-row-ends chunk) map
            (ebb-history-chunk-row-map-width chunk) width)))
  (ebb-history-chunk-row-ends chunk))

(defun ebb--history-chunk-row-location (chunk row width)
  "Return (LINE . OFFSET) for physical ROW within CHUNK at WIDTH."
  (let* ((map (ebb--history-chunk-row-map chunk width))
         (count (length map))
         (lo 0)
         (hi count))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (> (aref map mid) row)
            (setq hi mid)
          (setq lo (1+ mid)))))
    (when (< lo count)
      (let* ((index (+ (ebb-history-chunk-first chunk) lo))
             (base (if (zerop lo) 0 (aref map (1- lo))))
             (length (ebb--history-chunk-line-length chunk index)))
        (cons (make-ebb-history-line
               :id (+ (ebb-history-chunk-id-start chunk) index)
               :text (ebb--history-chunk-line-text chunk index)
               :text-length length)
              (* (- row base) width))))))

(defun ebb--history-changed (screen &optional kind entry)
  "Update width-dependent history state for SCREEN.
KIND may be `append' for ENTRY or `last' for an extended newest line."
  (let ((incremental (ebb--history-map-current-p screen)))
    (cl-incf (ebb-screen-history-generation screen))
    (cond
     ((and incremental (eq kind 'append))
      (ebb--history-grow-map screen)
      (let* ((count (ebb-screen-history-map-count screen))
             (previous (if (zerop count) 0
                         (aref (ebb-screen-history-row-ends screen)
                               (1- count)))))
        (aset (ebb-screen-history-lines-vector screen) count entry)
        (aset (ebb-screen-history-row-ends screen) count
              (+ previous
                 (ebb--history-entry-row-count
                  entry (ebb-screen-width screen))))
        (setf (ebb-screen-history-map-count screen) (1+ count)
              (ebb-screen-history-row-map-generation screen)
              (ebb-screen-history-generation screen))))
     ((and incremental (eq kind 'last))
      (let* ((count (ebb-screen-history-map-count screen))
             (previous (if (<= count 1) 0
                         (aref (ebb-screen-history-row-ends screen)
                               (- count 2)))))
        (aset (ebb-screen-history-row-ends screen) (1- count)
              (+ previous
                 (ebb--history-entry-row-count
                  (aref (ebb-screen-history-lines-vector screen)
                        (1- count))
                  (ebb-screen-width screen))))
        (setf (ebb-screen-history-row-map-generation screen)
              (ebb-screen-history-generation screen))))
     (t
      ;; Once invalid, leave the map fields alone while bulk output continues.
      ;; Re-clearing five slots for every scrolled row is pure hot-path cost.
      (when (ebb-screen-history-row-ends screen)
        (setf (ebb-screen-history-row-ends screen) nil
              (ebb-screen-history-lines-vector screen) nil
              (ebb-screen-history-map-count screen) 0
              (ebb-screen-history-row-map-width screen) nil
              (ebb-screen-history-row-map-generation screen) nil))))))

(defun ebb--history-clear (screen)
  "Clear SCREEN's logical history."
  (setf (ebb-screen-scrollback screen) nil
        (ebb-screen-scrollback-length screen) 0
        (ebb-screen-scrollback-dirty screen) t)
  (ebb--history-changed screen))

(defun ebb--history-normalize (screen)
  "Convert legacy physical rows in SCREEN history to logical lines."
  (unless (ebb-screen-history-logical-p screen)
    (let ((physical (cl-some #'ebb-line-p
                             (ebb-screen-scrollback screen))))
      (setf (ebb-screen-history-logical-p screen) t)
      (when physical
        (let ((rows (reverse (ebb-screen-scrollback screen))))
          (setf (ebb-screen-scrollback screen) nil
                (ebb-screen-scrollback-length screen) 0)
          (dolist (row rows)
            (ebb--history-push-row screen row (ebb-screen-width screen)))
          (setf (ebb-screen-scrollback-dirty screen) t))))))

(defun ebb--history-plain-row-text (line width wrapped)
  "Return (TEXT . LENGTH) for a lazy plain LINE, or nil.
Preserve all columns when WRAPPED; otherwise LENGTH omits trailing blanks.
TEXT is transferred directly to history without copying."
  (when (and (ebb-line-text line)
             (= (length (ebb-line-text line)) width)
             (null (ebb-line-attr-runs line))
             (null (ebb-line-uniform-attr line)))
    (let* ((text (ebb-line-text line))
           (end (if wrapped width
                  (let ((i width))
                    (while (and (> i 0) (= (aref text (1- i)) ?\s))
                      (cl-decf i))
                    i))))
      (cons text end))))

(defun ebb--history-text-row-info (line width wrapped)
  "Return lazy single-width history metadata for LINE, or nil.
The result is (TEXT LENGTH ATTR-RUNS).  Styled trailing blanks remain logical
content; unstyled presentation padding is excluded."
  (when (and (ebb-line-text line)
             (= (length (ebb-line-text line)) width)
             (null (ebb-line-uniform-attr line)))
    (let* ((text (ebb-line-text line))
           (runs (ebb-line-attr-runs line))
           (end
            (if wrapped
                width
              (let ((i width))
                (while (and (> i 0) (= (aref text (1- i)) ?\s))
                  (cl-decf i))
                (dolist (run runs)
                  (setq i (max i (min width (nth 1 run)))))
                i))))
      (list text end
            (cl-loop for run in runs
                     for start = (nth 0 run)
                     for finish = (min end (nth 1 run))
                     when (< start finish)
                     collect (list start finish (nth 2 run)))))))

(defun ebb--shift-attr-runs (runs offset)
  "Return copies of RUNS shifted right by OFFSET columns."
  (mapcar (lambda (run)
            (list (+ offset (nth 0 run))
                  (+ offset (nth 1 run))
                  (nth 2 run)))
          runs))

(defun ebb--slice-attr-runs (runs start end)
  "Project RUNS intersecting [START, END) into slice-local columns."
  (cl-loop for run in runs
           for begin = (max start (nth 0 run))
           for finish = (min end (nth 1 run))
           when (< begin finish)
           collect (list (- begin start) (- finish start) (nth 2 run))))

(defun ebb--history-flush-batch ()
  "Flush dynamically collected plain rows as one history chunk."
  (when ebb--history-batch-rows
    (let* ((screen ebb--history-batch-screen)
           (rows (nreverse ebb--history-batch-rows))
           (count (length rows))
           (width (ebb-screen-width screen))
           (id-start (ebb-screen-history-next-id screen))
           (lengths (make-vector count 0))
           texts
           (i 0))
      (dolist (row rows)
        (push (car row) texts)
        (aset lengths i (cdr row))
        (cl-incf i))
      (let ((chunk (make-ebb-history-chunk
                    :id-start id-start
                    :text (apply #'concat (nreverse texts))
                    :source-width width
                    :lengths lengths)))
        (setf (ebb-screen-history-next-id screen) (+ id-start count))
        (push chunk (ebb-screen-scrollback screen))
        (cl-incf (ebb-screen-scrollback-length screen) count)
        (ebb--history-changed screen 'append chunk)))
    (setq ebb--history-batch-rows nil)))

(defun ebb--history-add-input-chunk (screen string starts lengths first count)
  "Append COUNT plain lines from STRING metadata to SCREEN history.
STARTS and LENGTHS describe all scanned lines; FIRST selects the first line
that actually scrolled off the display."
  (when (> count 0)
    (let* ((id-start (ebb-screen-history-next-id screen))
           (selected-starts (cl-subseq starts first (+ first count)))
           (selected-lengths (cl-subseq lengths first (+ first count)))
           (base (aref selected-starts 0))
           (last (1- count))
           (limit (+ (aref selected-starts last)
                     (aref selected-lengths last)))
           (owned-text (substring string base limit))
           (i 0)
           (chunk (make-ebb-history-chunk
                   :id-start id-start
                   :text owned-text
                   :starts selected-starts
                   :lengths selected-lengths)))
      (while (< i count)
        (aset selected-starts i (- (aref selected-starts i) base))
        (cl-incf i))
      (setf (ebb-screen-history-next-id screen) (+ id-start count))
      (push chunk (ebb-screen-scrollback screen))
      (cl-incf (ebb-screen-scrollback-length screen) count)
      (ebb--history-changed screen 'append chunk))))

(defun ebb--text-to-cells (text &optional attr-runs)
  "Return single-width cells for TEXT with ATTR-RUNS."
  (let* ((length (length text))
         (cells (make-vector length nil))
         (i 0))
    (while (< i length)
      (aset cells i (make-ebb-cell :char (aref text i)))
      (cl-incf i))
    (dolist (run attr-runs)
      (setq i (nth 0 run))
      (let ((end (min length (nth 1 run)))
            (attr (nth 2 run)))
        (while (< i end)
          (setf (ebb-cell-attr (aref cells i)) attr)
          (cl-incf i))))
    cells))

(defun ebb--history-line-ensure-cells (line)
  "Return LINE's cells, materializing its lazy text and attributes once."
  (if-let* ((text (ebb-history-line-text line)))
      (let ((cells
             (ebb--text-to-cells
              (substring text 0 (ebb-history-line-text-length line))
              (ebb-history-line-attr-runs line))))
        (setf (ebb-history-line-cells line) cells
              (ebb-history-line-text line) nil
              (ebb-history-line-text-length line) 0
              (ebb-history-line-attr-runs line) nil)
        cells)
    (ebb-history-line-cells line)))

(defsubst ebb--history-line-length (line)
  "Return LINE's logical cell length without forcing lazy text."
  (if (ebb-history-line-text line)
      (ebb-history-line-text-length line)
    (length (ebb-history-line-cells line))))

(defsubst ebb--history-line-width (line width)
  "Return LINE's logical row width at physical WIDTH."
  (if (eq (ebb-history-line-rendition line) 'normal)
      width
    (max 1 (/ width 2))))

(defsubst ebb--history-line-wrap-end (line offset width)
  "Return LINE's wrap end after OFFSET at physical WIDTH."
  (let ((logical-width (ebb--history-line-width line width)))
    (if (ebb-history-line-text line)
        (min (ebb-history-line-text-length line) (+ offset logical-width))
      (ebb--wrap-end (ebb-history-line-cells line) offset logical-width))))

(defun ebb--history-push-row (screen line width)
  "Append physical LINE of WIDTH columns to SCREEN's logical history."
  (setf (ebb-screen-history-logical-p screen) t)
  (let* ((wrapped (ebb-line-wrapped line))
         (rendition (ebb-line-rendition line))
         (normal (eq rendition 'normal))
         (logical-width (if normal width (max 1 (/ width 2))))
         (plain-info (and normal
                          (ebb--history-plain-row-text line width wrapped)))
         (text-info (and normal
                         (ebb--history-text-row-info line width wrapped)))
         (text (nth 0 text-info))
         (text-length (or (nth 1 text-info) 0))
         (attr-runs (nth 2 text-info))
         (cells (unless text-info
                  (let ((row-cells
                         (cl-subseq (ebb--line-ensure-cells line width)
                                    0 logical-width)))
                    (if wrapped
                        (vconcat row-cells)
                      (ebb--trim-trailing-blank-cells row-cells)))))
         (history (ebb-screen-scrollback screen))
         (current (car history)))
    (if (and (eq screen ebb--history-batch-screen)
             plain-info
             (not wrapped)
             (not (and current
                       (ebb-history-line-p current)
                       (ebb-history-line-open current)))
             (null (ebb-line-prompt-begins line))
             (null (ebb-line-prompt-ends line)))
        (push plain-info ebb--history-batch-rows)
      (when (eq screen ebb--history-batch-screen)
        (ebb--history-flush-batch)
        (setq history (ebb-screen-scrollback screen)
              current (car history)))
      (if (and current (ebb-history-line-p current)
               (ebb-history-line-open current)
               (eq rendition (ebb-history-line-rendition current)))
          (let ((offset (ebb--history-line-length current)))
            (if (and text-info (ebb-history-line-text current))
                (let ((current-length
                       (ebb-history-line-text-length current)))
                  (setf (ebb-history-line-text current)
                        (concat (substring (ebb-history-line-text current)
                                           0 current-length)
                                (substring text 0 text-length))
                        (ebb-history-line-text-length current)
                        (+ current-length text-length)
                        (ebb-history-line-attr-runs current)
                        (ebb--merge-attr-runs
                         (append
                          (ebb-history-line-attr-runs current)
                          (ebb--shift-attr-runs
                           attr-runs current-length)))))
              (setf (ebb-history-line-cells current)
                    (vconcat (ebb--history-line-ensure-cells current)
                             (or cells
                                 (ebb--text-to-cells
                                  (substring text 0 text-length)
                                  attr-runs)))))
            (setf (ebb-history-line-prompt-begins current)
                  (append (ebb-history-line-prompt-begins current)
                          (mapcar (lambda (column) (+ offset column))
                                  (ebb-line-prompt-begins line)))
                  (ebb-history-line-prompt-ends current)
                  (append (ebb-history-line-prompt-ends current)
                          (mapcar (lambda (column) (+ offset column))
                                  (ebb-line-prompt-ends line)))
                  (ebb-history-line-open current) wrapped)
            (cl-incf (ebb-history-line-generation current))
            ;; The newest logical line changed without increasing its count.
            (setf (ebb-screen-scrollback-dirty screen) t)
            (ebb--history-changed screen 'last))
        (let ((logical
               (make-ebb-history-line
                :id (prog1 (ebb-screen-history-next-id screen)
                      (cl-incf (ebb-screen-history-next-id screen)))
                :cells (if text-info [] (vconcat cells))
                :text text
                :text-length text-length
                :attr-runs attr-runs
                :rendition rendition
                :prompt-begins (copy-sequence (ebb-line-prompt-begins line))
                :prompt-ends (copy-sequence (ebb-line-prompt-ends line))
                :open wrapped)))
          (push logical (ebb-screen-scrollback screen))
          (cl-incf (ebb-screen-scrollback-length screen))
          (ebb--history-changed screen 'append logical))))))

(defun ebb-history-line-row-count (line width)
  "Return the number of WIDTH-column rows needed for logical history LINE."
  (let ((length (ebb--history-line-length line)))
    (if (zerop length) 1
      (let ((offset 0)
            (rows 0))
        (while (< offset length)
          (setq offset (ebb--history-line-wrap-end line offset width))
          (cl-incf rows))
        rows))))

(defun ebb-screen-history-row-map (screen)
  "Return SCREEN history's cumulative row map at its current width.
Each vector entry is the physical row immediately after one history entry,
ordered oldest first.  A plain chunk may represent many logical lines."
  (ebb--history-normalize screen)
  (let ((width (ebb-screen-width screen))
        (generation (ebb-screen-history-generation screen)))
    (unless (and (ebb-screen-history-row-ends screen)
                 (equal width (ebb-screen-history-row-map-width screen))
                 (equal generation
                        (ebb-screen-history-row-map-generation screen)))
      (let* ((ordered (vconcat (reverse (ebb-screen-scrollback screen))))
             (count (length ordered))
             (capacity (max 16 (* 2 count)))
             (lines (vconcat ordered (make-vector (- capacity count) nil)))
             (map (make-vector capacity 0))
             (rows 0)
             (i 0))
        (cl-loop for entry across ordered do
          (cl-incf rows (ebb--history-entry-row-count entry width))
          (aset map i rows)
          (cl-incf i))
        (setf (ebb-screen-history-row-ends screen) map
              (ebb-screen-history-lines-vector screen) lines
              (ebb-screen-history-map-count screen) count
              (ebb-screen-history-row-map-width screen) width
              (ebb-screen-history-row-map-generation screen) generation)))
    (ebb-screen-history-row-ends screen)))

(defun ebb-screen-history-row-count (screen)
  "Return SCREEN's physical history row count at the current width."
  (let ((map (ebb-screen-history-row-map screen))
        (count (ebb-screen-history-map-count screen)))
    (if (zerop count) 0 (aref map (1- count)))))

(defun ebb-screen-history-row-location (screen row)
  "Return (LINE . OFFSET) for zero-based physical history ROW in SCREEN."
  (let* ((map (ebb-screen-history-row-map screen))
         (count (ebb-screen-history-map-count screen)))
    (when (and (>= row 0)
               (< row (if (zerop count) 0 (aref map (1- count)))))
      (let ((lo 0)
            (hi count))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi) 2)))
            (if (> (aref map mid) row)
                (setq hi mid)
              (setq lo (1+ mid)))))
        (let* ((entry-index lo)
               (entry-start (if (zerop entry-index)
                               0
                             (aref map (1- entry-index))))
               (entry (aref (ebb-screen-history-lines-vector screen)
                            entry-index))
               (remaining (- row entry-start))
               (width (ebb-screen-width screen)))
          (if (ebb-history-line-p entry)
              (let ((offset 0))
                (while (> remaining 0)
                  (setq offset (ebb--history-line-wrap-end entry offset width))
                  (cl-decf remaining))
                (cons entry offset))
            (ebb--history-chunk-row-location entry remaining width)))))))

(defun ebb-screen-history-anchor-location (screen id offset)
  "Return current (ROW . COLUMN) for logical history ID and cell OFFSET."
  (ebb-screen-history-row-map screen)
  (let ((lo 0)
        (hi (ebb-screen-history-map-count screen))
        found)
    (while (< lo hi)
      (let* ((mid (/ (+ lo hi) 2))
             (entry (aref (ebb-screen-history-lines-vector screen) mid))
             (first-id (if (ebb-history-chunk-p entry)
                           (+ (ebb-history-chunk-id-start entry)
                              (ebb-history-chunk-first entry))
                         (ebb-history-line-id entry)))
             (last-id (+ first-id (1- (ebb--history-entry-line-count entry)))))
        (cond ((> id last-id) (setq lo (1+ mid)))
              ((< id first-id) (setq hi mid))
              (t (setq found mid lo hi)))))
    (when found
      (let* ((entry (aref (ebb-screen-history-lines-vector screen) found))
             (base (if (zerop found) 0
                     (aref (ebb-screen-history-row-ends screen)
                           (1- found)))))
        (if (ebb-history-line-p entry)
            (let ((position (ebb-history-line-offset-position
                             entry (ebb-screen-width screen) offset)))
              (cons (+ base (car position)) (cdr position)))
          (let* ((index (- id (ebb-history-chunk-id-start entry)))
                 (width (ebb-screen-width screen))
                 (map (ebb--history-chunk-row-map entry width))
                 (local (- index (ebb-history-chunk-first entry)))
                 (length (ebb--history-chunk-line-length entry index))
                 (position
                  (if (and (> offset 0)
                           (= offset length)
                           (zerop (% offset width)))
                      (cons (1- (/ offset width)) width)
                    (cons (/ offset width) (% offset width)))))
            (when (> local 0)
              (cl-incf base (aref map (1- local))))
            (cons (+ base (car position)) (cdr position))))))))

(defun ebb-screen-history-render-row (screen row)
  "Return physical history ROW from SCREEN as a `ebb-line'."
  (when-let* ((location (ebb-screen-history-row-location screen row))
              (logical (car location)))
    (let* ((width (ebb-screen-width screen))
           (length (ebb--history-line-length logical))
           (offset (cdr location))
           (end (if (< offset length)
                    (ebb--history-line-wrap-end logical offset width)
                  offset))
           (last (>= end length))
           (common
            (list
             :prompt-begins
             (cl-loop for column in (ebb-history-line-prompt-begins logical)
                      when (and (>= column offset) (< column end))
                      collect (- column offset))
             :prompt-ends
             (cl-loop for column in (ebb-history-line-prompt-ends logical)
                      when (and (> column offset) (<= column end))
                      collect (- column offset))
             :wrapped (not last)
             :dirty t))
           (line
            (if-let* ((plain (ebb-history-line-text logical)))
                (apply #'make-ebb-line
                       :text (concat (substring plain offset end)
                                     (make-string (- width (- end offset)) ?\s))
                       :cells-valid nil
                       :attr-runs
                       (ebb--slice-attr-runs
                        (ebb-history-line-attr-runs logical) offset end)
                       common)
              (let* ((cells (ebb-history-line-cells logical))
                     (row-cells (if (= offset end) []
                                  (cl-subseq cells offset end))))
                (apply #'make-ebb-line
                       :cells (ebb--fit-cells-to-width row-cells width)
                       common)))))
      (unless (eq (ebb-history-line-rendition logical) 'normal)
        (puthash line (ebb-history-line-rendition logical)
                 ebb--line-renditions))
      line)))

(defun ebb-history-line-offset-position (line width offset)
  "Return LINE's physical (ROW . COLUMN) for cell OFFSET at WIDTH."
  (let ((width (ebb--history-line-width line width))
        (length (ebb--history-line-length line))
        (start 0)
        (row 0)
        end)
    (if (zerop length)
        '(0 . 0)
      (catch 'position
        (while (< start length)
          (setq end (ebb--history-line-wrap-end line start width))
          ;; A boundary offset belongs to the following row, except at the
          ;; logical end where the previous row's end is the only position.
          (when (or (< offset end)
                    (and (= end length) (= offset end)))
            (throw 'position (cons row (min width (- offset start)))))
          (setq start end)
          (cl-incf row))
        (cons row (min width (- length start)))))))

(defun ebb-history-line-end-offset-position (line width offset)
  "Return the physical position immediately after OFFSET cells in LINE."
  (if (zerop offset)
      '(0 . 0)
    (let ((position
           (ebb-history-line-offset-position line width (1- offset))))
      (cons (car position) (1+ (cdr position))))))

(defun ebb-screen-prompt-end-locations (screen)
  "Return ordered (ROW . COLUMN) prompt-end locations for SCREEN."
  (ebb--history-normalize screen)
  (let ((width (ebb-screen-width screen))
        (row-base 0)
        locations)
    (dolist (entry (reverse (ebb-screen-scrollback screen)))
      (when (ebb-history-line-p entry)
        (dolist (offset (sort (copy-sequence
                               (ebb-history-line-prompt-ends entry)) #'<))
          (let ((position
                 (ebb-history-line-end-offset-position entry width offset)))
            (push (cons (+ row-base (car position)) (cdr position)) locations))))
      (cl-incf row-base (ebb--history-entry-row-count entry width)))
    (dotimes (row (ebb-screen-height screen))
      (dolist (column (sort (copy-sequence
                             (ebb-line-prompt-ends
                              (ebb--line-at screen row))) #'<))
        (push (cons (+ row-base row) column) locations)))
    (nreverse locations)))

;;;; ---- Internal Scrolling ---------------------------------------------

(defun ebb--scroll-region-up (screen count)
  "Scroll the scroll region up by COUNT lines.
Top lines go to scrollback (if on main screen)."
  (let* ((top (ebb-screen-scroll-top screen))
         (bot (ebb-screen-scroll-bottom screen))
         (height (ebb-screen-height screen))
         (lines (ebb-screen-lines screen))
         (width (ebb-screen-width screen))
         (n (min count (1+ (- bot top)))))
    ;; Push lines scrolling off the terminal's top edge into scrollback.
    ;; Inner scroll regions do not leave the display and must not become
    ;; history lines.
    (when (and (not (ebb-screen-alt-screen screen))
               (zerop top))
      (dotimes (i n)
        (ebb--history-push-row
         screen (ebb--line-at screen (+ top i)) width)))
    (if (and (zerop top) (= bot (1- height)))
        ;; Full-screen scrolling is the dominant bulk-output path.  Advance a
        ;; logical top index instead of moving every row object in the vector.
        (let ((old-start (ebb-screen-line-start screen))
              (i 0))
          (setf (ebb-screen-line-start screen) (% (+ old-start n) height))
          ;; The physical slots that used to hold the top rows are now the
          ;; bottom logical rows; replace them with fresh blank rows.  Do not
          ;; reuse the scrolled-off line objects because scrollback references
          ;; them.
          (while (< i n)
            (aset lines (% (+ old-start i) height)
                  (ebb--make-empty-line width))
            (cl-incf i)))
      ;; Inner/partial regions are uncommon; keep logical row order correct via
      ;; accessors rather than physically normalizing the whole display.
      (let ((i top)
            (end (- bot n)))
        (while (<= i end)
          (ebb--set-line-at screen i (ebb--line-at screen (+ i n)))
          (cl-incf i)))
      (let ((i (1+ (- bot n))))
        (while (<= i bot)
          (ebb--set-line-at screen i (ebb--make-empty-line width))
          (cl-incf i))))
    ;; Mark all lines in region dirty.
    (ebb--mark-region-dirty screen top bot)
    ;; Trim scrollback
    (ebb--trim-scrollback screen)))

(defun ebb--scroll-region-down (screen count)
  "Scroll the scroll region down by COUNT lines.
Bottom lines are discarded."
  (let* ((top (ebb-screen-scroll-top screen))
         (bot (ebb-screen-scroll-bottom screen))
         (height (ebb-screen-height screen))
         (width (ebb-screen-width screen))
         (n (min count (1+ (- bot top)))))
    (if (and (zerop top) (= bot (1- height)))
        (let ((new-start (% (+ (ebb-screen-line-start screen) height (- n))
                            height))
              (i 0))
          (setf (ebb-screen-line-start screen) new-start)
          (while (< i n)
            (ebb--set-line-at screen i (ebb--make-empty-line width))
            (cl-incf i)))
      ;; Shift lines down.
      (let ((i bot)
            (end (+ top n)))
        (while (>= i end)
          (ebb--set-line-at screen i (ebb--line-at screen (- i n)))
          (cl-decf i)))
      ;; Fill top with empty lines.
      (let ((i top)
            (end (+ top n -1)))
        (while (<= i end)
          (ebb--set-line-at screen i (ebb--make-empty-line width))
          (cl-incf i))))
    ;; Mark dirty.
    (ebb--mark-region-dirty screen top bot)))

(defun ebb--trim-scrollback (screen)
  "Trim scrollback to max lines."
  (let ((max (ebb-screen-scrollback-max screen))
        (len (ebb-screen-scrollback-length screen))
        (batch (ebb-screen-scrollback-trim-batch screen)))
    (when (> len (+ max batch))
      ;; Keep the newest MAX logical lines (the list is newest first).
      ;; A boundary chunk retains only its newest suffix via FIRST.
      (if (zerop max)
          (setf (ebb-screen-scrollback screen) nil)
        (let ((remaining max)
              (entries (ebb-screen-scrollback screen))
              previous)
          (while (and entries (> remaining 0))
            (let* ((entry (car entries))
                   (count (ebb--history-entry-line-count entry)))
              (if (<= count remaining)
                  (progn
                    (cl-decf remaining count)
                    (setq previous entries
                          entries (cdr entries)))
                (when (ebb-history-chunk-p entry)
                  (setf (ebb-history-chunk-first entry)
                        (- (length (ebb-history-chunk-lengths entry)) remaining)
                        (ebb-history-chunk-row-ends entry) nil
                        (ebb-history-chunk-row-map-width entry) nil)
                  (ebb--history-chunk-compact entry))
                (setq remaining 0
                      previous entries
                      entries (cdr entries)))))
          (when previous
            (setcdr previous nil)))
        (setf (ebb-screen-scrollback-length screen) max)
        (ebb--history-changed screen)))))

;;;; ---- Character Writing ----------------------------------------------

(defun ebb--previous-cell (screen)
  "Return the cell immediately before SCREEN's cursor, or nil.
A pending wrap leaves the cursor on the final cell.  Across a line boundary,
only join with a line that was auto-wrapped."
  (let* ((y (ebb-screen-cursor-y screen))
         (x (ebb-screen-cursor-x screen))
         (width (ebb-screen-width screen))
         (line (ebb--line-at screen y)))
    (cond
     ((ebb-screen-pending-wrap screen)
      (ebb--line-ensure-cells line width)
      (cons line x))
     ((> x 0)
      (let* ((cells (ebb--line-ensure-cells line width))
             (column (1- x)))
        (while (and (> column 0)
                    (zerop (ebb-cell-width (aref cells column))))
          (cl-decf column))
        (cons line column)))
     ((and (> y 0) (ebb-line-wrapped (ebb--line-at screen (1- y))))
      (let* ((previous (ebb--line-at screen (1- y)))
             (cells (ebb--line-ensure-cells previous width))
             (column (1- width)))
        (while (and (> column 0)
                    (zerop (ebb-cell-width (aref cells column))))
          (cl-decf column))
        (cons previous column))))))

(defun ebb--append-to-previous-cell (screen char)
  "Append zero-width CHAR to the cell before SCREEN's cursor."
  (when-let* ((target (ebb--previous-cell screen))
              (cell (aref (ebb-line-cells (car target)) (cdr target))))
    (setf (ebb-cell-combining cell)
          (concat (ebb-cell-combining cell) (string char)))
    (setf (ebb-line-text (car target)) nil
          (ebb-line-attr-runs (car target)) nil
          (ebb-line-uniform-attr (car target)) nil
          (ebb-line-rendered (car target)) nil
          (ebb-line-dirty (car target)) t)
    (ebb--mark-dirty screen
                        (if (eq (car target)
                                (ebb--line-at screen
                                                (ebb-screen-cursor-y screen)))
                            (ebb-screen-cursor-y screen)
                          (1- (ebb-screen-cursor-y screen))))
    t))

(defun ebb--append-joined-char (screen char)
  "Append CHAR when it continues a preceding zero-width joiner sequence."
  (when-let* ((target (ebb--previous-cell screen))
              (cell (aref (ebb-line-cells (car target)) (cdr target)))
              (suffix (ebb-cell-combining cell)))
    (when (eq (aref suffix (1- (length suffix))) #x200d)
      (ebb--append-to-previous-cell screen char))))

(defun ebb--apply-pending-wrap (screen)
  "Apply SCREEN's pending automatic wrap before writing a character."
  (when (ebb-screen-pending-wrap screen)
    (setf (ebb-screen-pending-wrap screen) nil)
    (when (ebb-screen-auto-wrap screen)
      (setf (ebb-line-wrapped
             (ebb--line-at screen (ebb-screen-cursor-y screen)))
            t
            (ebb-screen-cursor-x screen) 0)
      (if (= (ebb-screen-cursor-y screen)
             (ebb-screen-scroll-bottom screen))
          (ebb--scroll-region-up screen 1)
        (cl-incf (ebb-screen-cursor-y screen)))
      (remhash (ebb--line-at screen (ebb-screen-cursor-y screen))
               ebb--reverse-wrap-barriers))))

(defun ebb--write-single-width-char
    (screen translated line cell attr column row screen-width)
  "Write TRANSLATED into an ordinary single-width CELL on SCREEN."
  (setf (ebb-cell-char cell) translated
        (ebb-cell-combining cell) nil
        (ebb-cell-width cell) 1
        (ebb-cell-attr cell) attr)
  (let ((uniform (ebb-line-uniform-attr line)))
    (if (and (ebb-line-text line)
             (< translated 128)
             (null (ebb-line-attr-runs line))
             (or (and (null attr) (null uniform))
                 (and attr uniform
                      (or (eq attr uniform) (equal attr uniform)))))
        (aset (ebb-line-text line) column translated)
      (setf (ebb-line-text line) nil
            (ebb-line-attr-runs line) nil
            (ebb-line-uniform-attr line) nil)))
  (ebb--mark-written-protection screen line column (1+ column))
  (setf (ebb-line-dirty line) t
        (ebb-screen-last-char screen) translated)
  (ebb--mark-dirty screen row)
  (let ((new-column (1+ column)))
    (if (>= new-column screen-width)
        (setf (ebb-screen-cursor-x screen) (1- screen-width)
              (ebb-screen-pending-wrap screen)
              (and (ebb-screen-auto-wrap screen) t))
      (setf (ebb-screen-cursor-x screen) new-column))))

(defun ebb--write-general-char
    (screen translated char-width column row line cells attr screen-width)
  "Write TRANSLATED with CHAR-WIDTH through SCREEN's general cell path."
  (setf (ebb-line-text line) nil
        (ebb-line-attr-runs line) nil
        (ebb-line-uniform-attr line) nil)
  ;; A double-width character cannot start in the final column.
  (when (and (> char-width 1) (>= column (1- screen-width)))
    (aset cells column (make-ebb-cell :char ?\s :width 1 :attr nil))
    (setf (ebb-line-dirty line) t)
    (ebb--mark-dirty screen row)
    (if (ebb-screen-auto-wrap screen)
        (progn
          (setf (ebb-line-wrapped line) t
                (ebb-screen-cursor-x screen) 0)
          (if (= row (ebb-screen-scroll-bottom screen))
              (ebb--scroll-region-up screen 1)
            (cl-incf (ebb-screen-cursor-y screen)))
          (setq column 0
                row (ebb-screen-cursor-y screen)
                line (ebb--line-at screen row)
                cells (ebb--line-ensure-cells line screen-width)))
      (setq column (1- screen-width))))
  ;; Clean up continuation cells only when the destination overlaps one.
  (unless (= (ebb-cell-width (aref cells column)) 1)
    (ebb--clear-wide-char-at screen row column))
  (when (and (> char-width 1)
             (< (1+ column) screen-width)
             (/= (ebb-cell-width (aref cells (1+ column))) 1))
    (ebb--clear-wide-char-at screen row (1+ column)))
  (when (ebb-screen-insert-mode screen)
    ;; Copy shifted cells: the destination must not alias the source cell that
    ;; is about to be overwritten at COLUMN.
    (cl-loop for i from (1- screen-width) above (+ column char-width -1)
             do (aset cells i (copy-ebb-cell
                               (aref cells (- i char-width))))))
  (let ((cell (aref cells column)))
    (setf (ebb-cell-char cell) translated
          (ebb-cell-combining cell) nil
          (ebb-cell-width cell) char-width
          (ebb-cell-attr cell) attr))
  (cl-loop for i from 1 below char-width
           while (< (+ column i) screen-width)
           for cell = (aref cells (+ column i))
           do (setf (ebb-cell-char cell) ?\s
                    (ebb-cell-combining cell) nil
                    (ebb-cell-width cell) 0
                    (ebb-cell-attr cell) attr))
  (ebb--mark-written-protection
   screen line column (+ column char-width))
  (setf (ebb-line-dirty line) t
        (ebb-screen-last-char screen) translated)
  (ebb--mark-dirty screen row)
  (let ((new-column (+ column char-width)))
    (if (>= new-column screen-width)
        (setf (ebb-screen-cursor-x screen) (1- screen-width)
              (ebb-screen-pending-wrap screen)
              (and (ebb-screen-auto-wrap screen) t))
      (setf (ebb-screen-cursor-x screen) new-column))))

(defun ebb-screen-write-char (screen char)
  "Write CHAR at the current cursor position.
Handles combining and double-width characters."
  (let* ((char (ebb--normalize-display-char char))
         (char-width (ebb--char-display-width char)))
    (catch 'ebb-screen-write-char
      (when (zerop char-width)
        (ebb--append-to-previous-cell screen char)
        (throw 'ebb-screen-write-char nil))
      (when (ebb--append-joined-char screen char)
        (throw 'ebb-screen-write-char nil))
      (ebb--apply-pending-wrap screen)
      (let* ((screen-width (ebb-screen-line-width screen))
             (column (ebb-screen-cursor-x screen))
             (row (ebb-screen-cursor-y screen))
             (translated (ebb--translate-charset screen char))
             (line (ebb--line-at screen row))
             (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
             (attr (ebb--cell-attr-for-write screen))
             (cell (aref cells column)))
        (if (and (= char-width 1)
                 (not (ebb-screen-insert-mode screen))
                 (= (ebb-cell-width cell) 1))
            (ebb--write-single-width-char
             screen translated line cell attr column row screen-width)
          (ebb--write-general-char
           screen translated char-width column row line cells attr
           screen-width))))))

(defun ebb-screen-write-string (screen string start end)
  "Write printable STRING bytes from START to END to SCREEN.
This is the hot path for ground-state text.  It writes contiguous ASCII
runs directly into existing cells and falls back to `ebb-screen-write-char'
for wide/non-ASCII/insert-mode cases."
  (let ((i start))
    (while (< i end)
      (if (or (ebb-screen-insert-mode screen)
              (not (eq (ebb-screen-charset-active screen) 'g0))
              (not (eq (ebb-screen-charset-g0 screen) 'us-ascii))
              (>= (aref string i) 128))
          (progn
            (ebb-screen-write-char screen (aref string i))
            (cl-incf i))
        ;; Honor pending wrap before writing the next byte.
        (when (ebb-screen-pending-wrap screen)
          (setf (ebb-screen-pending-wrap screen) nil)
          (when (ebb-screen-auto-wrap screen)
            (let ((line (ebb--line-at screen (ebb-screen-cursor-y screen))))
              (setf (ebb-line-wrapped line) t))
            (setf (ebb-screen-cursor-x screen) 0)
            (if (= (ebb-screen-cursor-y screen)
                   (ebb-screen-scroll-bottom screen))
                (ebb--scroll-region-up screen 1)
              (cl-incf (ebb-screen-cursor-y screen)))))
        (let* ((cx (ebb-screen-cursor-x screen))
               (cy (ebb-screen-cursor-y screen))
               (width (ebb-screen-line-width screen cy))
               (line (ebb--line-at screen cy))
               (attr-template (ebb-screen-current-attr screen))
               (default-attr (not (ebb--attr-non-default-p attr-template)))
               (line-text (ebb-line-text line))
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
            (when (ebb-line-attr-runs line)
              (ebb--line-set-attr-run line cx (+ cx (- limit start-i)) nil))
            (setq i limit)
            (setf (ebb-line-uniform-attr line) nil)
            (setf (ebb-line-cells-valid line) nil))
           ((and (not default-attr) line-text (= ascii-end limit))
            (store-substring line-text cx (substring string i limit))
            (ebb--line-set-attr-run line cx (+ cx (- limit start-i)) attr-template)
            (setq i limit)
            (setf (ebb-line-uniform-attr line) nil)
            (setf (ebb-line-cells-valid line) nil))
           (t
            (let ((cells (ebb--line-ensure-cells
                          line (ebb-screen-width screen))))
              (unless default-attr
                (setf (ebb-line-text line) nil
                      (ebb-line-attr-runs line) nil
                      (ebb-line-uniform-attr line) nil))
              ;; Stop before cells that need wide-char cleanup or non-ASCII chars.
              ;; The two loops differ only in the attribute stored on each cell.
              (let ((cell-attr (if default-attr nil attr-template)))
                (while (and (< i ascii-end)
                            (= (ebb-cell-width
                                (aref cells (+ cx (- i start-i))))
                               1))
                  (let* ((col (+ cx (- i start-i)))
                         (cell (aref cells col))
                         (ch (aref string i)))
                    (setf (ebb-cell-char cell) ch)
                    (setf (ebb-cell-combining cell) nil)
                    (setf (ebb-cell-width cell) 1)
                    (setf (ebb-cell-attr cell) cell-attr))
                  (cl-incf i))))))
          (if (= i start-i)
              ;; Could not use the fast row writer for this byte.
              (progn
                (ebb-screen-write-char screen (aref string i))
                (cl-incf i))
            (ebb--mark-written-protection
             screen line cx (+ cx (- i start-i)))
            (setf (ebb-line-rendered line) nil)
            (setf (ebb-line-dirty line) t)
            (ebb--mark-dirty screen cy)
            (setf (ebb-screen-last-char screen) (aref string (1- i)))
            (let ((new-cx (+ cx (- i start-i))))
              (if (>= new-cx width)
                  (progn
                    (setf (ebb-screen-cursor-x screen) (1- width))
                    (when (ebb-screen-auto-wrap screen)
                      (setf (ebb-screen-pending-wrap screen) t)))
                (setf (ebb-screen-cursor-x screen) new-cx)))))))))

(defun ebb--plain-blank-line-p (line width)
  "Return non-nil when LINE is an untouched plain blank row of WIDTH."
  (let ((text (ebb-line-text line))
        (i 0))
    (and text
         (= (length text) width)
         (not (ebb-line-wrapped line))
         (null (ebb-line-attr-runs line))
         (null (ebb-line-uniform-attr line))
         (null (ebb-line-prompt-begins line))
         (null (ebb-line-prompt-ends line))
         (progn
           (while (and (< i width) (= (aref text i) ?\s))
             (cl-incf i))
           (= i width)))))

(defun ebb--simple-crlf-block-p (screen lengths)
  "Return non-nil when SCREEN can apply plain lines LENGTHS in bulk."
  (let* ((width (ebb-screen-width screen))
         (height (ebb-screen-height screen))
         (row (ebb-screen-cursor-y screen))
         (count (length lengths))
         (i 0))
    (and (not (ebb-screen-alt-screen screen))
         (zerop (ebb-screen-scroll-top screen))
         (= (ebb-screen-scroll-bottom screen) (1- height))
         (zerop (ebb-screen-cursor-x screen))
         (not (ebb-screen-pending-wrap screen))
         (not (ebb-screen-insert-mode screen))
         (eq (ebb-screen-charset-active screen) 'g0)
         (eq (ebb-screen-charset-g0 screen) 'us-ascii)
         (not (ebb--attr-non-default-p (ebb-screen-current-attr screen)))
         (progn
           (while (and (< i count) (<= (aref lengths i) width))
             (cl-incf i))
           (= i count))
         (progn
           (setq i row)
           (while (and (< i height)
                       (ebb--plain-blank-line-p (ebb--line-at screen i) width))
             (cl-incf i))
           (= i height)))))

(defun ebb--apply-simple-crlf-block (screen string starts lengths)
  "Apply parsed plain CRLF lines to SCREEN without per-line scrolling."
  (let* ((width (ebb-screen-width screen))
         (height (ebb-screen-height screen))
         (bottom (1- height))
         (cursor-y (ebb-screen-cursor-y screen))
         (count (length lengths))
         (scrolls (max 0 (- count (- bottom cursor-y))))
         (existing-history (min scrolls cursor-y))
         (input-history (- scrolls existing-history))
         (history-lengths (copy-sequence lengths))
         (new-lines (make-vector height nil))
         (destination 0)
         (i existing-history))
    ;; At most HEIGHT existing rows need ordinary handling; all bulk input
    ;; rows that leave the viewport become one chunk below.
    (dotimes (row existing-history)
      (ebb--history-push-row screen (ebb--line-at screen row) width))
    ;; Closed logical history omits terminal padding just like the ordinary
    ;; row path.  Keep raw LENGTHS for viewport writes and REP semantics.
    (setq i 0)
    (while (< i input-history)
      (let ((length (aref history-lengths i))
            (start (aref starts i)))
        (while (and (> length 0)
                    (= (aref string (+ start length -1)) ?\s))
          (cl-decf length))
        (aset history-lengths i length))
      (cl-incf i))
    (ebb--history-add-input-chunk
     screen string starts history-lengths 0 input-history)
    ;; Existing prefix rows that survived scrolling retain their metadata.
    (setq i existing-history)
    (while (< i cursor-y)
      (aset new-lines destination (ebb--line-at screen i))
      (cl-incf destination)
      (cl-incf i))
    ;; Only the bounded viewport suffix is materialized as line objects.
    (setq i input-history)
    (while (< i count)
      (let* ((length (aref lengths i))
             (start (aref starts i))
             (text (concat (substring string start (+ start length))
                           (make-string (- width length) ?\s))))
        (aset new-lines destination
              (make-ebb-line :cells nil :cells-valid nil :text text :dirty t)))
      (cl-incf destination)
      (cl-incf i))
    (while (< destination height)
      (aset new-lines destination (ebb--make-empty-line width))
      (cl-incf destination))
    (setf (ebb-screen-lines screen) new-lines
          (ebb-screen-line-start screen) 0
          (ebb-screen-cursor-x screen) 0
          (ebb-screen-cursor-y screen) (min bottom (+ cursor-y count))
          (ebb-screen-pending-wrap screen) nil)
    (setq i (1- count))
    (while (and (>= i 0) (zerop (aref lengths i)))
      (cl-decf i))
    (when (>= i 0)
      (setf (ebb-screen-last-char screen)
            (aref string (+ (aref starts i) (1- (aref lengths i))))))
    (if (> scrolls 0)
        (ebb--mark-region-dirty screen 0 bottom)
      (ebb--mark-region-dirty
       screen cursor-y (min bottom (+ cursor-y count -1))))
    (ebb--trim-scrollback screen)))

(defun ebb-screen-write-crlf-block (screen string start end)
  "Try to write printable ASCII CRLF lines from STRING START through END.
Return the first unconsumed index, or nil unless at least two complete lines
are available.  Simple main-screen output is applied and stored in bulk."
  (let ((i start)
        starts
        lengths
        consumed
        done)
    (while (and (< i end) (not done))
      (let ((line-start i))
        (while (and (< i end)
                    (let ((char (aref string i)))
                      (and (>= char ?\s) (< char #x7f))))
          (cl-incf i))
        (if (and (< (1+ i) end)
                 (= (aref string i) ?\r)
                 (= (aref string (1+ i)) ?\n))
            (progn
              (push line-start starts)
              (push (- i line-start) lengths)
              (cl-incf i 2)
              (setq consumed i))
          (setq done t))))
    (when (cdr starts)
      (let ((starts-vector (vconcat (nreverse starts)))
            (lengths-vector (vconcat (nreverse lengths))))
        (if (ebb--simple-crlf-block-p screen lengths-vector)
            (ebb--apply-simple-crlf-block
             screen string starts-vector lengths-vector)
          (let ((line 0)
                (count (length lengths-vector))
                (ebb--history-batch-screen screen)
                (ebb--history-batch-rows nil))
            (unwind-protect
                (while (< line count)
                  (let ((line-start (aref starts-vector line)))
                    (ebb-screen-write-string
                     screen string line-start
                     (+ line-start (aref lengths-vector line))))
                  (ebb-screen-carriage-return screen)
                  (ebb-screen-index screen)
                  (cl-incf line))
              (ebb--history-flush-batch)
              (ebb--trim-scrollback screen))))
        consumed))))

;;;; ---- Cursor Movement ------------------------------------------------

(defun ebb-screen-cursor-backward (screen count)
  "Move SCREEN backward COUNT cells, honoring reverse-wrap mode."
  (let* ((reverse-mode (ebb--reverse-wrap-mode screen))
         (pending (ebb-screen-pending-wrap screen))
         (moves (if (and pending reverse-mode (> count 0))
                    (1- count) count)))
    (setf (ebb-screen-pending-wrap screen) nil)
    (dotimes (_ moves)
    (let* ((x (ebb-screen-cursor-x screen))
           (y (ebb-screen-cursor-y screen))
           (left (ebb-screen-left-margin screen))
           (right (ebb-screen-right-margin screen))
           (inside-horizontal
            (and (ebb-screen-horizontal-margins-enabled-p screen)
                 (<= left x right)))
           (minimum (if inside-horizontal left 0)))
      (cond
       ((> x minimum)
        (cl-decf (ebb-screen-cursor-x screen)))
       ((and reverse-mode
             (ebb-screen-auto-wrap screen)
             (not (gethash (ebb--line-at screen y)
                           ebb--reverse-wrap-barriers)))
        (let* ((top (ebb-screen-scroll-top screen))
               (bottom (ebb-screen-scroll-bottom screen))
               (inside-vertical (<= top y bottom))
               (target-row
                (cond
                 ((and inside-vertical (= y top)) bottom)
                 ((> y 0) (1- y))
                 ((eq reverse-mode 'extended)
                  (1- (ebb-screen-height screen)))
                 (t nil))))
          (when target-row
            (setf (ebb-screen-cursor-y screen) target-row
                  (ebb-screen-cursor-x screen)
                  (if (ebb-screen-horizontal-margins-enabled-p screen)
                      (ebb-screen-right-margin screen target-row)
                    (1- (ebb-screen-line-width screen target-row))))))))))))

(defun ebb-screen-cursor-move (screen direction count)
  "Move cursor in DIRECTION by COUNT.  DIRECTION: up, down, left, right."
  (if (eq direction 'left)
      (ebb-screen-cursor-backward screen count)
    (setf (ebb-screen-pending-wrap screen) nil)
    (pcase direction
    ('up
     (let* ((y (ebb-screen-cursor-y screen))
            (within (or (ebb-screen-origin-mode screen)
                        (<= (ebb-screen-scroll-top screen) y
                            (ebb-screen-scroll-bottom screen))))
            (min-y (if within (ebb-screen-scroll-top screen) 0)))
       (setf (ebb-screen-cursor-y screen) (max min-y (- y count)))))
    ('down
     (let* ((y (ebb-screen-cursor-y screen))
            (within (or (ebb-screen-origin-mode screen)
                        (<= (ebb-screen-scroll-top screen) y
                            (ebb-screen-scroll-bottom screen))))
            (max-y (if within
                       (ebb-screen-scroll-bottom screen)
                     (1- (ebb-screen-height screen)))))
       (setf (ebb-screen-cursor-y screen) (min max-y (+ y count)))))
      ('right
       (let* ((x (ebb-screen-cursor-x screen))
              (left (ebb-screen-left-margin screen))
              (right (ebb-screen-right-margin screen))
              (max-x (if (and (ebb-screen-horizontal-margins-enabled-p screen)
                              (<= left x right))
                         right (1- (ebb-screen-line-width screen)))))
         (setf (ebb-screen-cursor-x screen) (min max-x (+ x count))))))))

(defun ebb-screen-cursor-goto (screen row col)
  "Move cursor to ROW, COL (0-indexed, origin-mode aware)."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let* ((origin (ebb-screen-origin-mode screen))
         (min-y (if origin (ebb-screen-scroll-top screen) 0))
         (max-y (if origin
                    (ebb-screen-scroll-bottom screen)
                  (1- (ebb-screen-height screen))))
         (actual-row (+ min-y row))
         (target-row (ebb--clamp actual-row min-y max-y))
         (min-x (if (and origin
                         (ebb-screen-horizontal-margins-enabled-p screen))
                    (ebb-screen-left-margin screen) 0))
         (max-x (if (and origin
                         (ebb-screen-horizontal-margins-enabled-p screen))
                    (ebb-screen-right-margin screen target-row)
                  (1- (ebb-screen-line-width screen target-row)))))
    (setf (ebb-screen-cursor-y screen) target-row)
    (setf (ebb-screen-cursor-x screen)
          (ebb--clamp (+ min-x col) min-x max-x))))

(defun ebb-screen-cursor-next-line (screen count)
  "Move cursor to the active left edge COUNT lines down."
  (ebb-screen-carriage-return screen)
  (ebb-screen-cursor-move screen 'down count))

(defun ebb-screen-cursor-prev-line (screen count)
  "Move cursor to the active left edge COUNT lines up."
  (ebb-screen-carriage-return screen)
  (ebb-screen-cursor-move screen 'up count))

;;;; ---- Index / Reverse Index / CR / BS --------------------------------

(defun ebb-screen--inside-horizontal-margins-p (screen)
  "Return non-nil when SCREEN's cursor can scroll the active region."
  (or (not (ebb-screen-horizontal-margins-enabled-p screen))
      (<= (ebb-screen-left-margin screen)
          (ebb-screen-cursor-x screen)
          (ebb-screen-right-margin screen))))

(defun ebb-screen--index (screen horizontal-eligible)
  "Index SCREEN using HORIZONTAL-ELIGIBLE for region scrolling."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let ((y (ebb-screen-cursor-y screen)))
    (cond
     ((= y (ebb-screen-scroll-bottom screen))
      (when horizontal-eligible
        (ebb--scroll-region-up screen 1)))
     ((< y (1- (ebb-screen-height screen)))
      (cl-incf (ebb-screen-cursor-y screen)))))
  (setf (ebb-screen-cursor-x screen)
        (min (ebb-screen-cursor-x screen)
             (1- (ebb-screen-line-width screen)))))

(defun ebb-screen-index (screen)
  "Index: move cursor down, scrolling if at scroll bottom.
Handles LF, VT, FF."
  (ebb-screen--index screen (ebb-screen--inside-horizontal-margins-p screen)))

(defun ebb-screen-reverse-index (screen)
  "Reverse index: move cursor up, scrolling down if at scroll top."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let ((y (ebb-screen-cursor-y screen)))
    (cond
     ((= y (ebb-screen-scroll-top screen))
      (when (ebb-screen--inside-horizontal-margins-p screen)
        (ebb--scroll-region-down screen 1)))
     ((> y 0)
      (cl-decf (ebb-screen-cursor-y screen)))))
  (setf (ebb-screen-cursor-x screen)
        (min (ebb-screen-cursor-x screen)
             (1- (ebb-screen-line-width screen)))))

(defun ebb-screen-next-line (screen)
  "NEL: carriage return + index."
  (let ((horizontal-eligible
         (ebb-screen--inside-horizontal-margins-p screen)))
    (ebb-screen--index screen horizontal-eligible)
    (ebb-screen-carriage-return screen))
  (puthash (ebb--line-at screen (ebb-screen-cursor-y screen)) t
           ebb--reverse-wrap-barriers))

(defun ebb-screen-carriage-return (screen)
  "Move cursor to the active left edge."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let ((x (ebb-screen-cursor-x screen))
        (left (ebb-screen-left-margin screen)))
    (setf (ebb-screen-cursor-x screen)
          (if (and (ebb-screen-horizontal-margins-enabled-p screen)
                   (or (ebb-screen-origin-mode screen) (>= x left)))
              left 0))))

(defun ebb-screen-backspace (screen)
  "Move cursor backward by one cell."
  (ebb-screen-cursor-backward screen 1))

;;;; ---- SGR Attributes -------------------------------------------------

(defun ebb-screen-set-attr (screen prop value)
  "Set attribute PROP to VALUE on SCREEN's current-attr."
  (let ((attr (ebb-attr-copy (ebb-screen-current-attr screen))))
    (setf (ebb-screen-current-attr screen) attr)
    (pcase prop
      (:fg        (setf (ebb-attr-fg attr) value))
      (:bg        (setf (ebb-attr-bg attr) value))
      (:ul-color  (setf (ebb-attr-ul-color attr) value))
      (:bold      (setf (ebb-attr-bold attr) value))
      (:faint     (setf (ebb-attr-faint attr) value))
      (:italic    (setf (ebb-attr-italic attr) value))
      (:underline (setf (ebb-attr-underline attr) value))
      (:blink     (setf (ebb-attr-blink attr) value))
      (:inverse   (setf (ebb-attr-inverse attr) value))
      (:conceal   (setf (ebb-attr-conceal attr) value))
      (:crossed   (setf (ebb-attr-crossed attr) value))
      (:font      (setf (ebb-attr-font attr) value)))))

(defun ebb-screen-reset-attr (screen)
  "Reset all attributes to defaults."
  (let ((old (ebb-screen-current-attr screen)))
    (setf (ebb-screen-current-attr screen)
          (make-ebb-attr :hyperlink (ebb-attr-hyperlink old)
                         :hyperlink-id (ebb-attr-hyperlink-id old)))))

(defun ebb-screen-set-hyperlink (screen uri &optional id)
  "Set SCREEN's active OSC 8 hyperlink to URI and ID."
  (let* ((active (and uri (not (string-empty-p uri))))
         (attr (ebb-attr-copy (ebb-screen-current-attr screen))))
    (setf (ebb-attr-hyperlink attr) (and active uri)
          (ebb-attr-hyperlink-id attr) (and active (or id (gensym "ebb-link-")))
          (ebb-screen-current-attr screen) attr)))

;;;; ---- Erasing --------------------------------------------------------

(defun ebb-screen--erase-in-display-unprotected (screen mode)
  "Erase in display without preserving protected cells."
  (let ((cy (ebb-screen-cursor-y screen))
        (height (ebb-screen-height screen)))
    (pcase mode
      (0 ;; Erase from cursor to end of display
       (ebb-screen--erase-in-line-unprotected screen 0)
       (cl-loop for r from (1+ cy) below height
                do (ebb--erase-whole-line screen r)))
      (1 ;; Erase from start to cursor
       (ebb-screen--erase-in-line-unprotected screen 1)
       (cl-loop for r from 0 below cy
                do (ebb--erase-whole-line screen r)))
      (2 ;; Erase whole display
       (ebb-screen-mark-viewport-reset screen)
       (let ((bg (and (ebb-screen-current-attr screen)
                      (ebb-attr-bg (ebb-screen-current-attr screen)))))
         (if (null bg)
             ;; Common full-frame path: clear by replacing row objects with
             ;; lazy blank lines instead of materializing and clearing every
             ;; cell.  ED 2 does not affect scrollback.
             (let ((r 0)
                   (width (ebb-screen-width screen)))
               (while (< r height)
                 (ebb--set-line-at screen r (ebb--make-empty-line width))
                 (cl-incf r))
               (ebb--mark-region-dirty screen 0 (1- height)))
           (cl-loop for r from 0 below height
                    do (ebb--erase-whole-line screen r)))))
      (3 ;; Erase scrollback
       (ebb--history-clear screen)))))

(defun ebb-screen--erase-in-line-unprotected (screen mode)
  "Erase in line without preserving protected cells."
  (let* ((cx (ebb-screen-cursor-x screen))
         (cy (ebb-screen-cursor-y screen))
         (width (ebb-screen-width screen))
         (line (ebb--line-at screen cy))
         (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
         (initialized (ebb--line-initialized-cells line cells width))
         (ecell (ebb--make-erase-cell screen)))
    (pcase mode
      (0 ;; cursor to end
       (cl-loop for i from cx below width
                do (aset cells i (copy-ebb-cell ecell))
                and do (aset initialized i t))
       (if (and (null (ebb-cell-attr ecell))
                (ebb-line-text line)
                (null (ebb-line-uniform-attr line)))
           (progn
             (cl-loop for i from cx below width
                      do (aset (ebb-line-text line) i ?\s))
             (ebb--line-set-attr-run line cx width nil))
         (setf (ebb-line-text line) nil
               (ebb-line-attr-runs line) nil
               (ebb-line-uniform-attr line) nil)))
      (1 ;; start to cursor
       (cl-loop for i from 0 to cx
                do (aset cells i (copy-ebb-cell ecell))
                and do (aset initialized i t))
       (if (and (null (ebb-cell-attr ecell))
                (ebb-line-text line)
                (null (ebb-line-uniform-attr line)))
           (progn
             (cl-loop for i from 0 to cx
                      do (aset (ebb-line-text line) i ?\s))
             (ebb--line-set-attr-run line 0 (1+ cx) nil))
         (setf (ebb-line-text line) nil
               (ebb-line-attr-runs line) nil
               (ebb-line-uniform-attr line) nil)))
      (2 ;; whole line
       (cl-loop for i from 0 below width
                do (aset cells i (copy-ebb-cell ecell))
                and do (aset initialized i t))
       (setf (ebb-line-prompt-begins line) nil)
       (setf (ebb-line-prompt-ends line) nil)
       (if (null (ebb-cell-attr ecell))
           (progn
             (setf (ebb-line-text line) (make-string width ?\s))
             (setf (ebb-line-uniform-attr line) nil))
         (setf (ebb-line-text line) nil)
         (setf (ebb-line-uniform-attr line) nil))
       (setf (ebb-line-attr-runs line) nil)))
    (setf (ebb-line-dirty line) t)
    (ebb--mark-dirty screen cy)))

(defun ebb--erase-whole-line (screen row)
  "Erase entire line ROW with BCE."
  (let* ((width (ebb-screen-width screen))
         (line (ebb--line-at screen row))
         (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
         (initialized (ebb--line-initialized-cells line cells width))
         (ecell (ebb--make-erase-cell screen)))
    (cl-loop for i from 0 below width
             do (aset cells i (copy-ebb-cell ecell))
             and do (aset initialized i t))
    (if (null (ebb-cell-attr ecell))
        (progn
          (setf (ebb-line-text line) (make-string width ?\s))
          (setf (ebb-line-uniform-attr line) nil))
      (setf (ebb-line-text line) nil)
      (setf (ebb-line-uniform-attr line) nil))
    (setf (ebb-line-attr-runs line) nil)
    (setf (ebb-line-prompt-begins line) nil)
    (setf (ebb-line-prompt-ends line) nil)
    (setf (ebb-line-wrapped line) nil)
    (setf (ebb-line-dirty line) t)
    (ebb--mark-dirty screen row)))

(defun ebb-screen--erase-chars-unprotected (screen count)
  "Erase COUNT characters starting at cursor without protection."
  (let* ((cx (ebb-screen-cursor-x screen))
         (cy (ebb-screen-cursor-y screen))
         (width (ebb-screen-width screen))
         (line (ebb--line-at screen cy))
         (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
         (initialized (ebb--line-initialized-cells line cells width))
         (ecell (ebb--make-erase-cell screen))
         (end (min (+ cx count) width)))
    (cl-loop for i from cx below end
             do (aset initialized i t))
    (cl-loop for i from cx below end
             do (aset cells i (copy-ebb-cell ecell)))
    (if (and (null (ebb-cell-attr ecell))
             (ebb-line-text line)
             (null (ebb-line-uniform-attr line)))
        (progn
          (cl-loop for i from cx below end
                   do (aset (ebb-line-text line) i ?\s))
          (ebb--line-set-attr-run line cx end nil))
      (setf (ebb-line-text line) nil
            (ebb-line-attr-runs line) nil
            (ebb-line-uniform-attr line) nil))
    (setf (ebb-line-dirty line) t)
    (ebb--mark-dirty screen cy)))

(defun ebb-screen--protected-snapshot (screen table)
  "Return protected cells from TABLE as coordinate snapshots for SCREEN."
  (let ((width (ebb-screen-width screen))
        snapshots)
    (dotimes (row (ebb-screen-height screen))
      (let* ((line (ebb--line-at screen row))
             (bits (gethash line table)))
        (when (and bits (cl-loop for bit across bits thereis bit))
          (let* ((cells (ebb--line-ensure-cells line width))
                 (initialized (ebb--line-initialized-cells line cells width)))
            (dotimes (column (min width (length bits)))
              (when (aref bits column)
                (push (list row column
                            (copy-ebb-cell (aref cells column))
                            (aref initialized column))
                      snapshots)))))))
    snapshots))

(defun ebb-screen--restore-protected-snapshot (screen table snapshots)
  "Restore SCREEN cell SNAPSHOTS and their protection bits in TABLE."
  (let ((width (ebb-screen-width screen)))
    (dolist (snapshot snapshots)
      (pcase-let ((`(,row ,column ,cell ,initialized-p) snapshot))
        (when (and (< row (ebb-screen-height screen)) (< column width))
          (let* ((line (ebb--line-at screen row))
                 (cells (ebb--line-ensure-cells line width))
                 (initialized (ebb--line-initialized-cells line cells width))
                 (bits (ebb--line-protection-bits table line width)))
            (aset cells column cell)
            (aset initialized column initialized-p)
            (aset bits column t)
            (setf (ebb-line-text line) nil
                  (ebb-line-attr-runs line) nil
                  (ebb-line-uniform-attr line) nil
                  (ebb-line-rendered line) nil
                  (ebb-line-dirty line) t)
            (ebb--mark-dirty screen row)))))))

(defun ebb-screen--initialized-snapshot (screen)
  "Return coordinates of initialized cells on SCREEN."
  (let ((width (ebb-screen-width screen))
        coordinates)
    (dotimes (row (ebb-screen-height screen))
      (let* ((line (ebb--line-at screen row))
             (cells (ebb--line-ensure-cells line width))
             (bits (ebb--line-initialized-cells line cells width)))
        (dotimes (column width)
          (when (aref bits column)
            (push (cons row column) coordinates)))))
    coordinates))

(defun ebb-screen--restore-initialized-snapshot (screen coordinates)
  "Mark prior initialized cell COORDINATES initialized on SCREEN."
  (let ((width (ebb-screen-width screen)))
    (dolist (coordinate coordinates)
      (when (and (< (car coordinate) (ebb-screen-height screen))
                 (< (cdr coordinate) width))
        (let* ((line (ebb--line-at screen (car coordinate)))
               (cells (ebb--line-ensure-cells line width))
               (bits (ebb--line-initialized-cells line cells width)))
          (aset bits (cdr coordinate) t))))))

(defun ebb-screen--preserving-protection (screen table function &rest args)
  "Call FUNCTION with ARGS while preserving cells protected in TABLE."
  (let* ((snapshots (ebb-screen--protected-snapshot screen table))
         (initialized (and snapshots
                           (ebb-screen--initialized-snapshot screen))))
    (apply function screen args)
    (when initialized
      (ebb-screen--restore-initialized-snapshot screen initialized))
    (ebb-screen--restore-protected-snapshot screen table snapshots)))

(defun ebb-screen--erase-preserving-dec-and-iso
    (screen function mode)
  "Call erase FUNCTION for MODE preserving DEC and ISO protected cells."
  (ebb-screen--preserving-protection
   screen ebb--dec-protected-cells
   (lambda (target erase-mode)
     (ebb-screen--preserving-protection
      target ebb--iso-protected-cells function erase-mode))
   mode))

(defun ebb-screen-erase-in-display (screen mode)
  "Erase display while preserving ISO-protected cells."
  (ebb-screen--preserving-protection
   screen ebb--iso-protected-cells
   #'ebb-screen--erase-in-display-unprotected mode))

(defun ebb-screen-erase-in-line (screen mode)
  "Erase line while preserving ISO-protected cells."
  (ebb-screen--preserving-protection
   screen ebb--iso-protected-cells
   #'ebb-screen--erase-in-line-unprotected mode))

(defun ebb-screen-erase-chars (screen count)
  "Erase characters while preserving ISO-protected cells."
  (ebb-screen--preserving-protection
   screen ebb--iso-protected-cells
   #'ebb-screen--erase-chars-unprotected count))

(defun ebb-screen-dec-erase-in-display (screen mode)
  "Selectively erase display with xterm-compatible protection."
  (ebb-screen--erase-preserving-dec-and-iso
   screen #'ebb-screen--erase-in-display-unprotected mode))

(defun ebb-screen-dec-erase-in-line (screen mode)
  "Selectively erase line with xterm-compatible protection."
  (ebb-screen--erase-preserving-dec-and-iso
   screen #'ebb-screen--erase-in-line-unprotected mode))

;;;; ---- Scrolling (public) ---------------------------------------------

(defun ebb-screen-scroll (screen direction count)
  "Scroll COUNT lines.  DIRECTION: up or down."
  (pcase direction
    ('up   (ebb--scroll-region-up screen count))
    ('down (ebb--scroll-region-down screen count))))

;;;; ---- Line Operations ------------------------------------------------

(defun ebb-screen-insert-lines (screen count)
  "Insert COUNT blank lines at cursor row, within scroll region."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let* ((cy (ebb-screen-cursor-y screen))
         (top (ebb-screen-scroll-top screen))
         (bot (ebb-screen-scroll-bottom screen))
         (width (ebb-screen-width screen)))
    ;; Only operates within scroll region and when cursor is in it.
    (when (and (>= cy top) (<= cy bot)
               (ebb-screen--inside-horizontal-margins-p screen))
      (let ((n (min count (1+ (- bot cy)))))
        (if (ebb-screen-horizontal-margins-enabled-p screen)
            (let ((left (ebb-screen-left-margin screen))
                  (right (ebb-screen-right-margin screen)))
              ;; Shift only the rectangular region between the margins.
              (cl-loop for row from bot downto (+ cy n)
                       for destination = (ebb--line-at screen row)
                       for source = (ebb--line-at screen (- row n))
                       for destination-cells = (ebb--line-ensure-cells destination width)
                       for source-cells = (ebb--line-ensure-cells source width)
                       for destination-bits = (ebb--line-initialized-cells
                                               destination destination-cells width)
                       for source-bits = (ebb--line-initialized-cells
                                          source source-cells width)
                       for destination-dec = (ebb--line-protection-bits
                                              ebb--dec-protected-cells
                                              destination width)
                       for source-dec = (ebb--line-protection-bits
                                         ebb--dec-protected-cells source width)
                       for destination-iso = (ebb--line-protection-bits
                                              ebb--iso-protected-cells
                                              destination width)
                       for source-iso = (ebb--line-protection-bits
                                         ebb--iso-protected-cells source width)
                       do (cl-loop for column from left to right
                                   do (aset destination-cells column
                                            (copy-ebb-cell
                                             (aref source-cells column)))
                                   and do (aset destination-bits column
                                                (aref source-bits column))
                                   and do (aset destination-dec column
                                                (aref source-dec column))
                                   and do (aset destination-iso column
                                                (aref source-iso column))))
              (cl-loop for row from cy below (+ cy n)
                       for line = (ebb--line-at screen row)
                       for cells = (ebb--line-ensure-cells line width)
                       for bits = (ebb--line-initialized-cells line cells width)
                       for dec-bits = (ebb--line-protection-bits
                                       ebb--dec-protected-cells line width)
                       for iso-bits = (ebb--line-protection-bits
                                       ebb--iso-protected-cells line width)
                       do (cl-loop for column from left to right
                                   do (aset cells column (make-ebb-cell))
                                   and do (aset bits column nil)
                                   and do (aset dec-bits column nil)
                                   and do (aset iso-bits column nil)))
              (cl-loop for row from cy to bot
                       do (setf (ebb-line-text (ebb--line-at screen row)) nil
                                (ebb-line-attr-runs (ebb--line-at screen row)) nil
                                (ebb-line-uniform-attr (ebb--line-at screen row)) nil
                                (ebb-line-dirty (ebb--line-at screen row)) t)))
          ;; Without horizontal margins whole rows can move directly.
          (cl-loop for i from bot downto (+ cy n)
                   do (ebb--set-line-at screen i (ebb--line-at screen (- i n))))
          (cl-loop for i from cy below (+ cy n)
                   do (ebb--set-line-at screen i (ebb--make-empty-line width))))
        (ebb--mark-region-dirty screen cy bot)))))

(defun ebb-screen-delete-lines (screen count)
  "Delete COUNT lines at cursor row, within scroll region."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let* ((cy (ebb-screen-cursor-y screen))
         (top (ebb-screen-scroll-top screen))
         (bot (ebb-screen-scroll-bottom screen))
         (width (ebb-screen-width screen)))
    (when (and (>= cy top) (<= cy bot)
               (ebb-screen--inside-horizontal-margins-p screen))
      (let ((n (min count (1+ (- bot cy)))))
        (if (ebb-screen-horizontal-margins-enabled-p screen)
            (let ((left (ebb-screen-left-margin screen))
                  (right (ebb-screen-right-margin screen)))
              (cl-loop for row from cy to (- bot n)
                       for destination = (ebb--line-at screen row)
                       for source = (ebb--line-at screen (+ row n))
                       for destination-cells = (ebb--line-ensure-cells destination width)
                       for source-cells = (ebb--line-ensure-cells source width)
                       for destination-bits = (ebb--line-initialized-cells
                                               destination destination-cells width)
                       for source-bits = (ebb--line-initialized-cells
                                          source source-cells width)
                       for destination-dec = (ebb--line-protection-bits
                                              ebb--dec-protected-cells
                                              destination width)
                       for source-dec = (ebb--line-protection-bits
                                         ebb--dec-protected-cells source width)
                       for destination-iso = (ebb--line-protection-bits
                                              ebb--iso-protected-cells
                                              destination width)
                       for source-iso = (ebb--line-protection-bits
                                         ebb--iso-protected-cells source width)
                       do (cl-loop for column from left to right
                                   do (aset destination-cells column
                                            (copy-ebb-cell
                                             (aref source-cells column)))
                                   and do (aset destination-bits column
                                                (aref source-bits column))
                                   and do (aset destination-dec column
                                                (aref source-dec column))
                                   and do (aset destination-iso column
                                                (aref source-iso column))))
              (cl-loop for row from (1+ (- bot n)) to bot
                       for line = (ebb--line-at screen row)
                       for cells = (ebb--line-ensure-cells line width)
                       for bits = (ebb--line-initialized-cells line cells width)
                       for dec-bits = (ebb--line-protection-bits
                                       ebb--dec-protected-cells line width)
                       for iso-bits = (ebb--line-protection-bits
                                       ebb--iso-protected-cells line width)
                       do (cl-loop for column from left to right
                                   do (aset cells column (make-ebb-cell))
                                   and do (aset bits column nil)
                                   and do (aset dec-bits column nil)
                                   and do (aset iso-bits column nil)))
              (cl-loop for row from cy to bot
                       do (setf (ebb-line-text (ebb--line-at screen row)) nil
                                (ebb-line-attr-runs (ebb--line-at screen row)) nil
                                (ebb-line-uniform-attr (ebb--line-at screen row)) nil
                                (ebb-line-dirty (ebb--line-at screen row)) t)))
          (cl-loop for i from cy to (- bot n)
                   do (ebb--set-line-at screen i (ebb--line-at screen (+ i n))))
          (cl-loop for i from (1+ (- bot n)) to bot
                   do (ebb--set-line-at screen i (ebb--make-empty-line width))))
        (ebb--mark-region-dirty screen cy bot)))))

;;;; ---- Character Operations -------------------------------------------

(defun ebb-screen-insert-chars (screen count)
  "Insert COUNT blank characters at cursor, shifting right."
  (let* ((cx (ebb-screen-cursor-x screen))
         (cy (ebb-screen-cursor-y screen))
         (left (ebb-screen-left-margin screen))
         (right (ebb-screen-right-margin screen))
         (margins (ebb-screen-horizontal-margins-enabled-p screen)))
    (when (or (not margins) (<= left cx right))
      (let* ((line (ebb--line-at screen cy))
             (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
             (initialized
              (ebb--line-initialized-cells line cells (ebb-screen-width screen)))
             (dec-bits (ebb--line-protection-bits
                        ebb--dec-protected-cells line (ebb-screen-width screen)))
             (iso-bits (ebb--line-protection-bits
                        ebb--iso-protected-cells line (ebb-screen-width screen)))
             (n (min count (1+ (- right cx)))))
        (setf (ebb-line-text line) nil
              (ebb-line-attr-runs line) nil
              (ebb-line-uniform-attr line) nil)
        ;; Shift right within the active horizontal region.
        (cl-loop for i from right downto (+ cx n)
                 do (aset cells i (aref cells (- i n)))
                 and do (aset initialized i (aref initialized (- i n)))
                 and do (aset dec-bits i (aref dec-bits (- i n)))
                 and do (aset iso-bits i (aref iso-bits (- i n))))
        (cl-loop for i from cx below (+ cx n)
                 do (aset cells i (make-ebb-cell))
                 and do (aset initialized i t)
                 and do (aset dec-bits i nil)
                 and do (aset iso-bits i nil))
        (setf (ebb-line-dirty line) t)
        (ebb--mark-dirty screen cy)))))

(defun ebb-screen-delete-chars (screen count)
  "Delete COUNT characters at cursor, shifting left."
  (let* ((cx (ebb-screen-cursor-x screen))
         (cy (ebb-screen-cursor-y screen))
         (left (ebb-screen-left-margin screen))
         (right (ebb-screen-right-margin screen))
         (margins (ebb-screen-horizontal-margins-enabled-p screen)))
    (when (or (not margins) (<= left cx right))
      (let* ((line (ebb--line-at screen cy))
             (cells (ebb--line-ensure-cells line (ebb-screen-width screen)))
             (initialized
              (ebb--line-initialized-cells line cells (ebb-screen-width screen)))
             (dec-bits (ebb--line-protection-bits
                        ebb--dec-protected-cells line (ebb-screen-width screen)))
             (iso-bits (ebb--line-protection-bits
                        ebb--iso-protected-cells line (ebb-screen-width screen)))
             (n (min count (1+ (- right cx))))
             (fill-start (1+ (- right n))))
        (setf (ebb-line-text line) nil
              (ebb-line-attr-runs line) nil
              (ebb-line-uniform-attr line) nil)
        ;; Shift left within the active horizontal region.
        (cl-loop for i from cx below fill-start
                 do (aset cells i (aref cells (+ i n)))
                 and do (aset initialized i (aref initialized (+ i n)))
                 and do (aset dec-bits i (aref dec-bits (+ i n)))
                 and do (aset iso-bits i (aref iso-bits (+ i n))))
        (cl-loop for i from fill-start to right
                 do (aset cells i (make-ebb-cell))
                 and do (aset initialized i t)
                 and do (aset dec-bits i nil)
                 and do (aset iso-bits i nil))
        (setf (ebb-line-dirty line) t)
        (ebb--mark-dirty screen cy)))))

(defun ebb-screen-repeat-char (screen count)
  "Repeat the last written character COUNT times."
  (when-let* ((ch (ebb-screen-last-char screen)))
    (dotimes (_ count)
      (ebb-screen-write-char screen ch))))

;;;; ---- Tab Stops ------------------------------------------------------

(defun ebb-screen-tab-forward (screen count)
  "Move cursor forward to the next tab stop, COUNT times."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let* ((cx (ebb-screen-cursor-x screen))
         (margins (ebb-screen-horizontal-margins-enabled-p screen))
         (right (ebb-screen-right-margin screen))
         ;; Positions before the region tab into it; positions beyond it must
         ;; not be pulled backward to the right margin.
         (max-x (if (and margins (<= cx right))
                    right
                  (1- (ebb-screen-line-width screen))))
         (stops (ebb-screen-tab-stops screen)))
    (dotimes (_ count)
      (let ((next (cl-find-if (lambda (s) (> s cx)) stops)))
        (setq cx (if next (min next max-x) max-x))))
    (setf (ebb-screen-cursor-x screen) cx)))

(defun ebb-screen-tab-backward (screen count)
  "Move cursor backward to the previous tab stop, COUNT times."
  (setf (ebb-screen-pending-wrap screen) nil)
  (let ((cx (ebb-screen-cursor-x screen))
        (stops (reverse (ebb-screen-tab-stops screen))))
    (dotimes (_ count)
      (let ((prev (cl-find-if (lambda (s) (< s cx)) stops)))
        (setq cx (or prev 0))))
    (setf (ebb-screen-cursor-x screen) cx)))

(defun ebb-screen-set-tab-stop (screen)
  "Set a tab stop at the current cursor column."
  (let ((cx (ebb-screen-cursor-x screen)))
    (unless (member cx (ebb-screen-tab-stops screen))
      (setf (ebb-screen-tab-stops screen)
            (sort (cons cx (ebb-screen-tab-stops screen)) #'<)))))

(defun ebb-screen-clear-tab-stop (screen mode)
  "Clear tab stops.  MODE: 0=current, 3=all."
  (pcase mode
    (0 (setf (ebb-screen-tab-stops screen)
             (delq (ebb-screen-cursor-x screen)
                   (ebb-screen-tab-stops screen))))
    (3 (setf (ebb-screen-tab-stops screen) nil))))

;;;; ---- Scroll Region --------------------------------------------------

(defun ebb-screen-set-scroll-region (screen top bottom)
  "Set scroll region to [TOP, BOTTOM] (0-indexed, inclusive)."
  (let ((max-row (1- (ebb-screen-height screen))))
    (setq top (ebb--clamp top 0 max-row))
    (setq bottom (ebb--clamp bottom 0 max-row))
    (when (< top bottom)
      (setf (ebb-screen-scroll-top screen) top)
      (setf (ebb-screen-scroll-bottom screen) bottom)
      ;; DECSTBM homes cursor
      (ebb-screen-cursor-goto screen 0 0))))

;;;; ---- Alternate Screen -----------------------------------------------

(defun ebb--resize-alt-save (saved new-width new-height)
  "Resize SAVED's main-screen model while the alternate screen is active."
  (let ((main (ebb-screen--make
               :lines (ebb-alt-save-lines saved)
               :width (ebb-alt-save-width saved)
               :height (ebb-alt-save-height saved)
               :line-start (ebb-alt-save-line-start saved)
               :cursor-x (ebb-alt-save-cursor-x saved)
               :cursor-y (ebb-alt-save-cursor-y saved)
               :pending-wrap (ebb-alt-save-pending-wrap saved)
               :scroll-top (ebb-alt-save-scroll-top saved)
               :scroll-bottom (ebb-alt-save-scroll-bottom saved)
               :scrollback (ebb-alt-save-scrollback saved)
               :scrollback-length (ebb-alt-save-scrollback-length saved)
               :history-next-id (ebb-alt-save-history-next-id saved)
               :history-generation (ebb-alt-save-history-generation saved)
               :auto-wrap (ebb-alt-save-auto-wrap saved))))
    (ebb-screen-resize main new-width new-height)
    (setf (ebb-alt-save-lines saved) (ebb-screen-lines main)
          (ebb-alt-save-width saved) (ebb-screen-width main)
          (ebb-alt-save-height saved) (ebb-screen-height main)
          (ebb-alt-save-line-start saved) (ebb-screen-line-start main)
          (ebb-alt-save-cursor-x saved) (ebb-screen-cursor-x main)
          (ebb-alt-save-cursor-y saved) (ebb-screen-cursor-y main)
          (ebb-alt-save-pending-wrap saved) (ebb-screen-pending-wrap main)
          (ebb-alt-save-scroll-top saved) (ebb-screen-scroll-top main)
          (ebb-alt-save-scroll-bottom saved) (ebb-screen-scroll-bottom main)
          (ebb-alt-save-scrollback saved) (ebb-screen-scrollback main)
          (ebb-alt-save-scrollback-length saved)
          (ebb-screen-scrollback-length main)
          (ebb-alt-save-history-next-id saved) (ebb-screen-history-next-id main)
          (ebb-alt-save-history-generation saved)
          (ebb-screen-history-generation main))))

(defun ebb-screen-enter-alt (screen)
  "Enter alternate screen buffer."
  (unless (ebb-screen-alt-screen screen)
    (if-let* ((state (gethash screen ebb--saved-cursor-renditions)))
        (puthash screen state ebb--alt-saved-cursor-renditions)
      (remhash screen ebb--alt-saved-cursor-renditions))
    (remhash screen ebb--saved-cursor-renditions)
    ;; Save main screen state
    (setf (ebb-screen-alt-screen screen)
          (make-ebb-alt-save
           :lines (ebb-screen-lines screen)
           :width (ebb-screen-width screen)
           :height (ebb-screen-height screen)
           :line-start (ebb-screen-line-start screen)
           :cursor-x (ebb-screen-cursor-x screen)
           :cursor-y (ebb-screen-cursor-y screen)
           :pending-wrap (ebb-screen-pending-wrap screen)
           :cursor-saved-x (ebb-screen-cursor-saved-x screen)
           :cursor-saved-y (ebb-screen-cursor-saved-y screen)
           :cursor-saved-attr (ebb-screen-cursor-saved-attr screen)
           :current-attr (ebb-attr-copy (ebb-screen-current-attr screen))
           :scroll-top (ebb-screen-scroll-top screen)
           :scroll-bottom (ebb-screen-scroll-bottom screen)
           :scrollback (ebb-screen-scrollback screen)
           :scrollback-length (ebb-screen-scrollback-length screen)
           :history-next-id (ebb-screen-history-next-id screen)
           :history-generation (ebb-screen-history-generation screen)
           :auto-wrap (ebb-screen-auto-wrap screen)
           :origin-mode (ebb-screen-origin-mode screen)
           :insert-mode (ebb-screen-insert-mode screen)))
    ;; Create fresh alt screen
    (let ((w (ebb-screen-width screen))
          (h (ebb-screen-height screen)))
      (let ((lines (make-vector h nil)))
        (dotimes (i h)
          (aset lines i (ebb--make-empty-line w)))
        (setf (ebb-screen-lines screen) lines))
      (setf (ebb-screen-line-start screen) 0)
      (setf (ebb-screen-cursor-x screen) 0)
      (setf (ebb-screen-cursor-y screen) 0)
      (setf (ebb-screen-pending-wrap screen) nil)
      (setf (ebb-screen-scroll-top screen) 0)
      (setf (ebb-screen-scroll-bottom screen) (1- h))
      (ebb--history-clear screen)
      (setf (ebb-screen-dirty-lines screen)
            (number-sequence 0 (1- h)))
      (setf (ebb-screen-dirty-map screen) (make-vector h t))
      (setf (ebb-screen-dirty-count screen) h))))

(defun ebb-screen-leave-alt (screen)
  "Leave alternate screen buffer, restoring main screen."
  (when-let* ((saved (ebb-screen-alt-screen screen)))
    (setf (ebb-screen-lines screen) (ebb-alt-save-lines saved))
    (setf (ebb-screen-line-start screen) (ebb-alt-save-line-start saved))
    (setf (ebb-screen-cursor-x screen) (ebb-alt-save-cursor-x saved))
    (setf (ebb-screen-cursor-y screen) (ebb-alt-save-cursor-y saved))
    (setf (ebb-screen-pending-wrap screen) (ebb-alt-save-pending-wrap saved))
    (setf (ebb-screen-cursor-saved-x screen) (ebb-alt-save-cursor-saved-x saved))
    (setf (ebb-screen-cursor-saved-y screen) (ebb-alt-save-cursor-saved-y saved))
    (setf (ebb-screen-cursor-saved-attr screen) (ebb-alt-save-cursor-saved-attr saved))
    (if-let* ((state (gethash screen ebb--alt-saved-cursor-renditions)))
        (puthash screen state ebb--saved-cursor-renditions)
      (remhash screen ebb--saved-cursor-renditions))
    (remhash screen ebb--alt-saved-cursor-renditions)
    (setf (ebb-screen-current-attr screen) (ebb-alt-save-current-attr saved))
    (setf (ebb-screen-scroll-top screen) (ebb-alt-save-scroll-top saved))
    (setf (ebb-screen-scroll-bottom screen) (ebb-alt-save-scroll-bottom saved))
    (setf (ebb-screen-scrollback screen) (ebb-alt-save-scrollback saved))
    (setf (ebb-screen-scrollback-length screen)
          (ebb-alt-save-scrollback-length saved))
    (setf (ebb-screen-history-next-id screen)
          (ebb-alt-save-history-next-id saved))
    (setf (ebb-screen-history-generation screen)
          (ebb-alt-save-history-generation saved))
    (setf (ebb-screen-history-row-ends screen) nil)
    (setf (ebb-screen-scrollback-dirty screen) t)
    (setf (ebb-screen-auto-wrap screen) (ebb-alt-save-auto-wrap saved))
    (setf (ebb-screen-origin-mode screen) (ebb-alt-save-origin-mode saved))
    (setf (ebb-screen-insert-mode screen) (ebb-alt-save-insert-mode saved))
    (setf (ebb-screen-alt-screen screen) nil)
    ;; Everything is dirty
    (setf (ebb-screen-dirty-lines screen)
          (number-sequence 0 (1- (ebb-screen-height screen))))
    (setf (ebb-screen-dirty-map screen)
          (make-vector (ebb-screen-height screen) t))
    (setf (ebb-screen-dirty-count screen) (ebb-screen-height screen))))

;;;; ---- Save / Restore Cursor ------------------------------------------

(defun ebb-screen-save-cursor (screen)
  "Save cursor and rendition state (DECSC)."
  (setf (ebb-screen-cursor-saved-x screen) (ebb-screen-cursor-x screen)
        (ebb-screen-cursor-saved-y screen) (ebb-screen-cursor-y screen)
        (ebb-screen-cursor-saved-attr screen)
        (ebb-attr-copy (ebb-screen-current-attr screen)))
  (puthash screen
           (list (ebb-screen-origin-mode screen)
                 (ebb-screen-auto-wrap screen)
                 (ebb-screen-charset-g0 screen)
                 (ebb-screen-charset-g1 screen)
                 (ebb-screen-charset-g2 screen)
                 (ebb-screen-charset-g3 screen)
                 (ebb-screen-charset-active screen))
           ebb--saved-cursor-renditions))

(defun ebb-screen-restore-cursor (screen)
  "Restore cursor and rendition state (DECRC)."
  (setf (ebb-screen-pending-wrap screen) nil
        (ebb-screen-cursor-y screen)
        (ebb--clamp (ebb-screen-cursor-saved-y screen)
                    0 (1- (ebb-screen-height screen))))
  (setf (ebb-screen-cursor-x screen)
        (ebb--clamp (ebb-screen-cursor-saved-x screen)
                    0 (1- (ebb-screen-line-width screen))))
  (when-let* ((state (gethash screen ebb--saved-cursor-renditions)))
    (setf (ebb-screen-origin-mode screen) (nth 0 state)
          (ebb-screen-auto-wrap screen) (nth 1 state)
          (ebb-screen-charset-g0 screen) (nth 2 state)
          (ebb-screen-charset-g1 screen) (nth 3 state)
          (ebb-screen-charset-g2 screen) (nth 4 state)
          (ebb-screen-charset-g3 screen) (nth 5 state)
          (ebb-screen-charset-active screen) (nth 6 state)))
  (when (ebb-screen-cursor-saved-attr screen)
    (setf (ebb-screen-current-attr screen)
          (ebb-attr-copy (ebb-screen-cursor-saved-attr screen)))))

;;;; ---- Mode Setting ---------------------------------------------------

(defun ebb-screen-alignment-test (screen)
  "Fill SCREEN with `E' characters for the DEC screen alignment test."
  (let ((width (ebb-screen-width screen))
        (height (ebb-screen-height screen)))
    (dotimes (row height)
      (ebb--set-line-at
       screen row
       (make-ebb-line :cells nil
                      :cells-valid nil
                      :text (make-string width ?E)
                      :dirty t)))
    (remhash screen ebb--horizontal-margins)
    (setf (ebb-screen-scroll-top screen) 0
          (ebb-screen-scroll-bottom screen) (1- height)
          (ebb-screen-cursor-x screen) 0
          (ebb-screen-cursor-y screen) 0
          (ebb-screen-pending-wrap screen) nil)
    (ebb-screen-mark-viewport-reset screen)
    (ebb--mark-region-dirty screen 0 (1- height))))

(defun ebb-screen-column-mode-enabled-p (screen)
  "Return non-nil when SCREEN is in 132-column mode."
  (and (gethash screen ebb--column-mode-screens) t))

(defun ebb-screen-set-column-mode (screen wide)
  "Select 132 columns when WIDE is non-nil, otherwise 80 columns.
DECCOLM clears the display, restores full-screen margins, and homes the cursor."
  (puthash screen (and wide t) ebb--column-mode-screens)
  (remhash screen ebb--horizontal-margins)
  (let ((width (if wide 132 80))
        (height (ebb-screen-height screen)))
    (ebb-screen-resize screen width height)
    (setf (ebb-screen-scroll-top screen) 0
          (ebb-screen-scroll-bottom screen) (1- height)
          (ebb-screen-cursor-x screen) 0
          (ebb-screen-cursor-y screen) 0
          (ebb-screen-pending-wrap screen) nil
          (ebb-screen-tab-stops screen) (ebb--default-tab-stops width))
    (ebb-screen-erase-in-display screen 2)))

(defun ebb-screen-set-mode (screen mode value)
  "Set a DECSET/DECRST MODE to VALUE (t or nil)."
  (pcase mode
    (1    (setf (ebb-screen-keypad-mode screen) value))
    (3    (ebb-screen-set-column-mode screen value))
    (6    (setf (ebb-screen-origin-mode screen) value)
          (ebb-screen-cursor-goto screen 0 0))
    (7    (setf (ebb-screen-auto-wrap screen) value))
    (9    (setf (ebb-screen-mouse-mode screen) (and value 'x10)))
    (12   (setf (ebb-screen-cursor-blink screen) value))
    (25   (setf (ebb-screen-cursor-visible screen) value))
    (45   (ebb--set-reverse-wrap-mode screen nil value))
    (69   (ebb-screen-set-horizontal-margin-mode screen value))
    (80   nil) ;; sixel scrolling - TODO
    (1045 (ebb--set-reverse-wrap-mode screen t value))
    (1000 (setf (ebb-screen-mouse-mode screen) (and value 'normal)))
    (1002 (setf (ebb-screen-mouse-mode screen) (and value 'button-event)))
    (1003 (setf (ebb-screen-mouse-mode screen) (and value 'any-event)))
    (1004 (setf (ebb-screen-focus-events screen) value))
    (1006 (setf (ebb-screen-mouse-sgr screen) value))
    (1047 ;; Alt screen only (no cursor save)
     (if value
         (ebb-screen-enter-alt screen)
       (ebb-screen-leave-alt screen)))
    (1048 ;; Cursor save only
     (if value
         (ebb-screen-save-cursor screen)
       (ebb-screen-restore-cursor screen)))
    (1049 ;; Alt screen + cursor save
     (if value
         (progn
           (ebb-screen-save-cursor screen)
           (ebb-screen-enter-alt screen))
       (ebb-screen-leave-alt screen)
       (ebb-screen-restore-cursor screen)))
    (2004 (setf (ebb-screen-bracketed-paste screen) value))))

(defun ebb-screen-set-cursor-style (screen style)
  "Set the cursor style.  STYLE: 0-6."
  (setf (ebb-screen-cursor-style screen)
        (pcase style
          ((or 0 1) :blinking-block)
          (2 :block)
          (3 :blinking-underline)
          (4 :underline)
          (5 :blinking-bar)
          (6 :bar)
          (_ :block))))

;;;; ---- Resize ---------------------------------------------------------

(defun ebb--resize-alt-screen
    (screen old-lines old-width old-pending-wrap new-width new-height)
  "Resize SCREEN's alternate grid without reflowing OLD-LINES.
OLD-WIDTH and OLD-PENDING-WRAP describe the previous grid; NEW-WIDTH and
NEW-HEIGHT specify the replacement grid."
  (let ((lines (make-vector new-height nil)))
    (dotimes (row new-height)
      (aset lines row
            (if (< row (length old-lines))
                (let* ((line (aref old-lines row))
                       (cells (ebb--line-ensure-cells line old-width))
                       (normalized
                        (car (ebb--normalize-cells-for-width cells new-width))))
                  (setf (ebb-line-cells line)
                        (ebb--fit-cells-to-width normalized new-width)
                        (ebb-line-cells-valid line) t
                        (ebb-line-text line) nil
                        (ebb-line-attr-runs line) nil
                        (ebb-line-uniform-attr line) nil
                        (ebb-line-rendered line) nil
                        (ebb-line-dirty line) t)
                  line)
              (ebb--make-empty-line new-width))))
    (setf (ebb-screen-lines screen) lines
          (ebb-screen-line-start screen) 0
          (ebb-screen-width screen) new-width
          (ebb-screen-height screen) new-height
          (ebb-screen-cursor-x screen)
          (if (and old-pending-wrap (> new-width old-width))
              old-width
            (min (ebb-screen-cursor-x screen) (1- new-width)))
          (ebb-screen-cursor-y screen)
          (min (ebb-screen-cursor-y screen) (1- new-height))
          (ebb-screen-pending-wrap screen)
          (and old-pending-wrap
               (<= new-width old-width)
               (ebb-screen-auto-wrap screen)))))

(defun ebb--normalize-logical-lines-for-width
    (logical-lines cursor-index cursor-offset new-width)
  "Normalize LOGICAL-LINES and cursor position for NEW-WIDTH.
Translate CURSOR-INDEX and CURSOR-OFFSET through the normalization and return
a cons of normalized lines and the translated offset."
  (let (normalized)
    (cl-loop for logical in logical-lines
             for index from 0
             for result = (ebb--normalize-cells-for-width
                           (car logical) new-width
                           (and (= index cursor-index) cursor-offset))
             do
             (when (= index cursor-index)
               (setq cursor-offset (cdr result)))
             (push (list
                    (car result)
                    (nth 1 logical)
                    (mapcar
                     (lambda (marker)
                       (cdr (ebb--normalize-cells-for-width
                             (car logical) new-width marker)))
                     (nth 2 logical))
                    (mapcar
                     (lambda (marker)
                       (cdr (ebb--normalize-cells-for-width
                             (car logical) new-width marker)))
                     (nth 3 logical)))
                   normalized))
    (cons (nreverse normalized) cursor-offset)))

(defun ebb--resize-main-screen
    (screen old-lines old-width old-cursor-x cursor-anchor end
            new-width new-height)
  "Resize and reflow SCREEN's main grid from OLD-LINES.
OLD-WIDTH, OLD-CURSOR-X, CURSOR-ANCHOR, and END describe the old grid;
NEW-WIDTH and NEW-HEIGHT specify the replacement grid."
  (let* ((logical-lines (ebb--unwrap-lines
                         (cl-subseq old-lines 0 end) old-width))
         (cursor-index (car cursor-anchor))
         (cursor-offset (cdr cursor-anchor))
         (cursor-beyond-content
          (> cursor-offset
             (length (car (nth cursor-index logical-lines))))))
    (when cursor-beyond-content
      (setq cursor-offset
            (length (car (nth cursor-index logical-lines)))))
    (pcase-let* ((`(,normalized . ,translated-offset)
                  (ebb--normalize-logical-lines-for-width
                   logical-lines cursor-index cursor-offset new-width))
                 (cursor-offset translated-offset)
                 (physical (ebb--rewrap-lines-all normalized new-width))
                 (count (length physical))
                 (cursor-position
                  (ebb--logical-offset-position
                   normalized cursor-index cursor-offset new-width))
                 (natural-start (max 0 (- count new-height)))
                 (start (min natural-start (car cursor-position)))
                 (visible-end (min count (+ start new-height)))
                 (displaced (cl-subseq physical 0 start))
                 (visible (cl-subseq physical start visible-end))
                 (cursor-column
                  (if cursor-beyond-content
                      (min old-cursor-x (1- new-width))
                    (cdr cursor-position)))
                 (pending-wrap
                  (and (not cursor-beyond-content)
                       (ebb-screen-auto-wrap screen)
                       (= cursor-column new-width))))
      (setf (ebb-screen-width screen) new-width
            (ebb-screen-height screen) new-height)
      (dolist (line displaced)
        (ebb--history-push-row screen line new-width))
      (when displaced
        (setf (ebb-screen-scrollback-dirty screen) t)
        (ebb--trim-scrollback screen))
      (setf (ebb-screen-lines screen)
            (vconcat visible
                     (cl-loop repeat (- new-height (length visible))
                              collect (ebb--make-empty-line new-width)))
            (ebb-screen-line-start screen) 0
            (ebb-screen-cursor-x screen)
            (if (= cursor-column new-width) (1- new-width) cursor-column)
            (ebb-screen-cursor-y screen)
            (ebb--clamp (- (car cursor-position) start) 0 (1- new-height))
            (ebb-screen-pending-wrap screen) pending-wrap))))

(defun ebb-screen-resize (screen new-width new-height)
  "Resize SCREEN to NEW-WIDTH x NEW-HEIGHT.
Reflow main-screen lines, preserve the logical cursor, and reset the region."
  (when (and (> new-width 0) (> new-height 0)
             (or (/= new-width (ebb-screen-width screen))
                 (/= new-height (ebb-screen-height screen))))
    (when-let* ((saved (ebb-screen-alt-screen screen)))
      (ebb--resize-alt-save saved new-width new-height))
    (when (ebb-screen-scrollback screen)
      (setf (ebb-screen-scrollback-dirty screen) t)
      (ebb--history-changed screen))
    (let* ((old-lines (ebb--ordered-lines-vector screen))
           (old-width (ebb-screen-width screen))
           (old-cursor-x (ebb-screen-cursor-x screen))
           (old-pending-wrap (ebb-screen-pending-wrap screen))
           (cursor-anchor
            (ebb--cursor-logical-anchor
             old-lines old-width
             (ebb-screen-cursor-y screen)
             old-cursor-x old-pending-wrap))
           (end (length old-lines)))
      ;; Ignore bottom rows containing only terminal padding when shrinking.
      (while (and (> end (1+ (ebb-screen-cursor-y screen)))
                  (let ((line (aref old-lines (1- end))))
                    (and (not (ebb-line-wrapped line))
                         (= 0 (length
                               (ebb--trim-trailing-blank-cells
                                (ebb--line-ensure-cells line old-width)))))))
        (cl-decf end))
      (if (ebb-screen-alt-screen screen)
          (ebb--resize-alt-screen
           screen old-lines old-width old-pending-wrap new-width new-height)
        (ebb--resize-main-screen
         screen old-lines old-width old-cursor-x cursor-anchor end
         new-width new-height))
      (setf (ebb-screen-scroll-top screen) 0
            (ebb-screen-scroll-bottom screen) (1- new-height)
            (ebb-screen-dirty-lines screen) (number-sequence 0 (1- new-height))
            (ebb-screen-dirty-map screen) (make-vector new-height t)
            (ebb-screen-dirty-count screen) new-height
            (ebb-screen-tab-stops screen) (ebb--default-tab-stops new-width)))))

(defun ebb--unwrap-lines (lines old-width)
  "Merge wrapped physical lines into logical lines.
Returns list of (CELLS . TRAILING-WRAP-P)."
  (let ((result nil)
        (current-cells nil)
        (current-prompt-begins nil)
        (current-prompt-ends nil))
    (dotimes (i (length lines))
      (let* ((line (aref lines i))
             (base (length current-cells)))
        (setq current-prompt-begins
              (append current-prompt-begins
                      (mapcar (lambda (column) (+ base column))
                              (ebb-line-prompt-begins line)))
              current-prompt-ends
              (append current-prompt-ends
                      (mapcar (lambda (column) (+ base column))
                              (ebb-line-prompt-ends line))))
        (setq current-cells
              (vconcat (or current-cells [])
                       (ebb--line-ensure-cells line old-width)))
        (unless (ebb-line-wrapped line)
          ;; A non-wrapped line's right-padding is presentation, not logical
          ;; content.  Keeping it would make resize/reflow invent large runs of
          ;; blanks and split real content unexpectedly.
          (setq current-cells (ebb--trim-trailing-blank-cells current-cells))
          (push (list current-cells nil
                      current-prompt-begins current-prompt-ends)
                result)
          (setq current-cells nil
                current-prompt-begins nil
                current-prompt-ends nil))))
    (when current-cells
      (push (list current-cells t
                  current-prompt-begins current-prompt-ends)
            result))
    (nreverse result)))

(defun ebb--cursor-logical-anchor (lines width row column pending-wrap)
  "Return (LOGICAL-INDEX . OFFSET) for a physical cursor position."
  (let ((logical-index 0)
        (offset 0)
        (i 0))
    (while (< i row)
      (if (ebb-line-wrapped (aref lines i))
          (cl-incf offset width)
        (cl-incf logical-index)
        (setq offset 0))
      (cl-incf i))
    (cons logical-index (+ offset column (if pending-wrap 1 0)))))

(defun ebb--normalize-cells-for-width (cells width &optional offset)
  "Return (CELLS . OFFSET) with glyphs wider than WIDTH replaced.
OFFSET, when non-nil, is translated to the normalized cell sequence."
  (let ((i 0)
        (new-index 0)
        mapped
        result)
    (while (< i (length cells))
      (let* ((cell (aref cells i))
             (cell-width (max 1 (ebb-cell-width cell))))
        (if (> (ebb-cell-width cell) width)
            (progn
              (when (and offset (null mapped) (<= offset (+ i cell-width)))
                (setq mapped (if (<= offset i) new-index (1+ new-index))))
              (let ((replacement (copy-ebb-cell cell)))
                (setf (ebb-cell-char replacement) #xfffd
                      (ebb-cell-width replacement) 1
                      (ebb-cell-combining replacement) nil)
                (push replacement result))
              (cl-incf i cell-width))
          (when (and offset (null mapped) (<= offset i))
            (setq mapped new-index))
          (push cell result)
          (cl-incf i))
        (cl-incf new-index)))
    (cons (vconcat (nreverse result))
          (or mapped new-index))))

(defun ebb--cells-row-count (cells width)
  "Return the number of WIDTH-column rows needed for CELLS."
  (let ((length (length cells))
        (offset 0)
        (rows 0))
    (if (zerop length) 1
      (while (< offset length)
        (setq offset (ebb--wrap-end cells offset width))
        (cl-incf rows))
      rows)))

(defun ebb--cells-offset-position (cells width offset)
  "Return physical (ROW . COLUMN) for OFFSET in CELLS at WIDTH."
  (let ((length (length cells))
        (start 0)
        (row 0)
        end)
    (setq offset (min offset length))
    (if (zerop length)
        '(0 . 0)
      (catch 'position
        (while (< start length)
          (setq end (ebb--wrap-end cells start width))
          (when (or (< offset end)
                    (and (= end length) (= offset end)))
            (throw 'position
                   (cons row (min width (- offset start)))))
          (setq start end)
          (cl-incf row))
        (cons row (min width (- length start)))))))

(defun ebb--logical-offset-position (logical-lines index offset width)
  "Return physical position for logical line INDEX and OFFSET."
  (let ((row 0)
        (i 0))
    (while (< i index)
      (cl-incf row (ebb--cells-row-count (car (nth i logical-lines)) width))
      (cl-incf i))
    (let ((position
           (ebb--cells-offset-position
            (car (nth index logical-lines)) width offset)))
      (cons (+ row (car position)) (cdr position)))))

(defun ebb--blank-cell-p (cell)
  "Return non-nil when CELL is a default blank cell."
  (and (= (ebb-cell-char cell) ?\s)
       (= (ebb-cell-width cell) 1)
       (null (ebb-cell-attr cell))))

(defun ebb--trim-trailing-blank-cells (cells)
  "Return CELLS without trailing default blanks."
  (let ((end (length cells)))
    (while (and (> end 0)
                (ebb--blank-cell-p (aref cells (1- end))))
      (cl-decf end))
    (cl-subseq cells 0 end)))

(defun ebb--rewrap-lines-all (logical-lines new-width)
  "Re-wrap LOGICAL-LINES to NEW-WIDTH, returning every physical line."
  (let ((physical nil))
    (dolist (ll logical-lines)
      (let* ((cells (car ll))
             (len (length cells))
             (prompt-begins (nth 2 ll))
             (prompt-ends (nth 3 ll)))
        (if (<= len new-width)
            (push (make-ebb-line
                   :cells (ebb--fit-cells-to-width cells new-width)
                   :prompt-begins (copy-sequence prompt-begins)
                   :prompt-ends (copy-sequence prompt-ends)
                   :wrapped nil :dirty t)
                  physical)
          (let ((offset 0))
            (while (< offset len)
              ;; Avoid cutting a wide character away from its continuation
              ;; cell.  The model stores wide chars as a start cell followed by
              ;; width=0 continuation cells, so chunk boundaries must not fall
              ;; immediately before a continuation.
              (let* ((end (ebb--wrap-end cells offset new-width))
                     (chunk (cl-subseq cells offset end))
                     (last-chunk (>= end len))
                     (row-prompt-begins
                      (cl-loop for column in prompt-begins
                               when (and (>= column offset) (< column end))
                               collect (- column offset)))
                     (row-prompt-ends
                      (cl-loop for column in prompt-ends
                               when (and (> column offset) (<= column end))
                               collect (- column offset))))
                (push (make-ebb-line
                       :cells (ebb--fit-cells-to-width chunk new-width)
                       :prompt-begins row-prompt-begins
                       :prompt-ends row-prompt-ends
                       :wrapped (not last-chunk) :dirty t)
                      physical)
                (setq offset end)))))))
    (nreverse physical)))

(defun ebb--rewrap-lines (logical-lines new-width new-height)
  "Re-wrap LOGICAL-LINES to NEW-WIDTH, returning vector of NEW-HEIGHT lines."
  (let* ((physical (ebb--rewrap-lines-all logical-lines new-width))
         (count (length physical)))
    (cond
     ((> count new-height)
      (vconcat (last physical new-height)))
     ((< count new-height)
      (vconcat physical
               (cl-loop repeat (- new-height count)
                        collect (ebb--make-empty-line new-width))))
     (t (vconcat physical)))))

(defun ebb--wrap-end (cells offset width)
  "Return a safe wrap end for CELLS starting at OFFSET and WIDTH columns."
  (let* ((len (length cells))
         (end (min (+ offset width) len)))
    ;; If the next cell is a wide-char continuation, this boundary would split
    ;; the wide char; back up before its start.  Ensure forward progress even on
    ;; a one-column terminal.
    (while (and (< end len)
                (> end offset)
                (zerop (ebb-cell-width (aref cells end))))
      (cl-decf end))
    (if (= end offset)
        (min len (+ offset
                    (max 1 (ebb-cell-width (aref cells offset)))))
      end)))

(defun ebb--fit-cells-to-width (cells target-width)
  "Fit CELLS into TARGET-WIDTH without splitting an oversized glyph."
  (let ((result (ebb--make-empty-cells target-width))
        (source 0)
        (column 0)
        (length (length cells)))
    (while (and (< source length) (< column target-width))
      (let* ((cell (aref cells source))
             (cell-width (ebb-cell-width cell)))
        (cond
         ;; Ignore an orphan continuation rather than starting a row with it.
         ((zerop cell-width)
          (cl-incf source))
         ((> cell-width (- target-width column))
          (let ((replacement (copy-ebb-cell cell)))
            (setf (ebb-cell-char replacement) #xfffd
                  (ebb-cell-width replacement) 1
                  (ebb-cell-combining replacement) nil)
            (aset result column replacement))
          (cl-incf column)
          (cl-incf source (max 1 cell-width)))
         (t
          (let ((part 0))
            (while (< part cell-width)
              (aset result (+ column part)
                    (if (< (+ source part) length)
                        (aref cells (+ source part))
                      (make-ebb-cell :width 0)))
              (cl-incf part)))
          (cl-incf source cell-width)
          (cl-incf column cell-width)))))
    result))

(defun ebb--pad-cells (cells target-width)
  "Pad or truncate CELLS vector to TARGET-WIDTH."
  (let ((len (length cells)))
    (cond
     ((= len target-width) cells)
     ((< len target-width)
      (vconcat cells
               (cl-loop repeat (- target-width len)
                        collect (make-ebb-cell))))
     (t (cl-subseq cells 0 target-width)))))

;;;; ---- Reset ----------------------------------------------------------

(defun ebb-screen-soft-reset (screen)
  "Reset terminal modes and saved state without clearing the display (DECSTR)."
  (setf (ebb-screen-pending-wrap screen) nil
        (ebb-screen-cursor-saved-x screen) 0
        (ebb-screen-cursor-saved-y screen) 0
        (ebb-screen-cursor-saved-attr screen) nil
        (ebb-screen-current-attr screen) (make-ebb-attr)
        (ebb-screen-scroll-top screen) 0
        (ebb-screen-scroll-bottom screen) (1- (ebb-screen-height screen))
        (ebb-screen-auto-wrap screen) t
        (ebb-screen-insert-mode screen) nil
        (ebb-screen-origin-mode screen) nil
        (ebb-screen-keypad-mode screen) nil
        (ebb-screen-cursor-visible screen) t
        (ebb-screen-charset-g0 screen) 'us-ascii
        (ebb-screen-charset-g1 screen) 'us-ascii
        (ebb-screen-charset-g2 screen) 'us-ascii
        (ebb-screen-charset-g3 screen) 'us-ascii
        (ebb-screen-charset-active screen) 'g0)
  (remhash screen ebb--saved-cursor-renditions)
  (remhash screen ebb--alt-saved-cursor-renditions)
  (remhash screen ebb--horizontal-margins)
  (remhash screen ebb--reverse-wrap-modes)
  (remhash screen ebb--dec-protection-mode-screens)
  (remhash screen ebb--iso-protection-mode-screens))

(defun ebb-screen-reset (screen)
  "Full terminal reset (RIS)."
  (when (gethash screen ebb--column-mode-screens)
    (ebb-screen-resize screen 80 (ebb-screen-height screen)))
  (remhash screen ebb--column-mode-screens)
  (remhash screen ebb--horizontal-margins)
  (remhash screen ebb--reverse-wrap-modes)
  (remhash screen ebb--dec-protection-mode-screens)
  (remhash screen ebb--iso-protection-mode-screens)
  (let ((w (ebb-screen-width screen))
        (h (ebb-screen-height screen)))
    ;; Leave alt screen if active
    (when (ebb-screen-alt-screen screen)
      (setf (ebb-screen-alt-screen screen) nil))
    ;; Reset lines
    (let ((lines (make-vector h nil)))
      (dotimes (i h)
        (aset lines i (ebb--make-empty-line w)))
      (setf (ebb-screen-lines screen) lines))
    (setf (ebb-screen-line-start screen) 0)
    ;; Reset cursor
    (setf (ebb-screen-cursor-x screen) 0)
    (setf (ebb-screen-cursor-y screen) 0)
    (setf (ebb-screen-cursor-saved-x screen) 0)
    (setf (ebb-screen-cursor-saved-y screen) 0)
    (setf (ebb-screen-cursor-saved-attr screen) nil)
    (remhash screen ebb--saved-cursor-renditions)
    (remhash screen ebb--alt-saved-cursor-renditions)
    (setf (ebb-screen-cursor-style screen) :block)
    (setf (ebb-screen-cursor-visible screen) t)
    (setf (ebb-screen-pending-wrap screen) nil)
    ;; Reset attrs
    (setf (ebb-screen-current-attr screen) (make-ebb-attr))
    ;; Reset scroll region
    (setf (ebb-screen-scroll-top screen) 0)
    (setf (ebb-screen-scroll-bottom screen) (1- h))
    ;; Reset modes
    (setf (ebb-screen-auto-wrap screen) t)
    (setf (ebb-screen-insert-mode screen) nil)
    (setf (ebb-screen-origin-mode screen) nil)
    (setf (ebb-screen-keypad-mode screen) nil)
    (setf (ebb-screen-bracketed-paste screen) nil)
    (setf (ebb-screen-mouse-mode screen) nil)
    (setf (ebb-screen-mouse-sgr screen) nil)
    (setf (ebb-screen-focus-events screen) nil)
    ;; Reset charsets
    (setf (ebb-screen-charset-g0 screen) 'us-ascii)
    (setf (ebb-screen-charset-g1 screen) 'us-ascii)
    (setf (ebb-screen-charset-g2 screen) 'us-ascii)
    (setf (ebb-screen-charset-g3 screen) 'us-ascii)
    (setf (ebb-screen-charset-active screen) 'g0)
    ;; Reset scrollback
    (ebb--history-clear screen)
    ;; Reset tab stops
    (setf (ebb-screen-tab-stops screen)
          (ebb--default-tab-stops w))
    ;; Reset last char
    (setf (ebb-screen-last-char screen) nil)
    ;; Everything is dirty
    (setf (ebb-screen-dirty-lines screen)
          (number-sequence 0 (1- h)))
    (setf (ebb-screen-dirty-map screen) (make-vector h t))
    (setf (ebb-screen-dirty-count screen) h)))

;;;; ---- Character Set Designation --------------------------------------

(defun ebb-screen-designate-charset (screen slot charset-char)
  "Designate character set for SLOT (g0-g3) from CHARSET-CHAR."
  (let ((cs (pcase charset-char
              (?0 'dec-graphics)
              (?B 'us-ascii)
              (?A 'uk)
              (_  'us-ascii))))
    (pcase slot
      (?\( (setf (ebb-screen-charset-g0 screen) cs))
      ((or ?\) ?-) (setf (ebb-screen-charset-g1 screen) cs))
      ((or ?* ?.) (setf (ebb-screen-charset-g2 screen) cs))
      ((or ?+ ?/) (setf (ebb-screen-charset-g3 screen) cs)))))

(defun ebb-screen-shift-out (screen)
  "Invoke G1 character set (SO)."
  (setf (ebb-screen-charset-active screen) 'g1))

(defun ebb-screen-shift-in (screen)
  "Invoke G0 character set (SI)."
  (setf (ebb-screen-charset-active screen) 'g0))

;;;; ---- Plain Text -----------------------------------------------------

(defun ebb--line-plain-text (line width)
  "Return LINE as plain text with terminal padding removed."
  (let ((cells (ebb--line-ensure-cells line width))
        parts)
    (dotimes (i width)
      (let ((cell (aref cells i)))
        (unless (zerop (ebb-cell-width cell))
          (push (concat (string (ebb-cell-char cell))
                        (ebb-cell-combining cell))
                parts))))
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun ebb-screen-plain-text (screen)
  "Return scrollback and viewport text from SCREEN.
Rows joined by a soft wrap have no intervening newline."
  (let* ((width (ebb-screen-width screen))
         (lines (append (ebb-screen-scrollback-lines-raw screen)
                        (append (ebb--ordered-lines-vector screen) nil)))
         parts)
    (dolist (line lines)
      (push (ebb--line-plain-text line width) parts)
      (unless (ebb-line-wrapped line)
        (push "\n" parts)))
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun ebb-screen-virtual-line (screen row)
  "Return physical ROW across SCREEN history and live viewport."
  (let ((history-rows (ebb-screen-history-row-count screen)))
    (if (< row history-rows)
        (ebb-screen-history-render-row screen row)
      (ebb-screen-get-line screen (- row history-rows)))))

(defun ebb--cells-text-range (cells start end)
  "Return visible text from CELLS columns START through END."
  (let (parts)
    (cl-loop for column from start below (min end (length cells))
             for cell = (aref cells column)
             unless (zerop (ebb-cell-width cell))
             do (push (concat (string (ebb-cell-char cell))
                              (ebb-cell-combining cell))
                      parts))
    (apply #'concat (nreverse parts))))

(defun ebb-screen-text-range (screen start end)
  "Return plain text between virtual locations START and END.
Locations are (ROW . COLUMN) pairs."
  (when (or (> (car start) (car end))
            (and (= (car start) (car end)) (> (cdr start) (cdr end))))
    (cl-rotatef start end))
  (let ((width (ebb-screen-width screen))
        parts)
    (cl-loop for row from (car start) to (car end)
             for line = (ebb-screen-virtual-line screen row)
             when line do
             (let* ((from (if (= row (car start)) (cdr start) 0))
                    (to (if (= row (car end)) (cdr end) width))
                    (text (ebb--cells-text-range
                           (ebb--line-ensure-cells line width) from to)))
               (push (if (and (= to width) (not (ebb-line-wrapped line)))
                         (string-trim-right text)
                       text)
                     parts)
               (when (and (< row (car end))
                          (not (ebb-line-wrapped line)))
                 (push "\n" parts))))
    (apply #'concat (nreverse parts))))

;;;; ---- Rectangular Operations -----------------------------------------

(defun ebb-screen--invalidate-rect-line (screen row line)
  "Invalidate cached representations of LINE after changing ROW on SCREEN."
  (setf (ebb-line-text line) nil
        (ebb-line-attr-runs line) nil
        (ebb-line-uniform-attr line) nil
        (ebb-line-rendered line) nil
        (ebb-line-dirty line) t)
  (ebb--mark-dirty screen row))

(defun ebb-screen-fill-rect (screen char top left bottom right)
  "Fill the inclusive rectangle on SCREEN with CHAR.
Coordinates are zero-based, clipped, and validated by the caller."
  (let ((width (ebb-screen-width screen))
        (attr (and (ebb-screen-current-attr screen)
                   (ebb-attr-copy (ebb-screen-current-attr screen))))
        (dec (and (gethash screen ebb--dec-protection-mode-screens) t))
        (iso (and (gethash screen ebb--iso-protection-mode-screens) t)))
    (cl-loop for row from top to bottom
             for line = (ebb--line-at screen row)
             for cells = (ebb--line-ensure-cells line width)
             for initialized = (ebb--line-initialized-cells line cells width)
             for dec-bits = (ebb--line-protection-bits
                             ebb--dec-protected-cells line width)
             for iso-bits = (ebb--line-protection-bits
                             ebb--iso-protected-cells line width)
             do (ebb--clear-wide-char-at screen row left)
             and do (ebb--clear-wide-char-at screen row right)
             and do (cl-loop for column from left to right
                             do (aset cells column
                                      (make-ebb-cell :char char :width 1
                                                     :attr attr))
                         and do (aset initialized column t)
                         and do (aset dec-bits column dec)
                         and do (aset iso-bits column iso))
             do (ebb-screen--invalidate-rect-line screen row line))))

(defun ebb-screen-erase-rect (screen top left bottom right &optional selective)
  "Erase the inclusive rectangle on SCREEN.
When SELECTIVE is non-nil, preserve DEC-protected cells.  ISO protection does
not affect this operation, matching DECSERA semantics."
  (let ((width (ebb-screen-width screen))
        (erase-cell (ebb--make-erase-cell screen)))
    (cl-loop for row from top to bottom
             for line = (ebb--line-at screen row)
             for cells = (ebb--line-ensure-cells line width)
             for initialized = (ebb--line-initialized-cells line cells width)
             for dec-bits = (ebb--line-protection-bits
                             ebb--dec-protected-cells line width)
             for iso-bits = (ebb--line-protection-bits
                             ebb--iso-protected-cells line width)
             do (ebb--clear-wide-char-at screen row left)
             and do (ebb--clear-wide-char-at screen row right)
             and do (cl-loop for column from left to right
                             unless (and selective (aref dec-bits column))
                             do (aset cells column (copy-ebb-cell erase-cell))
                             and do (aset initialized column t)
                             and do (aset iso-bits column nil))
             do (ebb-screen--invalidate-rect-line screen row line))))

(defun ebb-screen-copy-rect
    (screen source-top source-left source-bottom source-right
            destination-top destination-left)
  "Copy an inclusive rectangle on SCREEN to DESTINATION-TOP/LEFT.
All coordinates are zero-based and source coordinates are already clipped."
  (let* ((width (ebb-screen-width screen))
         (height (- source-bottom source-top -1))
         (columns (- source-right source-left -1))
         snapshots)
    ;; Snapshot first so overlapping copies behave as if done simultaneously.
    (dotimes (row-offset height)
      (let* ((line (ebb--line-at screen (+ source-top row-offset)))
             (cells (ebb--line-ensure-cells line width))
             (initialized (ebb--line-initialized-cells line cells width))
             (dec-bits (ebb--line-protection-bits
                        ebb--dec-protected-cells line width))
             (iso-bits (ebb--line-protection-bits
                        ebb--iso-protected-cells line width))
             row)
        (dotimes (column-offset columns)
          (let ((column (+ source-left column-offset)))
            (push (list (copy-ebb-cell (aref cells column))
                        (aref initialized column)
                        (aref dec-bits column)
                        (aref iso-bits column))
                  row)))
        (push (nreverse row) snapshots)))
    (setq snapshots (nreverse snapshots))
    (cl-loop for snapshot-row in snapshots
             for row from destination-top
             while (< row (ebb-screen-height screen))
             when (>= row 0)
             do (let* ((line (ebb--line-at screen row))
                       (cells (ebb--line-ensure-cells line width))
                       (initialized (ebb--line-initialized-cells
                                     line cells width))
                       (dec-bits (ebb--line-protection-bits
                                  ebb--dec-protected-cells line width))
                       (iso-bits (ebb--line-protection-bits
                                  ebb--iso-protected-cells line width)))
                  (ebb--clear-wide-char-at screen row destination-left)
                  (ebb--clear-wide-char-at
                   screen row (+ destination-left (1- columns)))
                  (cl-loop for snapshot in snapshot-row
                           for column from destination-left
                           while (< column width)
                           when (>= column 0)
                           do (pcase-let ((`(,cell ,initialized-p
                                                   ,dec-protected-p
                                                   ,iso-protected-p)
                                           snapshot))
                                (aset cells column cell)
                                (aset initialized column initialized-p)
                                (aset dec-bits column dec-protected-p)
                                (aset iso-bits column iso-protected-p)))
                  (ebb-screen--invalidate-rect-line screen row line)))))

;;;; ---- Query ----------------------------------------------------------

(defun ebb-screen-checksum-rect (screen top left bottom right)
  "Return the DEC checksum for the inclusive rectangle on SCREEN.
TOP, LEFT, BOTTOM, and RIGHT are zero-based and already clipped.  Untouched
blank cells contribute zero, matching the original xterm/DEC checksum mode."
  (let ((sum 0))
    (cl-loop for row from top to bottom
             for line = (ebb--line-at screen row)
             for cells = (ebb--line-ensure-cells line (ebb-screen-width screen))
             for initialized = (ebb--line-initialized-cells
                                line cells (ebb-screen-width screen))
             do (cl-loop for column from left to right
                         for cell = (aref cells column)
                         for char = (ebb-cell-char cell)
                         unless (= (ebb-cell-width cell) 0)
                         do (setq sum
                                  (logand #xffff
                                          (+ sum
                                             (if (= char ?\s)
                                                 (if (aref initialized column)
                                                     ?\s 0)
                                               char))))))
    ;; DEC represents zero as 10000 rather than truncating it to four digits.
    (- #x10000 sum)))

(defun ebb-screen-get-line (screen row)
  "Return the ebb-line at ROW."
  (when (and (>= row 0) (< row (ebb-screen-height screen)))
    (let ((line (ebb--line-at screen row)))
      (ebb--line-ensure-cells line (ebb-screen-width screen))
      line)))

(defun ebb-screen-scrollback-lines-raw (screen)
  "Return current-width physical history rows, oldest first."
  (let ((count (ebb-screen-history-row-count screen))
        rows)
    (dotimes (row count)
      (let ((line (ebb-screen-history-render-row screen row)))
        ;; This compatibility query promises physical cell-backed rows; the
        ;; renderer itself consumes lazy text directly.
        (ebb--line-ensure-cells line (ebb-screen-width screen))
        (push line rows)))
    (nreverse rows)))

(defun ebb-screen-scrollback-lines (screen)
  "Return current-width physical history rows, oldest first."
  (ebb-screen-scrollback-lines-raw screen))

;;;; ---- Cell copying helper (used by erase) ----------------------------

(defun copy-ebb-cell (cell)
  "Return a shallow copy of CELL."
  (make-ebb-cell :char (ebb-cell-char cell)
                   :combining (ebb-cell-combining cell)
                   :width (ebb-cell-width cell)
                   :attr (ebb-cell-attr cell)))

(provide 'ebb-term)
;;; ebb-term.el ends here
