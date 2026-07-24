;;; ebb-render.el --- Buffer renderer for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders the ebb screen model into an Emacs buffer.
;; Only dirty lines are re-rendered.  The buffer is write-only from the
;; terminal's perspective -- undo is always disabled.
;;
;; Buffer layout:
;;   [bounded history slab...] display-begin [display lines...] display-end
;; Logical history remains in the screen model; only nearby rows are inserted.

;;; Code:

(require 'cl-lib)
(require 'ebb-term)

(defvar ebb-link-map)

;;;; ---- Color Tables ---------------------------------------------------

(defconst ebb-render--ansi-colors
  ["#000000" "#cd0000" "#00cd00" "#cdcd00"
   "#0000ee" "#cd00cd" "#00cdcd" "#e5e5e5"
   "#7f7f7f" "#ff0000" "#00ff00" "#ffff00"
   "#5c5cff" "#ff00ff" "#00ffff" "#ffffff"]
  "Standard 16 ANSI color values.")

;;;; ---- Per-color Named Faces (like eat) --------------------------------

;; Generate 256 named faces so users can customize individual colors
;; via their theme or custom settings.
(defgroup ebb-faces nil
  "Faces used by ebb terminal emulator."
  :group 'ebb)

(defun ebb-render--256color-hex (n)
  "Compute default hex color for 256-color palette index N."
  (cond
   ((< n 16)
    (aref ebb-render--ansi-colors n))
   ((< n 232)
    (let* ((idx (- n 16))
           (b-idx (% idx 6))
           (g-idx (% (/ idx 6) 6))
           (r-idx (/ idx 36))
           (vals [0 95 135 175 215 255]))
      (format "#%02x%02x%02x"
              (aref vals r-idx) (aref vals g-idx) (aref vals b-idx))))
   (t
    (let ((v (ebb--clamp (+ 8 (* 10 (- n 232))) 0 255)))
      (format "#%02x%02x%02x" v v v)))))

;; Define 256 named faces at compile/load time
(defvar ebb-render--color-faces (make-vector 256 nil)
  "Vector of face symbols for palette indices 0-255.")

(defvar ebb-render--indexed-color-cache (make-vector 256 nil)
  "Cached face-resolved color strings for indexed palette entries.")

(defvar ebb-render--attr-face-cache (make-hash-table :test #'equal)
  "Cache from immutable `ebb-attr' values to rendered face plists.")

(defconst ebb-render--attr-face-cache-limit 4096
  "Maximum number of cached attribute face specs before clearing the cache.")

(defvar ebb-render-after-refresh-hook nil
  "Hook run after refreshing an Ebb buffer, with the render state.")

;; Palette 0-15 inherit Emacs ansi-color faces (theme-aware), like eat/ghostel.
(defconst ebb-render--ansi-face-names
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Base names for ANSI colors 0-7 / bright 8-15.")

(dotimes (i 256)
  (let* ((sym (intern (format "ebb-color-%d" i)))
         (spec
          (cond
           ((< i 8)
            `((t :inherit
                 ,(intern (format "ansi-color-%s"
                                 (aref ebb-render--ansi-face-names i))))))
           ((< i 16)
            `((t :inherit
                 ,(intern (format "ansi-color-bright-%s"
                                 (aref ebb-render--ansi-face-names
                                       (- i 8)))))))
           (t
            (let ((hex (ebb-render--256color-hex i)))
              `((t :foreground ,hex :background ,hex)))))))
    (eval `(defface ,sym
             ',spec
             ,(format "Ebb color %d." i)
             :group 'ebb-faces))
    (aset ebb-render--color-faces i sym)))

;; Font faces (10 fonts, 0-9)
(defvar ebb-render--font-faces (make-vector 10 nil)
  "Vector of face symbols for font indices 0-9.")

(dotimes (i 10)
  (let ((sym (intern (format "ebb-font-%d" i))))
    (eval `(defface ,sym
             '((t :family nil))
             ,(format "Ebb font %d." i)
             :group 'ebb-faces))
    (aset ebb-render--font-faces i sym)))

(defun ebb-render--color-to-string (color)
  "Convert a color value to a hex string.
COLOR is nil (default), integer 0-255, or (R G B) list.
For indexed colors, uses the corresponding named face's foreground,
allowing theme/user customization."
  (cond
   ((null color) nil)
   ((listp color)
    (format "#%02x%02x%02x"
            (ebb--clamp (car color) 0 255)
            (ebb--clamp (cadr color) 0 255)
            (ebb--clamp (caddr color) 0 255)))
   ((and (integerp color) (<= 0 color 255))
    ;; Use the named face's foreground for customizability.
    ;; face-foreground can be a named color ("red3"); normalize to #RRGGBB
    ;; so inverse/render comparisons stay stable.  Fall back through the
    ;; inherit chain (ansi-color-*) then the xterm hex table.
    (or (aref ebb-render--indexed-color-cache color)
        (let* ((face-sym (aref ebb-render--color-faces color))
               (raw (or (face-foreground face-sym nil t)
                        (ebb-render--256color-hex color)))
               (value
                (cond
                 ;; Already hex — keep it. color-values is lossy on TTY/batch.
                 ((and (stringp raw) (string-prefix-p "#" raw) (= (length raw) 7))
                  raw)
                 ((and (stringp raw) (color-values raw))
                  (apply #'format "#%02x%02x%02x"
                         (mapcar (lambda (c) (ash c -8))
                                 (color-values raw))))
                 (t raw))))
          (aset ebb-render--indexed-color-cache color value)
          value)))
   (t nil)))

;;;; ---- Faces ----------------------------------------------------------

(defface ebb-bold    '((t :weight bold))       "Bold text."    :group 'ebb)
(defface ebb-faint   '((t :weight light))      "Faint text."   :group 'ebb)
(defface ebb-italic  '((t :slant italic))      "Italic text."  :group 'ebb)
(defface ebb-crossed '((t :strike-through t))  "Struck text."  :group 'ebb)

(defface ebb-default
  '((t :inherit default))
  "Base face for default text in ebb terminal buffers.
Customize this to give ebb buffers a different default foreground,
background, font, or size than the rest of Emacs."
  :group 'ebb)

(defface ebb-cursor
  '((t :inverse-video t))
  "Face for the terminal cursor."
  :group 'ebb)

;;;; ---- Render State ---------------------------------------------------

(cl-defstruct (ebb-render-state (:copier nil))
  "State for the buffer renderer."
  (screen nil)           ; ebb-screen
  (buffer nil)           ; Emacs buffer
  (region-begin nil)     ; marker at start of terminal region
  (region-end nil)       ; marker at end of terminal region
  (display-begin nil)    ; marker at start of display area
  (cursor-overlay nil)   ; overlay for cursor
  (scrollback-count 0)   ; history rows currently materialized
  (history-start-row 0)  ; absolute physical row of first materialized row
  (history-total-rows 0)
  (history-generation -1)
  (virtual-mark nil)
  (history-cache nil))

;;;; ---- Constructor ----------------------------------------------------

(defun ebb-render-create (screen buffer &optional begin end)
  "Create a render state for SCREEN displayed in BUFFER.

When BEGIN and END are non-nil, render only that buffer region.  This lets
Eshell retain everything outside an inline terminal."
  (let ((render (make-ebb-render-state
                 :screen screen :buffer buffer
                 :history-cache (make-hash-table :test #'equal)))
        (w (ebb-screen-width screen))
        (h (ebb-screen-height screen)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (unless begin (erase-buffer))
        (let ((region-begin (copy-marker (or begin (point-min))))
              (region-end (copy-marker (or end (point-max)))))
          (set-marker-insertion-type region-begin nil)
          (set-marker-insertion-type region-end t)
          (delete-region region-begin region-end)
          (goto-char region-begin)
          ;; Insert display lines (H lines of W spaces, newline-separated).
          (dotimes (i h)
            (insert (make-string w ?\s))
            (when (< i (1- h)) (insert "\n")))
          (setf (ebb-render-state-region-begin render) region-begin
                (ebb-render-state-region-end render) region-end)
          ;; New scrollback is inserted before this marker.
          (let ((m (copy-marker region-begin)))
            (set-marker-insertion-type m t)
            (setf (ebb-render-state-display-begin render) m))
          ;; Create cursor overlay (1 char wide at origin).
          (let ((ov (make-overlay region-begin (1+ region-begin) buffer)))
            (overlay-put ov 'face 'ebb-cursor)
            (overlay-put ov 'priority 100)
            (setf (ebb-render-state-cursor-overlay render) ov)))))
    render))

;;;; ---- Main Refresh ---------------------------------------------------

(defun ebb-render-refresh (render)
  "Refresh the buffer from the screen model.
Only dirty display lines are re-rendered, but scrollback and cursor
state are reconciled independently so metadata-only updates are visible."
  (let* ((screen (ebb-render-state-screen render))
         (buffer (ebb-render-state-buffer render))
         (dirty (ebb-screen-get-dirty screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((preserve-view
                (eq (bound-and-true-p ebb--input-mode) 'emacs))
               (saved-point (and preserve-view
                                 (ebb-render-buffer-anchor render (point))))
               (saved-mark (and preserve-view (mark t)
                                (ebb-render-buffer-anchor render (mark t))))
               (saved-mark-active (and preserve-view mark-active))
               (saved-window
                (and preserve-view
                     (eq (window-buffer (selected-window)) buffer)
                     (selected-window)))
               (saved-window-start
                (and saved-window
                     (ebb-render-buffer-anchor
                      render (window-start saved-window)))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (buffer-undo-list t))
            ;; Reconcile scrollback independently of dirty display rows.
            (ebb-render--update-scrollback render)
            ;; Render dirty display lines.
            (dolist (row dirty)
              (ebb-render--update-line render row))
            ;; Cursor movement/visibility/style can change without dirtying a row.
            (ebb-render--update-cursor render))
          (when preserve-view
            (ebb-render-goto-anchor render saved-point t)
            (if saved-mark
                (progn
                  (save-excursion
                    (ebb-render-goto-anchor render saved-mark t)
                    (set-marker (mark-marker) (point)))
                  (setq mark-active saved-mark-active))
              (set-marker (mark-marker) nil))
            (when (window-live-p saved-window)
              (save-excursion
                (ebb-render-goto-anchor render saved-window-start t)
                (set-window-start saved-window (point) t))))
          (run-hook-with-args 'ebb-render-after-refresh-hook render)))
      (ebb-screen-clear-dirty screen))))

;;;; ---- Scrollback Rendering -------------------------------------------

(defun ebb-render--update-scrollback (render)
  "Materialize the history range needed by every window showing RENDER."
  (let* ((screen (ebb-render-state-screen render))
         ;; Capture model anchors before replacing any materialized text.  In
         ;; particular, numeric row indices are not stable when old history is
         ;; trimmed.
         (windows
          (cl-loop for window in (get-buffer-window-list
                                  (ebb-render-state-buffer render) nil t)
                   collect (cons window
                                 (ebb-render-buffer-anchor
                                  render (window-start window)))))
         (point-anchor (ebb-render-buffer-anchor render (point)))
         (total (ebb-screen-history-row-count screen))
         (generation (ebb-screen-history-generation screen))
         (capacity (ebb-render--history-capacity render))
         (history-locations
          (delq nil
                (mapcar (lambda (entry)
                          (ebb-render--anchor-location
                           render (cdr entry) total))
                        windows)))
         (point-location (ebb-render--anchor-location
                          render point-anchor total))
         (history-rows
          (delq nil
                (mapcar (lambda (location)
                          (and (< (car location) total) (car location)))
                        (append history-locations (list point-location)))))
         ;; A buffer shared by several windows needs one contiguous slab that
         ;; covers all their starts.  Usually this remains CAPACITY rows; widely
         ;; separated windows intentionally expand it rather than corrupting one
         ;; another's view.
         (margin (max 1 (/ capacity 3)))
         (start (if history-rows
                    (max 0 (- (apply #'min history-rows) margin))
                  (max 0 (- total capacity))))
         (needed-end (if history-rows
                         (min total (+ (apply #'max history-rows)
                                       capacity))
                       total))
         (count (min (- total start)
                     (max capacity (- needed-end start)))))
    (when (or (/= generation
                  (ebb-render-state-history-generation render))
              (/= start (ebb-render-state-history-start-row render))
              (/= count (ebb-render-state-scrollback-count render))
              (/= total (ebb-render-state-history-total-rows render)))
      (ebb-render--rebuild-scrollback render start count total generation)
      (dolist (entry windows)
        (when-let ((position
                    (ebb-render--anchor-buffer-position
                     render (cdr entry))))
          (set-window-start (car entry) position t))))
    (ebb-screen-clear-scrollback-dirty screen)))

(defun ebb-render--history-capacity (render)
  "Return the usual bounded number of history rows for RENDER."
  (let* ((buffer (ebb-render-state-buffer render))
         (windows (get-buffer-window-list buffer nil t))
         (height (max 24 (cl-loop for window in windows
                                  maximize (window-body-height window)
                                  into maximum
                                  finally return (or maximum 0)))))
    (* 3 height)))

(defun ebb-render--anchor-location (render anchor total)
  "Return ANCHOR's current virtual location using history size TOTAL."
  (pcase anchor
    (`(history ,id ,offset)
     (ebb-screen-history-anchor-location
      (ebb-render-state-screen render) id offset))
    (`(viewport ,row ,column)
     (cons (+ total row) column))))

(defun ebb-render--anchor-buffer-position (render anchor)
  "Return ANCHOR's position when it is present in RENDER's current slab."
  (when-let ((location
              (ebb-render--anchor-location
               render anchor (ebb-render-state-history-total-rows render))))
    (let* ((row (car location))
           (column (cdr location))
           (total (ebb-render-state-history-total-rows render))
           (start (ebb-render-state-history-start-row render))
           (count (ebb-render-state-scrollback-count render)))
      (when (or (>= row total)
                (and (>= row start) (< row (+ start count))))
        (save-excursion
          (if (< row total)
              (progn
                (goto-char (ebb-render-state-region-begin render))
                (forward-line (- row start)))
            (goto-char (ebb-render-state-display-begin render))
            (forward-line (- row total)))
          (move-to-column column)
          (point))))))

(defun ebb-render--history-row-string (render row)
  "Return cached rendered history ROW for RENDER."
  (let* ((screen (ebb-render-state-screen render))
         (width (ebb-screen-width screen))
         (location (ebb-screen-history-row-location screen row))
         (logical (car location))
         (key (list (ebb-history-line-id logical)
                    (ebb-history-line-generation logical)
                    (cdr location) width))
         (cache (ebb-render-state-history-cache render)))
    (or (gethash key cache)
        (let* ((line (ebb-screen-history-render-row screen row))
               (string (concat
                        (ebb-render--apply-line-metadata
                         line
                         (ebb-render--line-to-string-scrollback line width)
                         width)
                        "\n")))
          (put-text-property 0 (length string) 'ebb-history-id
                             (ebb-history-line-id logical) string)
          (put-text-property 0 (length string) 'ebb-history-offset
                             (cdr location) string)
          (when (> (hash-table-count cache)
                   (* 2 (ebb-render--history-capacity render)))
            (clrhash cache))
          (puthash key string cache)
          string))))

(defun ebb-render--rebuild-scrollback
    (render &optional start count total generation)
  "Replace RENDER's bounded history slab from START for COUNT rows."
  (let* ((screen (ebb-render-state-screen render))
         (display-begin (ebb-render-state-display-begin render))
         (total (or total (ebb-screen-history-row-count screen)))
         (start (or start (max 0 (- total (ebb-render--history-capacity render)))))
         (count (or count (min (ebb-render--history-capacity render)
                               (- total start))))
         (generation (or generation (ebb-screen-history-generation screen))))
    (save-excursion
      (delete-region (ebb-render-state-region-begin render) display-begin)
      (goto-char (ebb-render-state-region-begin render))
      (dotimes (offset count)
        (insert (ebb-render--history-row-string render (+ start offset)))))
    (setf (ebb-render-state-history-start-row render) start
          (ebb-render-state-scrollback-count render) count
          (ebb-render-state-history-total-rows render) total
          (ebb-render-state-history-generation render) generation)))

(defun ebb-render-scroll-history (render rows)
  "Scroll RENDER's selected window by virtual history ROWS."
  (let* ((window (selected-window))
         (screen (ebb-render-state-screen render))
         (total (ebb-screen-history-row-count screen))
         (display-begin (ebb-render-state-display-begin render))
         (slab-start (ebb-render-state-history-start-row render))
         (current
          (if (>= (window-start window) (marker-position display-begin))
              total
            (+ slab-start
               (- (line-number-at-pos (window-start window))
                  (line-number-at-pos
                   (ebb-render-state-region-begin render))))))
         (target (ebb--clamp (+ current rows) 0 total))
         (capacity (ebb-render--history-capacity render))
         (other-windows
          (cl-loop for other in (get-buffer-window-list
                                 (ebb-render-state-buffer render) nil t)
                   unless (eq other window)
                   collect (cons other
                                 (ebb-render-buffer-anchor
                                  render (window-start other)))))
         (other-rows
          (delq nil
                (mapcar
                 (lambda (entry)
                   (when-let ((location
                               (ebb-render--anchor-location
                                render (cdr entry) total)))
                     (and (< (car location) total) (car location))))
                 other-windows)))
         (wanted-rows (if (< target total)
                          (cons target other-rows)
                        other-rows))
         (start (if wanted-rows
                    (max 0 (- (apply #'min wanted-rows) (/ capacity 3)))
                  (max 0 (- total capacity))))
         (needed-end (if wanted-rows
                         (min total (+ (apply #'max wanted-rows) capacity))
                       total))
         (count (min (- total start)
                     (max capacity (- needed-end start))))
         (inhibit-read-only t)
         (inhibit-modification-hooks t)
         (buffer-undo-list t))
    (when (and mark-active (mark t))
      (setf (ebb-render-state-virtual-mark render)
            (ebb-render-buffer-location render (mark t))))
    (ebb-render--rebuild-scrollback
     render start count total (ebb-screen-history-generation screen))
    (dolist (entry other-windows)
      (when-let ((position
                  (ebb-render--anchor-buffer-position render (cdr entry))))
        (set-window-start (car entry) position t)))
    (if (= target total)
        (set-window-start window display-begin t)
      (save-excursion
        (goto-char (ebb-render-state-region-begin render))
        (forward-line (- target start))
        (set-window-start window (point) t)))
    (unless (pos-visible-in-window-p (point) window)
      (goto-char (window-start window)))
    target))

(defun ebb-render-buffer-location (render &optional position)
  "Return virtual (ROW . COLUMN) for POSITION in RENDER's buffer."
  (let* ((position (or position (point)))
         (display-begin (marker-position
                         (ebb-render-state-display-begin render)))
         (region-begin (marker-position
                        (ebb-render-state-region-begin render)))
         (row (if (< position display-begin)
                  (+ (ebb-render-state-history-start-row render)
                     (- (line-number-at-pos position)
                        (line-number-at-pos region-begin)))
                (+ (ebb-render-state-history-total-rows render)
                   (- (line-number-at-pos position)
                      (line-number-at-pos display-begin)))))
         (column (save-excursion (goto-char position) (current-column))))
    (cons row column)))

(defun ebb-render-buffer-anchor (render &optional position)
  "Return stable model anchor for POSITION in RENDER's buffer."
  (let* ((position (or position (point)))
         (display-begin (marker-position
                         (ebb-render-state-display-begin render))))
    (if (< position display-begin)
        (save-excursion
          (goto-char position)
          (let* ((bol (line-beginning-position))
                 (id (get-text-property bol 'ebb-history-id))
                 (offset (get-text-property bol 'ebb-history-offset)))
            (and id (list 'history id (+ offset (current-column))))))
      (let ((location (ebb-render-buffer-location render position)))
        (list 'viewport
              (- (car location)
                 (ebb-render-state-history-total-rows render))
              (cdr location))))))

(defun ebb-render-goto-anchor (render anchor &optional no-recenter)
  "Move point to stable model ANCHOR in RENDER."
  (pcase anchor
    (`(history ,id ,offset)
     (when-let ((location
                 (ebb-screen-history-anchor-location
                  (ebb-render-state-screen render) id offset)))
       (ebb-render-goto-location
        render (car location) (cdr location) no-recenter)))
    (`(viewport ,row ,column)
     (ebb-render-goto-location
      render (+ (ebb-screen-history-row-count
                 (ebb-render-state-screen render)) row)
      column no-recenter))))

(defun ebb-render-goto-location (render row column &optional no-recenter)
  "Move point to virtual ROW and COLUMN in RENDER.
When NO-RECENTER is non-nil, leave window positioning unchanged."
  (let* ((screen (ebb-render-state-screen render))
         (history-rows (ebb-screen-history-row-count screen))
         (capacity (ebb-render--history-capacity render)))
    (if (< row history-rows)
        (let* ((start (min (max 0 (- row (/ capacity 3)))
                           (max 0 (- history-rows capacity))))
               (count (min capacity (- history-rows start)))
               (inhibit-read-only t)
               (inhibit-modification-hooks t)
               (buffer-undo-list t))
          (ebb-render--rebuild-scrollback
           render start count history-rows
           (ebb-screen-history-generation screen))
          (goto-char (ebb-render-state-region-begin render))
          (forward-line (- row start)))
      (goto-char (ebb-render-state-display-begin render))
      (forward-line (- row history-rows)))
    (move-to-column column)
    (unless no-recenter (recenter))
    (point)))

(defun ebb-render--line-to-string-scrollback (line width)
  "Convert LINE for scrollback rendering."
  (when (ebb-line-text line)
    (setf (ebb-line-text line)
          (ebb-render--safe-string (ebb-line-text line))))
  (cond
   ((and (ebb-line-text line)
         (ebb-line-attr-runs line)
         (= (length (ebb-line-text line)) width))
    (setf (ebb-line-dirty line) nil)
    (ebb-render--text-runs-to-string line width))
   ((and (ebb-line-text line)
         (= (length (ebb-line-text line)) width))
    (setf (ebb-line-dirty line) nil)
    (if-let ((attr (ebb-line-uniform-attr line)))
        (let ((s (copy-sequence (ebb-line-text line))))
          (ebb-render--apply-attr-properties s attr)
          s)
      (ebb-line-text line)))
   ((ebb-render--cells-to-string-scrollback-fast (ebb-line-cells line) width))
   (t
    (ebb-render--line-to-string line width))))

(defun ebb-render--cells-to-string-scrollback-fast (cells width)
  "Return unstyled CELLS as visible scrollback text, or nil if styled."
  (catch 'styled
    (let ((s (make-string width ?\s t))
          (i 0)
          (pos 0)
          (cols 0))
      (while (< i width)
        (let* ((cell (aref cells i))
               (cw (ebb-cell-width cell)))
          (when (or (ebb-cell-attr cell)
                    (ebb-cell-combining cell))
            (throw 'styled nil))
          (cond
           ((zerop cw)
            (cl-incf i))
           (t
            (aset s pos (ebb-render--safe-char (ebb-cell-char cell)))
            (cl-incf pos)
            (cl-incf cols cw)
            (cl-incf i cw)))))
      (substring s 0 (+ pos (max 0 (- width cols)))))))

(defun ebb-render--line-to-string (line width)
  "Convert LINE to a string of WIDTH terminal columns."
  (when (ebb-line-text line)
    (setf (ebb-line-text line)
          (ebb-render--safe-string (ebb-line-text line))))
  (cond
   ((and (ebb-line-text line)
         (ebb-line-attr-runs line)
         (= (length (ebb-line-text line)) width))
    (let ((s (ebb-render--text-runs-to-string line width)))
      (setf (ebb-line-rendered line) s)
      (setf (ebb-line-dirty line) nil)
      s))
   ((and (ebb-line-text line)
         (ebb-line-uniform-attr line)
         (= (length (ebb-line-text line)) width))
    (let ((s (copy-sequence (ebb-line-text line))))
      (ebb-render--apply-attr-properties s (ebb-line-uniform-attr line))
      (setf (ebb-line-rendered line) s)
      (setf (ebb-line-dirty line) nil)
      s))
   ((and (ebb-line-text line)
         (= (length (ebb-line-text line)) width))
    (setf (ebb-line-dirty line) nil)
    (ebb-line-text line))
   ((and (not (ebb-line-dirty line))
         (ebb-line-rendered line)
         (= (length (ebb-line-rendered line)) width))
    (ebb-line-rendered line))
   (t
    (let ((s (ebb-render--cells-to-string (ebb-line-cells line) width)))
      (setf (ebb-line-rendered line) s)
      (setf (ebb-line-dirty line) nil)
      s))))

(defun ebb-render--text-runs-to-string (line width)
  "Render LINE's text plus attribute runs into a propertized string."
  (let ((s (copy-sequence (ebb-line-text line))))
    (dolist (run (ebb-line-attr-runs line))
      (let* ((begin (nth 0 run))
             (end (min width (nth 1 run)))
             (part (substring s begin end)))
        (ebb-render--apply-attr-properties part (nth 2 run))
        (setf (substring s begin end) part)))
    s))

(defun ebb-render--apply-line-metadata (line string width)
  "Apply shell metadata from LINE to STRING of WIDTH columns."
  (if (not (or (ebb-line-prompt-begins line)
               (ebb-line-prompt-ends line)))
      string
    (let ((result (copy-sequence string)))
      (dolist (column (ebb-line-prompt-begins line))
        (when (< column width)
          (put-text-property column (1+ column)
                             'ebb-shell-prompt-begin t result)))
      (dolist (column (ebb-line-prompt-ends line))
        (let ((position (max 0 (1- column))))
          (when (< position width)
            (put-text-property position (1+ position)
                               'ebb-shell-prompt-end t result))))
      result)))

;;;; ---- Display Line Rendering -----------------------------------------

(defun ebb-render--update-line (render row)
  "Re-render display line ROW in the buffer."
  (let* ((screen (ebb-render-state-screen render))
         (line (ebb--line-at screen row))
         (width (ebb-screen-width screen))
         (display-begin (ebb-render-state-display-begin render)))
    (when line
      (save-excursion
        (goto-char display-begin)
        (forward-line row)
        (let* ((bol (point))
               (eol (min (line-end-position)
                         (marker-position
                          (ebb-render-state-region-end render))))
               (new (ebb-render--apply-line-metadata
                     line (ebb-render--line-to-string line width) width)))
          ;; TUI programs often repaint rows with identical content.  Avoid a
          ;; buffer modification when the rendered text/properties are already
          ;; correct; cursor overlay movement is handled separately.
          (unless (and (= (- eol bol) (length new))
                       (equal-including-properties
                        new (buffer-substring bol eol)))
            (let ((display-start (marker-position display-begin)))
              (delete-region bol eol)
              (insert new)
              ;; Replacing row zero must not move the marker that separates
              ;; scrollback from the terminal viewport.
              (set-marker display-begin display-start))))))))

;;;; ---- Cell-to-String Conversion --------------------------------------

(defun ebb-render--safe-char (char)
  "Return a renderable Unicode version of CHAR."
  (ebb--normalize-display-char char))

(defun ebb-render--safe-string (string)
  "Return STRING as multibyte Unicode with internal raw bytes replaced."
  (let ((length (length string))
        (index 0))
    (while (and (< index length) (<= (aref string index) #x10ffff))
      (cl-incf index))
    (if (= index length)
        (if (multibyte-string-p string) string (string-to-multibyte string))
      (let ((result (make-string length ?\s t)))
        (dotimes (position length result)
          (aset result position
                (ebb-render--safe-char (aref string position))))))))

(defun ebb-render--cells-to-string (cells width)
  "Convert a vector of ebb-cells to a propertized string.
Handles double-width characters by inserting invisible spacers."
  (or (ebb-render--cells-to-string-fast cells width)
      (ebb-render--cells-to-string-uniform cells width)
      (ebb-render--cells-to-string-general cells width)))

(defun ebb-render--apply-attr-properties (string attr)
  "Apply face and hyperlink properties from ATTR to STRING."
  (when-let ((face (ebb-render--attr-to-face attr)))
    (put-text-property 0 (length string) 'face face string))
  (when-let ((uri (and attr (ebb-attr-hyperlink attr))))
    (put-text-property 0 (length string) 'help-echo uri string)
    (put-text-property 0 (length string) 'mouse-face 'highlight string)
    (put-text-property 0 (length string) 'keymap ebb-link-map string)
    (put-text-property 0 (length string) 'ebb-link-id
                       (ebb-attr-hyperlink-id attr) string))
  string)

(defun ebb-render--cells-to-string-fast (cells width)
  "Fast path for default-attribute CELLS, or nil if styled.
Handles both single-width and wide characters without consing per-cell run
lists; falls back only when a styled cell is present."
  (catch 'styled
    (let ((s (make-string width ?\s t))
          (i 0)
          (pos 0))
      (while (< i width)
        (let* ((cell (aref cells i))
               (cw (ebb-cell-width cell)))
          (when (or (ebb-cell-attr cell)
                    (ebb-cell-combining cell))
            (throw 'styled nil))
          (cond
           ((zerop cw)
            (cl-incf i))
           ((> cw 1)
            (aset s pos (ebb-render--safe-char (ebb-cell-char cell)))
            (let ((end (min width (+ pos cw))))
              (when (< (1+ pos) end)
                (put-text-property (1+ pos) end 'invisible t s)
                (put-text-property (1+ pos) end 'ebb-wide-spacer t s)))
            (cl-incf pos cw)
            (cl-incf i cw))
           (t
            (aset s pos (ebb-render--safe-char (ebb-cell-char cell)))
            (cl-incf pos)
            (cl-incf i)))))
      s)))

(defun ebb-render--cells-to-string-uniform (cells width)
  "Fast path for single-width rows with one shared/equal attribute."
  (let ((attr (ebb-cell-attr (aref cells 0)))
        (i 0))
    (when attr
      (while (and (< i width)
                  (let ((cell (aref cells i)))
                    (and (= (ebb-cell-width cell) 1)
                         (not (ebb-cell-combining cell))
                         (or (eq (ebb-cell-attr cell) attr)
                             (equal (ebb-cell-attr cell) attr)))))
        (cl-incf i))
      (when (= i width)
        (let ((s (make-string width ?\s t)))
          (dotimes (j width)
            (aset s j (ebb-render--safe-char
                       (ebb-cell-char (aref cells j)))))
          (ebb-render--apply-attr-properties s attr)
          s)))))

(defun ebb-render--cells-to-string-general (cells width)
  "General propertized conversion for CELLS."
  (let ((parts nil)
        (i 0))
    ;; Process cells one at a time, grouping single-width same-attr runs
    (while (< i width)
      (let* ((cell (aref cells i))
             (cw (ebb-cell-width cell)))
        (cond
         ;; Zero-width continuation cell: skip (already handled by wide char)
         ((zerop cw)
          (cl-incf i))
         ;; Double-width character: emit char + invisible spacer
         ((> cw 1)
          (let* ((ch (ebb-render--safe-char (ebb-cell-char cell)))
                 (attr (ebb-cell-attr cell))
                 (s (concat (string ch)
                            (ebb-render--safe-string
                             (or (ebb-cell-combining cell) ""))))
                 ;; Invisible spacers for the extra columns
                 (spacer (propertize (make-string (1- cw) ?\s)
                                     'invisible t
                                     'ebb-wide-spacer t)))
            (ebb-render--apply-attr-properties s attr)
            (ebb-render--apply-attr-properties spacer attr)
            (push s parts)
            (push spacer parts))
          ;; Skip the char cell and its continuation cells
          (cl-incf i cw))
         ;; Normal single-width: emit grapheme cells, grouping simple runs.
         (t
          (let ((attr (ebb-cell-attr cell)))
            (if (ebb-cell-combining cell)
                (let ((s (concat
                          (string (ebb-render--safe-char
                                   (ebb-cell-char cell)))
                          (ebb-render--safe-string
                           (ebb-cell-combining cell))))
                      (run-attr attr))
                  (ebb-render--apply-attr-properties s run-attr)
                  (push s parts)
                  (cl-incf i))
              (let ((chars nil))
                (while (and (< i width)
                            (let ((c (aref cells i)))
                              (and (= (ebb-cell-width c) 1)
                                   (not (ebb-cell-combining c))
                                   (equal (ebb-cell-attr c) attr))))
                  (push (ebb-render--safe-char
                         (ebb-cell-char (aref cells i)))
                        chars)
                  (cl-incf i))
                (when chars
                  (let ((s (apply #'string (nreverse chars))))
                    (ebb-render--apply-attr-properties s attr)
                    (push s parts))))))))))
    ;; Build result and ensure it is exactly width display columns
    (let ((result (apply #'concat (nreverse parts))))
      ;; Pad with spaces if display width is short
      ;; Note: `string-width' accounts for wide chars
      (let ((dw (string-width result)))
        (if (< dw width)
            (concat result (make-string (- width dw) ?\s))
          result)))))

;;;; ---- Attribute to Face ----------------------------------------------

(defun ebb-render--attr-to-face (attr)
  "Convert ebb-attr ATTR to an Emacs face spec (plist or list)."
  (when (and attr (ebb--attr-non-default-p attr))
    (or (gethash attr ebb-render--attr-face-cache)
        (let ((face nil)
              (fg (ebb-attr-fg attr))
              (bg (ebb-attr-bg attr))
              (inv (ebb-attr-inverse attr)))
      ;; Handle inverse: swap fg/bg, using actual Emacs default colors
      ;; when fg or bg is nil (matches eat's behavior)
      (when inv
        (let* ((default-fg (or (ebb-render--color-to-string fg)
                               (face-foreground 'ebb-default nil t)
                               (face-foreground 'default nil t)
                               "#ffffff"))
               (default-bg (or (ebb-render--color-to-string bg)
                               (face-background 'ebb-default nil t)
                               (face-background 'default nil t)
                               "#000000")))
          ;; Swap: what was foreground becomes background and vice versa
          (setq face (plist-put face :foreground default-bg))
          (setq face (plist-put face :background default-fg))))
      ;; Foreground (non-inverse)
      (unless inv
        (when-let ((fg-str (ebb-render--color-to-string fg)))
          (setq face (plist-put face :foreground fg-str))))
      ;; Background (non-inverse)
      (unless inv
        (when-let ((bg-str (ebb-render--color-to-string bg)))
          (setq face (plist-put face :background bg-str))))
      ;; Underline
      (when-let ((ul (ebb-attr-underline attr)))
        (let ((ul-color (ebb-render--color-to-string
                         (ebb-attr-ul-color attr))))
          (setq face
                (plist-put
                 face :underline
                 (let ((style (pcase ul
                                ('double 'double-line)
                                ('curly  'wave)
                                ('dotted 'dots)
                                ('dashed 'dashes)
                                (_ 'line))))
                   (if (and (eq style 'line) (null ul-color))
                       t
                     `(:style ,style ,@(and ul-color `(:color ,ul-color)))))))))
      ;; Strikethrough
      (when (ebb-attr-crossed attr)
        (setq face (plist-put face :strike-through t)))
      ;; Conceal (make invisible by setting fg = bg)
      (when (ebb-attr-conceal attr)
        (let ((bg-str (or (plist-get face :background) "black")))
          (setq face (plist-put face :foreground bg-str))))
      ;; Bold/faint/italic/font via named faces so users can customize them.
      ;; Font 0 is the default face; only alternate fonts (1-9) need inherit.
      (let* ((font (ebb-attr-font attr))
             (inherit (delq nil
                            (list (and (ebb-attr-bold attr) 'ebb-bold)
                                  (and (ebb-attr-faint attr) 'ebb-faint)
                                  (and (ebb-attr-italic attr) 'ebb-italic)
                                  (and (integerp font) (<= 1 font 9)
                                       (aref ebb-render--font-faces font))))))
        (when inherit
          (setq face (plist-put face :inherit inherit))))
      ;; Return as anonymous face (a plist), caching because styled terminal
      ;; output tends to reuse a small set of attributes across many cells.
      (when (> (hash-table-count ebb-render--attr-face-cache)
               ebb-render--attr-face-cache-limit)
        (clrhash ebb-render--attr-face-cache))
      (puthash attr face ebb-render--attr-face-cache)
      face))))

;;;; ---- Cursor Rendering -----------------------------------------------

(defun ebb-render--cursor-type-for-style (style)
  "Map cursor STYLE keyword to Emacs `cursor-type' value."
  (pcase style
    (:block 'box)
    (:blinking-block 'box)
    (:underline '(hbar . 2))
    (:blinking-underline '(hbar . 2))
    (:bar '(bar . 2))
    (:blinking-bar '(bar . 2))
    (_ 'box)))

(defun ebb-render--cursor-blink-p (style)
  "Return non-nil if cursor STYLE should blink."
  (memq style '(:blinking-block :blinking-underline :blinking-bar)))

(defun ebb-render--update-cursor (render)
  "Update the cursor overlay position and visibility.

The terminal cursor is drawn with the `ebb-cursor' overlay.  Live
input modes hide the native Emacs cursor so only one caret is visible;
point/window-point still track the terminal cell for input and yank.
In `emacs' mode the native cursor follows point and the overlay is a
hint at the live terminal position."
  (let* ((screen (ebb-render-state-screen render))
         (ov (ebb-render-state-cursor-overlay render))
         (display-begin (ebb-render-state-display-begin render))
         (cx (ebb-screen-cursor-x screen))
         (cy (ebb-screen-cursor-y screen))
         (visible (ebb-screen-cursor-visible screen))
         (style (ebb-screen-cursor-style screen))
         (emacs-mode (eq (bound-and-true-p ebb--input-mode) 'emacs))
         (viewport-reset (ebb-screen-take-viewport-reset screen)))
    (when ov
      (if (not visible)
          (progn
            (overlay-put ov 'face nil)
            (setq-local cursor-type nil))
        (let (pos)
          (save-excursion
            (goto-char display-begin)
            (forward-line cy)
            (let* ((bol (point))
                   (eol (min (line-end-position)
                             (marker-position
                              (ebb-render-state-region-end render)))))
              (setq pos (min (+ bol cx) eol))
              (move-overlay ov pos
                            (min (1+ pos)
                                 (marker-position
                                  (ebb-render-state-region-end render))))))
          (overlay-put ov 'face 'ebb-cursor)
          (if emacs-mode
              (setq-local cursor-type
                          (ebb-render--cursor-type-for-style style))
            ;; Overlay is the only caret; native cursor would lag at an
            ;; old window-point and look like a second/wrong cursor.
            (setq-local cursor-type nil)
            (goto-char pos)
            (dolist (win (get-buffer-window-list nil nil t))
              (set-window-point win pos)
              (when viewport-reset
                (set-window-start win display-begin t))))
          (if (ebb-render--cursor-blink-p style)
              (blink-cursor-mode 1)
            (blink-cursor-mode -1)))))))

;;;; ---- Invalidation ---------------------------------------------------

(defun ebb-render--invalidate-screen-lines (screen)
  "Discard rendered face caches stored on every line in SCREEN."
  (dolist (line (append (ebb--ordered-lines-vector screen) nil))
    (setf (ebb-line-rendered line) nil)
    (setf (ebb-line-dirty line) t)))

(defun ebb-render--theme-changed (&rest _)
  "Clear theme-derived caches and fully redraw live Ebb render states."
  (fillarray ebb-render--indexed-color-cache nil)
  (clrhash ebb-render--attr-face-cache)
  (when (boundp 'ebb--render)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let ((render (buffer-local-value 'ebb--render buffer)))
          (ebb-render--invalidate-screen-lines
           (ebb-render-state-screen render))
          (clrhash (ebb-render-state-history-cache render))
          (ebb-render-full-reset render))))))

(defun ebb-render--after-load-theme (&rest _)
  "Emacs 28 fallback advice for invalidating colors after `load-theme'."
  (ebb-render--theme-changed))

(defun ebb-render--install-theme-invalidation ()
  "Install the available theme-change invalidation path."
  (if (boundp 'enable-theme-functions)
      (add-hook 'enable-theme-functions #'ebb-render--theme-changed)
    (unless (advice-member-p #'ebb-render--after-load-theme 'load-theme)
      (advice-add 'load-theme :after #'ebb-render--after-load-theme))))

(defun ebb-render-invalidate-all (render)
  "Mark all display lines as needing re-render."
  (let* ((screen (ebb-render-state-screen render))
         (h (ebb-screen-height screen)))
    (setf (ebb-screen-dirty-lines screen)
          (number-sequence 0 (1- h)))))

;;;; ---- Full Re-render (for resize) ------------------------------------

(defun ebb-render-full-reset (render)
  "Completely rebuild the buffer contents from the screen model.
Used after resize when the display area size has changed."
  (let* ((screen (ebb-render-state-screen render))
         (buffer (ebb-render-state-buffer render))
         (w (ebb-screen-width screen))
         (h (ebb-screen-height screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((preserve-view
                (eq (bound-and-true-p ebb--input-mode) 'emacs))
               (saved-point (and preserve-view
                                 (ebb-render-buffer-anchor render (point))))
               (saved-mark (and preserve-view (mark t)
                                (ebb-render-buffer-anchor render (mark t))))
               (saved-mark-active (and preserve-view mark-active))
               (saved-windows
                (cl-loop for window in (get-buffer-window-list buffer nil t)
                         collect (cons window
                                       (ebb-render-buffer-anchor
                                        render (window-start window))))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (buffer-undo-list t))
            (goto-char (ebb-render-state-region-begin render))
            (delete-region (point)
                           (ebb-render-state-region-end render))
          ;; Materialize a slab covering every window's stable model anchor.
          (let* ((total (ebb-screen-history-row-count screen))
                 (capacity (ebb-render--history-capacity render))
                 (rows
                  (delq nil
                        (mapcar
                         (lambda (entry)
                           (when-let ((location
                                       (ebb-render--anchor-location
                                        render (cdr entry) total)))
                             (and (< (car location) total) (car location))))
                         saved-windows)))
                 (start (if rows
                            (max 0 (- (apply #'min rows) (/ capacity 3)))
                          (max 0 (- total capacity))))
                 (needed-end (if rows
                                 (min total (+ (apply #'max rows) capacity))
                               total))
                 (count (min (- total start)
                             (max capacity (- needed-end start)))))
            (dotimes (offset count)
              (insert (ebb-render--history-row-string
                       render (+ start offset))))
            (setf (ebb-render-state-history-start-row render) start
                  (ebb-render-state-scrollback-count render) count
                  (ebb-render-state-history-total-rows render) total
                  (ebb-render-state-history-generation render)
                  (ebb-screen-history-generation screen)))
          ;; Place display-begin marker
          (let ((m (ebb-render-state-display-begin render)))
            (set-marker m (point))
            ;; Temporarily disable advance-on-insert so display lines
            ;; go AFTER the marker, not before it
            (set-marker-insertion-type m nil))
          ;; Insert display lines
          (dotimes (i h)
            (let ((line (ebb-screen-get-line screen i)))
              (insert (if line
                          (ebb-render--apply-line-metadata
                           line (ebb-render--line-to-string line w) w)
                        (make-string w ?\s)))
              (when (< i (1- h))
                (insert "\n"))))
          ;; Now re-enable advance-on-insert so future scrollback
          ;; insertions push the marker forward
          (set-marker-insertion-type
           (ebb-render-state-display-begin render) t)
          ;; Reset cursor overlay
          (let ((ov (ebb-render-state-cursor-overlay render))
                (begin (ebb-render-state-region-begin render)))
            (when ov
              (move-overlay ov begin (1+ begin))))
          ;; Update cursor
          (ebb-render--update-cursor render)
            ;; Clear dirty since we just rendered everything
            (ebb-screen-clear-dirty screen)
            (ebb-screen-clear-scrollback-dirty screen))
          (when preserve-view
            (ebb-render-goto-anchor render saved-point t)
            (if saved-mark
                (progn
                  (save-excursion
                    (ebb-render-goto-anchor render saved-mark t)
                    (set-marker (mark-marker) (point)))
                  (setq mark-active saved-mark-active))
              (set-marker (mark-marker) nil))
            )
          (dolist (entry saved-windows)
            (when (window-live-p (car entry))
              (if-let ((position
                        (ebb-render--anchor-buffer-position
                         render (cdr entry))))
                  (set-window-start (car entry) position t)
                (set-window-start
                 (car entry) (ebb-render-state-display-begin render) t)))))))))

(defun ebb-render-resize-height (render)
  "Rebuild only RENDER's viewport after a height-only terminal resize."
  (let* ((screen (ebb-render-state-screen render))
         (buffer (ebb-render-state-buffer render))
         (width (ebb-screen-width screen))
         (height (ebb-screen-height screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t)
              (display-begin (ebb-render-state-display-begin render)))
          (ebb-render--update-scrollback render)
          (save-excursion
            (goto-char display-begin)
            (delete-region (point) (ebb-render-state-region-end render))
            (set-marker-insertion-type display-begin nil)
            (dotimes (row height)
              (let ((line (ebb-screen-get-line screen row)))
                (insert (if line
                            (ebb-render--apply-line-metadata
                             line (ebb-render--line-to-string line width) width)
                          (make-string width ?\s)))
                (when (< row (1- height)) (insert "\n"))))
            (set-marker-insertion-type display-begin t))
          (ebb-render--update-cursor render)
          (ebb-screen-clear-dirty screen)
          (ebb-screen-clear-scrollback-dirty screen))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun ebb-render-destroy (render)
  "Clean up render state."
  (when-let ((ov (ebb-render-state-cursor-overlay render)))
    (delete-overlay ov))
  (dolist (m (list (ebb-render-state-display-begin render)
                   (ebb-render-state-region-begin render)
                   (ebb-render-state-region-end render)))
    (when m (set-marker m nil))))

(ebb-render--install-theme-invalidation)

(provide 'ebb-render)
;;; ebb-render.el ends here
