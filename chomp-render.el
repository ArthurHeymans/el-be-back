;;; chomp-render.el --- Buffer renderer for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Renders the chomp screen model into an Emacs buffer.
;; Only dirty lines are re-rendered.  The buffer is write-only from the
;; terminal's perspective -- undo is always disabled.
;;
;; Buffer layout:
;;   [bounded history slab...] display-begin [display lines...] display-end
;; Logical history remains in the screen model; only nearby rows are inserted.

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

;; Palette 0-15 inherit Emacs ansi-color faces (theme-aware), like eat/ghostel.
(defconst chomp-render--ansi-face-names
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Base names for ANSI colors 0-7 / bright 8-15.")

(dotimes (i 256)
  (let* ((sym (intern (format "chomp-color-%d" i)))
         (spec
          (cond
           ((< i 8)
            `((t :inherit
                 ,(intern (format "ansi-color-%s"
                                 (aref chomp-render--ansi-face-names i))))))
           ((< i 16)
            `((t :inherit
                 ,(intern (format "ansi-color-bright-%s"
                                 (aref chomp-render--ansi-face-names
                                       (- i 8)))))))
           (t
            (let ((hex (chomp-render--256color-hex i)))
              `((t :foreground ,hex :background ,hex)))))))
    (eval `(defface ,sym
             ',spec
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
    ;; Use the named face's foreground for customizability.
    ;; face-foreground can be a named color ("red3"); normalize to #RRGGBB
    ;; so inverse/render comparisons stay stable.  Fall back through the
    ;; inherit chain (ansi-color-*) then the xterm hex table.
    (or (aref chomp-render--indexed-color-cache color)
        (let* ((face-sym (aref chomp-render--color-faces color))
               (raw (or (face-foreground face-sym nil t)
                        (chomp-render--256color-hex color)))
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
          (aset chomp-render--indexed-color-cache color value)
          value)))
   (t nil)))

;;;; ---- Faces ----------------------------------------------------------

(defface chomp-bold    '((t :weight bold))       "Bold text."    :group 'chomp)
(defface chomp-faint   '((t :weight light))      "Faint text."   :group 'chomp)
(defface chomp-italic  '((t :slant italic))      "Italic text."  :group 'chomp)
(defface chomp-crossed '((t :strike-through t))  "Struck text."  :group 'chomp)

(defface chomp-default
  '((t :inherit default))
  "Base face for default text in chomp terminal buffers.
Customize this to give chomp buffers a different default foreground,
background, font, or size than the rest of Emacs."
  :group 'chomp)

(defface chomp-cursor
  '((t :inverse-video t))
  "Face for the terminal cursor."
  :group 'chomp)

;;;; ---- Render State ---------------------------------------------------

(cl-defstruct (chomp-render-state (:copier nil))
  "State for the buffer renderer."
  (screen nil)           ; chomp-screen
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

(defun chomp-render-create (screen buffer &optional begin end)
  "Create a render state for SCREEN displayed in BUFFER.

When BEGIN and END are non-nil, render only that buffer region.  This lets
Eshell retain everything outside an inline terminal."
  (let ((render (make-chomp-render-state
                 :screen screen :buffer buffer
                 :history-cache (make-hash-table :test #'equal)))
        (w (chomp-screen-width screen))
        (h (chomp-screen-height screen)))
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
          (setf (chomp-render-state-region-begin render) region-begin
                (chomp-render-state-region-end render) region-end)
          ;; New scrollback is inserted before this marker.
          (let ((m (copy-marker region-begin)))
            (set-marker-insertion-type m t)
            (setf (chomp-render-state-display-begin render) m))
          ;; Create cursor overlay (1 char wide at origin).
          (let ((ov (make-overlay region-begin (1+ region-begin) buffer)))
            (overlay-put ov 'face 'chomp-cursor)
            (overlay-put ov 'priority 100)
            (setf (chomp-render-state-cursor-overlay render) ov)))))
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
        (let* ((preserve-view
                (eq (bound-and-true-p chomp--input-mode) 'emacs))
               (saved-point (and preserve-view
                                 (chomp-render-buffer-anchor render (point))))
               (saved-mark (and preserve-view (mark t)
                                (chomp-render-buffer-anchor render (mark t))))
               (saved-mark-active (and preserve-view mark-active))
               (saved-window
                (and preserve-view
                     (eq (window-buffer (selected-window)) buffer)
                     (selected-window)))
               (saved-window-start
                (and saved-window
                     (chomp-render-buffer-anchor
                      render (window-start saved-window)))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (buffer-undo-list t))
            ;; Reconcile scrollback independently of dirty display rows.
            (chomp-render--update-scrollback render)
            ;; Render dirty display lines.
            (dolist (row dirty)
              (chomp-render--update-line render row))
            ;; Cursor movement/visibility/style can change without dirtying a row.
            (chomp-render--update-cursor render))
          (when preserve-view
            (chomp-render-goto-anchor render saved-point t)
            (if saved-mark
                (progn
                  (save-excursion
                    (chomp-render-goto-anchor render saved-mark t)
                    (set-marker (mark-marker) (point)))
                  (setq mark-active saved-mark-active))
              (set-marker (mark-marker) nil))
            (when (window-live-p saved-window)
              (save-excursion
                (chomp-render-goto-anchor render saved-window-start t)
                (set-window-start saved-window (point) t))))))
      (chomp-screen-clear-dirty screen))))

;;;; ---- Scrollback Rendering -------------------------------------------

(defun chomp-render--update-scrollback (render)
  "Materialize the history range needed by every window showing RENDER."
  (let* ((screen (chomp-render-state-screen render))
         ;; Capture model anchors before replacing any materialized text.  In
         ;; particular, numeric row indices are not stable when old history is
         ;; trimmed.
         (windows
          (cl-loop for window in (get-buffer-window-list
                                  (chomp-render-state-buffer render) nil t)
                   collect (cons window
                                 (chomp-render-buffer-anchor
                                  render (window-start window)))))
         (point-anchor (chomp-render-buffer-anchor render (point)))
         (total (chomp-screen-history-row-count screen))
         (generation (chomp-screen-history-generation screen))
         (capacity (chomp-render--history-capacity render))
         (history-locations
          (delq nil
                (mapcar (lambda (entry)
                          (chomp-render--anchor-location
                           render (cdr entry) total))
                        windows)))
         (point-location (chomp-render--anchor-location
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
                  (chomp-render-state-history-generation render))
              (/= start (chomp-render-state-history-start-row render))
              (/= count (chomp-render-state-scrollback-count render))
              (/= total (chomp-render-state-history-total-rows render)))
      (chomp-render--rebuild-scrollback render start count total generation)
      (dolist (entry windows)
        (when-let ((position
                    (chomp-render--anchor-buffer-position
                     render (cdr entry))))
          (set-window-start (car entry) position t))))
    (chomp-screen-clear-scrollback-dirty screen)))

(defun chomp-render--history-capacity (render)
  "Return the usual bounded number of history rows for RENDER."
  (let* ((buffer (chomp-render-state-buffer render))
         (windows (get-buffer-window-list buffer nil t))
         (height (max 24 (cl-loop for window in windows
                                  maximize (window-body-height window)
                                  into maximum
                                  finally return (or maximum 0)))))
    (* 3 height)))

(defun chomp-render--anchor-location (render anchor total)
  "Return ANCHOR's current virtual location using history size TOTAL."
  (pcase anchor
    (`(history ,id ,offset)
     (chomp-screen-history-anchor-location
      (chomp-render-state-screen render) id offset))
    (`(viewport ,row ,column)
     (cons (+ total row) column))))

(defun chomp-render--anchor-buffer-position (render anchor)
  "Return ANCHOR's position when it is present in RENDER's current slab."
  (when-let ((location
              (chomp-render--anchor-location
               render anchor (chomp-render-state-history-total-rows render))))
    (let* ((row (car location))
           (column (cdr location))
           (total (chomp-render-state-history-total-rows render))
           (start (chomp-render-state-history-start-row render))
           (count (chomp-render-state-scrollback-count render)))
      (when (or (>= row total)
                (and (>= row start) (< row (+ start count))))
        (save-excursion
          (if (< row total)
              (progn
                (goto-char (chomp-render-state-region-begin render))
                (forward-line (- row start)))
            (goto-char (chomp-render-state-display-begin render))
            (forward-line (- row total)))
          (move-to-column column)
          (point))))))

(defun chomp-render--history-row-string (render row)
  "Return cached rendered history ROW for RENDER."
  (let* ((screen (chomp-render-state-screen render))
         (width (chomp-screen-width screen))
         (location (chomp-screen-history-row-location screen row))
         (logical (car location))
         (key (list (chomp-history-line-id logical)
                    (chomp-history-line-generation logical)
                    (cdr location) width))
         (cache (chomp-render-state-history-cache render)))
    (or (gethash key cache)
        (let* ((line (chomp-screen-history-render-row screen row))
               (string (concat
                        (chomp-render--apply-line-metadata
                         line
                         (chomp-render--line-to-string-scrollback line width)
                         width)
                        "\n")))
          (put-text-property 0 (length string) 'chomp-history-id
                             (chomp-history-line-id logical) string)
          (put-text-property 0 (length string) 'chomp-history-offset
                             (cdr location) string)
          (when (> (hash-table-count cache)
                   (* 2 (chomp-render--history-capacity render)))
            (clrhash cache))
          (puthash key string cache)
          string))))

(defun chomp-render--rebuild-scrollback
    (render &optional start count total generation)
  "Replace RENDER's bounded history slab from START for COUNT rows."
  (let* ((screen (chomp-render-state-screen render))
         (display-begin (chomp-render-state-display-begin render))
         (total (or total (chomp-screen-history-row-count screen)))
         (start (or start (max 0 (- total (chomp-render--history-capacity render)))))
         (count (or count (min (chomp-render--history-capacity render)
                               (- total start))))
         (generation (or generation (chomp-screen-history-generation screen))))
    (save-excursion
      (delete-region (chomp-render-state-region-begin render) display-begin)
      (goto-char (chomp-render-state-region-begin render))
      (dotimes (offset count)
        (insert (chomp-render--history-row-string render (+ start offset)))))
    (setf (chomp-render-state-history-start-row render) start
          (chomp-render-state-scrollback-count render) count
          (chomp-render-state-history-total-rows render) total
          (chomp-render-state-history-generation render) generation)))

(defun chomp-render-scroll-history (render rows)
  "Scroll RENDER's selected window by virtual history ROWS."
  (let* ((window (selected-window))
         (screen (chomp-render-state-screen render))
         (total (chomp-screen-history-row-count screen))
         (display-begin (chomp-render-state-display-begin render))
         (slab-start (chomp-render-state-history-start-row render))
         (current
          (if (>= (window-start window) (marker-position display-begin))
              total
            (+ slab-start
               (- (line-number-at-pos (window-start window))
                  (line-number-at-pos
                   (chomp-render-state-region-begin render))))))
         (target (chomp--clamp (+ current rows) 0 total))
         (capacity (chomp-render--history-capacity render))
         (other-windows
          (cl-loop for other in (get-buffer-window-list
                                 (chomp-render-state-buffer render) nil t)
                   unless (eq other window)
                   collect (cons other
                                 (chomp-render-buffer-anchor
                                  render (window-start other)))))
         (other-rows
          (delq nil
                (mapcar
                 (lambda (entry)
                   (when-let ((location
                               (chomp-render--anchor-location
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
      (setf (chomp-render-state-virtual-mark render)
            (chomp-render-buffer-location render (mark t))))
    (chomp-render--rebuild-scrollback
     render start count total (chomp-screen-history-generation screen))
    (dolist (entry other-windows)
      (when-let ((position
                  (chomp-render--anchor-buffer-position render (cdr entry))))
        (set-window-start (car entry) position t)))
    (if (= target total)
        (set-window-start window display-begin t)
      (save-excursion
        (goto-char (chomp-render-state-region-begin render))
        (forward-line (- target start))
        (set-window-start window (point) t)))
    (unless (pos-visible-in-window-p (point) window)
      (goto-char (window-start window)))
    target))

(defun chomp-render-buffer-location (render &optional position)
  "Return virtual (ROW . COLUMN) for POSITION in RENDER's buffer."
  (let* ((position (or position (point)))
         (display-begin (marker-position
                         (chomp-render-state-display-begin render)))
         (region-begin (marker-position
                        (chomp-render-state-region-begin render)))
         (row (if (< position display-begin)
                  (+ (chomp-render-state-history-start-row render)
                     (- (line-number-at-pos position)
                        (line-number-at-pos region-begin)))
                (+ (chomp-render-state-history-total-rows render)
                   (- (line-number-at-pos position)
                      (line-number-at-pos display-begin)))))
         (column (save-excursion (goto-char position) (current-column))))
    (cons row column)))

(defun chomp-render-buffer-anchor (render &optional position)
  "Return stable model anchor for POSITION in RENDER's buffer."
  (let* ((position (or position (point)))
         (display-begin (marker-position
                         (chomp-render-state-display-begin render))))
    (if (< position display-begin)
        (save-excursion
          (goto-char position)
          (let* ((bol (line-beginning-position))
                 (id (get-text-property bol 'chomp-history-id))
                 (offset (get-text-property bol 'chomp-history-offset)))
            (and id (list 'history id (+ offset (current-column))))))
      (let ((location (chomp-render-buffer-location render position)))
        (list 'viewport
              (- (car location)
                 (chomp-render-state-history-total-rows render))
              (cdr location))))))

(defun chomp-render-goto-anchor (render anchor &optional no-recenter)
  "Move point to stable model ANCHOR in RENDER."
  (pcase anchor
    (`(history ,id ,offset)
     (when-let ((location
                 (chomp-screen-history-anchor-location
                  (chomp-render-state-screen render) id offset)))
       (chomp-render-goto-location
        render (car location) (cdr location) no-recenter)))
    (`(viewport ,row ,column)
     (chomp-render-goto-location
      render (+ (chomp-screen-history-row-count
                 (chomp-render-state-screen render)) row)
      column no-recenter))))

(defun chomp-render-goto-location (render row column &optional no-recenter)
  "Move point to virtual ROW and COLUMN in RENDER.
When NO-RECENTER is non-nil, leave window positioning unchanged."
  (let* ((screen (chomp-render-state-screen render))
         (history-rows (chomp-screen-history-row-count screen))
         (capacity (chomp-render--history-capacity render)))
    (if (< row history-rows)
        (let* ((start (min (max 0 (- row (/ capacity 3)))
                           (max 0 (- history-rows capacity))))
               (count (min capacity (- history-rows start)))
               (inhibit-read-only t)
               (inhibit-modification-hooks t)
               (buffer-undo-list t))
          (chomp-render--rebuild-scrollback
           render start count history-rows
           (chomp-screen-history-generation screen))
          (goto-char (chomp-render-state-region-begin render))
          (forward-line (- row start)))
      (goto-char (chomp-render-state-display-begin render))
      (forward-line (- row history-rows)))
    (move-to-column column)
    (unless no-recenter (recenter))
    (point)))

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
          (when (or (chomp-cell-attr cell)
                    (chomp-cell-combining cell))
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

(defun chomp-render--apply-line-metadata (line string width)
  "Apply shell metadata from LINE to STRING of WIDTH columns."
  (if (not (or (chomp-line-prompt-begins line)
               (chomp-line-prompt-ends line)))
      string
    (let ((result (copy-sequence string)))
      (dolist (column (chomp-line-prompt-begins line))
        (when (< column width)
          (put-text-property column (1+ column)
                             'chomp-shell-prompt-begin t result)))
      (dolist (column (chomp-line-prompt-ends line))
        (let ((position (max 0 (1- column))))
          (when (< position width)
            (put-text-property position (1+ position)
                               'chomp-shell-prompt-end t result))))
      result)))

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
               (eol (min (line-end-position)
                         (marker-position
                          (chomp-render-state-region-end render))))
               (new (chomp-render--apply-line-metadata
                     line (chomp-render--line-to-string line width) width)))
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
          (when (or (chomp-cell-attr cell)
                    (chomp-cell-combining cell))
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
                         (not (chomp-cell-combining cell))
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
                 (s (concat (string ch) (chomp-cell-combining cell)))
                 ;; Invisible spacers for the extra columns
                 (spacer (propertize (make-string (1- cw) ?\s)
                                     'invisible t
                                     'chomp-wide-spacer t)))
            (when face
              (put-text-property 0 (length s) 'face face s)
              (put-text-property 0 (length spacer) 'face face spacer))
            (push s parts)
            (push spacer parts))
          ;; Skip the char cell and its continuation cells
          (cl-incf i cw))
         ;; Normal single-width: emit grapheme cells, grouping simple runs.
         (t
          (let ((attr (chomp-cell-attr cell)))
            (if (chomp-cell-combining cell)
                (let ((s (concat (string (chomp-cell-char cell))
                                 (chomp-cell-combining cell)))
                      (face (chomp-render--attr-to-face attr)))
                  (when face
                    (put-text-property 0 (length s) 'face face s))
                  (push s parts)
                  (cl-incf i))
              (let ((chars nil))
                (while (and (< i width)
                            (let ((c (aref cells i)))
                              (and (= (chomp-cell-width c) 1)
                                   (not (chomp-cell-combining c))
                                   (equal (chomp-cell-attr c) attr))))
                  (push (chomp-cell-char (aref cells i)) chars)
                  (cl-incf i))
                (when chars
                  (let ((s (apply #'string (nreverse chars)))
                        (face (chomp-render--attr-to-face attr)))
                    (when face
                      (put-text-property 0 (length s) 'face face s))
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
                               (face-foreground 'chomp-default nil t)
                               (face-foreground 'default nil t)
                               "#ffffff"))
               (default-bg (or (chomp-render--color-to-string bg)
                               (face-background 'chomp-default nil t)
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
      ;; Underline
      (when-let ((ul (chomp-attr-underline attr)))
        (let ((ul-color (chomp-render--color-to-string
                         (chomp-attr-ul-color attr))))
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
      (when (chomp-attr-crossed attr)
        (setq face (plist-put face :strike-through t)))
      ;; Conceal (make invisible by setting fg = bg)
      (when (chomp-attr-conceal attr)
        (let ((bg-str (or (plist-get face :background) "black")))
          (setq face (plist-put face :foreground bg-str))))
      ;; Bold/faint/italic/font via named faces so users can customize them.
      ;; Font 0 is the default face; only alternate fonts (1-9) need inherit.
      (let* ((font (chomp-attr-font attr))
             (inherit (delq nil
                            (list (and (chomp-attr-bold attr) 'chomp-bold)
                                  (and (chomp-attr-faint attr) 'chomp-faint)
                                  (and (chomp-attr-italic attr) 'chomp-italic)
                                  (and (integerp font) (<= 1 font 9)
                                       (aref chomp-render--font-faces font))))))
        (when inherit
          (setq face (plist-put face :inherit inherit))))
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
  "Update the cursor overlay position and visibility.

The terminal cursor is drawn with the `chomp-cursor' overlay.  Live
input modes hide the native Emacs cursor so only one caret is visible;
point/window-point still track the terminal cell for input and yank.
In `emacs' mode the native cursor follows point and the overlay is a
hint at the live terminal position."
  (let* ((screen (chomp-render-state-screen render))
         (ov (chomp-render-state-cursor-overlay render))
         (display-begin (chomp-render-state-display-begin render))
         (cx (chomp-screen-cursor-x screen))
         (cy (chomp-screen-cursor-y screen))
         (visible (chomp-screen-cursor-visible screen))
         (style (chomp-screen-cursor-style screen))
         (emacs-mode (eq (bound-and-true-p chomp--input-mode) 'emacs))
         (viewport-reset (chomp-screen-take-viewport-reset screen)))
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
                              (chomp-render-state-region-end render)))))
              (setq pos (min (+ bol cx) eol))
              (move-overlay ov pos
                            (min (1+ pos)
                                 (marker-position
                                  (chomp-render-state-region-end render))))))
          (overlay-put ov 'face 'chomp-cursor)
          (if emacs-mode
              (setq-local cursor-type
                          (chomp-render--cursor-type-for-style style))
            ;; Overlay is the only caret; native cursor would lag at an
            ;; old window-point and look like a second/wrong cursor.
            (setq-local cursor-type nil)
            (goto-char pos)
            (dolist (win (get-buffer-window-list nil nil t))
              (set-window-point win pos)
              (when viewport-reset
                (set-window-start win display-begin t))))
          (if (chomp-render--cursor-blink-p style)
              (blink-cursor-mode 1)
            (blink-cursor-mode -1)))))))

;;;; ---- Invalidation ---------------------------------------------------

(defun chomp-render--invalidate-screen-lines (screen)
  "Discard rendered face caches stored on every line in SCREEN."
  (dolist (line (append (chomp--ordered-lines-vector screen) nil))
    (setf (chomp-line-rendered line) nil)
    (setf (chomp-line-dirty line) t)))

(defun chomp-render--theme-changed (&rest _)
  "Clear theme-derived caches and fully redraw live Chomp render states."
  (fillarray chomp-render--indexed-color-cache nil)
  (clrhash chomp-render--attr-face-cache)
  (when (boundp 'chomp--render)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let ((render (buffer-local-value 'chomp--render buffer)))
          (chomp-render--invalidate-screen-lines
           (chomp-render-state-screen render))
          (clrhash (chomp-render-state-history-cache render))
          (chomp-render-full-reset render))))))

(defun chomp-render--after-load-theme (&rest _)
  "Emacs 28 fallback advice for invalidating colors after `load-theme'."
  (chomp-render--theme-changed))

(defun chomp-render--install-theme-invalidation ()
  "Install the available theme-change invalidation path."
  (if (boundp 'enable-theme-functions)
      (add-hook 'enable-theme-functions #'chomp-render--theme-changed)
    (unless (advice-member-p #'chomp-render--after-load-theme 'load-theme)
      (advice-add 'load-theme :after #'chomp-render--after-load-theme))))

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
        (let* ((preserve-view
                (eq (bound-and-true-p chomp--input-mode) 'emacs))
               (saved-point (and preserve-view
                                 (chomp-render-buffer-anchor render (point))))
               (saved-mark (and preserve-view (mark t)
                                (chomp-render-buffer-anchor render (mark t))))
               (saved-mark-active (and preserve-view mark-active))
               (saved-windows
                (cl-loop for window in (get-buffer-window-list buffer nil t)
                         collect (cons window
                                       (chomp-render-buffer-anchor
                                        render (window-start window))))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (buffer-undo-list t))
            (goto-char (chomp-render-state-region-begin render))
            (delete-region (point)
                           (chomp-render-state-region-end render))
          ;; Materialize a slab covering every window's stable model anchor.
          (let* ((total (chomp-screen-history-row-count screen))
                 (capacity (chomp-render--history-capacity render))
                 (rows
                  (delq nil
                        (mapcar
                         (lambda (entry)
                           (when-let ((location
                                       (chomp-render--anchor-location
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
              (insert (chomp-render--history-row-string
                       render (+ start offset))))
            (setf (chomp-render-state-history-start-row render) start
                  (chomp-render-state-scrollback-count render) count
                  (chomp-render-state-history-total-rows render) total
                  (chomp-render-state-history-generation render)
                  (chomp-screen-history-generation screen)))
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
                          (chomp-render--apply-line-metadata
                           line (chomp-render--line-to-string line w) w)
                        (make-string w ?\s)))
              (when (< i (1- h))
                (insert "\n"))))
          ;; Now re-enable advance-on-insert so future scrollback
          ;; insertions push the marker forward
          (set-marker-insertion-type
           (chomp-render-state-display-begin render) t)
          ;; Reset cursor overlay
          (let ((ov (chomp-render-state-cursor-overlay render))
                (begin (chomp-render-state-region-begin render)))
            (when ov
              (move-overlay ov begin (1+ begin))))
          ;; Update cursor
          (chomp-render--update-cursor render)
            ;; Clear dirty since we just rendered everything
            (chomp-screen-clear-dirty screen)
            (chomp-screen-clear-scrollback-dirty screen))
          (when preserve-view
            (chomp-render-goto-anchor render saved-point t)
            (if saved-mark
                (progn
                  (save-excursion
                    (chomp-render-goto-anchor render saved-mark t)
                    (set-marker (mark-marker) (point)))
                  (setq mark-active saved-mark-active))
              (set-marker (mark-marker) nil))
            )
          (dolist (entry saved-windows)
            (when (window-live-p (car entry))
              (if-let ((position
                        (chomp-render--anchor-buffer-position
                         render (cdr entry))))
                  (set-window-start (car entry) position t)
                (set-window-start
                 (car entry) (chomp-render-state-display-begin render) t)))))))))

(defun chomp-render-resize-height (render)
  "Rebuild only RENDER's viewport after a height-only terminal resize."
  (let* ((screen (chomp-render-state-screen render))
         (buffer (chomp-render-state-buffer render))
         (width (chomp-screen-width screen))
         (height (chomp-screen-height screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t)
              (display-begin (chomp-render-state-display-begin render)))
          (chomp-render--update-scrollback render)
          (save-excursion
            (goto-char display-begin)
            (delete-region (point) (chomp-render-state-region-end render))
            (set-marker-insertion-type display-begin nil)
            (dotimes (row height)
              (let ((line (chomp-screen-get-line screen row)))
                (insert (if line
                            (chomp-render--apply-line-metadata
                             line (chomp-render--line-to-string line width) width)
                          (make-string width ?\s)))
                (when (< row (1- height)) (insert "\n"))))
            (set-marker-insertion-type display-begin t))
          (chomp-render--update-cursor render)
          (chomp-screen-clear-dirty screen)
          (chomp-screen-clear-scrollback-dirty screen))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun chomp-render-destroy (render)
  "Clean up render state."
  (when-let ((ov (chomp-render-state-cursor-overlay render)))
    (delete-overlay ov))
  (dolist (m (list (chomp-render-state-display-begin render)
                   (chomp-render-state-region-begin render)
                   (chomp-render-state-region-end render)))
    (when m (set-marker m nil))))

(chomp-render--install-theme-invalidation)

(provide 'chomp-render)
;;; chomp-render.el ends here
