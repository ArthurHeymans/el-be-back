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

;;;; ---- Glyph Fitting --------------------------------------------------

;; Terminal cells are square-ish slots of `frame-char-width' pixels, but
;; Emacs draws each character with whatever font actually covers it.  Nerd
;; Font icons, emoji and other fallback glyphs are routinely wider (and
;; taller) than the cell the terminal assigned them, so a row that holds
;; exactly WIDTH characters can still spill past the window edge -- which is
;; what makes `ls' output with icons look ragged.  Constrain such glyphs with
;; a `display' spec: `min-width' pins the slot to the number of terminal
;; columns the cell occupies, and a `height' scale shrinks glyphs that would
;; otherwise overflow it.

(defcustom ebb-fit-glyphs t
  "Non-nil to constrain overwide glyphs to their terminal cells.

On graphical frames, characters rendered from fallback fonts (Nerd Font
icons, emoji, CJK fallbacks) can be wider or taller than the terminal
cell they occupy, pushing the rest of the row past the window edge.  When
non-nil, such characters get a `display' property that pins them to their
cell width and scales them down to fit."
  :type 'boolean
  :group 'ebb)

(defconst ebb-render--glyph-miss (make-symbol "miss")
  "Sentinel distinguishing a cache miss from a cached nil spec.")

(defvar ebb-render--glyph-cache (make-hash-table :test #'eql)
  "Cache of display specs keyed by character and cell width.")

(defvar ebb-render--glyph-stamp nil
  "Font geometry the entries in `ebb-render--glyph-cache' were measured for.")

(defvar ebb-render--cell-pixel-width 0
  "Pixel width of one terminal cell for the current stamp.")

(defvar ebb-render--cell-pixel-height 0
  "Pixel height of one terminal cell for the current stamp.")

(defun ebb-render--string-pixel-size (string)
  "Return (WIDTH . HEIGHT) in pixels for STRING drawn in `ebb-default'."
  (let ((text (propertize string 'face 'ebb-default)))
    (with-temp-buffer
      (setq-local line-prefix nil)
      (setq-local wrap-prefix nil)
      (insert text)
      (let ((size (buffer-text-pixel-size nil nil t)))
        (cons (car size) (cdr size))))))

(defun ebb-render--glyph-cache-valid-p ()
  "Refresh the glyph cache when font geometry changed; return non-nil if usable."
  (let ((stamp (list (frame-char-width) (frame-char-height)
                     (bound-and-true-p text-scale-mode-amount)
                     (face-attribute 'ebb-default :height nil t)
                     (face-attribute 'ebb-default :family nil t))))
    (unless (equal stamp ebb-render--glyph-stamp)
      (clrhash ebb-render--glyph-cache)
      (let ((size (ignore-errors (ebb-render--string-pixel-size " "))))
        (setq ebb-render--cell-pixel-width (or (car size) 0)
              ebb-render--cell-pixel-height (or (cdr size) 0)
              ebb-render--glyph-stamp stamp)))
    (> ebb-render--cell-pixel-width 0)))

(defun ebb-render--quantize-scale (scale)
  "Round SCALE down to a step Emacs will not round back up.
Emacs turns a `height' scale into an integer pixel size; flooring on that
grid keeps the scaled glyph inside its cell."
  (let ((pixels (max 1 ebb-render--cell-pixel-height)))
    (/ (ffloor (* pixels scale)) (float pixels))))

(defun ebb-render--glyph-display-spec (char cw)
  "Return a display spec pinning CHAR to CW terminal columns, or nil.
nil means the glyph already fits its cell and needs no adjustment."
  (let* ((key (+ (* char 2) (1- cw)))
         (cached (gethash key ebb-render--glyph-cache
                          ebb-render--glyph-miss)))
    (if (not (eq cached ebb-render--glyph-miss))
        cached
      (puthash
       key
       (let* ((size (ignore-errors
                      (ebb-render--string-pixel-size (string char))))
              (width (or (car size) 0))
              (height (or (cdr size) 0))
              (target-width (* cw ebb-render--cell-pixel-width))
              (target-height ebb-render--cell-pixel-height))
         (cond
          ((zerop width) nil)
          ((and (= width target-width) (<= height target-height)) nil)
          (t
           (let ((scale (min (/ (float target-width) width)
                             (if (> height 0)
                                 (/ (float target-height) height)
                               1.0))))
             (if (< scale 1.0)
                 (list (list 'min-width (list cw))
                       (list 'height (ebb-render--quantize-scale scale)))
               (list (list 'min-width (list cw))))))))
       ebb-render--glyph-cache))))

(defun ebb-render--fit-glyphs (string)
  "Return STRING with overwide glyphs pinned to their terminal cells.
ASCII-only rows are returned unchanged, and STRING is copied only when an
adjustment is actually needed."
  (if (or (null string)
          (not ebb-fit-glyphs)
          (not (multibyte-string-p string))
          (not (display-graphic-p))
          (not (fboundp 'buffer-text-pixel-size))
          (not (ebb-render--glyph-cache-valid-p)))
      string
    (let ((length (length string))
          (index 0)
          (copied nil))
      (while (< index length)
        (let ((char (aref string index)))
          (when (and (>= char #x80)
                     (not (get-text-property index 'display string)))
            (let* ((cw (cond
                        ;; Viewport rows pad a wide cell with an invisible
                        ;; spacer for each extra column.
                        ((and (< (1+ index) length)
                              (get-text-property (1+ index)
                                                 'ebb-wide-spacer string))
                         2)
                        ;; Trimmed scrollback rows record the cell width
                        ;; instead, since they carry no spacers.
                        ((get-text-property index 'ebb-cell-width string))
                        (t 1)))
                   (spec (ebb-render--glyph-display-spec char cw)))
              (when spec
                (unless copied
                  (setq string (copy-sequence string)
                        copied t))
                (put-text-property index (1+ index) 'display spec string)))))
        (cl-incf index))
      string)))

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

(defcustom ebb-kitty-graphics-render-cache-limit (* 64 1024 1024)
  "Approximate byte limit for cached scaled Kitty image objects.
The estimate includes encoded source bytes and a four-byte-per-pixel backend
surface.  Oversized objects can render once but are not retained."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-render-entry-limit (* 16 1024 1024)
  "Maximum estimated bytes for one cached scaled Kitty image object.
Larger objects still render once per row but are never retained, so one
huge image cannot evict the whole LRU cache.  Must be positive to cache
anything; zero disables render caching while leaving rendering intact."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-layout-cache-limit 1024
  "Maximum rows retained in one render state's graphics layout cache.
Layout keys use absolute model rows, which advance with scrollback even
when the graphics generation is unchanged; without a bound a long session
with a static image would accumulate one entry per scrolled row.  When the
limit is exceeded the cache is dropped (it is purely recomputable) rather
than evicted selectively.  Zero disables layout caching."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-elisp-rgba-pixel-limit (* 512 512)
  "Maximum RGBA pixels composited by the slow Elisp fallback.
Larger raw RGBA images require the fast PNG helper rather than blocking the
Emacs main thread with a multi-million-iteration compositing loop."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-allow-slow-rgba t
  "If non-nil, composite raw RGBA images in Elisp when the PNG helper is
unavailable.  The fallback blocks the main thread proportionally to the
image size (bounded by `ebb-kitty-graphics-elisp-rgba-pixel-limit'); set
to nil on machines without python3 if hostile programs might send raw
RGBA graphics.  PNG and RGB images are unaffected."
  :type 'boolean
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
  (history-graphics-generation -1)
  (virtual-mark nil)
  (history-cache nil)
  (graphics-cache nil)
  (graphics-cache-sizes nil)
  (graphics-cache-order nil)
  (graphics-cache-bytes 0)
  (graphics-state nil)
  (graphics-generation -1)
  (graphics-placement-order nil)
  (graphics-layout-cache nil)
  (placeholder-row-cache nil)
  (placeholder-history-row-cache nil))

;;;; ---- Constructor ----------------------------------------------------

(defun ebb-render-create (screen buffer &optional begin end)
  "Create a render state for SCREEN displayed in BUFFER.

When BEGIN and END are non-nil, render only that buffer region.  This lets
Eshell retain everything outside an inline terminal."
  (let ((render (make-ebb-render-state
                 :screen screen :buffer buffer
                 :history-cache (make-hash-table :test #'equal)
                 :graphics-cache (make-hash-table :test #'equal)
                 :graphics-cache-sizes (make-hash-table :test #'equal)
                 :graphics-layout-cache (make-hash-table :test #'equal)
                 :placeholder-row-cache (make-hash-table :test #'eql)
                 :placeholder-history-row-cache
                 (make-hash-table :test #'eql)))
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
          ;; Insert H empty display lines, newline-separated.  Row text is
          ;; materialized on demand; trailing blank cells are never padded
          ;; into the buffer so point cannot wander into dead space.
          (dotimes (i h)
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

(defun ebb-render-text-area-pixel-size (render)
  "Return RENDER's visible text area as (WIDTH . HEIGHT) pixels."
  (when-let* ((buffer (ebb-render-state-buffer render))
              (window (car (get-buffer-window-list buffer nil t))))
    (cons (window-body-width window t)
          (window-body-height window t))))

(defun ebb-render-cell-pixel-size (render)
  "Return RENDER's terminal cell size as (WIDTH . HEIGHT) pixels."
  (when-let* ((buffer (ebb-render-state-buffer render))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (if (ebb-render--glyph-cache-valid-p)
          (cons ebb-render--cell-pixel-width ebb-render--cell-pixel-height)
        (cons (frame-char-width) (frame-char-height))))))

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
            ;; Placeholder row prefixes depend on viewport text and are shared
            ;; by every dirty row rendered in this refresh.
            (clrhash (or (ebb-render-state-placeholder-row-cache render)
                         (setf (ebb-render-state-placeholder-row-cache render)
                               (make-hash-table :test #'eql))))
            (clrhash
             (or (ebb-render-state-placeholder-history-row-cache render)
                 (setf (ebb-render-state-placeholder-history-row-cache render)
                       (make-hash-table :test #'eql))))
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
              (let ((window-anchor
                     ;; Point decides whether this window follows the live
                     ;; viewport.  A transient state with point in history and
                     ;; window-start in the viewport can occur after ordinary
                     ;; point motion, before redisplay has exposed point.  Do
                     ;; not restore those conflicting intentions: anchor the
                     ;; reading window at point until redisplay establishes a
                     ;; history window-start of its own.
                     (if (and (eq (car-safe saved-point) 'history)
                              (eq (car-safe saved-window-start) 'viewport)
                              ;; A bounded scrollback refresh may have trimmed
                              ;; the saved history line.  In that case there is
                              ;; no reading position left to preserve, so keep
                              ;; the live viewport anchor instead.
                              (ebb-render--anchor-location
                               render saved-point
                               (ebb-screen-history-row-count screen)))
                         saved-point
                       saved-window-start)))
                (save-excursion
                  (ebb-render-goto-anchor render window-anchor t)
                  (set-window-start saved-window (point) t)))))
          ;; Apply the reset only after saved windows have been restored,
          ;; so restoration cannot move windows away from display-begin.
          (ebb-render--apply-viewport-reset render)
          ;; Hook errors must not leave the dirty state set, or every
          ;; subsequent refresh would re-render the same rows forever.
          (condition-case hook-error
              (run-hook-with-args 'ebb-render-after-refresh-hook render)
            (error (message "[ebb] after-refresh hook error: %S"
                            hook-error)))))
      (ebb-screen-clear-dirty screen))))

;;;; ---- Scrollback Rendering -------------------------------------------

(defun ebb-render--history-graphics-signature (screen)
  "Return a stable signature for graphics that can affect SCREEN history."
  (let* ((graphics (ebb-screen-graphics screen))
         (limit (ebb-screen-history-row-count screen)))
    (delq
     nil
     (mapcar
      (lambda (placement)
        (when (or (ebb-graphics-placement-virtual placement)
                  (< (ebb-graphics-placement-row placement) limit))
          (let ((image (gethash (ebb-graphics-placement-image-id placement)
                                (ebb-graphics-state-images graphics))))
            (list (ebb-graphics-placement-image-id placement)
                  (ebb-graphics-placement-placement-id placement)
                  (ebb-graphics-placement-row placement)
                  (ebb-graphics-placement-column placement)
                  (ebb-graphics-placement-columns placement)
                  (ebb-graphics-placement-rows placement)
                  (ebb-graphics-placement-row-offset placement)
                  (ebb-graphics-placement-z-index placement)
                  (ebb-graphics-placement-virtual placement)
                  (and image (ebb-graphics-image-cache-token image))))))
      (ebb-graphics-state-placements graphics)))))

(defun ebb-render--update-scrollback (render)
  "Materialize the history range needed by every window showing RENDER."
  (let* ((screen (ebb-render-state-screen render))
         ;; Capture model anchors before replacing any materialized text.  In
         ;; particular, numeric row indices are not stable when old history is
         ;; trimmed.
         (windows
          (cl-loop for window in (get-buffer-window-list
                                  (ebb-render-state-buffer render) nil t)
                   collect (list window
                                 (ebb-render-buffer-anchor
                                  render (window-start window))
                                 (ebb-render-buffer-anchor
                                  render (window-point window)))))
         (point-anchor (ebb-render-buffer-anchor render (point)))
         (mark-anchor (and (mark t)
                           (ebb-render-buffer-anchor render (mark t))))
         (total (ebb-screen-history-row-count screen))
         (generation (ebb-screen-history-generation screen))
         (graphics-generation (ebb-render--history-graphics-signature screen))
         (capacity (ebb-render--history-capacity render))
         (history-locations
          (delq nil
                (cl-loop for (_window start-anchor window-point-anchor)
                         in windows
                         collect (ebb-render--anchor-location
                                  render start-anchor total)
                         collect (ebb-render--anchor-location
                                  render window-point-anchor total))))
         (point-location (ebb-render--anchor-location
                          render point-anchor total))
         (mark-location (ebb-render--anchor-location
                         render mark-anchor total))
         (history-rows
          (delq nil
                (mapcar (lambda (location)
                          (and location
                               (< (car location) total)
                               (car location)))
                        (append history-locations
                                (list point-location mark-location)))))
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
              (not (equal graphics-generation
                          (ebb-render-state-history-graphics-generation
                           render)))
              (/= start (ebb-render-state-history-start-row render))
              (/= count (ebb-render-state-scrollback-count render))
              (/= total (ebb-render-state-history-total-rows render)))
      (ebb-render--rebuild-scrollback
       render start count total generation graphics-generation)
      (pcase-dolist (`(,window ,start-anchor ,window-point-anchor) windows)
        (when-let* ((position
                    (ebb-render--anchor-buffer-position
                     render start-anchor)))
          (set-window-start window position t))
        (ebb-render--restore-window-point render window window-point-anchor))
      ;; Rebuilding the slab collapsed any point or mark that was inside it
      ;; to the top of the slab.  Emacs input mode restores them again from
      ;; its own saved anchors; the live input modes only correct point when
      ;; the cursor is visible, so re-anchor both here.
      (when-let* ((position
                  (ebb-render--anchor-buffer-position render point-anchor)))
        (goto-char position))
      (when mark-anchor
        (when-let* ((position
                    (ebb-render--anchor-buffer-position render mark-anchor)))
          (set-marker (mark-marker) position))))
    (ebb-screen-clear-scrollback-dirty screen)))

(defun ebb-render--restore-window-point (render window anchor)
  "Move WINDOW's point to ANCHOR, falling back to its window-start.
A rebuilt scrollback slab collapses window-point markers that were inside
it to the top of the slab; redisplay would then force that stale position
visible and jump the window into old history."
  (let ((position
         (or (and anchor
                  (ebb-render--anchor-buffer-position render anchor))
             (window-start window))))
    (when position
      (set-window-point window position))))

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

(defun ebb-render--anchor-history-row (render anchor total)
  "Return ANCHOR's history row when it lies within RENDER's history."
  (when-let* ((location (ebb-render--anchor-location render anchor total)))
    (and (< (car location) total) (car location))))

(defun ebb-render--anchor-buffer-position (render anchor)
  "Return ANCHOR's position when it is present in RENDER's current slab."
  (when-let* ((location
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

(defun ebb-render--history-row-string (render row &optional graphics-signature)
  "Return cached rendered history ROW for RENDER.
GRAPHICS-SIGNATURE is a precomputed `ebb-render--history-graphics-signature'
value for the current screen; it is computed on demand when absent.  Callers
rendering many rows in a loop should compute it once and pass it in."
  (let* ((screen (ebb-render-state-screen render))
         (width (ebb-screen-width screen))
         (location (ebb-screen-history-row-location screen row))
         (logical (car location))
         (key (list (ebb-history-line-id logical)
                    (ebb-history-line-generation logical)
                    (or graphics-signature
                        (ebb-render--history-graphics-signature screen))
                    (cdr location) width))
         (cache (ebb-render-state-history-cache render)))
    (or (gethash key cache)
        (let* ((line (ebb-screen-history-render-row screen row))
               (string (concat
                        (ebb-render--trim-trailing
                         (ebb-render--apply-graphics
                          render row
                          (ebb-render--apply-line-metadata
                           line
                           (ebb-render--line-to-string-scrollback line width))
                          t))
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
    (render &optional start count total generation graphics-generation)
  "Replace RENDER's bounded history slab from START for COUNT rows."
  (let* ((screen (ebb-render-state-screen render))
         (display-begin (ebb-render-state-display-begin render))
         (total (or total (ebb-screen-history-row-count screen)))
         (start (or start (max 0 (- total (ebb-render--history-capacity render)))))
         (count (or count (min (ebb-render--history-capacity render)
                               (- total start))))
         (generation (or generation (ebb-screen-history-generation screen)))
         (graphics-generation
          (if (null graphics-generation)
              (ebb-render--history-graphics-signature screen)
            graphics-generation)))
    (save-excursion
      (delete-region (ebb-render-state-region-begin render) display-begin)
      (goto-char (ebb-render-state-region-begin render))
      (dotimes (offset count)
        (insert (ebb-render--history-row-string
                 render (+ start offset) graphics-generation))))
    (setf (ebb-render-state-history-start-row render) start
          (ebb-render-state-scrollback-count render) count
          (ebb-render-state-history-total-rows render) total
          (ebb-render-state-history-generation render) generation
          (ebb-render-state-history-graphics-generation render)
          graphics-generation)))

(defun ebb-render-scroll-history (render rows)
  "Scroll RENDER's selected window by virtual history ROWS."
  (let* ((window (selected-window))
         (screen (ebb-render-state-screen render))
         (total (ebb-screen-history-row-count screen))
         (display-begin (ebb-render-state-display-begin render))
         (window-start (window-start window))
         (current
          (if (>= window-start (marker-position display-begin))
              ;; Redisplay may have left window-start below the viewport
              ;; top; count from display-begin so scrolling stays relative.
              (+ total
                 (- (line-number-at-pos window-start)
                    (line-number-at-pos (marker-position display-begin))))
            ;; Slab row arithmetic is stale when parsed-but-unrendered
            ;; output trimmed old history; the line's stable id is not.
            (or (car (ebb-render--anchor-location
                      render
                      (ebb-render-buffer-anchor render window-start)
                      total))
                (+ (ebb-render-state-history-start-row render)
                   (- (line-number-at-pos window-start)
                      (line-number-at-pos
                       (ebb-render-state-region-begin render)))))))
         ;; A window-start below the viewport top (left there by redisplay)
         ;; may scroll backward relatively; forward it can only stay put.
         (target (ebb--clamp (+ current rows) 0 (max total current)))
         (capacity (ebb-render--history-capacity render))
         (point-anchor (ebb-render-buffer-anchor render (point)))
         (mark-anchor (and (mark t)
                           (ebb-render-buffer-anchor render (mark t))))
         (other-windows
          (cl-loop for other in (get-buffer-window-list
                                 (ebb-render-state-buffer render) nil t)
                   unless (eq other window)
                   collect (list other
                                 (ebb-render-buffer-anchor
                                  render (window-start other))
                                 (ebb-render-buffer-anchor
                                  render (window-point other)))))
         ;; Size the slab from window points too, or a nonselected point
         ;; parked in another history area loses its anchor in the rebuild.
         (other-rows
          (delq nil
                (cl-loop for (nil start-anchor window-point-anchor)
                         in other-windows
                         collect (ebb-render--anchor-history-row
                                  render start-anchor total)
                         collect (ebb-render--anchor-history-row
                                  render window-point-anchor total))))
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
    (pcase-dolist (`(,other ,start-anchor ,window-point-anchor)
                   other-windows)
      (when-let* ((position
                  (ebb-render--anchor-buffer-position render start-anchor)))
        (set-window-start other position t))
      (ebb-render--restore-window-point render other window-point-anchor))
    ;; The rebuild collapsed point and mark positions that were inside the
    ;; slab; put them back on their lines before deciding visibility.
    (when-let* ((position
                (ebb-render--anchor-buffer-position render point-anchor)))
      (goto-char position))
    (when mark-anchor
      (when-let* ((position
                  (ebb-render--anchor-buffer-position render mark-anchor)))
        (set-marker (mark-marker) position)))
    (if (>= target total)
        (if (= target total)
            (set-window-start window display-begin t)
          ;; Keep a start inside the viewport relative instead of
          ;; snapping it back to the viewport top.
          (save-excursion
            (goto-char display-begin)
            (forward-line (- target total))
            (set-window-start window (point) t)))
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
     (when-let* ((location
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
        (let* ((slab-start (ebb-render-state-history-start-row render))
               (slab-end (+ slab-start
                            (ebb-render-state-scrollback-count render))))
          (unless (and (>= row slab-start) (< row slab-end))
            (let* ((start (min (max 0 (- row (/ capacity 3)))
                               (max 0 (- history-rows capacity))))
                   (count (min capacity (- history-rows start)))
                   (inhibit-read-only t)
                   (inhibit-modification-hooks t)
                   (buffer-undo-list t))
              (ebb-render--rebuild-scrollback
               render start count history-rows
               (ebb-screen-history-generation screen))))
          (goto-char (ebb-render-state-region-begin render))
          (forward-line (- row
                           (ebb-render-state-history-start-row render))))
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
  (ebb-render--apply-line-rendition
   line
   (ebb-render--fit-glyphs
    (cond
     ((and (ebb-line-text line)
           (ebb-line-attr-runs line)
           (= (length (ebb-line-text line)) width))
      (setf (ebb-line-dirty line) nil)
      (ebb-render--text-runs-to-string line width))
     ((and (ebb-line-text line)
           (= (length (ebb-line-text line)) width))
      (setf (ebb-line-dirty line) nil)
      (if-let* ((attr (ebb-line-uniform-attr line)))
          (let ((s (copy-sequence (ebb-line-text line))))
            (ebb-render--apply-attr-properties s attr)
            s)
        (ebb-line-text line)))
     ((ebb-render--cells-to-string-scrollback-fast
       (ebb-line-cells line) width))
     (t
      (ebb-render--cells-to-string (ebb-line-cells line) width))))
   width))

(defun ebb-render--cells-to-string-scrollback-fast (cells width)
  "Return unstyled CELLS as visible scrollback text, or nil if styled."
  (catch 'styled
    (let ((chars nil)
          (wide-ranges nil)
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
            (push (ebb-render--safe-char (ebb-cell-char cell)) chars)
            ;; No spacer columns here, so record the cell width for
            ;; `ebb-render--fit-glyphs'.
            (when (> cw 1)
              (push (cons pos cw) wide-ranges))
            (cl-incf pos)
            (cl-incf cols cw)
            (cl-incf i cw)))))
      (let ((s (apply #'string
                      (append (nreverse chars)
                              (make-list (max 0 (- width cols)) ?\s)))))
        (dolist (range wide-ranges)
          (put-text-property (car range) (1+ (car range))
                             'ebb-cell-width (cdr range) s))
        s))))

(defun ebb-render--apply-line-rendition (line string width)
  "Apply LINE's DEC width rendition to STRING for a WIDTH-column screen."
  (if (eq (ebb-line-rendition line) 'normal)
      string
    (let* ((logical-width (max 1 (/ width 2)))
           (result (copy-sequence
                    (truncate-string-to-width string logical-width))))
      ;; Emacs cannot clip separate top and bottom halves of a text glyph as a
      ;; VT100 did.  Preserve the correct double-width layout and render both
      ;; double-height halves as expanded lines.
      (add-face-text-property 0 (length result) '(:width ultra-expanded)
                              t result)
      result)))

(defun ebb-render--line-to-string (line width)
  "Convert LINE to a string for a WIDTH-column terminal display."
  (when (ebb-line-text line)
    (setf (ebb-line-text line)
          (ebb-render--safe-string (ebb-line-text line))))
  (ebb-render--apply-line-rendition
   line
   (ebb-render--fit-glyphs
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
   width))

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

(defun ebb-render--apply-line-metadata (line string)
  "Apply shell metadata from LINE to STRING."
  (if (not (or (ebb-line-prompt-begins line)
               (ebb-line-prompt-ends line)))
      string
    (let ((result (copy-sequence string)))
      (dolist (column (ebb-line-prompt-begins line))
        (when (< column (length result))
          (put-text-property column (1+ column)
                             'ebb-shell-prompt-begin t result)))
      (dolist (column (ebb-line-prompt-ends line))
        (let ((position (max 0 (1- column))))
          (when (< position (length result))
            (put-text-property position (1+ position)
                               'ebb-shell-prompt-end t result))))
      result)))

;;;; ---- Terminal Graphics ----------------------------------------------

(defconst ebb-render--placeholder-diacritics
  [#x305 #x30d #x30e #x310 #x312 #x33d #x33e #x33f #x346 #x34a #x34b #x34c
   #x350 #x351 #x352 #x357 #x35b #x363 #x364 #x365 #x366 #x367 #x368 #x369
   #x36a #x36b #x36c #x36d #x36e #x36f #x483 #x484 #x485 #x486 #x487 #x592
   #x593 #x594 #x595 #x597 #x598 #x599 #x59c #x59d #x59e #x59f #x5a0 #x5a1
   #x5a8 #x5a9 #x5ab #x5ac #x5af #x5c4 #x610 #x611 #x612 #x613 #x614 #x615
   #x616 #x617 #x657 #x658 #x659 #x65a #x65b #x65d #x65e #x6d6 #x6d7 #x6d8
   #x6d9 #x6da #x6db #x6dc #x6df #x6e0 #x6e1 #x6e2 #x6e4 #x6e7 #x6e8 #x6eb
   #x6ec #x730 #x732 #x733 #x735 #x736 #x73a #x73d #x73f #x740 #x741 #x743
   #x745 #x747 #x749 #x74a #x7eb #x7ec #x7ed #x7ee #x7ef #x7f0 #x7f1 #x7f3
   #x816 #x817 #x818 #x819 #x81b #x81c #x81d #x81e #x81f #x820 #x821 #x822
   #x823 #x825 #x826 #x827 #x829 #x82a #x82b #x82c #x82d #x951 #x953 #x954
   #xf82 #xf83 #xf86 #xf87 #x135d #x135e #x135f #x17dd #x193a #x1a17 #x1a75 #x1a76
   #x1a77 #x1a78 #x1a79 #x1a7a #x1a7b #x1a7c #x1b6b #x1b6d #x1b6e #x1b6f #x1b70 #x1b71
   #x1b72 #x1b73 #x1cd0 #x1cd1 #x1cd2 #x1cda #x1cdb #x1ce0 #x1dc0 #x1dc1 #x1dc3 #x1dc4
   #x1dc5 #x1dc6 #x1dc7 #x1dc8 #x1dc9 #x1dcb #x1dcc #x1dd1 #x1dd2 #x1dd3 #x1dd4 #x1dd5
   #x1dd6 #x1dd7 #x1dd8 #x1dd9 #x1dda #x1ddb #x1ddc #x1ddd #x1dde #x1ddf #x1de0 #x1de1
   #x1de2 #x1de3 #x1de4 #x1de5 #x1de6 #x1dfe #x20d0 #x20d1 #x20d4 #x20d5 #x20d6 #x20d7
   #x20db #x20dc #x20e1 #x20e7 #x20e9 #x20f0 #x2cef #x2cf0 #x2cf1 #x2de0 #x2de1 #x2de2
   #x2de3 #x2de4 #x2de5 #x2de6 #x2de7 #x2de8 #x2de9 #x2dea #x2deb #x2dec #x2ded #x2dee
   #x2def #x2df0 #x2df1 #x2df2 #x2df3 #x2df4 #x2df5 #x2df6 #x2df7 #x2df8 #x2df9 #x2dfa
   #x2dfb #x2dfc #x2dfd #x2dfe #x2dff #xa66f #xa67c #xa67d #xa6f0 #xa6f1 #xa8e0 #xa8e1
   #xa8e2 #xa8e3 #xa8e4 #xa8e5 #xa8e6 #xa8e7 #xa8e8 #xa8e9 #xa8ea #xa8eb #xa8ec #xa8ed
   #xa8ee #xa8ef #xa8f0 #xa8f1 #xaab0 #xaab2 #xaab3 #xaab7 #xaab8 #xaabe #xaabf #xaac1
   #xfe20 #xfe21 #xfe22 #xfe23 #xfe24 #xfe25 #xfe26 #x10a0f #x10a38 #x1d185 #x1d186
   #x1d187 #x1d188 #x1d189 #x1d1aa #x1d1ab #x1d1ac #x1d1ad #x1d242 #x1d243 #x1d244]
  "Kitty canonical row/column diacritics for byte-sized indices.")

(defconst ebb-render--placeholder-diacritic-indices
  (let ((table (make-hash-table :test #'eql)))
    (dotimes (index (length ebb-render--placeholder-diacritics))
      (puthash (aref ebb-render--placeholder-diacritics index) index table))
    table)
  "Reverse lookup table for Kitty placeholder diacritics.")

(defvar ebb-render--graphics-background-cache nil
  "Cached terminal background as (FACE-COLOR . RGB).")

(defun ebb-render--graphics-background ()
  "Return the terminal background as (RED GREEN BLUE) in 0..255."
  (let ((color (or (face-background 'ebb-default nil t) "black")))
    (if (equal color (car ebb-render--graphics-background-cache))
        (cdr ebb-render--graphics-background-cache)
      (let* ((values (color-values color))
             (rgb (if values
                      (mapcar (lambda (component) (ash component -8)) values)
                    '(0 0 0))))
        (setq ebb-render--graphics-background-cache (cons color rgb))
        rgb))))

(defun ebb-render--graphics-raw-png-helper (image)
  "Encode raw RGB or RGBA IMAGE as PNG using native zlib operations."
  (when-let* ((python (executable-find "python3")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert (ebb-graphics-image-data image))
      (let ((status
             (call-process-region
              (point-min) (point-max) python t t nil "-c"
              (concat
               "import binascii,struct,sys,zlib\n"
               "w,h,c=map(int,sys.argv[1:4]); src=sys.stdin.buffer.read(); stride=w*c\n"
               "raw=b''.join(b'\\0'+src[y*stride:(y+1)*stride] for y in range(h))\n"
               "def chunk(t,d): return struct.pack('>I',len(d))+t+d+struct.pack('>I',binascii.crc32(t+d)&0xffffffff)\n"
               "ihdr=struct.pack('>IIBBBBB',w,h,8,2 if c==3 else 6,0,0,0)\n"
               "sys.stdout.buffer.write(b'\\x89PNG\\r\\n\\x1a\\n'+chunk(b'IHDR',ihdr)+chunk(b'IDAT',zlib.compress(raw,1))+chunk(b'IEND',b''))\n")
              (number-to-string (ebb-graphics-image-width image))
              (number-to-string (ebb-graphics-image-height image))
              (number-to-string (/ (ebb-graphics-image-format image) 8)))))
        (and (eq status 0) (buffer-string))))))

(defun ebb-render--graphics-ppm-data (image background)
  "Return IMAGE as PPM bytes, or nil when its format is unsupported.
RGBA pixels are composited over BACKGROUND, a (RED GREEN BLUE) list, since
PPM carries no alpha channel.  The RGBA path runs synchronously on the
main thread and is skipped unless `ebb-kitty-graphics-allow-slow-rgba'
is non-nil; the RGB path is a plain header prepend and always available."
  (let ((format (ebb-graphics-image-format image))
        (width (ebb-graphics-image-width image))
        (height (ebb-graphics-image-height image))
        (data (ebb-graphics-image-data image)))
    (pcase format
      (24 (concat (format "P6\n%d %d\n255\n" width height) data))
      (32
       (let ((pixels (* width height)))
         (when (and ebb-kitty-graphics-allow-slow-rgba
                    (<= pixels ebb-kitty-graphics-elisp-rgba-pixel-limit))
           (let ((rgb (make-string (* pixels 3) 0)))
             (dotimes (pixel pixels)
               (let* ((source (* pixel 4))
                      (target (* pixel 3))
                      (alpha (aref data (+ source 3))))
                 (if (= alpha 255)
                     (progn
                       (aset rgb target (aref data source))
                       (aset rgb (1+ target) (aref data (1+ source)))
                       (aset rgb (+ target 2) (aref data (+ source 2))))
                   (dotimes (channel 3)
                     (aset rgb (+ target channel)
                           (/ (+ (* alpha (aref data (+ source channel)))
                                 (* (- 255 alpha) (nth channel background)))
                              255))))))
             (concat (format "P6\n%d %d\n255\n" width height)
                     (encode-coding-string rgb 'binary))))))
      (_ nil))))

(defun ebb-render--graphics-cache-touch (render key)
  "Mark cached graphics KEY as most recently used in RENDER."
  (setf (ebb-render-state-graphics-cache-order render)
        (cons key (delete key (ebb-render-state-graphics-cache-order render)))))

(defun ebb-render--graphics-cache-put (render key object bytes)
  "Cache OBJECT under KEY in RENDER when its estimated BYTES fit.
Objects larger than `ebb-kitty-graphics-render-entry-limit' render once
but are never retained, so one huge image cannot evict the whole cache."
  (when (and (> ebb-kitty-graphics-render-cache-limit 0)
             (> ebb-kitty-graphics-render-entry-limit 0)
             (<= bytes ebb-kitty-graphics-render-entry-limit)
             (<= bytes ebb-kitty-graphics-render-cache-limit))
    (let ((cache (ebb-render-state-graphics-cache render))
          (sizes (or (ebb-render-state-graphics-cache-sizes render)
                     (setf (ebb-render-state-graphics-cache-sizes render)
                           (make-hash-table :test #'equal)))))
      (while (and (ebb-render-state-graphics-cache-order render)
                  (> (+ (ebb-render-state-graphics-cache-bytes render) bytes)
                     ebb-kitty-graphics-render-cache-limit))
        (let* ((old-key (car (last
                              (ebb-render-state-graphics-cache-order render))))
               (old-size (gethash old-key sizes 0)))
          (setf (ebb-render-state-graphics-cache-order render)
                (delete old-key
                        (ebb-render-state-graphics-cache-order render)))
          (remhash old-key cache)
          (remhash old-key sizes)
          (cl-decf (ebb-render-state-graphics-cache-bytes render) old-size)))
      (puthash key object cache)
      (puthash key bytes sizes)
      (cl-incf (ebb-render-state-graphics-cache-bytes render) bytes)
      (ebb-render--graphics-cache-touch render key))))

(defun ebb-render--graphics-image-object (render image placement)
  "Return a cached, cell-box-sized Emacs image for IMAGE and PLACEMENT.
The box-sized SVG canvas is essential: Emacs clamps slices to the backing
image dimensions rather than stretching them over the propertized text run."
  (let* ((cell (or (ebb-render-cell-pixel-size render)
                   (cons (frame-char-width) (frame-char-height))))
         (cell-width (max 1 (car cell)))
         (cell-height (max 1 (cdr cell)))
         (draw-width (ebb-graphics-placement-pixel-width placement))
         (draw-height (ebb-graphics-placement-pixel-height placement))
         (width (* (or (ebb-graphics-placement-box-columns placement)
                       (ebb-graphics-placement-columns placement))
                   cell-width))
         (height (* (or (ebb-graphics-placement-box-rows placement)
                        (ebb-graphics-placement-rows placement))
                    cell-height))
         (background (ebb-render--graphics-background))
         (key (list (ebb-graphics-image-cache-token image)
                    width height draw-width draw-height background))
         (cache (ebb-render-state-graphics-cache render))
         (missing (make-symbol "missing"))
         (cached (gethash key cache missing)))
    (if (not (eq cached missing))
        (progn
          (ebb-render--graphics-cache-touch render key)
          (unless (eq cached :failed) cached))
      (let* ((source-key
              (list (ebb-graphics-image-format image)
                    (and (= (ebb-graphics-image-format image) 32) background)))
             (source-info
              (if (equal source-key
                         (ebb-graphics-image-render-source-key image))
                  (ebb-graphics-image-render-source image)
                (let* ((format (ebb-graphics-image-format image))
                       (png (and (memq format '(24 32))
                                 (ebb-render--graphics-raw-png-helper image)))
                       (converted
                        (cond
                         ((= format 100)
                          (cons "image/png" (ebb-graphics-image-data image)))
                         (png (cons "image/png" png))
                         (t
                          (when-let* ((ppm (ebb-render--graphics-ppm-data
                                            image background)))
                            (cons "image/x-portable-pixmap" ppm))))))
                  (setf (ebb-graphics-image-render-source-key image) source-key
                        (ebb-graphics-image-render-source image) converted)
                  converted)))
             (mime (car-safe source-info))
             (source (cdr-safe source-info))
             (fill (format "#%02x%02x%02x"
                           (nth 0 background) (nth 1 background)
                           (nth 2 background)))
             (draw-x (max 0 (/ (- width draw-width) 2)))
             (draw-y (max 0 (/ (- height draw-height) 2)))
             (svg
              (and source (equal mime "image/png")
                   (format
                    (concat "<svg xmlns='http://www.w3.org/2000/svg' "
                            "width='%d' height='%d' viewBox='0 0 %d %d'>"
                            "<rect width='100%%' height='100%%' fill='%s'/>"
                            "<image x='%d' y='%d' width='%d' height='%d' "
                            "preserveAspectRatio='none' href='data:%s;base64,%s'/>"
                            "</svg>")
                    width height width height fill draw-x draw-y
                    draw-width draw-height mime
                    (base64-encode-string source t))))
             (object
              (condition-case nil
                  (cond
                   ((and svg (image-type-available-p 'svg))
                    (create-image svg 'svg t :ascent 'center))
                   (source
                    ;; Reduced fallback for builds without SVG support.  It
                    ;; keeps slice geometry correct, though partial-cell
                    ;; padding is stretched rather than blank.
                    (create-image
                     source
                     (if (equal mime "image/png") 'png 'pbm)
                     t :width width :height height :scale 1 :ascent 'center)))
                (error nil))))
        (ebb-render--graphics-cache-put
         render key (or object :failed)
         (if object
             (+ (length source) (length (or svg "")) (* width height 4))
           0))
        object))))

(defun ebb-render--graphics-color-id (color)
  "Decode Kitty image or placement ID from terminal COLOR."
  (cond
   ((integerp color) color)
   ((and (listp color) (= (length color) 3))
    (+ (ash (nth 0 color) 16) (ash (nth 1 color) 8) (nth 2 color)))
   (t nil)))

(defun ebb-render--virtual-placement (graphics image-id placement-id)
  "Find the newest virtual placement matching IMAGE-ID and PLACEMENT-ID."
  (cl-find-if
   (lambda (placement)
     (and (ebb-graphics-placement-virtual placement)
          (= image-id (ebb-graphics-placement-image-id placement))
          (or (null placement-id)
              (zerop placement-id)
              (eql placement-id
                   (ebb-graphics-placement-placement-id placement)))))
   (ebb-graphics-state-placements graphics)))

(defun ebb-render--line-has-placeholder-placement-p
    (screen graphics row target &optional absolute)
  "Return non-nil when ROW contains a placeholder for TARGET.
ROW addresses history when ABSOLUTE is non-nil."
  (let* ((line (ebb-render--graphics-line screen row absolute))
         (cells (and line (ebb--line-ensure-cells line (ebb-screen-width screen))))
         (column 0)
         (limit (and cells (length cells)))
         found)
    (while (and (< column (or limit 0)) (not found))
      (let* ((cell (aref cells column))
             (attr (ebb-cell-attr cell))
             (low-id (and attr
                          (ebb-render--graphics-color-id (ebb-attr-fg attr))))
             (marks (and (= (ebb-cell-char cell) #x10eeee)
                         (string-to-list (or (ebb-cell-combining cell) ""))))
             (high (and (cddr marks)
                        (ebb-render--placeholder-diacritic-index
                         (nth 2 marks))))
             (image-id (and low-id (+ low-id (ash (or high 0) 24))))
             (placement-id
              (and attr
                   (ebb-render--graphics-color-id (ebb-attr-ul-color attr)))))
        (when (and (= (ebb-cell-char cell) #x10eeee)
                   image-id
                   (eq target (ebb-render--virtual-placement
                               graphics image-id placement-id)))
          (setq found t)))
      (cl-incf column))
    found))

(defun ebb-render--placeholder-tile-row
    (render screen graphics row placement &optional absolute)
  "Return PLACEMENT's tile row before ROW, caching line prefixes.
ROW addresses history when ABSOLUTE is non-nil."
  (let* ((cache
          (if absolute
              (or (ebb-render-state-placeholder-history-row-cache render)
                  (setf (ebb-render-state-placeholder-history-row-cache render)
                        (make-hash-table :test #'eql)))
            (or (ebb-render-state-placeholder-row-cache render)
                (setf (ebb-render-state-placeholder-row-cache render)
                      (make-hash-table :test #'eql)))))
         (limit (if absolute (1+ row) (ebb-screen-height screen)))
         (entry (gethash placement cache)))
    (when (or (null entry) (< (length (cdr entry)) (1+ limit)))
      (let ((replacement (make-vector (1+ limit) 0)))
        (when entry
          (cl-replace replacement (cdr entry)))
        (setq entry (cons (or (car-safe entry) 0) replacement))
        (puthash placement entry cache)))
    (let ((computed (car entry))
          (counts (cdr entry)))
      (while (< computed row)
        (aset counts (1+ computed)
              (+ (aref counts computed)
                 (if (ebb-render--line-has-placeholder-placement-p
                      screen graphics computed placement absolute)
                     1 0)))
        (cl-incf computed))
      (setcar entry computed)
      (aref counts row))))

(defun ebb-render--graphics-line (screen row absolute)
  "Return the model line for ROW, interpreting it as ABSOLUTE when non-nil."
  (if absolute
      (ebb-screen-history-render-row screen row)
    (ebb-screen-get-line screen row)))

(defun ebb-render--graphics-column-indices (line string width scrollback)
  "Map terminal column boundaries to character indices in STRING.
LINE supplies cell widths and combining suffixes.  SCROLLBACK accounts for
the compact history path, where a wide cell is one character carrying an
`ebb-cell-width' property instead of a character plus invisible spacers."
  (let ((indices (make-vector (1+ width) (length string)))
        (cells (and line (ebb--line-ensure-cells line width)))
        (column 0)
        (position 0)
        (limit (length string)))
    (while (and cells (< column width) (< position limit))
      (let* ((cell (aref cells column))
             (cell-width (max 1 (ebb-cell-width cell)))
             (combining-length (length (or (ebb-cell-combining cell) "")))
             (compact-wide
              (and scrollback
                   (get-text-property position 'ebb-cell-width string))))
        (aset indices column position)
        (dotimes (extra (1- cell-width))
          (aset indices (+ column extra 1)
                (if compact-wide
                    position
                  (min limit (+ position 1 combining-length extra)))))
        (setq position
              (min limit (+ position 1 combining-length
                            (if (and (> cell-width 1) (not compact-wide))
                                (1- cell-width)
                              0))))
        (cl-incf column cell-width)))
    ;; Renderers may pass a trimmed or empty carrier string.  Reserve one
    ;; character per remaining terminal column; callers pad to these indices
    ;; before applying a display property.
    (while (< column width)
      (aset indices column position)
      (cl-incf column)
      (cl-incf position))
    (aset indices width position)
    indices))

(defun ebb-render--placeholder-diacritic-index (character)
  "Return Kitty's numeric index for combining CHARACTER, or nil."
  (gethash character ebb-render--placeholder-diacritic-indices))

(defun ebb-render--placeholder-coordinates (cell)
  "Decode (ROW COLUMN HIGH-ID-BYTE) from CELL's Kitty diacritics."
  (let* ((marks (string-to-list (or (ebb-cell-combining cell) "")))
         (row (and marks
                   (ebb-render--placeholder-diacritic-index (nth 0 marks))))
         (column (and (cdr marks)
                      (ebb-render--placeholder-diacritic-index (nth 1 marks))))
         (high (and (cddr marks)
                    (ebb-render--placeholder-diacritic-index (nth 2 marks)))))
    (when (and high (> high 255))
      (setq high nil))
    (list row column high)))

(defun ebb-render--apply-virtual-graphics
    (render row string graphics cell-width cell-height &optional absolute)
  "Apply Unicode-placeholder graphics on ROW of STRING.
ROW is history-absolute when ABSOLUTE is non-nil."
  (let* ((screen (ebb-render-state-screen render))
         (line (ebb-render--graphics-line screen row absolute))
         (cells (and line (ebb--line-ensure-cells line (ebb-screen-width screen))))
         (result string)
         (indices (ebb-render--graphics-column-indices
                   line string (ebb-screen-width screen) absolute))
         (per-placement-columns (make-hash-table :test #'eq))
         previous-placeholder previous-low-id previous-placement-id
         previous-row previous-column previous-high-byte)
    (when (and cells (string-search (string #x10eeee) result))
      (dotimes (column (length cells))
        (let* ((cell (aref cells column))
               (attr (ebb-cell-attr cell)))
          (if (not (and (= (ebb-cell-char cell) #x10eeee) attr))
              (setq previous-placeholder nil)
            (pcase-let* ((`(,encoded-row ,encoded-column ,encoded-high-byte)
                          (ebb-render--placeholder-coordinates cell))
                         (low-id (ebb-render--graphics-color-id
                                  (ebb-attr-fg attr)))
                         (placement-id
                          (ebb-render--graphics-color-id
                           (ebb-attr-ul-color attr)))
                         (same-colors
                          (and previous-placeholder
                               (equal low-id previous-low-id)
                               (equal placement-id previous-placement-id)))
                         (tile-row
                          (or encoded-row (and same-colors previous-row)))
                         (tile-column
                          (or encoded-column
                              (and same-colors tile-row
                                   (eql tile-row previous-row)
                                   (integerp previous-column)
                                   (1+ previous-column))))
                         (high-byte
                          (or encoded-high-byte
                              (and same-colors tile-row tile-column
                                   (eql tile-row previous-row)
                                   (integerp previous-column)
                                   (= tile-column (1+ previous-column))
                                   previous-high-byte)
                              0))
                         (image-id (and low-id
                                        (+ low-id (ash high-byte 24))))
                         (placement (and image-id
                                         (ebb-render--virtual-placement
                                          graphics image-id placement-id))))
              (when placement
                (let* ((fallback-column
                        (gethash placement per-placement-columns 0))
                       (tile-column (or tile-column fallback-column))
                       (tile-row
                        (or tile-row
                            (ebb-render--placeholder-tile-row
                             render screen graphics row placement absolute)))
                       (columns (ebb-graphics-placement-columns placement))
                       (rows (ebb-graphics-placement-rows placement))
                       (image (gethash image-id
                                       (ebb-graphics-state-images graphics))))
                  (puthash placement (1+ tile-column) per-placement-columns)
                  (setq previous-row tile-row
                        previous-column tile-column)
                  (when (and image tile-row
                             (< tile-column columns) (< tile-row rows))
                    (when-let* ((object
                                 (ebb-render--graphics-image-object
                                  render image placement)))
                      (unless (multibyte-string-p result)
                        (setq result (string-to-multibyte result)))
                      ;; The carrier string may be trimmed or empty while
                      ;; indices reserve one character per terminal column.
                      ;; Pad first: positions past the end of the string
                      ;; would otherwise signal args-out-of-range here and
                      ;; break the whole refresh.
                      (let* ((text-start (aref indices column))
                             (text-end (max (1+ text-start)
                                            (aref indices (min (1+ column)
                                                               (1- (length indices)))))))
                        (when (< (length result) text-end)
                          (setq result
                                (concat result
                                        (make-string
                                         (- text-end (length result)) ?\s))))
                        (put-text-property
                         text-start text-end 'display
                         (list (list 'slice
                                     (* tile-column cell-width)
                                     (* tile-row cell-height)
                                     cell-width cell-height)
                               object)
                         result))))))
              (setq previous-placeholder t
                    previous-low-id low-id
                    previous-placement-id placement-id
                    previous-row (or tile-row previous-row)
                    previous-column (or tile-column previous-column)
                    previous-high-byte high-byte))))))
    result))

(defun ebb-render--apply-graphics (render row string &optional absolute)
  "Return STRING with graphics placements intersecting ROW.
ROW is viewport-relative unless ABSOLUTE is non-nil."
  (if (not (display-graphic-p))
      string
    (let* ((screen (ebb-render-state-screen render))
           (graphics (ebb-screen-graphics screen))
           (generation (ebb-graphics-state-generation graphics))
           (model-row (if (or absolute (ebb-screen-alt-screen screen))
                          row
                        (+ (ebb-screen-history-row-count screen) row)))
           (cell (or (ebb-render-cell-pixel-size render)
                     (cons (frame-char-width) (frame-char-height))))
           (cell-width (max 1 (car cell)))
           (cell-height (max 1 (cdr cell)))
           (width (ebb-screen-width screen))
           (line (ebb-render--graphics-line screen row absolute))
           (result string)
           (indices (ebb-render--graphics-column-indices
                     line string width absolute)))
      (unless (and (eq graphics (ebb-render-state-graphics-state render))
                   (= generation
                      (ebb-render-state-graphics-generation render)))
        ;; Bottom-most first: ascending z-index, and among equal z-index the
        ;; older placement (later in the newest-first list) below.  Cache this
        ;; order and each row's owner vector for the whole model generation.
        (setf (ebb-render-state-graphics-state render) graphics
              (ebb-render-state-graphics-generation render) generation
              (ebb-render-state-graphics-placement-order render)
              (sort
               (cl-remove-if #'ebb-graphics-placement-virtual
                             (reverse
                              (ebb-graphics-state-placements graphics)))
               (lambda (left right)
                 (let ((left-z (ebb-graphics-placement-z-index left))
                       (right-z (ebb-graphics-placement-z-index right)))
                   (if (= left-z right-z)
                       ;; Lower image IDs have the lower effective z-index, so
                       ;; they are visited first in this bottom-to-top order.
                       (< (ebb-graphics-placement-image-id left)
                          (ebb-graphics-placement-image-id right))
                     (< left-z right-z))))))
        (if (ebb-render-state-graphics-layout-cache render)
            (clrhash (ebb-render-state-graphics-layout-cache render))
          (setf (ebb-render-state-graphics-layout-cache render)
                (make-hash-table :test #'equal))))
      (let* ((layout-cache (or (ebb-render-state-graphics-layout-cache render)
                               (setf (ebb-render-state-graphics-layout-cache
                                      render)
                                     (make-hash-table :test #'equal))))
             (layout-key (cons model-row width))
             (missing (make-symbol "missing"))
             (owners (gethash layout-key layout-cache missing)))
        (when (eq owners missing)
          ;; Absolute model rows advance with scrollback while the graphics
          ;; generation stays unchanged.  Bound the cache: drop everything
          ;; and recompute the one needed row rather than growing one entry
          ;; per scrolled row over a long session.
          (when (>= (hash-table-count layout-cache)
                    (max 1 ebb-kitty-graphics-layout-cache-limit))
            (clrhash layout-cache))
          (setq owners (make-vector width nil))
          (dolist (placement
                   (ebb-render-state-graphics-placement-order render))
            (when (and (<= (ebb-graphics-placement-row placement) model-row)
                       (< model-row
                          (+ (ebb-graphics-placement-row placement)
                             (ebb-graphics-placement-rows placement))))
              (let ((start (max 0 (ebb-graphics-placement-column placement)))
                    (end (min width
                              (+ (ebb-graphics-placement-column placement)
                                 (ebb-graphics-placement-columns placement)))))
                (cl-loop for index from start below end
                         do (aset owners index placement)))))
          ;; A zero limit disables retention while keeping single-row
          ;; recomputation correct.
          (when (> ebb-kitty-graphics-layout-cache-limit 0)
            (puthash layout-key owners layout-cache)))
      (when (cl-find-if #'identity owners)
        ;; Resolve which placement is visible in each column, then emit one
        ;; image slice per run of columns owned by the same placement.  A
        ;; partially covered placement thus shows the correct part of its
        ;; image rather than restarting from its left edge.
        (let ((column 0))
          (while (< column width)
            (if-let* ((owner (aref owners column)))
                (let ((start column))
                  (while (and (< column width) (eq (aref owners column) owner))
                    (cl-incf column))
                  ;; Preserve the placement's cell run even when its image is
                  ;; temporarily unavailable.  Otherwise later placements are
                  ;; rendered against shifted string indices.
                  (when (< (length result) (aref indices column))
                    (setq result
                          (concat result
                                  (make-string
                                   (- (aref indices column) (length result))
                                   ?\s))))
                  (when-let* ((image
                               (gethash (ebb-graphics-placement-image-id owner)
                                        (ebb-graphics-state-images graphics)))
                              (object (ebb-render--graphics-image-object
                                       render image owner)))
                    (let ((slice-row (+ (ebb-graphics-placement-row-offset owner)
                                        (- model-row
                                           (ebb-graphics-placement-row owner))))
                          (slice-column
                           (- start (ebb-graphics-placement-column owner))))
                      (unless (multibyte-string-p result)
                        (setq result (string-to-multibyte result)))
                      (let* ((text-start (aref indices start))
                             (text-end (max (1+ text-start)
                                            (aref indices column))))
                        (when (< (length result) text-end)
                          (setq result
                                (concat result
                                        (make-string
                                         (- text-end (length result)) ?\s))))
                        (put-text-property
                         text-start text-end 'display
                         (list (list 'slice
                                     (* slice-column cell-width)
                                     (* slice-row cell-height)
                                     (* (- column start) cell-width)
                                     cell-height)
                               object)
                         result)))))
              (cl-incf column)))))
      )
      (ebb-render--apply-virtual-graphics
       render row result graphics cell-width cell-height absolute))))

;;;; ---- Display Line Rendering -----------------------------------------

(defun ebb-render--trim-trailing (string)
  "Strip trailing unpropertized spaces from rendered STRING.
Styled blanks (painted backgrounds, hyperlinks), invisible wide-cell
spacers, and characters carrying shell metadata keep their padding.
The width-only face on DEC double-size lines does not make otherwise
empty padding significant."
  (let ((end (length string)))
    (while (and (> end 0)
                (= (aref string (1- end)) ?\s)
                (let ((properties
                       (text-properties-at (1- end) string)))
                  (or (null properties)
                      (and (equal (plist-get properties 'face)
                                  '(:width ultra-expanded))
                           (cl-loop for (property _value) on properties
                                    by #'cddr
                                    always (eq property 'face))))))
      (setq end (1- end)))
    (if (= end (length string)) string (substring string 0 end))))

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
               (new (ebb-render--trim-trailing
                     (ebb-render--apply-graphics
                      render row
                      (ebb-render--apply-line-metadata
                       line (ebb-render--line-to-string line width))))))
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
  "Return STRING with internal raw bytes replaced by Unicode characters.
Clean unibyte ASCII is returned unchanged."
  (let ((length (length string))
        (index 0))
    (while (and (< index length) (<= (aref string index) #x10ffff))
      (cl-incf index))
    (if (= index length)
        ;; Keep clean ASCII strings unibyte.  The model writes them far more
        ;; often than the bounded renderer inserts them into an Emacs buffer.
        string
      (apply #'string
             (cl-loop for position below length
                      collect (ebb-render--safe-char (aref string position)))))))

(defun ebb-render--cells-to-string (cells width)
  "Convert a vector of ebb-cells to a propertized string.
Handles double-width characters by inserting invisible spacers."
  (or (ebb-render--cells-to-string-fast cells width)
      (ebb-render--cells-to-string-uniform cells width)
      (ebb-render--cells-to-string-general cells width)))

(defun ebb-render--apply-attr-properties (string attr)
  "Apply face and hyperlink properties from ATTR to STRING."
  (when-let* ((face (ebb-render--attr-to-face attr)))
    (put-text-property 0 (length string) 'face face string))
  (when-let* ((uri (and attr (ebb-attr-hyperlink attr))))
    (put-text-property 0 (length string) 'help-echo uri string)
    (put-text-property 0 (length string) 'mouse-face 'highlight string)
    (put-text-property 0 (length string) 'keymap ebb-link-map string)
    (put-text-property 0 (length string) 'ebb-link-id
                       (ebb-attr-hyperlink-id attr) string))
  string)

(defun ebb-render--cells-to-string-fast (cells width)
  "Fast path for default-attribute CELLS, or nil if styled.
Builds both single-width and wide characters, and falls back only when a
styled cell is present."
  (catch 'styled
    (let ((parts nil)
          (wide-ranges nil)
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
            (push (concat (string (ebb-render--safe-char (ebb-cell-char cell)))
                          (make-string (1- cw) ?\s))
                  parts)
            (push (cons (1+ pos) (+ pos cw)) wide-ranges)
            (cl-incf pos cw)
            (cl-incf i cw))
           (t
            (push (string (ebb-render--safe-char (ebb-cell-char cell))) parts)
            (cl-incf pos)
            (cl-incf i)))))
      (let ((s (apply #'concat (nreverse parts))))
        (dolist (range wide-ranges)
          (put-text-property (car range) (cdr range) 'invisible t s)
          (put-text-property (car range) (cdr range)
                             'ebb-wide-spacer t s))
        s))))

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
        (let ((s (apply #'string
                        (cl-loop for j below width
                                 collect (ebb-render--safe-char
                                          (ebb-cell-char (aref cells j)))))))
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
        (when-let* ((fg-str (ebb-render--color-to-string fg)))
          (setq face (plist-put face :foreground fg-str))))
      ;; Background (non-inverse)
      (unless inv
        (when-let* ((bg-str (ebb-render--color-to-string bg)))
          (setq face (plist-put face :background bg-str))))
      ;; Underline
      (when-let* ((ul (ebb-attr-underline attr)))
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
         (emacs-mode (eq (bound-and-true-p ebb--input-mode) 'emacs)))
    (when ov
      (if (not visible)
          (progn
            (overlay-put ov 'face nil)
            (overlay-put ov 'after-string nil)
            (if emacs-mode
                ;; Emacs mode uses the native cursor for navigation;
                ;; a hidden terminal cursor (DECTCEM, e.g. fullscreen
                ;; TUIs) must not hide it.  Only the overlay hint goes away.
                (setq-local cursor-type
                            (ebb-render--cursor-type-for-style style))
              (setq-local cursor-type nil)))
        (let* ((region-end (marker-position
                            (ebb-render-state-region-end render)))
               pos after-string)
          (save-excursion
            (goto-char display-begin)
            (forward-line cy)
            (let* ((bol (point))
                   (eol (min (line-end-position) region-end))
                   (target (+ bol cx)))
              (if (< target eol)
                  (setq pos target)
                ;; The cursor sits on a virtual cell beyond the row's
                ;; trimmed content.  Draw it with an `after-string' at
                ;; EOL instead of relying on buffer padding, which no
                ;; longer exists.
                (setq pos eol
                      after-string
                      (concat (make-string (max 0 (- target eol)) ?\s)
                              (propertize " " 'face 'ebb-cursor)))
                ;; Virtual cells must use the row's DEC rendition too;
                ;; otherwise a double-size cursor lands too far left.
                (unless (eq (ebb-line-rendition
                             (ebb--line-at screen cy))
                            'normal)
                  (add-face-text-property
                   0 (length after-string) '(:width ultra-expanded)
                   t after-string)))
              (move-overlay ov pos
                            (if after-string pos
                              (min (1+ pos) region-end)))))
          (overlay-put ov 'after-string after-string)
          (overlay-put ov 'face (if after-string nil 'ebb-cursor))
          (if emacs-mode
              (setq-local cursor-type
                          (ebb-render--cursor-type-for-style style))
            ;; Overlay is the only caret; native cursor would lag at an
            ;; old window-point and look like a second/wrong cursor.
            (setq-local cursor-type nil)
            (goto-char pos)
            (let ((display-start (marker-position display-begin)))
              (dolist (win (get-buffer-window-list nil nil t))
                (set-window-point win pos)
                ;; Pulling window-point to the cursor makes redisplay
                ;; scroll a history-reading window to the viewport anyway.
                ;; Do it coherently: the bounded slab does not necessarily
                ;; extend all the way to the viewport, so redisplay's own
                ;; minimal scroll can render stale history rows directly
                ;; above the live screen.
                (when (< (window-start win) display-start)
                  (set-window-start win display-start t))))))))))

(defun ebb-render--apply-viewport-reset (render)
  "Apply a pending viewport reset, moving windows to RENDER's display start.

Must run after any point/window-start restoration so that restoring a
saved window start cannot override the reset, and regardless of cursor
visibility."
  (when (ebb-screen-take-viewport-reset (ebb-render-state-screen render))
    (let ((display-begin (ebb-render-state-display-begin render)))
      (dolist (win (get-buffer-window-list nil nil t))
        (set-window-start win display-begin t)))))

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
  (setq ebb-render--graphics-background-cache nil)
  (when (boundp 'ebb--render)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let* ((render (buffer-local-value 'ebb--render buffer)))
          (ebb-render--invalidate-screen-lines
           (ebb-render-state-screen render))
          (clrhash (ebb-render-state-history-cache render))
          (ebb-render-full-reset render))))))

(defun ebb-render--install-theme-invalidation ()
  "Install the theme-change invalidation hook."
  (add-hook 'enable-theme-functions #'ebb-render--theme-changed))

(defun ebb-render-invalidate-all (render)
  "Mark all display lines as needing re-render."
  (let* ((screen (ebb-render-state-screen render))
         (h (ebb-screen-height screen)))
    (setf (ebb-screen-dirty-lines screen)
          (number-sequence 0 (1- h))
          (ebb-screen-dirty-map screen) (make-vector h t)
          (ebb-screen-dirty-count screen) h)))

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
                         collect (list window
                                       (ebb-render-buffer-anchor
                                        render (window-start window))
                                       (ebb-render-buffer-anchor
                                        render (window-point window))))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t)
                (buffer-undo-list t))
            (goto-char (ebb-render-state-region-begin render))
            (delete-region (point)
                           (ebb-render-state-region-end render))
          ;; Materialize a slab covering every window's stable model anchor.
          (let* ((total (ebb-screen-history-row-count screen))
                 (capacity (ebb-render--history-capacity render))
                 ;; Window points count too; see the scroll-history slab
                 ;; sizing.
                 (rows
                  (delq nil
                        (cl-loop for (nil start-anchor window-point-anchor)
                                 in saved-windows
                                 collect (ebb-render--anchor-history-row
                                          render start-anchor total)
                                 collect (ebb-render--anchor-history-row
                                          render window-point-anchor total))))
                 (start (if rows
                            (max 0 (- (apply #'min rows) (/ capacity 3)))
                          (max 0 (- total capacity))))
                 (needed-end (if rows
                                 (min total (+ (apply #'max rows) capacity))
                               total))
                 (count (min (- total start)
                             (max capacity (- needed-end start))))
                 ;; One signature for the whole slab: it is identical for
                 ;; every row of a single rebuild.
                 (graphics-signature
                  (ebb-render--history-graphics-signature screen)))
            (dotimes (offset count)
              (insert (ebb-render--history-row-string
                       render (+ start offset) graphics-signature)))
            (setf (ebb-render-state-history-start-row render) start
                  (ebb-render-state-scrollback-count render) count
                  (ebb-render-state-history-total-rows render) total
                  (ebb-render-state-history-generation render)
                  (ebb-screen-history-generation screen)
                  (ebb-render-state-history-graphics-generation render)
                  (ebb-render--history-graphics-signature screen)))
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
                          (ebb-render--trim-trailing
                           (ebb-render--apply-graphics
                            render i
                            (ebb-render--apply-line-metadata
                             line (ebb-render--line-to-string line w))))
                        ""))
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
          (pcase-dolist (`(,window ,start-anchor ,window-point-anchor)
                         saved-windows)
            (when (window-live-p window)
              (if-let* ((position
                        (ebb-render--anchor-buffer-position
                         render start-anchor)))
                  (set-window-start window position t)
                (set-window-start
                 window (ebb-render-state-display-begin render) t))
              (ebb-render--restore-window-point
               render window window-point-anchor)))
          ;; Update the cursor after window restoration so the live input
          ;; modes can move window-point to the terminal cursor.
          (ebb-render--update-cursor render)
          (ebb-render--apply-viewport-reset render))))))

(defun ebb-render-resize-height (render)
  "Rebuild only RENDER's viewport after a height-only terminal resize."
  (let* ((screen (ebb-render-state-screen render))
         (buffer (ebb-render-state-buffer render))
         (width (ebb-screen-width screen))
         (height (ebb-screen-height screen)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let* ((inhibit-read-only t)
               (inhibit-modification-hooks t)
               (buffer-undo-list t)
               (display-begin (ebb-render-state-display-begin render))
               (emacs-mode (eq (bound-and-true-p ebb--input-mode) 'emacs))
               (windows
                (cl-loop for window in (get-buffer-window-list buffer nil t)
                         collect
                         (list window
                               (ebb-render-buffer-anchor
                                render (window-start window))
                               (ebb-render-buffer-anchor
                                render (window-point window)))))
               ;; A shrinking window may move `window-start' into scrollback
               ;; before this hook runs merely to keep its live cursor visible.
               ;; In terminal input modes window-point identifies windows that
               ;; should continue following the terminal.  Emacs mode must
               ;; instead preserve intentional history scrolling.
               (following-windows
                (unless emacs-mode
                  (cl-loop for (window . _anchor) in windows
                           when (>= (window-point window)
                                    (marker-position display-begin))
                           collect window))))
          (ebb-render--update-scrollback render)
          (save-excursion
            (goto-char display-begin)
            (delete-region (point) (ebb-render-state-region-end render))
            (set-marker-insertion-type display-begin nil)
            (dotimes (row height)
              (let ((line (ebb-screen-get-line screen row)))
                (insert (if line
                            (ebb-render--trim-trailing
                             (ebb-render--apply-graphics
                              render row
                              (ebb-render--apply-line-metadata
                               line (ebb-render--line-to-string line width))))
                          ""))
                (when (< row (1- height)) (insert "\n"))))
            (set-marker-insertion-type display-begin t))
          (pcase-dolist (`(,window ,start-anchor ,window-point-anchor)
                         windows)
            (when (window-live-p window)
              (if (memq window following-windows)
                  (set-window-start window display-begin t)
                (if-let* ((position
                           (ebb-render--anchor-buffer-position
                            render start-anchor)))
                    (set-window-start window position t)
                  (set-window-start window display-begin t)))
              ;; Rebuilding the viewport collapsed window-points into
              ;; display-begin; restore them for every window.  The cursor
              ;; update below re-targets following windows only while the
              ;; terminal cursor is visible.
              (ebb-render--restore-window-point
               render window window-point-anchor)))
          ;; Update the cursor after window restoration so the live input
          ;; modes can move window-point to the terminal cursor.
          (ebb-render--update-cursor render)
          (ebb-render--apply-viewport-reset render)
          (ebb-screen-clear-dirty screen)
          (ebb-screen-clear-scrollback-dirty screen))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun ebb-render-destroy (render)
  "Clean up render state."
  (when-let* ((ov (ebb-render-state-cursor-overlay render)))
    (delete-overlay ov))
  (dolist (m (list (ebb-render-state-display-begin render)
                   (ebb-render-state-region-begin render)
                   (ebb-render-state-region-end render)))
    (when m (set-marker m nil))))

(ebb-render--install-theme-invalidation)

(provide 'ebb-render)
;;; ebb-render.el ends here
