;;; ebb-parse.el --- VT escape sequence parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; State machine parser for VT100/VT220/xterm escape sequences.
;; Consumes characters and drives ebb-screen operations.
;;
;; Design: O(1) CSI dispatch via vector table.  Every handler is
;; wrapped in error recovery -- unknown sequences are logged, never crash.
;; The parser is a pure transformer: characters in, screen operations out.

;;; Code:

(require 'cl-lib)
(require 'url-util)
(require 'ebb-term)

(defcustom ebb-enable-osc52 nil
  "If non-nil, allow terminal output to access the clipboard via OSC 52."
  :type 'boolean
  :group 'ebb)

;;;; ---- Data Structures ------------------------------------------------

(cl-defstruct (ebb-parser (:copier nil))
  "VT escape sequence parser."
  (state :ground)        ; current state keyword
  (screen nil)           ; ebb-screen to operate on
  (write-fn nil)         ; (lambda (string)) send bytes to PTY
  (emit-fn nil)          ; (lambda (type &rest args)) event callback
  ;; CSI collection
  (param-string "")      ; digits, semicolons, colons
  (private nil)          ; ?/>/= prefix char or nil
  (intermediates "")     ; ESC/CSI intermediate bytes (space etc.)
  ;; String collection
  (osc-string "")
  (dcs-string "")
  (dcs-params "")
  (dcs-final 0)
  ;; State for ESC inside string sequences
  (string-state nil)     ; :osc or :dcs when ESC seen inside string
  ;; Charset
  (charset-slot 0))

;;;; ---- Logging / Emit / Respond ---------------------------------------

(defvar ebb-parse-debug nil
  "When non-nil, log unknown escape sequences to *Messages*.")

(defvar ebb-parse--conformance-levels
  (make-hash-table :test #'eq :weakness 'key)
  "DECSCL conformance level keyed by terminal screen.")

(defvar ebb-parse--page-lengths
  (make-hash-table :test #'eq :weakness 'key)
  "Requested page length keyed by terminal screen.")

(defvar ebb-parse--ansi-mode-states
  (make-hash-table :test #'eq :weakness 'key)
  "ANSI mode state maps keyed by terminal screen.")

(defvar ebb-parse--dec-mode-states
  (make-hash-table :test #'eq :weakness 'key)
  "DEC private mode state maps keyed by terminal screen.")

(defun ebb-parse--record-mode (table screen mode enabled)
  "Record MODE as ENABLED for SCREEN in TABLE."
  (let ((states (or (gethash screen table)
                    (let ((map (make-hash-table :test #'eql)))
                      (puthash screen map table)
                      map))))
    (puthash mode (and enabled t) states)))

(defun ebb-parse--recorded-mode (table screen mode)
  "Return recorded MODE state for SCREEN from TABLE."
  (when-let* ((states (gethash screen table)))
    (gethash mode states)))

(defun ebb-parse--log (fmt &rest args)
  "Log a parser message when debug is enabled."
  (when ebb-parse-debug
    (apply #'message (concat "[ebb-parse] " fmt) args)))

(defun ebb-parse--respond (parser response)
  "Send RESPONSE string back to the PTY."
  (when-let* ((fn (ebb-parser-write-fn parser)))
    (funcall fn response)))

(defun ebb-parse--color-to-xterm (color-str)
  "Convert an Emacs color string to xterm rgb:RRRR/GGGG/BBBB format."
  (cond
   ((not color-str)
    "rgb:ffff/ffff/ffff")
   ;; Avoid display-color approximation for exact palette replies.
   ((string-match "\\`#\\([[:xdigit:]][[:xdigit:]]\\)\\([[:xdigit:]][[:xdigit:]]\\)\\([[:xdigit:]][[:xdigit:]]\\)\\'" color-str)
    (format "rgb:%s%s/%s%s/%s%s"
            (match-string 1 color-str) (match-string 1 color-str)
            (match-string 2 color-str) (match-string 2 color-str)
            (match-string 3 color-str) (match-string 3 color-str)))
   (t
    (let ((rgb (color-values color-str)))
      (if rgb
          (format "rgb:%04x/%04x/%04x"
                  (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
        "rgb:ffff/ffff/ffff")))))

(defun ebb-parse--256color-hex (n)
  "Return the default xterm 256-color palette entry N as #RRGGBB."
  (cond
   ((< n 16)
    (aref ["#000000" "#cd0000" "#00cd00" "#cdcd00"
           "#0000ee" "#cd00cd" "#00cdcd" "#e5e5e5"
           "#7f7f7f" "#ff0000" "#00ff00" "#ffff00"
           "#5c5cff" "#ff00ff" "#00ffff" "#ffffff"]
          n))
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

(defun ebb-parse--palette-color-to-xterm (n)
  "Return palette index N as an xterm rgb:RRRR/GGGG/BBBB string."
  (ebb-parse--color-to-xterm (ebb-parse--256color-hex n)))

(defun ebb-parse--emit (parser type &rest args)
  "Emit an event TYPE with ARGS via the parser callback."
  (when-let* ((fn (ebb-parser-emit-fn parser)))
    (apply fn type args)))

;;;; ---- Parameter Parsing ----------------------------------------------

(defun ebb-parse--parse-params (param-str)
  "Parse CSI parameter string into a vector of integers or sub-param lists.
Splits on semicolons.  Within each parameter, colons delimit sub-parameters.
A parameter with colons becomes a list of integers; without, a plain integer.
Empty segments become 0.  Values clamped to 16384."
  (if (string-empty-p param-str)
      (vector)
    (vconcat
     (mapcar (lambda (s)
               (if (string-match-p ":" s)
                   ;; Sub-parameters: return as a list
                   (mapcar (lambda (sub)
                             (if (string-empty-p sub) 0
                               (min (string-to-number sub) 16384)))
                           (split-string s ":"))
                 ;; Simple parameter
                 (if (string-empty-p s) 0
                   (min (string-to-number s) 16384))))
             (split-string param-str ";")))))

(defsubst ebb-parse--param (params index default)
  "Get PARAMS[INDEX], or DEFAULT if missing or zero.
If the parameter is a sub-parameter list, return the first element."
  (let ((val (if (< index (length params)) (aref params index) 0)))
    (cond
     ((listp val) (let ((v (car val))) (if (zerop v) default v)))
     ((zerop val) default)
     (t val))))

;;;; ---- Constructor ----------------------------------------------------

(defun ebb-parse-create (screen &optional write-fn emit-fn)
  "Create a parser for SCREEN with optional WRITE-FN and EMIT-FN."
  (make-ebb-parser
   :screen screen
   :write-fn write-fn
   :emit-fn emit-fn))

(defun ebb-parse-cancel-sequence (parser)
  "Return PARSER to ground state, discarding an incomplete sequence."
  (setf (ebb-parser-state parser) :ground
        (ebb-parser-param-string parser) ""
        (ebb-parser-private parser) nil
        (ebb-parser-intermediates parser) ""
        (ebb-parser-osc-string parser) ""
        (ebb-parser-dcs-string parser) ""
        (ebb-parser-dcs-params parser) ""
        (ebb-parser-dcs-final parser) 0
        (ebb-parser-string-state parser) nil
        (ebb-parser-charset-slot parser) 0))

;;;; ---- Entry Point ----------------------------------------------------

(defun ebb-parse-bytes (parser string &optional start end)
  "Parse STRING from START to END through PARSER.
Returns the number of characters consumed."
  (let ((i (or start 0))
        (e (or end (length string)))
        (multibyte (multibyte-string-p string))
        (screen (ebb-parser-screen parser)))
    (while (< i e)
      (let ((ch (aref string i)))
        ;; Emacs preserves malformed decoded bytes as internal eight-bit
        ;; characters.  They cannot be stored in normal multibyte render
        ;; strings, so display them as the Unicode replacement character.
        (cond
         ((>= ch #x3fff80)
          (ebb-parse--process-char parser #xfffd)
          (cl-incf i))
         ;; C1 bytes are 8-bit controls.  Only ST (U+009C) is implemented: it
         ;; terminates a pending control string, mirroring the 7-bit ST
         ;; (ESC \).  Other 8-bit C1 forms are consumed without effect.
         ((and (>= ch #x80) (<= ch #x9f))
          (when (= ch #x9c)
            (pcase (ebb-parser-state parser)
              (:osc-string (ebb-parse--dispatch-osc parser))
              (:dcs-passthrough (ebb-parse--dispatch-dcs parser)))
            ;; An ESC inside a string defers the dispatch to the terminator;
            ;; complete the pending string here, as the 7-bit ST (ESC \\) does.
            (when (ebb-parser-string-state parser)
              (ebb-parse--complete-string parser))
            (setf (ebb-parser-state parser) :ground
                  (ebb-parser-string-state parser) nil))
          (cl-incf i))
         (t
          ;; The common case for terminal output is a run of printable text while
          ;; the parser is in ground state.  Dispatch that whole run to the
          ;; screen model at once instead of re-entering the parser for each byte.
          (if (and (eq (ebb-parser-state parser) :ground)
                   (= ch ?\e)
                   (< (+ i 2) e)
                   (= (aref string (1+ i)) ?\[))
              (let ((next (ebb-parse--fast-csi-at parser string (+ i 2) e)))
                (if next
                    (setq i next)
                  (ebb-parse--process-char parser ch)
                  (cl-incf i)))
            (if (and (eq (ebb-parser-state parser) :ground)
                     (>= ch ?\s)
                     (/= ch ?\x7f))
                (let ((run-start i))
                  (if multibyte
                      (while (and (< i e)
                                  (let ((c (aref string i)))
                                    (and (< c #x3fff80)
                                         (not (and (>= c #x80) (<= c #x9f)))
                                         (>= c ?\s)
                                         (/= c ?\x7f))))
                        (cl-incf i))
                    (while (and (< i e)
                                (let ((c (aref string i)))
                                  (and (>= c ?\s) (< c #x7f))))
                      (cl-incf i)))
                  (let ((crlf (and (< (1+ i) e)
                                   (= (aref string i) ?\r)
                                   (= (aref string (1+ i)) ?\n)))
                        block-end)
                    ;; Only probe for a block after the ordinary hot-path scan
                    ;; has already found a CRLF.  TUI and escape-heavy runs do
                    ;; not pay for a second failed printable scan.
                    (when (and crlf (not multibyte))
                      (setq block-end
                            (ebb-screen-write-crlf-block
                             screen string run-start e)))
                    (if block-end
                        (setq i block-end)
                      (ebb-screen-write-string screen string run-start i)
                      (when crlf
                        (ebb-screen-carriage-return screen)
                        (ebb-screen-index screen)
                        (cl-incf i 2)))))
              (ebb-parse--process-char parser ch)
              (cl-incf i)))))))
    (- i (or start 0))))

(defun ebb-parse--fast-csi-at (parser string start end)
  "Handle a common CSI beginning at START, returning the next index.
START is the first byte after ESC [.  Return nil when the sequence is not one
of the simple forms handled here."
  (catch 'done
    (let ((j start))
      (while (< j end)
        (let ((c (aref string j)))
          (cond
           ((or (and (>= c ?0) (<= c ?9)) (= c ?\;))
            (cl-incf j))
           ((and (>= c ?@) (<= c ?~))
            (let ((screen (ebb-parser-screen parser)))
              (cond
               ((= c ?H)
                (let ((row 0)
                      (col 0)
                      (in-col nil)
                      (k start))
                  (while (< k j)
                    (let ((p (aref string k)))
                      (if (= p ?\;)
                          (if in-col
                              (throw 'done nil)
                            (setq in-col t))
                        (if in-col
                            (setq col (+ (* col 10) (- p ?0)))
                          (setq row (+ (* row 10) (- p ?0))))))
                    (cl-incf k))
                  (ebb-screen-cursor-goto screen
                                            (1- (if (zerop row) 1 row))
                                            (1- (if (zerop col) 1 col)))
                  (throw 'done (1+ j))))
               ((and (= c ?J)
                     (= (- j start) 1)
                     (= (aref string start) ?2))
                (ebb-screen-erase-in-display screen 2)
                (throw 'done (1+ j)))
               ((= c ?m)
                (if (ebb-parse--fast-sgr-at parser string start j)
                    (throw 'done (1+ j))
                  (throw 'done nil)))
               (t (throw 'done nil)))))
           (t (throw 'done nil))))))))

(defun ebb-parse--fast-sgr-at (parser string start end)
  "Apply simple SGR params from STRING[START, END); return non-nil if handled.
This avoids the allocation-heavy generic `split-string' parameter parser for
common color-heavy output.  The caller has already verified that the bytes are
only digits and semicolons."
  (let ((screen (ebb-parser-screen parser)))
    (if (= start end)
        (progn
          (ebb-screen-reset-attr screen)
          t)
      (or (ebb-parse--fast-simple-sgr-at screen string start end)
          (catch 'fallback
        ;; Most SGR sequences are very short.  Keep the vector stack-local and
        ;; fall back for unusually long forms rather than growing structures in
        ;; the hot path.
        (let ((params (make-vector 16 0))
              (count 0)
              (value 0)
              (have-value nil)
              (i start))
          (while (<= i end)
            (if (or (= i end) (= (aref string i) ?\;))
                (progn
                  (when (= count (length params))
                    (throw 'fallback nil))
                  (aset params count (if have-value value 0))
                  (cl-incf count)
                  (setq value 0)
                  (setq have-value nil))
              (setq value (+ (* value 10) (- (aref string i) ?0)))
              (setq have-value t))
            (cl-incf i))
          (let ((idx 0))
            (while (< idx count)
              (let ((p (aref params idx)))
                (cond
                 ((= p 0)  (ebb-screen-reset-attr screen))
                 ((= p 1)  (ebb-screen-set-attr screen :bold t))
                 ((= p 2)  (ebb-screen-set-attr screen :faint t))
                 ((= p 3)  (ebb-screen-set-attr screen :italic t))
                 ((= p 4)  (ebb-screen-set-attr screen :underline 'line))
                 ((= p 5)  (ebb-screen-set-attr screen :blink 'slow))
                 ((= p 6)  (ebb-screen-set-attr screen :blink 'fast))
                 ((= p 7)  (ebb-screen-set-attr screen :inverse t))
                 ((= p 8)  (ebb-screen-set-attr screen :conceal t))
                 ((= p 9)  (ebb-screen-set-attr screen :crossed t))
                 ((and (>= p 10) (<= p 19))
                  (ebb-screen-set-attr screen :font (- p 10)))
                 ((= p 21) (ebb-screen-set-attr screen :underline 'double))
                 ((= p 22) (ebb-screen-set-attr screen :bold nil)
                            (ebb-screen-set-attr screen :faint nil))
                 ((= p 23) (ebb-screen-set-attr screen :italic nil))
                 ((= p 24) (ebb-screen-set-attr screen :underline nil))
                 ((= p 25) (ebb-screen-set-attr screen :blink nil))
                 ((= p 27) (ebb-screen-set-attr screen :inverse nil))
                 ((= p 28) (ebb-screen-set-attr screen :conceal nil))
                 ((= p 29) (ebb-screen-set-attr screen :crossed nil))
                 ((and (>= p 30) (<= p 37))
                  (ebb-screen-set-attr screen :fg (- p 30)))
                 ((= p 38)
                  (cond
                   ((and (< (+ idx 2) count) (= (aref params (1+ idx)) 5))
                    (ebb-screen-set-attr screen :fg (aref params (+ idx 2)))
                    (cl-incf idx 2))
                   ((and (< (+ idx 4) count) (= (aref params (1+ idx)) 2))
                    (ebb-screen-set-attr
                     screen :fg
                     (list (aref params (+ idx 2))
                           (aref params (+ idx 3))
                           (aref params (+ idx 4))))
                    (cl-incf idx 4))
                   (t (throw 'fallback nil))))
                 ((= p 39) (ebb-screen-set-attr screen :fg nil))
                 ((and (>= p 40) (<= p 47))
                  (ebb-screen-set-attr screen :bg (- p 40)))
                 ((= p 48)
                  (cond
                   ((and (< (+ idx 2) count) (= (aref params (1+ idx)) 5))
                    (ebb-screen-set-attr screen :bg (aref params (+ idx 2)))
                    (cl-incf idx 2))
                   ((and (< (+ idx 4) count) (= (aref params (1+ idx)) 2))
                    (ebb-screen-set-attr
                     screen :bg
                     (list (aref params (+ idx 2))
                           (aref params (+ idx 3))
                           (aref params (+ idx 4))))
                    (cl-incf idx 4))
                   (t (throw 'fallback nil))))
                 ((= p 49) (ebb-screen-set-attr screen :bg nil))
                 ((= p 58)
                  (cond
                   ((and (< (+ idx 2) count) (= (aref params (1+ idx)) 5))
                    (ebb-screen-set-attr screen :ul-color (aref params (+ idx 2)))
                    (cl-incf idx 2))
                   ((and (< (+ idx 4) count) (= (aref params (1+ idx)) 2))
                    (ebb-screen-set-attr
                     screen :ul-color
                     (list (aref params (+ idx 2))
                           (aref params (+ idx 3))
                           (aref params (+ idx 4))))
                    (cl-incf idx 4))
                   (t (throw 'fallback nil))))
                 ((= p 59) (ebb-screen-set-attr screen :ul-color nil))
                 ((and (>= p 90) (<= p 97))
                  (ebb-screen-set-attr screen :fg (+ 8 (- p 90))))
                 ((and (>= p 100) (<= p 107))
                  (ebb-screen-set-attr screen :bg (+ 8 (- p 100))))
                 (t (throw 'fallback nil))))
              (cl-incf idx)))
          t))))))

(defun ebb-parse--fast-simple-sgr-at (screen string start end)
  "Handle the shortest and most frequent SGR forms in STRING[START, END)."
  (let ((len (- end start)))
    (cond
     ((and (= len 1) (= (aref string start) ?0))
      (ebb-screen-reset-attr screen)
      t)
     ((and (= len 2)
           (= (aref string start) ?3)
           (<= ?0 (aref string (1+ start)))
           (<= (aref string (1+ start)) ?7))
      (ebb-screen-set-attr screen :fg (- (aref string (1+ start)) ?0))
      t)
     ((and (= len 2)
           (= (aref string start) ?4)
           (<= ?0 (aref string (1+ start)))
           (<= (aref string (1+ start)) ?7))
      (ebb-screen-set-attr screen :bg (- (aref string (1+ start)) ?0))
      t))))

;;;; ---- Main Dispatch --------------------------------------------------

(defun ebb-parse--process-char (parser ch)
  "Process a single character CH through the parser state machine."
  (cond
   ;; ESC always starts a new escape sequence
   ((= ch ?\e)
    ;; Save string-state if we were inside OSC/DCS
    (setf (ebb-parser-string-state parser)
          (pcase (ebb-parser-state parser)
            (:osc-string :osc)
            (:dcs-passthrough :dcs)
            (_ nil)))
    (let ((inside-dcs (eq (ebb-parser-state parser) :dcs-passthrough)))
      (setf (ebb-parser-state parser) :escape)
      (setf (ebb-parser-param-string parser) "")
      (setf (ebb-parser-private parser) nil)
      ;; Preserve DCS intermediates until ST dispatches the completed string.
      (unless inside-dcs
        (setf (ebb-parser-intermediates parser) ""))))

   ;; CAN and SUB abort any in-progress escape sequence, control sequence,
   ;; or control string and return to the ground state.
   ((memq ch '(?\x18 ?\x1a))
    (unless (eq (ebb-parser-state parser) :ground)
      (ebb-parse-cancel-sequence parser)))

   ;; C0 controls (0x00-0x1F) handled inline in most states
   ((and (< ch ?\s)
         (memq (ebb-parser-state parser)
               '(:ground :escape :escape-intermediate
                 :csi-entry :csi-param :csi-intermediate :csi-ignored)))
    (ebb-parse--dispatch-c0 parser ch))

   ;; DEL -- ignore
   ((= ch ?\x7f) nil)

   ;; State-specific processing
   (t
    (pcase (ebb-parser-state parser)
      (:ground            (ebb-parse--ground parser ch))
      (:escape            (ebb-parse--escape parser ch))
      (:escape-intermediate
       (ebb-parse--escape-intermediate parser ch))
      (:csi-entry         (ebb-parse--csi-entry parser ch))
      (:csi-param         (ebb-parse--csi-param parser ch))
      (:csi-intermediate  (ebb-parse--csi-intermediate parser ch))
      (:csi-ignored       (ebb-parse--csi-ignored parser ch))
      (:osc-string        (ebb-parse--osc-string parser ch))
      (:dcs-entry         (ebb-parse--dcs-entry parser ch))
      (:dcs-param         (ebb-parse--dcs-param parser ch))
      (:dcs-passthrough   (ebb-parse--dcs-passthrough parser ch))
      (:charset-designate (ebb-parse--charset-designate parser ch))
      (:sos-pm-apc        (ebb-parse--sos-pm-apc parser ch))))))

;;;; ---- C0 Control Dispatch --------------------------------------------

(defun ebb-parse--dispatch-c0 (parser ch)
  "Handle C0 control character CH."
  (let ((screen (ebb-parser-screen parser)))
    (cond
     ((= ch ?\a) (ebb-parse--emit parser 'bell))          ; BEL
     ((= ch ?\b) (ebb-screen-backspace screen))            ; BS
     ((= ch ?\t) (ebb-screen-tab-forward screen 1))        ; HT
     ((= ch ?\n) (ebb-screen-index screen))                ; LF
     ((= ch ?\v) (ebb-screen-index screen))                ; VT
     ((= ch ?\f) (ebb-screen-index screen))                ; FF
     ((= ch ?\r) (ebb-screen-carriage-return screen))      ; CR
     ((= ch 14)  (ebb-screen-shift-out screen))            ; SO
     ((= ch 15)  (ebb-screen-shift-in screen))             ; SI
     (t nil))))                                               ; NUL etc.

;;;; ---- State: Ground --------------------------------------------------

(defun ebb-parse--ground (parser ch)
  "Handle printable character in ground state."
  (when (>= ch ?\s)
    (ebb-screen-write-char (ebb-parser-screen parser) ch)))

;;;; ---- State: Escape --------------------------------------------------

(defun ebb-parse--escape (parser ch)
  "Handle character after ESC."
  ;; Preserve DCS intermediates only for the ESC \\ string terminator.
  (unless (and (= ch ?\\) (ebb-parser-string-state parser))
    (setf (ebb-parser-intermediates parser) ""))
  (cond
   ;; ST (ESC \) terminates a pending string
   ((and (= ch ?\\) (ebb-parser-string-state parser))
    (ebb-parse--complete-string parser)
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-state parser) :ground))
   ;; CSI
   ((= ch ?\[)
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-state parser) :csi-entry))
   ;; OSC
   ((= ch ?\])
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-osc-string parser) "")
    (setf (ebb-parser-state parser) :osc-string))
   ;; DCS
   ((= ch ?P)
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-dcs-params parser) "")
    (setf (ebb-parser-dcs-string parser) "")
    (setf (ebb-parser-intermediates parser) "")
    (setf (ebb-parser-state parser) :dcs-entry))
   ;; Charset designation
   ((memq ch '(?\( ?\) ?* ?+ ?- ?. ?/))
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-charset-slot parser) ch)
    (setf (ebb-parser-state parser) :charset-designate))
   ;; SOS / PM / APC
   ((memq ch '(?X ?^ ?_))
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-state parser) :sos-pm-apc))
   ;; Other ESC intermediate sequences, such as DECALN (ESC # 8).
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-string-state parser) nil)
    (setf (ebb-parser-intermediates parser) (string ch))
    (setf (ebb-parser-state parser) :escape-intermediate))
   ;; Simple ESC commands
   (t
    (setf (ebb-parser-string-state parser) nil)
    (ebb-parse--dispatch-esc parser ch)
    (setf (ebb-parser-state parser) :ground))))

(defun ebb-parse--escape-intermediate (parser ch)
  "Collect and dispatch an ESC sequence with intermediate bytes."
  (cond
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser)
          (concat (ebb-parser-intermediates parser) (string ch))))
   ((and (>= ch ?0) (<= ch ?~))
    (ebb-parse--dispatch-esc-intermediate parser ch)
    (setf (ebb-parser-state parser) :ground))
   (t
    (setf (ebb-parser-state parser) :ground))))

(defun ebb-parse--dispatch-esc-intermediate (parser ch)
  "Dispatch an ESC sequence ending in CH after intermediate bytes."
  (let ((intermediates (ebb-parser-intermediates parser))
        (screen (ebb-parser-screen parser)))
    (condition-case err
        (cond
         ((string= intermediates "#")
          (pcase ch
            (?3 (ebb-screen-set-line-rendition screen 'double-height-top))
            (?4 (ebb-screen-set-line-rendition screen 'double-height-bottom))
            (?5 (ebb-screen-set-line-rendition screen 'normal))
            (?6 (ebb-screen-set-line-rendition screen 'double-width))
            (?8 (ebb-screen-alignment-test screen))
            (_ (ebb-parse--log "Unknown ESC #%c" ch))))
         ;; ESC % G and ESC % @ select UTF-8 and ISO 2022 respectively.
         ;; Input decoding is managed by the process coding system, but the
         ;; complete sequence must still be consumed rather than displayed.
         ((and (string= intermediates "%") (memq ch '(?G ?@))) nil)
         (t
          (ebb-parse--log "Unknown ESC %s%c" intermediates ch)))
      (error
       (ebb-parse--log "ESC intermediate dispatch error for %s%c: %S"
                       intermediates ch err)))))

(defun ebb-parse--dispatch-esc (parser ch)
  "Dispatch a simple ESC sequence."
  (let ((screen (ebb-parser-screen parser)))
    (condition-case err
        (pcase ch
          (?7 (ebb-screen-save-cursor screen))
          (?8 (ebb-screen-restore-cursor screen))
          (?D (ebb-screen-index screen))
          (?E (ebb-screen-next-line screen))
          (?H (ebb-screen-set-tab-stop screen))
          (?M (ebb-screen-reverse-index screen))
          (?V (ebb-screen-set-iso-protection screen t))
          (?W (ebb-screen-set-iso-protection screen nil))
          (?c (ebb-screen-reset screen)
              (remhash screen ebb-parse--conformance-levels)
              (remhash screen ebb-parse--page-lengths)
              (remhash screen ebb-parse--ansi-mode-states)
              (remhash screen ebb-parse--dec-mode-states)
              (ebb-parse--emit parser 'reset))
          (?n (setf (ebb-screen-charset-active screen) 'g2))
          (?o (setf (ebb-screen-charset-active screen) 'g3))
          (_ (ebb-parse--log "Unknown ESC %c (0x%02x)" ch ch)))
      (error (ebb-parse--log "ESC dispatch error for %c: %S" ch err)))))

(defun ebb-parse--complete-string (parser)
  "Complete a pending OSC or DCS string."
  (pcase (ebb-parser-string-state parser)
    (:osc (ebb-parse--dispatch-osc parser))
    (:dcs (ebb-parse--dispatch-dcs parser))))

;;;; ---- State: CSI Entry -----------------------------------------------

(defun ebb-parse--csi-entry (parser ch)
  "Handle first char after CSI (ESC [)."
  (cond
   ;; Private marker
   ((memq ch '(?? ?> ?=))
    (setf (ebb-parser-private parser) ch)
    (setf (ebb-parser-state parser) :csi-param))
   ;; Parameter char
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;) (= ch ?:))
    (setf (ebb-parser-param-string parser) (string ch))
    (setf (ebb-parser-state parser) :csi-param))
   ;; Intermediate byte
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser) (string ch))
    (setf (ebb-parser-state parser) :csi-intermediate))
   ;; Final byte -- dispatch immediately
   ((and (>= ch ?@) (<= ch ?~))
    (ebb-parse--dispatch-csi parser ch))
   ;; Invalid
   (t (setf (ebb-parser-state parser) :ground))))

;;;; ---- State: CSI Param -----------------------------------------------

(defun ebb-parse--csi-param (parser ch)
  "Collect CSI parameters."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;) (= ch ?:))
    (setf (ebb-parser-param-string parser)
          (concat (ebb-parser-param-string parser) (string ch))))
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser) (string ch))
    (setf (ebb-parser-state parser) :csi-intermediate))
   ((and (>= ch ?@) (<= ch ?~))
    (ebb-parse--dispatch-csi parser ch))
   (t (setf (ebb-parser-state parser) :ground))))

;;;; ---- State: CSI Intermediate ----------------------------------------

(defun ebb-parse--csi-intermediate (parser ch)
  "Collect CSI intermediate bytes."
  (cond
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser)
          (concat (ebb-parser-intermediates parser) (string ch))))
   ((and (>= ch ?@) (<= ch ?~))
    (ebb-parse--dispatch-csi parser ch))
   ;; A parameter byte after an intermediate byte makes the sequence
   ;; malformed; consume it (and the remaining bytes) without effect.
   ((and (>= ch ?0) (<= ch ??))
    (setf (ebb-parser-state parser) :csi-ignored))
   (t (setf (ebb-parser-state parser) :ground))))

(defun ebb-parse--csi-ignored (parser ch)
  "Consume the remainder of a malformed CSI sequence.
The sequence ends at the first final byte; C0 controls and ESC are handled
in process-char."
  (when (and (>= ch ?@) (<= ch ?~))
    (setf (ebb-parser-state parser) :ground)))

;;;; ---- State: OSC String ----------------------------------------------

(defun ebb-parse--osc-string (parser ch)
  "Collect OSC string payload."
  (cond
   ;; BEL terminates
   ((= ch ?\a)
    (ebb-parse--dispatch-osc parser)
    (setf (ebb-parser-state parser) :ground))
   ;; ESC handled in process-char
   ;; C0 controls are discarded while the OSC string continues.
   ((< ch ?\s) nil)
   ;; Accumulate (limit length for safety)
   ((< (length (ebb-parser-osc-string parser)) 65536)
    (setf (ebb-parser-osc-string parser)
          (concat (ebb-parser-osc-string parser) (string ch))))))

;;;; ---- State: DCS Entry/Param/Passthrough -----------------------------

(defun ebb-parse--dcs-entry (parser ch)
  "Handle first char after DCS (ESC P)."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;))
    (setf (ebb-parser-dcs-params parser) (string ch))
    (setf (ebb-parser-state parser) :dcs-param))
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser)
          (concat (ebb-parser-intermediates parser) (string ch))))
   ((and (>= ch ?@) (<= ch ?~))
    (setf (ebb-parser-dcs-final parser) ch)
    (setf (ebb-parser-state parser) :dcs-passthrough))
   ;; C0 controls are ignored in the DCS header and the state is preserved.
   ((< ch ?\s) nil)
   (t (setf (ebb-parser-state parser) :ground))))

(defun ebb-parse--dcs-param (parser ch)
  "Collect DCS parameters."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;))
    (setf (ebb-parser-dcs-params parser)
          (concat (ebb-parser-dcs-params parser) (string ch))))
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (ebb-parser-intermediates parser)
          (concat (ebb-parser-intermediates parser) (string ch))))
   ((and (>= ch ?@) (<= ch ?~))
    (setf (ebb-parser-dcs-final parser) ch)
    (setf (ebb-parser-state parser) :dcs-passthrough))
   ;; C0 controls are ignored in the DCS header and the state is preserved.
   ((< ch ?\s) nil)
   (t (setf (ebb-parser-state parser) :ground))))

(defun ebb-parse--dcs-passthrough (parser ch)
  "Accumulate DCS body.  ESC handled in process-char for ST."
  ;; Just accumulate (limit for safety)
  (when (< (length (ebb-parser-dcs-string parser)) 1048576)
    (setf (ebb-parser-dcs-string parser)
          (concat (ebb-parser-dcs-string parser) (string ch)))))

;;;; ---- State: Charset Designate ---------------------------------------

(defun ebb-parse--charset-designate (parser ch)
  "Handle charset designation: ESC SLOT CH."
  (ebb-screen-designate-charset
   (ebb-parser-screen parser)
   (ebb-parser-charset-slot parser)
   ch)
  (setf (ebb-parser-state parser) :ground))

;;;; ---- State: SOS/PM/APC (consume and ignore) ------------------------

(defun ebb-parse--sos-pm-apc (parser ch)
  "Consume SOS/PM/APC strings until ST.  ESC handled in process-char."
  ;; Just ignore the character; ESC \ (ST) transitions via :escape state
  (ignore parser ch))

;;;; ---- CSI Dispatch Table ---------------------------------------------

(defun ebb-parse--csi-unknown (parser params)
  "Handler for unrecognized CSI sequences."
  (ebb-parse--log "Unknown CSI %s %s %c"
                    (or (ebb-parser-private parser) "")
                    (ebb-parser-param-string parser)
                    0)
  (ignore parser params))

(defvar ebb-parse--csi-dispatch
  (let ((tbl (make-vector 128 #'ebb-parse--csi-unknown)))
    (aset tbl ?@ #'ebb-parse--csi-ich)
    (aset tbl ?A #'ebb-parse--csi-cuu)
    (aset tbl ?B #'ebb-parse--csi-cud)
    (aset tbl ?C #'ebb-parse--csi-cuf)
    (aset tbl ?D #'ebb-parse--csi-cub)
    (aset tbl ?E #'ebb-parse--csi-cnl)
    (aset tbl ?F #'ebb-parse--csi-cpl)
    (aset tbl ?G #'ebb-parse--csi-cha)
    (aset tbl ?H #'ebb-parse--csi-cup)
    (aset tbl ?I #'ebb-parse--csi-cht)
    (aset tbl ?J #'ebb-parse--csi-ed)
    (aset tbl ?K #'ebb-parse--csi-el)
    (aset tbl ?L #'ebb-parse--csi-il)
    (aset tbl ?M #'ebb-parse--csi-dl)
    (aset tbl ?P #'ebb-parse--csi-dch)
    (aset tbl ?S #'ebb-parse--csi-su)
    (aset tbl ?T #'ebb-parse--csi-sd)
    (aset tbl ?X #'ebb-parse--csi-ech)
    (aset tbl ?Z #'ebb-parse--csi-cbt)
    (aset tbl ?` #'ebb-parse--csi-hpa)
    (aset tbl ?a #'ebb-parse--csi-hpr)
    (aset tbl ?b #'ebb-parse--csi-rep)
    (aset tbl ?c #'ebb-parse--csi-da)
    (aset tbl ?d #'ebb-parse--csi-vpa)
    (aset tbl ?e #'ebb-parse--csi-vpr)
    (aset tbl ?f #'ebb-parse--csi-hvp)
    (aset tbl ?g #'ebb-parse--csi-tbc)
    (aset tbl ?h #'ebb-parse--csi-sm)
    (aset tbl ?j #'ebb-parse--csi-cub)   ; VPB alias for CUB
    (aset tbl ?k #'ebb-parse--csi-cuu)   ; VPB alias for CUU
    (aset tbl ?l #'ebb-parse--csi-rm)
    (aset tbl ?m #'ebb-parse--csi-sgr)
    (aset tbl ?n #'ebb-parse--csi-dsr)
    (aset tbl ?r #'ebb-parse--csi-decstbm)
    (aset tbl ?s #'ebb-parse--csi-scp)
    (aset tbl ?t #'ebb-parse--csi-winops)
    (aset tbl ?u #'ebb-parse--csi-rcp)
    tbl)
  "CSI final-byte dispatch table.  Indexed by character code.")

(defun ebb-parse--fast-csi (parser final-byte param)
  "Fast-path common CSI sequences; return non-nil when handled."
  (and (null (ebb-parser-private parser))
       (string-empty-p (ebb-parser-intermediates parser))
       (let ((screen (ebb-parser-screen parser)))
         (cond
          ;; TUI repaint streams are dominated by row/column addressing.
          ((= final-byte ?H)
           (let ((row 0)
                 (col 0)
                 (in-col nil)
                 (ok t)
                 (i 0)
                 (len (length param)))
             (while (and ok (< i len))
               (let ((ch (aref param i)))
                 (cond
                  ((and (>= ch ?0) (<= ch ?9))
                   (if in-col
                       (setq col (+ (* col 10) (- ch ?0)))
                     (setq row (+ (* row 10) (- ch ?0)))))
                  ((= ch ?\;)
                   (if in-col
                       (setq ok nil)
                     (setq in-col t)))
                  (t
                   (setq ok nil))))
               (cl-incf i))
             (when ok
               (ebb-screen-cursor-goto screen
                                         (1- (if (zerop row) 1 row))
                                         (1- (if (zerop col) 1 col)))
               t)))
          ;; Full-screen clear is common at frame start.
          ((and (= final-byte ?J)
                (= (length param) 1)
                (= (aref param 0) ?2))
           (ebb-screen-erase-in-display screen 2)
           t)
          ;; Simple SGR reset and ANSI foreground/background colors.
          ((= final-byte ?m)
           (cond
            ((or (string-empty-p param) (string= param "0"))
             (ebb-screen-reset-attr screen)
             t)
            ((and (= (length param) 2)
                  (= (aref param 0) ?4)
                  (<= ?0 (aref param 1))
                  (<= (aref param 1) ?7))
             (ebb-screen-set-attr screen :bg (- (aref param 1) ?0))
             t)
            ((and (= (length param) 2)
                  (= (aref param 0) ?3)
                  (<= ?0 (aref param 1))
                  (<= (aref param 1) ?7))
             (ebb-screen-set-attr screen :fg (- (aref param 1) ?0))
             t)))
          (t nil)))))

(defun ebb-parse--rect-coordinates (screen params offset)
  "Return a clipped rectangle from PARAMS beginning at OFFSET for SCREEN.
Rectangle coordinates are one-based in the sequence.  In origin mode they are
relative to the top and left margins."
  (let* ((height (ebb-screen-height screen))
         (width (ebb-screen-width screen))
         (origin-row (if (ebb-screen-origin-mode screen)
                         (ebb-screen-scroll-top screen) 0))
         (origin-column (if (ebb-screen-origin-mode screen)
                            (ebb-screen-left-margin screen) 0))
         (top (+ origin-row
                 (1- (ebb-parse--param params offset 1))))
         (left (+ origin-column
                  (1- (ebb-parse--param params (1+ offset) 1))))
         (bottom (+ origin-row
                    (1- (ebb-parse--param params (+ offset 2) height))))
         (right (+ origin-column
                   (1- (ebb-parse--param params (+ offset 3) width)))))
    (when (and (<= top bottom) (<= left right)
               (< top height) (< left width)
               (>= bottom 0) (>= right 0))
      (list (ebb--clamp top 0 (1- height))
            (ebb--clamp left 0 (1- width))
            (ebb--clamp bottom 0 (1- height))
            (ebb--clamp right 0 (1- width))))))

(defun ebb-parse--dispatch-csi-intermediate (parser final-byte params)
  "Handle CSI FINAL-BYTE sequences distinguished by intermediate bytes.
Return non-nil when the sequence was handled."
  (let ((intermediates (ebb-parser-intermediates parser))
        (screen (ebb-parser-screen parser)))
    (cond
     ;; DECSTR - soft terminal reset.
     ((and (= final-byte ?p) (string= intermediates "!"))
      (ebb-screen-soft-reset screen)
      (remhash screen ebb-parse--ansi-mode-states)
      (remhash screen ebb-parse--dec-mode-states)
      t)
     ;; DECRQM - request ANSI or DEC private mode state.
     ((and (= final-byte ?p) (string= intermediates "$"))
      (let* ((mode (ebb-parse--param params 0 0))
             (private (eql (ebb-parser-private parser) ??))
             (level (or (gethash screen ebb-parse--conformance-levels) 65))
             (status
              (when (>= level 63)
                (if private
                    (cond
                     ((= mode 8) 4)
                     ((= mode 60) 4)
                     ((= mode 1)
                      (if (ebb-screen-keypad-mode screen) 1 2))
                     ((= mode 3)
                      (if (ebb-screen-column-mode-enabled-p screen) 1 2))
                     ((= mode 6)
                      (if (ebb-screen-origin-mode screen) 1 2))
                     ((= mode 7)
                      (if (ebb-screen-auto-wrap screen) 1 2))
                     ((= mode 25)
                      (if (ebb-screen-cursor-visible screen) 1 2))
                     ((= mode 69)
                      (if (ebb-screen-horizontal-margins-enabled-p screen) 1 2))
                     ((memq mode '(4 5 18 19 35 42 66 67))
                      (if (ebb-parse--recorded-mode
                           ebb-parse--dec-mode-states screen mode) 1 2))
                     (t 0))
                  (cond
                   ((memq mode '(2 4 12 20))
                    (if (ebb-parse--recorded-mode
                         ebb-parse--ansi-mode-states screen mode) 1 2))
                   ((memq mode '(1 5 7 10 11 13 14 15 16 17 18 19)) 4)
                   (t 0))))))
        (when status
          (ebb-parse--respond
           parser
           (format "\e[%s%d;%d$y" (if private "?" "") mode status))))
      t)
     ;; DECSCL - select conformance level.  Ebb always uses 7-bit replies, but
     ;; changing the level still performs the specified terminal reset.
     ((and (= final-byte ?p) (string= intermediates "\""))
      (puthash screen (ebb-parse--param params 0 65)
               ebb-parse--conformance-levels)
      (remhash screen ebb-parse--ansi-mode-states)
      (remhash screen ebb-parse--dec-mode-states)
      (ebb-screen-reset screen)
      (ebb-parse--emit parser 'reset)
      t)
     ;; DECSCA - select character protection attribute.
     ((and (= final-byte ?q) (string= intermediates "\""))
      (ebb-screen-set-dec-protection
       screen (= (ebb-parse--param params 0 0) 1))
      t)
     ;; DECSCUSR - set cursor style.
     ((and (= final-byte ?q) (string= intermediates " "))
      (ebb-parse--csi-decscusr parser params)
      t)
     ;; DECSNLS - set number of lines per screen.
     ((and (= final-byte ?|) (string= intermediates "*"))
      (puthash screen (ebb-parse--param params 0 (ebb-screen-height screen))
               ebb-parse--page-lengths)
      t)
     ;; DECCRA - copy rectangular area.
     ((and (= final-byte ?v) (string= intermediates "$"))
      (when-let* ((source (ebb-parse--rect-coordinates screen params 0)))
        (let* ((origin-row (if (ebb-screen-origin-mode screen)
                               (ebb-screen-scroll-top screen) 0))
               (origin-column (if (ebb-screen-origin-mode screen)
                                  (ebb-screen-left-margin screen) 0))
               (destination-top
                (+ origin-row (1- (ebb-parse--param params 5 1))))
               (destination-left
                (+ origin-column (1- (ebb-parse--param params 6 1))))
               (source-page (ebb-parse--param params 4 1))
               (destination-page (ebb-parse--param params 7 1)))
          (when (and (= source-page 1) (= destination-page 1))
            (apply #'ebb-screen-copy-rect
                   screen
                   (append source
                           (list destination-top destination-left))))))
      t)
     ;; DECERA and DECSERA - erase rectangular area.
     ((and (memq final-byte '(?z ?{)) (string= intermediates "$"))
      (when-let* ((rectangle (ebb-parse--rect-coordinates screen params 0)))
        (apply #'ebb-screen-erase-rect
               screen (append rectangle (list (= final-byte ?{)))))
      t)
     ;; DECFRA - fill rectangular area.
     ((and (= final-byte ?x) (string= intermediates "$"))
      (let ((char (ebb-parse--param params 0 0)))
        (when (and (or (<= 32 char 126) (<= 160 char 255)))
          (when-let* ((rectangle (ebb-parse--rect-coordinates screen params 1)))
            (apply #'ebb-screen-fill-rect screen char rectangle))))
      t)
     ;; DECRQCRA - request checksum of rectangular area.
     ((and (= final-byte ?y) (string= intermediates "*"))
      (let* ((pid (if (> (length params) 0) (aref params 0) 0))
             (rectangle (ebb-parse--rect-coordinates screen params 2))
             (checksum (if rectangle
                           (apply #'ebb-screen-checksum-rect screen rectangle)
                         0)))
        (ebb-parse--respond parser
                           (format "\eP%d!~%04X\e\\" pid checksum)))
      t)
     (t nil))))

(defun ebb-parse--dispatch-csi (parser final-byte)
  "Parse parameters and dispatch CSI sequence."
  (condition-case err
      (let ((param (ebb-parser-param-string parser)))
        (unless (ebb-parse--fast-csi parser final-byte param)
          (let ((params (ebb-parse--parse-params param)))
            (if (string-empty-p (ebb-parser-intermediates parser))
                (let ((handler (if (< final-byte 128)
                                   (aref ebb-parse--csi-dispatch final-byte)
                                 #'ebb-parse--csi-unknown)))
                  (funcall handler parser params))
              ;; Unrecognized intermediate + final-byte combinations are
              ;; ignored, per the DEC parser's csi-ignored state.  They must
              ;; not fall through to the plain final-byte table.
              (unless (ebb-parse--dispatch-csi-intermediate
                       parser final-byte params)
                (ebb-parse--log
                 "Unknown CSI intermediate sequence [%s%c]"
                 (ebb-parser-intermediates parser)
                 final-byte))))))
    (error
     (ebb-parse--log "CSI dispatch error for %c: %S" final-byte err)))
  (setf (ebb-parser-state parser) :ground))

;;;; ---- CSI Handlers ---------------------------------------------------

;; ICH - Insert Character
(defun ebb-parse--csi-ich (parser params)
  (ebb-screen-insert-chars (ebb-parser-screen parser)
                             (ebb-parse--param params 0 1)))

;; CUU - Cursor Up
(defun ebb-parse--csi-cuu (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'up (ebb-parse--param params 0 1)))

;; CUD - Cursor Down
(defun ebb-parse--csi-cud (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'down (ebb-parse--param params 0 1)))

;; CUF - Cursor Forward
(defun ebb-parse--csi-cuf (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'right (ebb-parse--param params 0 1)))

;; CUB - Cursor Back
(defun ebb-parse--csi-cub (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'left (ebb-parse--param params 0 1)))

;; CNL - Cursor Next Line
(defun ebb-parse--csi-cnl (parser params)
  (ebb-screen-cursor-next-line (ebb-parser-screen parser)
                                 (ebb-parse--param params 0 1)))

;; CPL - Cursor Previous Line
(defun ebb-parse--csi-cpl (parser params)
  (ebb-screen-cursor-prev-line (ebb-parser-screen parser)
                                  (ebb-parse--param params 0 1)))

;; CHA - Cursor Horizontal Absolute
(defun ebb-parse--csi-cha (parser params)
  (let* ((screen (ebb-parser-screen parser))
         (origin (and (ebb-screen-origin-mode screen)
                      (ebb-screen-horizontal-margins-enabled-p screen)))
         (min-x (if origin (ebb-screen-left-margin screen) 0))
         (max-x (if origin
                    (ebb-screen-right-margin screen)
                  (1- (ebb-screen-line-width screen))))
         (col (+ min-x (1- (ebb-parse--param params 0 1)))))
    (setf (ebb-screen-pending-wrap screen) nil)
    (setf (ebb-screen-cursor-x screen)
          (ebb--clamp col min-x max-x))))

;; CUP - Cursor Position
(defun ebb-parse--csi-cup (parser params)
  (ebb-screen-cursor-goto (ebb-parser-screen parser)
                            (1- (ebb-parse--param params 0 1))
                            (1- (ebb-parse--param params 1 1))))

;; CHT - Cursor Horizontal Tab
(defun ebb-parse--csi-cht (parser params)
  (ebb-screen-tab-forward (ebb-parser-screen parser)
                            (ebb-parse--param params 0 1)))

;; ED - Erase in Display
(defun ebb-parse--csi-ed (parser params)
  (funcall (if (eql (ebb-parser-private parser) ??)
               #'ebb-screen-dec-erase-in-display
             #'ebb-screen-erase-in-display)
           (ebb-parser-screen parser)
           (ebb-parse--param params 0 0)))

;; EL - Erase in Line
(defun ebb-parse--csi-el (parser params)
  (funcall (if (eql (ebb-parser-private parser) ??)
               #'ebb-screen-dec-erase-in-line
             #'ebb-screen-erase-in-line)
           (ebb-parser-screen parser)
           (ebb-parse--param params 0 0)))

;; IL - Insert Line
(defun ebb-parse--csi-il (parser params)
  (ebb-screen-insert-lines (ebb-parser-screen parser)
                             (ebb-parse--param params 0 1)))

;; DL - Delete Line
(defun ebb-parse--csi-dl (parser params)
  (ebb-screen-delete-lines (ebb-parser-screen parser)
                             (ebb-parse--param params 0 1)))

;; DCH - Delete Character
(defun ebb-parse--csi-dch (parser params)
  (ebb-screen-delete-chars (ebb-parser-screen parser)
                             (ebb-parse--param params 0 1)))

;; SU - Scroll Up
(defun ebb-parse--csi-su (parser params)
  (if (eql (ebb-parser-private parser) ??)
      ;; XTSMGRAPHICS: CSI ? Ps ; Pm S
      (ebb-parse--csi-xtsmgraphics parser params)
    (ebb-screen-scroll (ebb-parser-screen parser)
                         'up (ebb-parse--param params 0 1))))

;; XTSMGRAPHICS - Send/query graphics attributes
(defun ebb-parse--csi-xtsmgraphics (parser params)
  "Handle XTSMGRAPHICS (CSI ? Ps ; Pm S).
Ps=1: color register count, Ps=2: graphics geometry.
Pm=1: read, Pm=4: read maximum."
  (let ((attr (ebb-parse--param params 0 0))
        (op (ebb-parse--param params 1 0)))
    (if (memq op '(1 4))
        (pcase attr
          (1 ;; Color registers: report 256
           (ebb-parse--respond parser "\e[?1;0;256S"))
          (2 ;; Graphics geometry: report pixel size based on screen
           (let* ((screen (ebb-parser-screen parser))
                  (pw (min (* (ebb-screen-width screen) 8) 1000))
                  (ph (min (* (ebb-screen-height screen) 16) 1000)))
             (ebb-parse--respond parser (format "\e[?2;0;%d;%dS" pw ph))))
          (_ ;; Unknown attribute
           (ebb-parse--respond parser (format "\e[?%d;1S" attr))))
      ;; Unsupported operation
      (ebb-parse--respond
       parser
       (format "\e[?%d;%dS" attr (if (<= 1 attr 2) (if (<= 2 op 3) 3 2) 1))))))

;; SD - Scroll Down
(defun ebb-parse--csi-sd (parser params)
  (ebb-screen-scroll (ebb-parser-screen parser)
                       'down (ebb-parse--param params 0 1)))

;; ECH - Erase Character
(defun ebb-parse--csi-ech (parser params)
  (ebb-screen-erase-chars (ebb-parser-screen parser)
                            (ebb-parse--param params 0 1)))

;; CBT - Cursor Backward Tab
(defun ebb-parse--csi-cbt (parser params)
  (ebb-screen-tab-backward (ebb-parser-screen parser)
                             (ebb-parse--param params 0 1)))

;; HPA - Horizontal Position Absolute
(defun ebb-parse--csi-hpa (parser params)
  (ebb-parse--csi-cha parser params))

;; HPR - Horizontal Position Relative
(defun ebb-parse--csi-hpr (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'right (ebb-parse--param params 0 1)))

;; REP - Repeat last character
(defun ebb-parse--csi-rep (parser params)
  (ebb-screen-repeat-char (ebb-parser-screen parser)
                            (ebb-parse--param params 0 1)))

;; DA - Device Attributes
(defun ebb-parse--csi-da (parser _params)
  (cond
   ((not (ebb-parser-private parser))
    ;; Primary DA: report as VT220 with ANSI color
    (ebb-parse--respond parser "\e[?62;22c"))
   ((eql (ebb-parser-private parser) ?>)
    ;; Secondary DA
    (ebb-parse--respond parser "\e[>1;1;0c"))))

;; VPA - Vertical Position Absolute
(defun ebb-parse--csi-vpa (parser params)
  (let* ((screen (ebb-parser-screen parser))
         (row (1- (ebb-parse--param params 0 1)))
         (min-y (if (ebb-screen-origin-mode screen)
                    (ebb-screen-scroll-top screen) 0))
         (max-y (if (ebb-screen-origin-mode screen)
                    (ebb-screen-scroll-bottom screen)
                  (1- (ebb-screen-height screen)))))
    (setf (ebb-screen-pending-wrap screen) nil)
    (setf (ebb-screen-cursor-y screen)
          (ebb--clamp (+ min-y row) min-y max-y))))

;; VPR - Vertical Position Relative
(defun ebb-parse--csi-vpr (parser params)
  (ebb-screen-cursor-move (ebb-parser-screen parser)
                            'down (ebb-parse--param params 0 1)))

;; HVP - Horizontal Vertical Position (same as CUP)
(defun ebb-parse--csi-hvp (parser params)
  (ebb-parse--csi-cup parser params))

;; TBC - Tab Clear
(defun ebb-parse--csi-tbc (parser params)
  (ebb-screen-clear-tab-stop (ebb-parser-screen parser)
                               (ebb-parse--param params 0 0)))

;; SM - Set Mode
(defun ebb-parse--csi-sm (parser params)
  (let ((screen (ebb-parser-screen parser)))
    (if (ebb-parser-private parser)
        ;; DECSET
        (dotimes (i (length params))
          (let ((mode (aref params i)))
            (ebb-parse--record-mode
             ebb-parse--dec-mode-states screen mode t)
            (ebb-screen-set-mode screen mode t)
            (ebb-parse--emit parser 'mode-set mode t)))
      ;; Standard SM
      (dotimes (i (length params))
        (let ((mode (aref params i)))
          (ebb-parse--record-mode
           ebb-parse--ansi-mode-states screen mode t)
          (pcase mode
            (4 (setf (ebb-screen-insert-mode screen) t))))))))

;; RM - Reset Mode
(defun ebb-parse--csi-rm (parser params)
  (let ((screen (ebb-parser-screen parser)))
    (if (ebb-parser-private parser)
        ;; DECRST
        (dotimes (i (length params))
          (let ((mode (aref params i)))
            (ebb-parse--record-mode
             ebb-parse--dec-mode-states screen mode nil)
            (ebb-screen-set-mode screen mode nil)
            (ebb-parse--emit parser 'mode-set mode nil)))
      ;; Standard RM
      (dotimes (i (length params))
        (let ((mode (aref params i)))
          (ebb-parse--record-mode
           ebb-parse--ansi-mode-states screen mode nil)
          (pcase mode
            (4 (setf (ebb-screen-insert-mode screen) nil))))))))

;; DECSTBM - Set Scrolling Region
(defun ebb-parse--csi-decstbm (parser params)
  ;; DECSTBM is CSI Pt;Pb r without a private marker; CSI ? Ps r is the
  ;; xterm-specific XTRESTORE extension, which Ebb does not implement.
  (when (null (ebb-parser-private parser))
    (let* ((screen (ebb-parser-screen parser))
           (h (ebb-screen-height screen))
           (top (1- (ebb-parse--param params 0 1)))
           (bot (1- (ebb-parse--param params 1 h))))
      (ebb-screen-set-scroll-region screen top bot))))

;; SCP / DECSLRM - Save Cursor Position or set left/right margins.
(defun ebb-parse--csi-scp (parser params)
  (when (and (null (ebb-parser-private parser))
             (string-empty-p (ebb-parser-intermediates parser)))
    (let ((screen (ebb-parser-screen parser)))
      (if (ebb-screen-horizontal-margins-enabled-p screen)
          (ebb-screen-set-horizontal-margins
           screen
           (1- (ebb-parse--param params 0 1))
           (1- (ebb-parse--param params 1 (ebb-screen-width screen))))
        (when (zerop (length params))
          (ebb-screen-save-cursor screen))))))

;; RCP - Restore Cursor Position
(defun ebb-parse--csi-rcp (parser params)
  (when (and (null (ebb-parser-private parser))
             (string-empty-p (ebb-parser-intermediates parser))
             (zerop (length params)))
    (ebb-screen-restore-cursor (ebb-parser-screen parser))))

;; DECSCUSR - Set Cursor Style (with SP intermediate)
(defun ebb-parse--csi-decscusr (parser params)
  (when (string= (ebb-parser-intermediates parser) " ")
    (let ((style (ebb-parse--param params 0 0)))
      (ebb-screen-set-cursor-style (ebb-parser-screen parser) style)
      (ebb-parse--emit parser 'cursor-style style))))

;; DSR - Device Status Report
(defun ebb-parse--csi-dsr (parser params)
  (let ((screen (ebb-parser-screen parser)))
    (pcase (ebb-parse--param params 0 0)
      (5 (ebb-parse--respond parser "\e[0n"))
      (6 (let ((row (ebb-screen-cursor-y screen))
               (column (ebb-screen-cursor-x screen)))
           (when (ebb-screen-origin-mode screen)
             (setq row (- row (ebb-screen-scroll-top screen)))
             (when (ebb-screen-horizontal-margins-enabled-p screen)
               (setq column (- column (ebb-screen-left-margin screen)))))
           (ebb-parse--respond
            parser
            (format "\e[%d;%dR" (1+ row) (1+ column))))))))

;; Window manipulation (CSI t) -- character-cell resize and size reporting.
(defun ebb-parse--csi-winops (parser params)
  (let ((screen (ebb-parser-screen parser)))
    (pcase (ebb-parse--param params 0 0)
      ;; Resize text area in character cells.  A zero/omitted dimension keeps
      ;; the current size, which is useful when no display maximum is known.
      (8
       (let ((height (ebb-parse--param
                      params 1 (ebb-screen-height screen)))
             (width (ebb-parse--param
                     params 2 (ebb-screen-width screen))))
         (when (and (> width 0) (> height 0))
           (ebb-screen-resize screen width height)
           (puthash screen height ebb-parse--page-lengths)
           (ebb-parse--emit parser 'resize-request width height))))
      ;; DECSLPP - set number of lines per page.
      ((pred (lambda (value) (>= value 24)))
       (puthash screen (ebb-parse--param params 0 24)
                ebb-parse--page-lengths))
      ;; Report terminal size in chars.
      (18 (ebb-parse--respond
           parser
           (format "\e[8;%d;%dt"
                   (ebb-screen-height screen)
                   (ebb-screen-width screen))))
      ;; Report display size in chars.  Emacs does not expose a separate
      ;; terminal display grid, so report the available model dimensions.
      (19 (ebb-parse--respond
           parser
           (format "\e[9;%d;%dt"
                   (ebb-screen-height screen)
                   (ebb-screen-width screen))))
      (_ nil))))

;;;; ---- SGR Handler (Select Graphic Rendition) -------------------------

(defun ebb-parse--set-colon-sgr-color (screen params)
  "Apply colon-form color PARAMS to SCREEN."
  (let ((property (pcase (car params)
                    (38 :fg)
                    (48 :bg)
                    (58 :ul-color))))
    (when property
      (pcase (cdr params)
        (`(5 ,index)
         (ebb-screen-set-attr screen property index))
        (`(2 ,r ,g ,b)
         (ebb-screen-set-attr screen property (list r g b)))
        (`(2 ,_color-space ,r ,g ,b)
         (ebb-screen-set-attr screen property (list r g b)))))))

(defun ebb-parse--set-semicolon-sgr-color (screen params index attribute)
  "Set SCREEN's ATTRIBUTE from semicolon-form PARAMS at INDEX.
Return the number of additional parameters consumed."
  (let ((len (length params)))
    (if (>= (1+ index) len)
        0
      (let ((sub (aref params (1+ index))))
        (cond
         ((and (= sub 2) (< (+ index 4) len))
          (ebb-screen-set-attr
           screen attribute
           (list (aref params (+ index 2))
                 (aref params (+ index 3))
                 (aref params (+ index 4))))
          4)
         ((and (= sub 5) (< (+ index 2) len))
          (ebb-screen-set-attr screen attribute (aref params (+ index 2)))
          2)
         (t 1))))))

(defun ebb-parse--apply-colon-sgr (screen param)
  "Apply colon-form SGR PARAM to SCREEN."
  (pcase (car param)
    (4
     (ebb-screen-set-attr
      screen :underline
      (pcase (if (cdr param) (cadr param) 1)
        (0 nil)
        (1 'line)
        (2 'double)
        (3 'curly)
        (4 'dotted)
        (5 'dashed)
        (_ 'line))))
    ((or 38 48 58)
     (ebb-parse--set-colon-sgr-color screen param))))

(defun ebb-parse--apply-simple-sgr (screen param)
  "Apply non-extended numeric SGR PARAM to SCREEN."
  (cond
   ((= param 0) (ebb-screen-reset-attr screen))
   ((= param 1) (ebb-screen-set-attr screen :bold t))
   ((= param 2) (ebb-screen-set-attr screen :faint t))
   ((= param 3) (ebb-screen-set-attr screen :italic t))
   ((= param 4) (ebb-screen-set-attr screen :underline 'line))
   ((= param 5) (ebb-screen-set-attr screen :blink 'slow))
   ((= param 6) (ebb-screen-set-attr screen :blink 'fast))
   ((= param 7) (ebb-screen-set-attr screen :inverse t))
   ((= param 8) (ebb-screen-set-attr screen :conceal t))
   ((= param 9) (ebb-screen-set-attr screen :crossed t))
   ((<= 10 param 19) (ebb-screen-set-attr screen :font (- param 10)))
   ((= param 21) (ebb-screen-set-attr screen :underline 'double))
   ((= param 22)
    (ebb-screen-set-attr screen :bold nil)
    (ebb-screen-set-attr screen :faint nil))
   ((= param 23) (ebb-screen-set-attr screen :italic nil))
   ((= param 24) (ebb-screen-set-attr screen :underline nil))
   ((= param 25) (ebb-screen-set-attr screen :blink nil))
   ((= param 27) (ebb-screen-set-attr screen :inverse nil))
   ((= param 28) (ebb-screen-set-attr screen :conceal nil))
   ((= param 29) (ebb-screen-set-attr screen :crossed nil))
   ((<= 30 param 37) (ebb-screen-set-attr screen :fg (- param 30)))
   ((= param 39) (ebb-screen-set-attr screen :fg nil))
   ((<= 40 param 47) (ebb-screen-set-attr screen :bg (- param 40)))
   ((= param 49) (ebb-screen-set-attr screen :bg nil))
   ((= param 59) (ebb-screen-set-attr screen :ul-color nil))
   ((<= 90 param 97) (ebb-screen-set-attr screen :fg (+ 8 (- param 90))))
   ((<= 100 param 107) (ebb-screen-set-attr screen :bg (+ 8 (- param 100))))))

(defun ebb-parse--csi-sgr (parser params)
  "Handle SGR (CSI m) attributes in PARAMS for PARSER.
Private CSI sequences ending in `m' are not SGR and are ignored."
  (unless (ebb-parser-private parser)
    (let ((screen (ebb-parser-screen parser))
          (len (length params)))
      (if (zerop len)
          (ebb-screen-reset-attr screen)
        (let ((i 0))
          (while (< i len)
            (let ((param (aref params i)))
              (cond
               ((listp param)
                (ebb-parse--apply-colon-sgr screen param))
               ((memq param '(38 48 58))
                (cl-incf
                 i
                 (ebb-parse--set-semicolon-sgr-color
                  screen params i
                  (pcase param (38 :fg) (48 :bg) (58 :ul-color)))))
               (t
                (ebb-parse--apply-simple-sgr screen param))))
            (cl-incf i)))))))

;;;; ---- OSC Helpers ----------------------------------------------------

(defun ebb-parse--handle-osc-4 (parser payload)
  "Handle OSC 4 color palette queries in PAYLOAD.
Supports xterm-style query pairs such as 4;2;? and 4;0;?;1;?.
Palette-setting requests are ignored."
  (let ((parts (vconcat (split-string payload ";"))))
    (cl-loop for i from 0 below (length parts) by 2
             for index-str = (aref parts i)
             for spec = (and (< (1+ i) (length parts))
                             (aref parts (1+ i)))
             when (and spec
                       (string-match-p "\\`[0-9]+\\'" index-str)
                       (string= spec "?"))
             do (let ((index (string-to-number index-str)))
                  (when (<= 0 index 255)
                    (ebb-parse--respond
                     parser
                     (format "\e]4;%d;%s\e\\"
                             index
                             (ebb-parse--palette-color-to-xterm index))))))))

(defun ebb-parse--handle-progress (parser payload)
  "Emit a normalized progress event parsed from OSC 9;4 PAYLOAD."
  (when (string-match "\\`\\([0-4]\\);\\(-?[0-9]+\\)\\'" payload)
    (let ((state (aref [remove set error indeterminate pause]
                       (string-to-number (match-string 1 payload))))
          (progress (ebb--clamp (string-to-number (match-string 2 payload))
                                  0 100)))
      (ebb-parse--emit parser 'progress state progress))))

(defun ebb-parse--handle-osc-777 (parser payload)
  "Emit a notification described by OSC 777 PAYLOAD."
  (when (string-match "\\`notify;\\([^;]+\\);\\(.+\\)\\'" payload)
    (ebb-parse--emit parser 'notification
                       (match-string 1 payload) (match-string 2 payload))))

(defun ebb-parse--handle-osc-52 (parser payload)
  "Handle OSC 52 clipboard manipulation.
PAYLOAD format: TARGET;BASE64-DATA
TARGET is one or more of: c (clipboard), p (primary), s (secondary), etc.
If BASE64-DATA is `?' this is a query; otherwise it's a set operation."
  (when (and ebb-enable-osc52
             (string-match "\\`\\([^;]*\\);\\(.*\\)\\'" payload))
    (let ((_target (match-string 1 payload))
          (data (match-string 2 payload)))
      (cond
       ;; Query: respond with current clipboard contents
       ((string= data "?")
        (let* ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                         (ignore-errors (current-kill 0 t))
                         ""))
               (encoded (base64-encode-string (encode-coding-string text 'utf-8) t)))
          (ebb-parse--respond parser (format "\e]52;c;%s\e\\" encoded))))
       ;; Empty string: clear clipboard
       ((string-empty-p data)
        (when (fboundp 'gui-set-selection)
          (gui-set-selection 'CLIPBOARD ""))
        (kill-new ""))
       ;; Set: decode and set clipboard
       (t
        (let ((text (decode-coding-string
                     (base64-decode-string data)
                     'utf-8)))
          (kill-new text)
          (when (fboundp 'gui-set-selection)
            (ignore-errors (gui-set-selection 'CLIPBOARD text)))
          (ebb-parse--emit parser 'clipboard text)))))))

;;;; ---- OSC Dispatch ---------------------------------------------------

(defun ebb-parse--dispatch-osc (parser)
  "Dispatch a completed OSC sequence."
  (condition-case err
      (let* ((str (ebb-parser-osc-string parser))
             (screen (ebb-parser-screen parser)))
        (if (string-match "\\`\\([0-9]+\\)\\(?:;\\(\\(?:.\\|\n\\)*\\)\\)?\\'" str)
            (let ((num (string-to-number (match-string 1 str)))
                  (payload (or (match-string 2 str) "")))
              (pcase num
                ;; Set title (+ icon name)
                ((or 0 1 2)
                 (setf (ebb-screen-title screen) payload)
                 (ebb-parse--emit parser 'title payload))
                ;; Set CWD
                (7
                 (let ((host nil)
                       (cwd payload))
                   (when (string-match "\\`file://\\([^/]*\\)/\\(.*\\)\\'" payload)
                     (setq host (match-string 1 payload)
                           ;; Paths arrive percent-encoded; without this,
                           ;; remote reports (trusted, never stat'ed)
                           ;; corrupt `default-directory' on any space.
                           cwd (concat "/" (decode-coding-string
                                            (url-unhex-string
                                             (match-string 2 payload))
                                            'utf-8))))
                   (setf (ebb-screen-cwd screen) cwd)
                   ;; HOST lets the handler build a TRAMP path when the
                   ;; report comes from a remote shell.
                   (ebb-parse--emit parser 'cwd cwd host)))
                ;; OSC 8 ; params ; URI.  An empty URI ends the link.
                (8
                 (when (string-match "\\`\\([^;]*\\);\\(.*\\)\\'" payload)
                   (let* ((params (match-string 1 payload))
                          (uri (match-string 2 payload))
                          (id (and (string-match
                                    "\\(?:\\`\\|:\\)id=\\([^:]*\\)" params)
                                   (match-string 1 params))))
                     (ebb-screen-set-hyperlink screen uri id))))
                ;; Query palette entries
                (4
                 (ebb-parse--handle-osc-4 parser payload))
                ;; Desktop notification and progress.
                (9
                 (cond
                  ((string-prefix-p "4;" payload)
                   (ebb-parse--handle-progress parser (substring payload 2)))
                  ((not (string-empty-p payload))
                   (ebb-parse--emit parser 'notification nil payload))))
                ;; Query foreground
                (10
                 (when (string= payload "?")
                   (ebb-parse--respond
                    parser
                    (concat "\e]10;" (ebb-parse--color-to-xterm
                                      (face-foreground 'default nil t))
                            "\e\\"))))
                ;; Query background
                (11
                 (when (string= payload "?")
                   (ebb-parse--respond
                    parser
                    (concat "\e]11;" (ebb-parse--color-to-xterm
                                      (face-background 'default nil t))
                            "\e\\"))))
                ;; Shell integration
                (51
                 (ebb-parse--emit parser 'osc-51 payload))
                ;; Clipboard (selection manipulation)
                (52
                 (ebb-parse--handle-osc-52 parser payload))
                ;; rxvt notification protocol.
                (777
                 (ebb-parse--handle-osc-777 parser payload))
                ;; Unknown
                (_ (ebb-parse--log "Unknown OSC %d" num))))
          (ebb-parse--log "Malformed OSC: %s"
                            (substring str 0 (min (length str) 40)))))
    (error
     (ebb-parse--log "OSC dispatch error: %S" err))))

;;;; ---- DCS Dispatch ---------------------------------------------------

(defun ebb-parse--sgr-color-status (color foreground &optional code)
  "Return SGR parameters for COLOR.
FOREGROUND selects the legacy foreground/background palette codes unless
CODE is supplied, in which case CODE is used for extended colors."
  (let ((base (or code (if foreground 38 48))))
    (cond
     ((integerp color)
      (cond
       ((and (null code) (< color 8))
        (list (+ (if foreground 30 40) color)))
       ((and (null code) (< color 16))
        (list (+ (if foreground 90 100) (- color 8))))
       (t (list base 5 color))))
     ((and (listp color) (= (length color) 3))
      (append (list base 2) color)))))

(defun ebb-parse--sgr-status (screen)
  "Return SCREEN's current rendition as a DECRQSS parameter string."
  (let ((attr (ebb-screen-current-attr screen))
        (params '("0")))
    (when (ebb-attr-bold attr) (setq params (append params '("1"))))
    (when (ebb-attr-faint attr) (setq params (append params '("2"))))
    (when (ebb-attr-italic attr) (setq params (append params '("3"))))
    (when (ebb-attr-underline attr)
      (setq params
            (append params
                    (list (format "4:%d"
                                  (pcase (ebb-attr-underline attr)
                                    ('line 1) ('double 2) ('curly 3)
                                    ('dotted 4) ('dashed 5)
                                    (_ 1)))))))
    (when (and (ebb-attr-font attr) (> (ebb-attr-font attr) 0))
      (setq params (append params
                           (list (number-to-string
                                  (+ 10 (ebb-attr-font attr)))))))
    (when (eq (ebb-attr-blink attr) 'slow)
      (setq params (append params '("5"))))
    (when (eq (ebb-attr-blink attr) 'fast)
      (setq params (append params '("6"))))
    (when (ebb-attr-inverse attr) (setq params (append params '("7"))))
    (when (ebb-attr-conceal attr) (setq params (append params '("8"))))
    (when (ebb-attr-crossed attr) (setq params (append params '("9"))))
    (when (ebb-attr-fg attr)
      (setq params (append params
                           (mapcar #'number-to-string
                                   (ebb-parse--sgr-color-status
                                    (ebb-attr-fg attr) t)))))
    (when (ebb-attr-bg attr)
      (setq params (append params
                           (mapcar #'number-to-string
                                   (ebb-parse--sgr-color-status
                                    (ebb-attr-bg attr) nil)))))
    (when (ebb-attr-ul-color attr)
      (setq params (append params
                           (mapcar #'number-to-string
                                   (ebb-parse--sgr-color-status
                                    (ebb-attr-ul-color attr) nil 58)))))
    (mapconcat #'identity params ";")))

(defun ebb-parse--cursor-style-status (screen)
  "Return SCREEN's cursor style as a DECSCUSR parameter."
  (pcase (ebb-screen-cursor-style screen)
    (:blinking-block 1)
    (:block 2)
    (:blinking-underline 3)
    (:underline 4)
    (:blinking-bar 5)
    (:bar 6)
    (_ 2)))

(defun ebb-parse--decrqss-value (screen request)
  "Return the DECRQSS status value for REQUEST on SCREEN."
  (pcase request
    ((or "$}" "*x" "$~")
     (concat "0" request))
    ("\"q" (format "%d\"q"
                    (if (ebb-screen-dec-protection-enabled-p screen) 1 0)))
    ("\"p" (format "%d;1\"p"
                    (or (gethash screen ebb-parse--conformance-levels) 65)))
    ("r" (format "%d;%dr"
                 (1+ (ebb-screen-scroll-top screen))
                 (1+ (ebb-screen-scroll-bottom screen))))
    ("m" (concat (ebb-parse--sgr-status screen) "m"))
    (" q" (format "%d q" (ebb-parse--cursor-style-status screen)))
    ("s" (format "%d;%ds"
                 (1+ (ebb-screen-left-margin screen))
                 (1+ (ebb-screen-right-margin screen))))
    ("t" (format "%dt"
                 (or (gethash screen ebb-parse--page-lengths)
                     (ebb-screen-height screen))))
    ("*|" (format "%d*|"
                  (or (gethash screen ebb-parse--page-lengths)
                      (ebb-screen-height screen))))
    (_ nil)))

(defun ebb-parse--dispatch-dcs (parser)
  "Dispatch a completed DCS sequence."
  (condition-case err
      (let ((final (ebb-parser-dcs-final parser))
            (body (ebb-parser-dcs-string parser))
            (intermediates (ebb-parser-intermediates parser)))
        (cond
         ;; DECRQSS - request selection or setting status.
         ((and (= final ?q) (string= intermediates "$"))
          (if-let* ((value (ebb-parse--decrqss-value
                            (ebb-parser-screen parser) body)))
              (ebb-parse--respond parser (concat "\eP1$r" value "\e\\"))
            (ebb-parse--respond parser "\eP0$r\e\\")))
         ;; Sixel graphics.
         ((= final ?q) (ebb-parse--emit parser 'sixel body))
         (t (ebb-parse--log "Unknown DCS final %c" final))))
    (error
     (ebb-parse--log "DCS dispatch error: %S" err))))

(provide 'ebb-parse)
;;; ebb-parse.el ends here
