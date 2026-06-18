;;; chomp-parse.el --- VT escape sequence parser -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; State machine parser for VT100/VT220/xterm escape sequences.
;; Consumes characters and drives chomp-screen operations.
;;
;; Design: O(1) CSI dispatch via vector table.  Every handler is
;; wrapped in error recovery -- unknown sequences are logged, never crash.
;; The parser is a pure transformer: characters in, screen operations out.

;;; Code:

(require 'cl-lib)
(require 'chomp-term)

;;;; ---- Data Structures ------------------------------------------------

(cl-defstruct (chomp-parser (:copier nil))
  "VT escape sequence parser."
  (state :ground)        ; current state keyword
  (screen nil)           ; chomp-screen to operate on
  (write-fn nil)         ; (lambda (string)) send bytes to PTY
  (emit-fn nil)          ; (lambda (type &rest args)) event callback
  ;; CSI collection
  (param-string "")      ; digits, semicolons, colons
  (private nil)          ; ?/>/= prefix char or nil
  (intermediates "")     ; intermediate bytes (space etc.)
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

(defvar chomp-parse-debug nil
  "When non-nil, log unknown escape sequences to *Messages*.")

(defun chomp-parse--log (fmt &rest args)
  "Log a parser message when debug is enabled."
  (when chomp-parse-debug
    (apply #'message (concat "[chomp-parse] " fmt) args)))

(defun chomp-parse--respond (parser response)
  "Send RESPONSE string back to the PTY."
  (when-let ((fn (chomp-parser-write-fn parser)))
    (funcall fn response)))

(defun chomp-parse--color-to-xterm (color-str)
  "Convert an Emacs color string to xterm rgb:RRRR/GGGG/BBBB format."
  (if color-str
      (let ((rgb (color-values color-str)))
        (if rgb
            (format "rgb:%04x/%04x/%04x"
                    (nth 0 rgb) (nth 1 rgb) (nth 2 rgb))
          "rgb:ffff/ffff/ffff"))
    "rgb:ffff/ffff/ffff"))

(defun chomp-parse--emit (parser type &rest args)
  "Emit an event TYPE with ARGS via the parser callback."
  (when-let ((fn (chomp-parser-emit-fn parser)))
    (apply fn type args)))

;;;; ---- Parameter Parsing ----------------------------------------------

(defun chomp-parse--parse-params (param-str)
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

(defsubst chomp-parse--param (params index default)
  "Get PARAMS[INDEX], or DEFAULT if missing or zero.
If the parameter is a sub-parameter list, return the first element."
  (let ((val (if (< index (length params)) (aref params index) 0)))
    (cond
     ((listp val) (let ((v (car val))) (if (zerop v) default v)))
     ((zerop val) default)
     (t val))))

;;;; ---- Constructor ----------------------------------------------------

(defun chomp-parse-create (screen &optional write-fn emit-fn)
  "Create a parser for SCREEN with optional WRITE-FN and EMIT-FN."
  (make-chomp-parser
   :screen screen
   :write-fn write-fn
   :emit-fn emit-fn))

;;;; ---- Entry Point ----------------------------------------------------

(defun chomp-parse-bytes (parser string &optional start end)
  "Parse STRING from START to END through PARSER.
Returns the number of characters consumed."
  (let ((i (or start 0))
        (e (or end (length string)))
        (screen (chomp-parser-screen parser)))
    (while (< i e)
      (let ((ch (aref string i)))
        ;; The common case for terminal output is a run of printable text while
        ;; the parser is in ground state.  Dispatch that whole run to the
        ;; screen model at once instead of re-entering the parser for each byte.
        (if (and (eq (chomp-parser-state parser) :ground)
                 (>= ch ?\s)
                 (/= ch ?\x7f))
            (let ((run-start i))
              (while (and (< i e)
                          (let ((c (aref string i)))
                            (and (>= c ?\s) (/= c ?\x7f))))
                (cl-incf i))
              (chomp-screen-write-string screen string run-start i))
          (chomp-parse--process-char parser ch)
          (cl-incf i))))
    (- i (or start 0))))

;;;; ---- Main Dispatch --------------------------------------------------

(defun chomp-parse--process-char (parser ch)
  "Process a single character CH through the parser state machine."
  (cond
   ;; ESC always starts a new escape sequence
   ((= ch ?\e)
    ;; Save string-state if we were inside OSC/DCS
    (setf (chomp-parser-string-state parser)
          (pcase (chomp-parser-state parser)
            (:osc-string :osc)
            (:dcs-passthrough :dcs)
            (_ nil)))
    (setf (chomp-parser-state parser) :escape)
    (setf (chomp-parser-param-string parser) "")
    (setf (chomp-parser-private parser) nil)
    (setf (chomp-parser-intermediates parser) ""))

   ;; C0 controls (0x00-0x1F) handled inline in most states
   ((and (< ch ?\s)
         (memq (chomp-parser-state parser)
               '(:ground :escape :csi-entry :csi-param :csi-intermediate)))
    (chomp-parse--dispatch-c0 parser ch))

   ;; DEL -- ignore
   ((= ch ?\x7f) nil)

   ;; State-specific processing
   (t
    (pcase (chomp-parser-state parser)
      (:ground            (chomp-parse--ground parser ch))
      (:escape            (chomp-parse--escape parser ch))
      (:csi-entry         (chomp-parse--csi-entry parser ch))
      (:csi-param         (chomp-parse--csi-param parser ch))
      (:csi-intermediate  (chomp-parse--csi-intermediate parser ch))
      (:osc-string        (chomp-parse--osc-string parser ch))
      (:dcs-entry         (chomp-parse--dcs-entry parser ch))
      (:dcs-param         (chomp-parse--dcs-param parser ch))
      (:dcs-passthrough   (chomp-parse--dcs-passthrough parser ch))
      (:charset-designate (chomp-parse--charset-designate parser ch))
      (:sos-pm-apc        (chomp-parse--sos-pm-apc parser ch))))))

;;;; ---- C0 Control Dispatch --------------------------------------------

(defun chomp-parse--dispatch-c0 (parser ch)
  "Handle C0 control character CH."
  (let ((screen (chomp-parser-screen parser)))
    (cond
     ((= ch ?\a) (chomp-parse--emit parser 'bell))          ; BEL
     ((= ch ?\b) (chomp-screen-backspace screen))            ; BS
     ((= ch ?\t) (chomp-screen-tab-forward screen 1))        ; HT
     ((= ch ?\n) (chomp-screen-index screen))                ; LF
     ((= ch ?\v) (chomp-screen-index screen))                ; VT
     ((= ch ?\f) (chomp-screen-index screen))                ; FF
     ((= ch ?\r) (chomp-screen-carriage-return screen))      ; CR
     ((= ch 14)  (chomp-screen-shift-out screen))            ; SO
     ((= ch 15)  (chomp-screen-shift-in screen))             ; SI
     (t nil))))                                               ; NUL etc.

;;;; ---- State: Ground --------------------------------------------------

(defun chomp-parse--ground (parser ch)
  "Handle printable character in ground state."
  (when (>= ch ?\s)
    (chomp-screen-write-char (chomp-parser-screen parser) ch)))

;;;; ---- State: Escape --------------------------------------------------

(defun chomp-parse--escape (parser ch)
  "Handle character after ESC."
  (cond
   ;; ST (ESC \) terminates a pending string
   ((and (= ch ?\\) (chomp-parser-string-state parser))
    (chomp-parse--complete-string parser)
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-state parser) :ground))
   ;; CSI
   ((= ch ?\[)
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-state parser) :csi-entry))
   ;; OSC
   ((= ch ?\])
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-osc-string parser) "")
    (setf (chomp-parser-state parser) :osc-string))
   ;; DCS
   ((= ch ?P)
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-dcs-params parser) "")
    (setf (chomp-parser-dcs-string parser) "")
    (setf (chomp-parser-state parser) :dcs-entry))
   ;; Charset designation
   ((memq ch '(?\( ?\) ?* ?+ ?- ?. ?/))
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-charset-slot parser) ch)
    (setf (chomp-parser-state parser) :charset-designate))
   ;; SOS / PM / APC
   ((memq ch '(?X ?^ ?_))
    (setf (chomp-parser-string-state parser) nil)
    (setf (chomp-parser-state parser) :sos-pm-apc))
   ;; Simple ESC commands
   (t
    (setf (chomp-parser-string-state parser) nil)
    (chomp-parse--dispatch-esc parser ch)
    (setf (chomp-parser-state parser) :ground))))

(defun chomp-parse--dispatch-esc (parser ch)
  "Dispatch a simple ESC sequence."
  (let ((screen (chomp-parser-screen parser)))
    (condition-case err
        (pcase ch
          (?7 (chomp-screen-save-cursor screen))
          (?8 (chomp-screen-restore-cursor screen))
          (?D (chomp-screen-index screen))
          (?E (chomp-screen-next-line screen))
          (?M (chomp-screen-reverse-index screen))
          (?c (chomp-screen-reset screen)
              (chomp-parse--emit parser 'reset))
          (?n (setf (chomp-screen-charset-active screen) 'g2))
          (?o (setf (chomp-screen-charset-active screen) 'g3))
          (_ (chomp-parse--log "Unknown ESC %c (0x%02x)" ch ch)))
      (error (chomp-parse--log "ESC dispatch error for %c: %S" ch err)))))

(defun chomp-parse--complete-string (parser)
  "Complete a pending OSC or DCS string."
  (pcase (chomp-parser-string-state parser)
    (:osc (chomp-parse--dispatch-osc parser))
    (:dcs (chomp-parse--dispatch-dcs parser))))

;;;; ---- State: CSI Entry -----------------------------------------------

(defun chomp-parse--csi-entry (parser ch)
  "Handle first char after CSI (ESC [)."
  (cond
   ;; Private marker
   ((memq ch '(?? ?> ?=))
    (setf (chomp-parser-private parser) ch)
    (setf (chomp-parser-state parser) :csi-param))
   ;; Parameter char
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;) (= ch ?:))
    (setf (chomp-parser-param-string parser) (string ch))
    (setf (chomp-parser-state parser) :csi-param))
   ;; Intermediate byte
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (chomp-parser-intermediates parser) (string ch))
    (setf (chomp-parser-state parser) :csi-intermediate))
   ;; Final byte -- dispatch immediately
   ((and (>= ch ?@) (<= ch ?~))
    (chomp-parse--dispatch-csi parser ch))
   ;; Invalid
   (t (setf (chomp-parser-state parser) :ground))))

;;;; ---- State: CSI Param -----------------------------------------------

(defun chomp-parse--csi-param (parser ch)
  "Collect CSI parameters."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;) (= ch ?:))
    (setf (chomp-parser-param-string parser)
          (concat (chomp-parser-param-string parser) (string ch))))
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (chomp-parser-intermediates parser) (string ch))
    (setf (chomp-parser-state parser) :csi-intermediate))
   ((and (>= ch ?@) (<= ch ?~))
    (chomp-parse--dispatch-csi parser ch))
   (t (setf (chomp-parser-state parser) :ground))))

;;;; ---- State: CSI Intermediate ----------------------------------------

(defun chomp-parse--csi-intermediate (parser ch)
  "Collect CSI intermediate bytes."
  (cond
   ((and (>= ch ?\s) (<= ch ?/))
    (setf (chomp-parser-intermediates parser)
          (concat (chomp-parser-intermediates parser) (string ch))))
   ((and (>= ch ?@) (<= ch ?~))
    (chomp-parse--dispatch-csi parser ch))
   (t (setf (chomp-parser-state parser) :ground))))

;;;; ---- State: OSC String ----------------------------------------------

(defun chomp-parse--osc-string (parser ch)
  "Collect OSC string payload."
  (cond
   ;; BEL terminates
   ((= ch ?\a)
    (chomp-parse--dispatch-osc parser)
    (setf (chomp-parser-state parser) :ground))
   ;; ESC handled in process-char
   ;; Accumulate (limit length for safety)
   ((< (length (chomp-parser-osc-string parser)) 65536)
    (setf (chomp-parser-osc-string parser)
          (concat (chomp-parser-osc-string parser) (string ch))))))

;;;; ---- State: DCS Entry/Param/Passthrough -----------------------------

(defun chomp-parse--dcs-entry (parser ch)
  "Handle first char after DCS (ESC P)."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;))
    (setf (chomp-parser-dcs-params parser) (string ch))
    (setf (chomp-parser-state parser) :dcs-param))
   ((and (>= ch ?@) (<= ch ?~))
    (setf (chomp-parser-dcs-final parser) ch)
    (setf (chomp-parser-state parser) :dcs-passthrough))
   (t (setf (chomp-parser-state parser) :ground))))

(defun chomp-parse--dcs-param (parser ch)
  "Collect DCS parameters."
  (cond
   ((or (and (>= ch ?0) (<= ch ?9)) (= ch ?\;))
    (setf (chomp-parser-dcs-params parser)
          (concat (chomp-parser-dcs-params parser) (string ch))))
   ((and (>= ch ?@) (<= ch ?~))
    (setf (chomp-parser-dcs-final parser) ch)
    (setf (chomp-parser-state parser) :dcs-passthrough))
   (t (setf (chomp-parser-state parser) :ground))))

(defun chomp-parse--dcs-passthrough (parser ch)
  "Accumulate DCS body.  ESC handled in process-char for ST."
  ;; Just accumulate (limit for safety)
  (when (< (length (chomp-parser-dcs-string parser)) 1048576)
    (setf (chomp-parser-dcs-string parser)
          (concat (chomp-parser-dcs-string parser) (string ch)))))

;;;; ---- State: Charset Designate ---------------------------------------

(defun chomp-parse--charset-designate (parser ch)
  "Handle charset designation: ESC SLOT CH."
  (chomp-screen-designate-charset
   (chomp-parser-screen parser)
   (chomp-parser-charset-slot parser)
   ch)
  (setf (chomp-parser-state parser) :ground))

;;;; ---- State: SOS/PM/APC (consume and ignore) ------------------------

(defun chomp-parse--sos-pm-apc (parser ch)
  "Consume SOS/PM/APC strings until ST.  ESC handled in process-char."
  ;; Just ignore the character; ESC \ (ST) transitions via :escape state
  (ignore parser ch))

;;;; ---- CSI Dispatch Table ---------------------------------------------

(defun chomp-parse--csi-unknown (parser params)
  "Handler for unrecognized CSI sequences."
  (chomp-parse--log "Unknown CSI %s %s %c"
                    (or (chomp-parser-private parser) "")
                    (chomp-parser-param-string parser)
                    0)
  (ignore parser params))

(defvar chomp-parse--csi-dispatch
  (let ((tbl (make-vector 128 #'chomp-parse--csi-unknown)))
    (aset tbl ?@ #'chomp-parse--csi-ich)
    (aset tbl ?A #'chomp-parse--csi-cuu)
    (aset tbl ?B #'chomp-parse--csi-cud)
    (aset tbl ?C #'chomp-parse--csi-cuf)
    (aset tbl ?D #'chomp-parse--csi-cub)
    (aset tbl ?E #'chomp-parse--csi-cnl)
    (aset tbl ?F #'chomp-parse--csi-cpl)
    (aset tbl ?G #'chomp-parse--csi-cha)
    (aset tbl ?H #'chomp-parse--csi-cup)
    (aset tbl ?I #'chomp-parse--csi-cht)
    (aset tbl ?J #'chomp-parse--csi-ed)
    (aset tbl ?K #'chomp-parse--csi-el)
    (aset tbl ?L #'chomp-parse--csi-il)
    (aset tbl ?M #'chomp-parse--csi-dl)
    (aset tbl ?P #'chomp-parse--csi-dch)
    (aset tbl ?S #'chomp-parse--csi-su)
    (aset tbl ?T #'chomp-parse--csi-sd)
    (aset tbl ?X #'chomp-parse--csi-ech)
    (aset tbl ?Z #'chomp-parse--csi-cbt)
    (aset tbl ?` #'chomp-parse--csi-hpa)
    (aset tbl ?a #'chomp-parse--csi-hpr)
    (aset tbl ?b #'chomp-parse--csi-rep)
    (aset tbl ?c #'chomp-parse--csi-da)
    (aset tbl ?d #'chomp-parse--csi-vpa)
    (aset tbl ?e #'chomp-parse--csi-vpr)
    (aset tbl ?f #'chomp-parse--csi-hvp)
    (aset tbl ?g #'chomp-parse--csi-tbc)
    (aset tbl ?h #'chomp-parse--csi-sm)
    (aset tbl ?j #'chomp-parse--csi-cub)   ; VPB alias for CUB
    (aset tbl ?k #'chomp-parse--csi-cuu)   ; VPB alias for CUU
    (aset tbl ?l #'chomp-parse--csi-rm)
    (aset tbl ?m #'chomp-parse--csi-sgr)
    (aset tbl ?n #'chomp-parse--csi-dsr)
    (aset tbl ?q #'chomp-parse--csi-decscusr)
    (aset tbl ?r #'chomp-parse--csi-decstbm)
    (aset tbl ?s #'chomp-parse--csi-scp)
    (aset tbl ?t #'chomp-parse--csi-winops)
    (aset tbl ?u #'chomp-parse--csi-rcp)
    tbl)
  "CSI final-byte dispatch table.  Indexed by character code.")

(defun chomp-parse--dispatch-csi (parser final-byte)
  "Parse parameters and dispatch CSI sequence."
  (condition-case err
      (let ((params (chomp-parse--parse-params
                     (chomp-parser-param-string parser)))
            (handler (if (< final-byte 128)
                         (aref chomp-parse--csi-dispatch final-byte)
                       #'chomp-parse--csi-unknown)))
        (funcall handler parser params))
    (error
     (chomp-parse--log "CSI dispatch error for %c: %S" final-byte err)))
  (setf (chomp-parser-state parser) :ground))

;;;; ---- CSI Handlers ---------------------------------------------------

;; ICH - Insert Character
(defun chomp-parse--csi-ich (parser params)
  (chomp-screen-insert-chars (chomp-parser-screen parser)
                             (chomp-parse--param params 0 1)))

;; CUU - Cursor Up
(defun chomp-parse--csi-cuu (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'up (chomp-parse--param params 0 1)))

;; CUD - Cursor Down
(defun chomp-parse--csi-cud (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'down (chomp-parse--param params 0 1)))

;; CUF - Cursor Forward
(defun chomp-parse--csi-cuf (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'right (chomp-parse--param params 0 1)))

;; CUB - Cursor Back
(defun chomp-parse--csi-cub (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'left (chomp-parse--param params 0 1)))

;; CNL - Cursor Next Line
(defun chomp-parse--csi-cnl (parser params)
  (chomp-screen-cursor-next-line (chomp-parser-screen parser)
                                 (chomp-parse--param params 0 1)))

;; CPL - Cursor Previous Line
(defun chomp-parse--csi-cpl (parser params)
  (chomp-screen-cursor-prev-line (chomp-parser-screen parser)
                                  (chomp-parse--param params 0 1)))

;; CHA - Cursor Horizontal Absolute
(defun chomp-parse--csi-cha (parser params)
  (let* ((screen (chomp-parser-screen parser))
         (col (1- (chomp-parse--param params 0 1))))
    (setf (chomp-screen-pending-wrap screen) nil)
    (setf (chomp-screen-cursor-x screen)
          (chomp--clamp col 0 (1- (chomp-screen-width screen))))))

;; CUP - Cursor Position
(defun chomp-parse--csi-cup (parser params)
  (chomp-screen-cursor-goto (chomp-parser-screen parser)
                            (1- (chomp-parse--param params 0 1))
                            (1- (chomp-parse--param params 1 1))))

;; CHT - Cursor Horizontal Tab
(defun chomp-parse--csi-cht (parser params)
  (chomp-screen-tab-forward (chomp-parser-screen parser)
                            (chomp-parse--param params 0 1)))

;; ED - Erase in Display
(defun chomp-parse--csi-ed (parser params)
  (chomp-screen-erase-in-display (chomp-parser-screen parser)
                                 (chomp-parse--param params 0 0)))

;; EL - Erase in Line
(defun chomp-parse--csi-el (parser params)
  (chomp-screen-erase-in-line (chomp-parser-screen parser)
                              (chomp-parse--param params 0 0)))

;; IL - Insert Line
(defun chomp-parse--csi-il (parser params)
  (chomp-screen-insert-lines (chomp-parser-screen parser)
                             (chomp-parse--param params 0 1)))

;; DL - Delete Line
(defun chomp-parse--csi-dl (parser params)
  (chomp-screen-delete-lines (chomp-parser-screen parser)
                             (chomp-parse--param params 0 1)))

;; DCH - Delete Character
(defun chomp-parse--csi-dch (parser params)
  (chomp-screen-delete-chars (chomp-parser-screen parser)
                             (chomp-parse--param params 0 1)))

;; SU - Scroll Up
(defun chomp-parse--csi-su (parser params)
  (if (eql (chomp-parser-private parser) ??)
      ;; XTSMGRAPHICS: CSI ? Ps ; Pm S
      (chomp-parse--csi-xtsmgraphics parser params)
    (chomp-screen-scroll (chomp-parser-screen parser)
                         'up (chomp-parse--param params 0 1))))

;; XTSMGRAPHICS - Send/query graphics attributes
(defun chomp-parse--csi-xtsmgraphics (parser params)
  "Handle XTSMGRAPHICS (CSI ? Ps ; Pm S).
Ps=1: color register count, Ps=2: graphics geometry.
Pm=1: read, Pm=4: read maximum."
  (let ((attr (chomp-parse--param params 0 0))
        (op (chomp-parse--param params 1 0)))
    (if (memq op '(1 4))
        (pcase attr
          (1 ;; Color registers: report 256
           (chomp-parse--respond parser "\e[?1;0;256S"))
          (2 ;; Graphics geometry: report pixel size based on screen
           (let* ((screen (chomp-parser-screen parser))
                  (pw (min (* (chomp-screen-width screen) 8) 1000))
                  (ph (min (* (chomp-screen-height screen) 16) 1000)))
             (chomp-parse--respond parser (format "\e[?2;0;%d;%dS" pw ph))))
          (_ ;; Unknown attribute
           (chomp-parse--respond parser (format "\e[?%d;1S" attr))))
      ;; Unsupported operation
      (chomp-parse--respond
       parser
       (format "\e[?%d;%dS" attr (if (<= 1 attr 2) (if (<= 2 op 3) 3 2) 1))))))

;; SD - Scroll Down
(defun chomp-parse--csi-sd (parser params)
  (chomp-screen-scroll (chomp-parser-screen parser)
                       'down (chomp-parse--param params 0 1)))

;; ECH - Erase Character
(defun chomp-parse--csi-ech (parser params)
  (chomp-screen-erase-chars (chomp-parser-screen parser)
                            (chomp-parse--param params 0 1)))

;; CBT - Cursor Backward Tab
(defun chomp-parse--csi-cbt (parser params)
  (chomp-screen-tab-backward (chomp-parser-screen parser)
                             (chomp-parse--param params 0 1)))

;; HPA - Horizontal Position Absolute
(defun chomp-parse--csi-hpa (parser params)
  (chomp-parse--csi-cha parser params))

;; HPR - Horizontal Position Relative
(defun chomp-parse--csi-hpr (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'right (chomp-parse--param params 0 1)))

;; REP - Repeat last character
(defun chomp-parse--csi-rep (parser params)
  (chomp-screen-repeat-char (chomp-parser-screen parser)
                            (chomp-parse--param params 0 1)))

;; DA - Device Attributes
(defun chomp-parse--csi-da (parser _params)
  (cond
   ((not (chomp-parser-private parser))
    ;; Primary DA: report as VT220 with ANSI color
    (chomp-parse--respond parser "\e[?62;22c"))
   ((eql (chomp-parser-private parser) ?>)
    ;; Secondary DA
    (chomp-parse--respond parser "\e[>1;1;0c"))))

;; VPA - Vertical Position Absolute
(defun chomp-parse--csi-vpa (parser params)
  (let* ((screen (chomp-parser-screen parser))
         (row (1- (chomp-parse--param params 0 1)))
         (min-y (if (chomp-screen-origin-mode screen)
                    (chomp-screen-scroll-top screen) 0))
         (max-y (if (chomp-screen-origin-mode screen)
                    (chomp-screen-scroll-bottom screen)
                  (1- (chomp-screen-height screen)))))
    (setf (chomp-screen-pending-wrap screen) nil)
    (setf (chomp-screen-cursor-y screen)
          (chomp--clamp (+ min-y row) min-y max-y))))

;; VPR - Vertical Position Relative
(defun chomp-parse--csi-vpr (parser params)
  (chomp-screen-cursor-move (chomp-parser-screen parser)
                            'down (chomp-parse--param params 0 1)))

;; HVP - Horizontal Vertical Position (same as CUP)
(defun chomp-parse--csi-hvp (parser params)
  (chomp-parse--csi-cup parser params))

;; TBC - Tab Clear
(defun chomp-parse--csi-tbc (parser params)
  (chomp-screen-clear-tab-stop (chomp-parser-screen parser)
                               (chomp-parse--param params 0 0)))

;; SM - Set Mode
(defun chomp-parse--csi-sm (parser params)
  (let ((screen (chomp-parser-screen parser)))
    (if (chomp-parser-private parser)
        ;; DECSET
        (dotimes (i (length params))
          (let ((mode (aref params i)))
            (chomp-screen-set-mode screen mode t)
            (chomp-parse--emit parser 'mode-set mode t)))
      ;; Standard SM
      (dotimes (i (length params))
        (pcase (aref params i)
          (4 (setf (chomp-screen-insert-mode screen) t)))))))

;; RM - Reset Mode
(defun chomp-parse--csi-rm (parser params)
  (let ((screen (chomp-parser-screen parser)))
    (if (chomp-parser-private parser)
        ;; DECRST
        (dotimes (i (length params))
          (let ((mode (aref params i)))
            (chomp-screen-set-mode screen mode nil)
            (chomp-parse--emit parser 'mode-set mode nil)))
      ;; Standard RM
      (dotimes (i (length params))
        (pcase (aref params i)
          (4 (setf (chomp-screen-insert-mode screen) nil)))))))

;; DECSTBM - Set Scrolling Region
(defun chomp-parse--csi-decstbm (parser params)
  (let* ((screen (chomp-parser-screen parser))
         (h (chomp-screen-height screen))
         (top (1- (chomp-parse--param params 0 1)))
         (bot (1- (chomp-parse--param params 1 h))))
    (chomp-screen-set-scroll-region screen top bot)))

;; SCP - Save Cursor Position
(defun chomp-parse--csi-scp (parser _params)
  (chomp-screen-save-cursor (chomp-parser-screen parser)))

;; RCP - Restore Cursor Position
(defun chomp-parse--csi-rcp (parser _params)
  (chomp-screen-restore-cursor (chomp-parser-screen parser)))

;; DECSCUSR - Set Cursor Style (with SP intermediate)
(defun chomp-parse--csi-decscusr (parser params)
  (when (string= (chomp-parser-intermediates parser) " ")
    (let ((style (chomp-parse--param params 0 0)))
      (chomp-screen-set-cursor-style (chomp-parser-screen parser) style)
      (chomp-parse--emit parser 'cursor-style style))))

;; DSR - Device Status Report
(defun chomp-parse--csi-dsr (parser params)
  (let ((screen (chomp-parser-screen parser)))
    (pcase (chomp-parse--param params 0 0)
      (5 (chomp-parse--respond parser "\e[0n"))
      (6 (chomp-parse--respond
          parser
          (format "\e[%d;%dR"
                  (1+ (chomp-screen-cursor-y screen))
                  (1+ (chomp-screen-cursor-x screen))))))))

;; Window manipulation (CSI t) -- mostly ignore, report minimal
(defun chomp-parse--csi-winops (parser params)
  (let ((screen (chomp-parser-screen parser)))
    (pcase (chomp-parse--param params 0 0)
      ;; Report terminal size in chars
      (18 (chomp-parse--respond
           parser
           (format "\e[8;%d;%dt"
                   (chomp-screen-height screen)
                   (chomp-screen-width screen))))
      (_ nil))))

;;;; ---- SGR Handler (Select Graphic Rendition) -------------------------

(defun chomp-parse--csi-sgr (parser params)
  "Handle SGR (CSI m) -- the most complex single CSI handler."
  (let ((screen (chomp-parser-screen parser))
        (len (length params)))
    (if (zerop len)
        (chomp-screen-reset-attr screen)
      (let ((i 0))
        (while (< i len)
          (let ((p (aref params i)))
            (cond
             ;; Sub-parameter list (e.g., 4:0, 4:3 for underline styles)
             ((listp p)
              (when (and p (= (car p) 4))
                ;; Underline with sub-parameter: 4:0=off, 4:1=line,
                ;; 4:2=double, 4:3=curly, 4:4=dotted, 4:5=dashed
                (let ((sub (if (cdr p) (cadr p) 1)))
                  (chomp-screen-set-attr
                   screen :underline
                   (pcase sub
                     (0 nil)
                     (1 'line)
                     (2 'double)
                     (3 'curly)
                     (4 'dotted)
                     (5 'dashed)
                     (_ 'line))))))
             ;; Reset
             ((= p 0)  (chomp-screen-reset-attr screen))
             ;; Bold / faint / italic
             ((= p 1)  (chomp-screen-set-attr screen :bold t))
             ((= p 2)  (chomp-screen-set-attr screen :faint t))
             ((= p 3)  (chomp-screen-set-attr screen :italic t))
             ;; Underline (plain, no sub-params)
             ((= p 4)  (chomp-screen-set-attr screen :underline 'line))
             ;; Blink
             ((= p 5)  (chomp-screen-set-attr screen :blink 'slow))
             ((= p 6)  (chomp-screen-set-attr screen :blink 'fast))
             ;; Inverse / conceal / crossed
             ((= p 7)  (chomp-screen-set-attr screen :inverse t))
             ((= p 8)  (chomp-screen-set-attr screen :conceal t))
             ((= p 9)  (chomp-screen-set-attr screen :crossed t))
             ;; Fonts 10-19
             ((and (>= p 10) (<= p 19))
              (chomp-screen-set-attr screen :font (- p 10)))
             ;; Double underline
             ((= p 21) (chomp-screen-set-attr screen :underline 'double))
             ;; Reset intensity
             ((= p 22) (chomp-screen-set-attr screen :bold nil)
                        (chomp-screen-set-attr screen :faint nil))
             ;; Reset individual attrs
             ((= p 23) (chomp-screen-set-attr screen :italic nil))
             ((= p 24) (chomp-screen-set-attr screen :underline nil))
             ((= p 25) (chomp-screen-set-attr screen :blink nil))
             ((= p 27) (chomp-screen-set-attr screen :inverse nil))
             ((= p 28) (chomp-screen-set-attr screen :conceal nil))
             ((= p 29) (chomp-screen-set-attr screen :crossed nil))
             ;; Foreground ANSI 30-37
             ((and (>= p 30) (<= p 37))
              (chomp-screen-set-attr screen :fg (- p 30)))
             ;; Extended foreground
             ((= p 38)
              (when (< (1+ i) len)
                (let ((sub (aref params (1+ i))))
                  (cond
                   ;; Truecolor: 38;2;R;G;B
                   ((and (= sub 2) (<= (+ i 4) (1- len)))
                    (chomp-screen-set-attr
                     screen :fg
                     (list (aref params (+ i 2))
                           (aref params (+ i 3))
                           (aref params (+ i 4))))
                    (cl-incf i 4))
                   ;; 256-color: 38;5;N
                   ((and (= sub 5) (<= (+ i 2) (1- len)))
                    (chomp-screen-set-attr screen :fg (aref params (+ i 2)))
                    (cl-incf i 2))
                   (t (cl-incf i))))))
             ;; Default foreground
             ((= p 39) (chomp-screen-set-attr screen :fg nil))
             ;; Background ANSI 40-47
             ((and (>= p 40) (<= p 47))
              (chomp-screen-set-attr screen :bg (- p 40)))
             ;; Extended background
             ((= p 48)
              (when (< (1+ i) len)
                (let ((sub (aref params (1+ i))))
                  (cond
                   ((and (= sub 2) (<= (+ i 4) (1- len)))
                    (chomp-screen-set-attr
                     screen :bg
                     (list (aref params (+ i 2))
                           (aref params (+ i 3))
                           (aref params (+ i 4))))
                    (cl-incf i 4))
                   ((and (= sub 5) (<= (+ i 2) (1- len)))
                    (chomp-screen-set-attr screen :bg (aref params (+ i 2)))
                    (cl-incf i 2))
                   (t (cl-incf i))))))
             ;; Default background
             ((= p 49) (chomp-screen-set-attr screen :bg nil))
             ;; Underline color
             ((= p 58)
              (when (< (1+ i) len)
                (let ((sub (aref params (1+ i))))
                  (cond
                   ((and (= sub 2) (<= (+ i 4) (1- len)))
                    (chomp-screen-set-attr
                     screen :ul-color
                     (list (aref params (+ i 2))
                           (aref params (+ i 3))
                           (aref params (+ i 4))))
                    (cl-incf i 4))
                   ((and (= sub 5) (<= (+ i 2) (1- len)))
                    (chomp-screen-set-attr screen :ul-color
                                           (aref params (+ i 2)))
                    (cl-incf i 2))
                   (t (cl-incf i))))))
             ;; Default underline color
             ((= p 59) (chomp-screen-set-attr screen :ul-color nil))
             ;; Bright foreground 90-97
             ((and (>= p 90) (<= p 97))
              (chomp-screen-set-attr screen :fg (+ 8 (- p 90))))
             ;; Bright background 100-107
             ((and (>= p 100) (<= p 107))
              (chomp-screen-set-attr screen :bg (+ 8 (- p 100))))
             ;; Unknown -- silently skip
             (t nil)))
          (cl-incf i))))))

;;;; ---- OSC 52 Clipboard -----------------------------------------------

(defun chomp-parse--handle-osc-52 (parser payload)
  "Handle OSC 52 clipboard manipulation.
PAYLOAD format: TARGET;BASE64-DATA
TARGET is one or more of: c (clipboard), p (primary), s (secondary), etc.
If BASE64-DATA is `?' this is a query; otherwise it's a set operation."
  (when (string-match "\\`\\([^;]*\\);\\(.*\\)\\'" payload)
    (let ((_target (match-string 1 payload))
          (data (match-string 2 payload)))
      (cond
       ;; Query: respond with current clipboard contents
       ((string= data "?")
        (let* ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                         (ignore-errors (current-kill 0 t))
                         ""))
               (encoded (base64-encode-string (encode-coding-string text 'utf-8) t)))
          (chomp-parse--respond parser (format "\e]52;c;%s\e\\" encoded))))
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
          (chomp-parse--emit parser 'clipboard text)))))))

;;;; ---- OSC Dispatch ---------------------------------------------------

(defun chomp-parse--dispatch-osc (parser)
  "Dispatch a completed OSC sequence."
  (condition-case err
      (let* ((str (chomp-parser-osc-string parser))
             (screen (chomp-parser-screen parser)))
        (if (string-match "\\`\\([0-9]+\\)\\(?:;\\(\\(?:.\\|\n\\)*\\)\\)?\\'" str)
            (let ((num (string-to-number (match-string 1 str)))
                  (payload (or (match-string 2 str) "")))
              (pcase num
                ;; Set title (+ icon name)
                ((or 0 1 2)
                 (setf (chomp-screen-title screen) payload)
                 (chomp-parse--emit parser 'title payload))
                ;; Set CWD
                (7
                 (let ((cwd (if (string-match "file://[^/]*/\\(.*\\)" payload)
                                (concat "/" (match-string 1 payload))
                              payload)))
                   (setf (chomp-screen-cwd screen) cwd)
                   (chomp-parse--emit parser 'cwd cwd)))
                ;; Query foreground
                (10
                 (when (string= payload "?")
                   (chomp-parse--respond
                    parser
                    (concat "\e]10;" (chomp-parse--color-to-xterm
                                      (face-foreground 'default nil t))
                            "\e\\"))))
                ;; Query background
                (11
                 (when (string= payload "?")
                   (chomp-parse--respond
                    parser
                    (concat "\e]11;" (chomp-parse--color-to-xterm
                                      (face-background 'default nil t))
                            "\e\\"))))
                ;; Shell integration
                (51
                 (chomp-parse--emit parser 'osc-51 payload))
                ;; Clipboard (selection manipulation)
                (52
                 (chomp-parse--handle-osc-52 parser payload))
                ;; Unknown
                (_ (chomp-parse--log "Unknown OSC %d" num))))
          (chomp-parse--log "Malformed OSC: %s"
                            (substring str 0 (min (length str) 40)))))
    (error
     (chomp-parse--log "OSC dispatch error: %S" err))))

;;;; ---- DCS Dispatch ---------------------------------------------------

(defun chomp-parse--dispatch-dcs (parser)
  "Dispatch a completed DCS sequence."
  (condition-case err
      (let ((final (chomp-parser-dcs-final parser))
            (body (chomp-parser-dcs-string parser)))
        (pcase final
          ;; Sixel graphics
          (?q (chomp-parse--emit parser 'sixel body))
          ;; Unknown
          (_ (chomp-parse--log "Unknown DCS final %c" final))))
    (error
     (chomp-parse--log "DCS dispatch error: %S" err))))

(provide 'chomp-parse)
;;; chomp-parse.el ends here
