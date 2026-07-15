;;; chomp-test.el --- Tests for chomp -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the chomp terminal emulator, focused on the screen model
;; and parser since they are pure data transformers testable without a buffer.

;;; Code:

(require 'ert)
(require 'chomp-term)
(require 'chomp-parse)
(require 'chomp-render)
(require 'chomp-io)
(require 'chomp-input)
(require 'chomp)

;;;; ---- Test Helpers ---------------------------------------------------

(defmacro chomp-test-with-screen (spec &rest body)
  "Create a test screen per SPEC and evaluate BODY.
SPEC is a plist with :width (default 20) and :height (default 6).
Binds `screen' and `parser' in BODY."
  (declare (indent 1))
  (let ((width (or (plist-get spec :width) 20))
        (height (or (plist-get spec :height) 6)))
    `(let* ((screen (chomp-screen-create ,width ,height))
            (parser (chomp-parse-create screen)))
       ,@body)))

(defun chomp-test-output (parser str)
  "Feed STR through PARSER."
  (chomp-parse-bytes parser str))

(defun chomp-test-base64 (string)
  "Return STRING encoded as UTF-8 base64."
  (base64-encode-string (encode-coding-string string 'utf-8) t))

(defun chomp-test-display-line (screen row)
  "Return the text content of display line ROW as a string."
  (let* ((line (chomp-screen-get-line screen row))
         (cells (chomp-line-cells line))
         (chars nil))
    (dotimes (i (length cells))
      (push (chomp-cell-char (aref cells i)) chars))
    ;; Strip trailing spaces
    (string-trim-right (apply #'string (nreverse chars)))))

(defun chomp-test-display-text (screen)
  "Return all display lines as a list of strings (trailing spaces stripped)."
  (let ((lines nil))
    (dotimes (i (chomp-screen-height screen))
      (push (chomp-test-display-line screen i) lines))
    (nreverse lines)))

(defun chomp-test-cursor (screen)
  "Return cursor position as (X . Y)."
  (cons (chomp-screen-cursor-x screen)
        (chomp-screen-cursor-y screen)))

;;;; ---- Screen Model Tests ---------------------------------------------

(ert-deftest chomp-test-screen-create ()
  "Screen creation initializes correctly."
  (let ((s (chomp-screen-create 80 24)))
    (should (= 80 (chomp-screen-width s)))
    (should (= 24 (chomp-screen-height s)))
    (should (= 0 (chomp-screen-cursor-x s)))
    (should (= 0 (chomp-screen-cursor-y s)))
    (should (= 0 (chomp-screen-scroll-top s)))
    (should (= 23 (chomp-screen-scroll-bottom s)))
    (should (chomp-screen-auto-wrap s))
    (should (chomp-screen-cursor-visible s))))

(ert-deftest chomp-test-write-char ()
  "Writing characters places them and advances cursor."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-screen-write-char screen ?H)
    (chomp-screen-write-char screen ?i)
    (should (equal "Hi" (chomp-test-display-line screen 0)))
    (should (equal '(2 . 0) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-auto-wrap ()
  "Auto-wrap moves to next line at end of line."
  (chomp-test-with-screen (:width 5 :height 3)
    (dotimes (i 7)
      (chomp-screen-write-char screen (+ ?a i)))
    ;; "abcde" on line 0 (wrapped), "fg" on line 1
    (should (equal "abcde" (chomp-test-display-line screen 0)))
    (should (equal "fg" (chomp-test-display-line screen 1)))
    (should (chomp-line-wrapped (chomp-screen-get-line screen 0)))
    (should (equal '(2 . 1) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-cursor-move-clamp ()
  "Cursor movement clamps to screen bounds."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-screen-cursor-goto screen 0 0)
    (chomp-screen-cursor-move screen 'up 100)
    (should (= 0 (chomp-screen-cursor-y screen)))
    (chomp-screen-cursor-move screen 'left 100)
    (should (= 0 (chomp-screen-cursor-x screen)))
    (chomp-screen-cursor-move screen 'down 100)
    (should (= 5 (chomp-screen-cursor-y screen)))
    (chomp-screen-cursor-move screen 'right 100)
    (should (= 19 (chomp-screen-cursor-x screen)))))

(ert-deftest chomp-test-cursor-goto ()
  "Cursor goto positions correctly (0-indexed)."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-screen-cursor-goto screen 3 10)
    (should (equal '(10 . 3) (chomp-test-cursor screen)))
    ;; Clamp to bounds
    (chomp-screen-cursor-goto screen 100 100)
    (should (equal '(19 . 5) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-carriage-return ()
  "CR moves cursor to column 0."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-screen-cursor-goto screen 0 10)
    (chomp-screen-carriage-return screen)
    (should (= 0 (chomp-screen-cursor-x screen)))
    (should (= 0 (chomp-screen-cursor-y screen)))))

(ert-deftest chomp-test-index-scroll ()
  "Index at bottom of scroll region scrolls up."
  (chomp-test-with-screen (:width 10 :height 3)
    ;; Write text on all lines
    (chomp-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "line0"))
    (chomp-screen-cursor-goto screen 1 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "line1"))
    (chomp-screen-cursor-goto screen 2 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "line2"))
    ;; Now at row 2, index should scroll
    (chomp-screen-index screen)
    (should (equal "line1" (chomp-test-display-line screen 0)))
    (should (equal "line2" (chomp-test-display-line screen 1)))
    (should (equal "" (chomp-test-display-line screen 2)))))

(ert-deftest chomp-test-reverse-index ()
  "Reverse index at top of scroll region scrolls down."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "line0"))
    (chomp-screen-cursor-goto screen 1 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "line1"))
    ;; Cursor at row 0
    (chomp-screen-cursor-goto screen 0 0)
    (chomp-screen-reverse-index screen)
    ;; Line 0 should now be blank, old line 0 shifted to line 1
    (should (equal "" (chomp-test-display-line screen 0)))
    (should (equal "line0" (chomp-test-display-line screen 1)))
    (should (equal "line1" (chomp-test-display-line screen 2)))))

(ert-deftest chomp-test-erase-in-display ()
  "Erase in display works for all modes."
  (chomp-test-with-screen (:width 5 :height 3)
    ;; Fill screen
    (dotimes (r 3)
      (chomp-screen-cursor-goto screen r 0)
      (dotimes (_ 5) (chomp-screen-write-char screen ?X)))
    ;; Erase from cursor to end (mode 0)
    (chomp-screen-cursor-goto screen 1 2)
    (chomp-screen-erase-in-display screen 0)
    (should (equal "XXXXX" (chomp-test-display-line screen 0)))
    (should (equal "XX" (chomp-test-display-line screen 1)))
    (should (equal "" (chomp-test-display-line screen 2)))))

(ert-deftest chomp-test-erase-in-line ()
  "Erase in line works for all modes."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "0123456789"))
    ;; Erase from cursor to end (mode 0)
    (chomp-screen-cursor-goto screen 0 5)
    (chomp-screen-erase-in-line screen 0)
    (should (equal "01234" (chomp-test-display-line screen 0)))
    ;; Erase from start to cursor (mode 1)
    (chomp-screen-cursor-goto screen 0 2)
    (chomp-screen-erase-in-line screen 1)
    (should (equal "   34" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-insert-delete-lines ()
  "Insert and delete lines within scroll region."
  (chomp-test-with-screen (:width 5 :height 4)
    (dotimes (r 4)
      (chomp-screen-cursor-goto screen r 0)
      (chomp-screen-write-char screen (+ ?A r)))
    ;; Insert 1 line at row 1
    (chomp-screen-cursor-goto screen 1 0)
    (chomp-screen-insert-lines screen 1)
    (should (equal "A" (chomp-test-display-line screen 0)))
    (should (equal "" (chomp-test-display-line screen 1)))
    (should (equal "B" (chomp-test-display-line screen 2)))
    (should (equal "C" (chomp-test-display-line screen 3)))
    ;; D fell off the bottom

    ;; Delete 1 line at row 1
    (chomp-screen-cursor-goto screen 1 0)
    (chomp-screen-delete-lines screen 1)
    (should (equal "A" (chomp-test-display-line screen 0)))
    (should (equal "B" (chomp-test-display-line screen 1)))
    (should (equal "C" (chomp-test-display-line screen 2)))
    (should (equal "" (chomp-test-display-line screen 3)))))

(ert-deftest chomp-test-insert-delete-chars ()
  "Insert and delete characters on a line."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "ABCDE"))
    ;; Insert 2 chars at position 2
    (chomp-screen-cursor-goto screen 0 2)
    (chomp-screen-insert-chars screen 2)
    (should (equal "AB  CDE" (chomp-test-display-line screen 0)))
    ;; Delete 2 chars at position 2
    (chomp-screen-delete-chars screen 2)
    (should (equal "ABCDE" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-scroll-region ()
  "Scroll region confines scrolling."
  (chomp-test-with-screen (:width 5 :height 5)
    (dotimes (r 5)
      (chomp-screen-cursor-goto screen r 0)
      (chomp-screen-write-char screen (+ ?A r)))
    ;; Set scroll region to rows 1-3
    (chomp-screen-set-scroll-region screen 1 3)
    ;; Cursor homes to 0,0 after DECSTBM
    (should (equal '(0 . 0) (chomp-test-cursor screen)))
    ;; Scroll up within region
    (chomp-screen-cursor-goto screen 3 0)
    (chomp-screen-index screen)
    ;; Row 0 and 4 should be unchanged
    (should (equal "A" (chomp-test-display-line screen 0)))
    (should (equal "C" (chomp-test-display-line screen 1)))
    (should (equal "D" (chomp-test-display-line screen 2)))
    (should (equal "" (chomp-test-display-line screen 3)))
    (should (equal "E" (chomp-test-display-line screen 4)))
    (should-not (chomp-screen-scrollback-lines screen))))

(ert-deftest chomp-test-alt-screen ()
  "Alternate screen saves and restores main screen."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "main"))
    ;; Enter alt screen
    (chomp-screen-enter-alt screen)
    (should (equal "" (chomp-test-display-line screen 0)))
    (should (equal '(0 . 0) (chomp-test-cursor screen)))
    ;; Write on alt screen
    (mapc (lambda (c) (chomp-screen-write-char screen c)) (string-to-list "alt"))
    (should (equal "alt" (chomp-test-display-line screen 0)))
    ;; Leave alt screen
    (chomp-screen-leave-alt screen)
    (should (equal "main" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-save-restore-cursor ()
  "Save and restore cursor position and attributes."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-screen-cursor-goto screen 3 10)
    (chomp-screen-set-attr screen :bold t)
    (chomp-screen-save-cursor screen)
    ;; Move elsewhere and change attrs
    (chomp-screen-cursor-goto screen 0 0)
    (chomp-screen-reset-attr screen)
    ;; Restore
    (chomp-screen-restore-cursor screen)
    (should (equal '(10 . 3) (chomp-test-cursor screen)))
    (should (chomp-attr-bold (chomp-screen-current-attr screen)))))

(ert-deftest chomp-test-tab-stops ()
  "Tab stops work correctly."
  (chomp-test-with-screen (:width 40 :height 3)
    ;; Default tab stops: 8, 16, 24, 32
    (chomp-screen-cursor-goto screen 0 0)
    (chomp-screen-tab-forward screen 1)
    (should (= 8 (chomp-screen-cursor-x screen)))
    (chomp-screen-tab-forward screen 1)
    (should (= 16 (chomp-screen-cursor-x screen)))
    ;; Tab backward
    (chomp-screen-tab-backward screen 1)
    (should (= 8 (chomp-screen-cursor-x screen)))))

(ert-deftest chomp-test-resize-clamps-cursor ()
  "Resize clamps cursor to new bounds."
  (let ((screen (chomp-screen-create 80 24)))
    (chomp-screen-cursor-goto screen 20 70)
    (chomp-screen-resize screen 40 10)
    (should (= 39 (chomp-screen-cursor-x screen)))
    (should (= 9 (chomp-screen-cursor-y screen)))))

(ert-deftest chomp-test-resize-resets-scroll-region ()
  "Resize resets scroll region to full screen."
  (let ((screen (chomp-screen-create 80 24)))
    (chomp-screen-set-scroll-region screen 5 15)
    (chomp-screen-resize screen 80 30)
    (should (= 0 (chomp-screen-scroll-top screen)))
    (should (= 29 (chomp-screen-scroll-bottom screen)))))

(ert-deftest chomp-test-resize-marks-all-dirty ()
  "After resize, every line is marked dirty."
  (let ((screen (chomp-screen-create 80 24)))
    (chomp-screen-clear-dirty screen)
    (chomp-screen-resize screen 100 30)
    (should (= 30 (length (chomp-screen-get-dirty screen))))))

(ert-deftest chomp-test-reset ()
  "Full reset clears everything."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-screen-cursor-goto screen 3 10)
    (chomp-screen-set-attr screen :bold t)
    (chomp-screen-write-char screen ?X)
    (chomp-screen-reset screen)
    (should (equal '(0 . 0) (chomp-test-cursor screen)))
    (should (not (chomp-attr-bold (chomp-screen-current-attr screen))))
    (should (equal "" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-charset-dec-graphics ()
  "DEC graphics charset translates box-drawing characters."
  (chomp-test-with-screen (:width 10 :height 3)
    ;; Switch to DEC graphics
    (chomp-screen-designate-charset screen ?\( ?0)
    (setf (chomp-screen-charset-active screen) 'g0)
    ;; Write 'q' which should translate to horizontal line
    (chomp-screen-write-char screen ?q)
    (should (= #x2500 (chomp-cell-char
                        (aref (chomp-line-cells
                               (chomp-screen-get-line screen 0)) 0))))))

(ert-deftest chomp-test-scrollback ()
  "Lines scrolled off top go to scrollback."
  (chomp-test-with-screen (:width 5 :height 2)
    ;; Fill lines
    (chomp-screen-cursor-goto screen 0 0)
    (chomp-screen-write-char screen ?A)
    (chomp-screen-cursor-goto screen 1 0)
    (chomp-screen-write-char screen ?B)
    ;; Scroll up (pushes line 0 to scrollback)
    (chomp-screen-cursor-goto screen 1 0)
    (chomp-screen-index screen)
    ;; Check scrollback
    (let ((sb (chomp-screen-scrollback-lines screen)))
      (should (= 1 (length sb)))
      (should (= ?A (chomp-cell-char
                      (aref (chomp-line-cells (car sb)) 0)))))))

;;;; ---- Parser Tests ---------------------------------------------------

(ert-deftest chomp-test-parse-plain-text ()
  "Parser handles plain text."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "Hello")
    (should (equal "Hello" (chomp-test-display-line screen 0)))
    (should (equal '(5 . 0) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-parse-crlf ()
  "Parser handles CR LF."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "Hello\r\nWorld")
    (should (equal "Hello" (chomp-test-display-line screen 0)))
    (should (equal "World" (chomp-test-display-line screen 1)))))

(ert-deftest chomp-test-parse-cursor-movement ()
  "Parser handles CSI cursor movement."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; CUP to row 3, col 5 (1-indexed in VT)
    (chomp-test-output parser "\e[4;6H")
    (should (equal '(5 . 3) (chomp-test-cursor screen)))
    ;; CUU (up 2)
    (chomp-test-output parser "\e[2A")
    (should (equal '(5 . 1) (chomp-test-cursor screen)))
    ;; CUF (right 3)
    (chomp-test-output parser "\e[3C")
    (should (equal '(8 . 1) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-parse-erase ()
  "Parser handles CSI J and CSI K."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-test-output parser "XXXXXXXXXX")
    (chomp-test-output parser "\e[1;6H")  ; cursor at row 0, col 5
    (chomp-test-output parser "\e[0K")    ; erase to end of line
    (should (equal "XXXXX" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-parse-sgr-basic ()
  "Parser handles basic SGR attributes."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; Bold + red foreground
    (chomp-test-output parser "\e[1;31mHi\e[0m")
    (let* ((line (chomp-screen-get-line screen 0))
           (cell (aref (chomp-line-cells line) 0))
           (attr (chomp-cell-attr cell)))
      (should attr)
      (should (chomp-attr-bold attr))
      (should (= 1 (chomp-attr-fg attr))))))  ; red = 1

(ert-deftest chomp-test-private-csi-m-not-sgr ()
  "CSI > 4 ; 1 m (modifyOtherKeys) must not set underline."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e[>4;1m\e[1;36mchomp\e[0m")
    (let* ((line (chomp-screen-get-line screen 0))
           (cell (aref (chomp-line-cells line) 0))
           (attr (chomp-cell-attr cell)))
      (should attr)
      (should (chomp-attr-bold attr))
      (should (= 6 (chomp-attr-fg attr)))
      (should-not (chomp-attr-underline attr)))))

(ert-deftest chomp-test-parse-sgr-256color ()
  "Parser handles 256-color SGR."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e[38;5;196mR\e[0m")
    (let* ((line (chomp-screen-get-line screen 0))
           (cell (aref (chomp-line-cells line) 0))
           (attr (chomp-cell-attr cell)))
      (should (= 196 (chomp-attr-fg attr))))))

(ert-deftest chomp-test-parse-sgr-truecolor ()
  "Parser handles truecolor SGR."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e[38;2;255;128;0mO\e[0m")
    (let* ((line (chomp-screen-get-line screen 0))
           (cell (aref (chomp-line-cells line) 0))
           (attr (chomp-cell-attr cell)))
      (should (equal '(255 128 0) (chomp-attr-fg attr))))))

(ert-deftest chomp-test-parse-decset-alt-screen ()
  "Parser handles DECSET ?1049 (alt screen)."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-test-output parser "main")
    (chomp-test-output parser "\e[?1049h")  ; enter alt
    (should (equal "" (chomp-test-display-line screen 0)))
    (chomp-test-output parser "alt")
    (should (equal "alt" (chomp-test-display-line screen 0)))
    (chomp-test-output parser "\e[?1049l")  ; leave alt
    (should (equal "main" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-parse-scroll-region ()
  "Parser handles DECSTBM."
  (chomp-test-with-screen (:width 10 :height 5)
    (chomp-test-output parser "\e[2;4r")  ; scroll region rows 2-4 (1-indexed)
    (should (= 1 (chomp-screen-scroll-top screen)))   ; 0-indexed
    (should (= 3 (chomp-screen-scroll-bottom screen)))))

(ert-deftest chomp-test-parse-insert-delete ()
  "Parser handles ICH, DCH, IL, DL."
  (chomp-test-with-screen (:width 10 :height 3)
    (chomp-test-output parser "ABCDE")
    ;; Insert 2 chars at col 2
    (chomp-test-output parser "\e[1;3H")   ; cursor at row 1, col 3 (1-indexed)
    (chomp-test-output parser "\e[2@")     ; ICH 2
    (should (equal "AB  CDE" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-parse-esc-save-restore ()
  "Parser handles ESC 7/8 save/restore cursor."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e[3;5H")  ; goto row 3, col 5
    (chomp-test-output parser "\e7")      ; save
    (chomp-test-output parser "\e[1;1H")  ; goto origin
    (chomp-test-output parser "\e8")      ; restore
    (should (equal '(4 . 2) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-parse-osc-title ()
  "Parser handles OSC 0/2 title setting."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e]2;My Title\a")
    (should (equal "My Title" (chomp-screen-title screen)))))

(ert-deftest chomp-test-parse-osc-title-st ()
  "Parser handles OSC with ST (ESC \\) terminator."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "\e]2;My Title\e\\")
    (should (equal "My Title" (chomp-screen-title screen)))))

(ert-deftest chomp-test-parse-osc-notifications ()
  "OSC 9 and 777 emit only valid normalized notifications."
  (chomp-test-with-screen (:width 20 :height 6)
    (let (events)
      (setf (chomp-parser-emit-fn parser)
            (lambda (type &rest args) (push (cons type args) events)))
      (chomp-test-output parser "\e]9;body\a")
      (chomp-test-output parser "\e]777;notify;title;body text\a")
      (chomp-test-output parser "\e]9;\a")
      (chomp-test-output parser "\e]777;notify;;\a")
      (chomp-test-output parser "\e]777;unknown;title;body\a")
      (should (equal '((notification "title" "body text")
                       (notification nil "body"))
                     events)))))

(ert-deftest chomp-test-parse-osc-progress ()
  "OSC 9;4 progress states are normalized and percentages clamped."
  (chomp-test-with-screen (:width 20 :height 6)
    (let (events)
      (setf (chomp-parser-emit-fn parser)
            (lambda (type &rest args)
              (when (eq type 'progress) (push args events))))
      (dolist (payload '("0;0" "1;150" "2;-5" "3;42" "4;75"
                         "5;10" "bogus"))
        (chomp-test-output parser (format "\e]9;4;%s\a" payload)))
      (should (equal '((pause 75) (indeterminate 42) (error 0)
                       (set 100) (remove 0))
                     events)))))

(ert-deftest chomp-test-parse-error-recovery ()
  "Parser recovers from malformed sequences."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; Malformed CSI (no final byte, then normal text)
    (chomp-test-output parser "\e[999ZHello")
    ;; Should not crash; Hello might not appear (depends on unknown handler)
    ;; The key test is that we don't error
    (should t)))

(ert-deftest chomp-test-parse-partial-sequence ()
  "Parser handles sequences split across chunks."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; Send CSI CUP in two parts
    (chomp-test-output parser "\e[")
    (chomp-test-output parser "3;5H")
    (should (equal '(4 . 2) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-parse-bell ()
  "Parser emits bell event."
  (chomp-test-with-screen (:width 20 :height 6)
    (let ((bell-called nil))
      (setf (chomp-parser-emit-fn parser)
            (lambda (type &rest _args)
              (when (eq type 'bell)
                (setq bell-called t))))
      (chomp-test-output parser "\a")
      (should bell-called))))

(ert-deftest chomp-test-parse-backspace ()
  "Parser handles backspace."
  (chomp-test-with-screen (:width 20 :height 6)
    (chomp-test-output parser "AB\bC")
    ;; B is overwritten by C
    (should (equal "AC" (chomp-test-display-line screen 0)))))

(ert-deftest chomp-test-parse-tab ()
  "Parser handles horizontal tab."
  (chomp-test-with-screen (:width 40 :height 3)
    (chomp-test-output parser "A\tB")
    (should (= ?A (chomp-cell-char
                    (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 0))))
    (should (= ?B (chomp-cell-char
                    (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 8))))))

;;;; ---- Render Tests ---------------------------------------------------

(ert-deftest chomp-test-render-color-conversion ()
  "Color conversion produces correct hex strings."
  ;; ANSI 0-15 come from theme-backed faces (ansi-color-*)
  (should (stringp (chomp-render--color-to-string 0)))
  (should (stringp (chomp-render--color-to-string 1)))
  ;; 256-color
  (should (equal "#ff0000" (chomp-render--color-to-string 196)))
  ;; Truecolor
  (should (equal "#ff8000" (chomp-render--color-to-string '(255 128 0))))
  ;; Grayscale
  (should (string-match-p "#[0-9a-f]+" (chomp-render--color-to-string 240)))
  ;; nil
  (should (null (chomp-render--color-to-string nil))))

(ert-deftest chomp-test-render-theme-cache-invalidation ()
  "Theme changes clear color caches and redraw every live Chomp render state."
  (let ((buffers nil)
        (color "#111111")
        (resets 0)
        cleared
        (original-face-foreground (symbol-function 'face-foreground))
        (original-reset (symbol-function 'chomp-render-full-reset)))
    (unwind-protect
        (progn
          (fillarray chomp-render--indexed-color-cache nil)
          (clrhash chomp-render--attr-face-cache)
          (dotimes (i 2)
            (let* ((buffer (generate-new-buffer (format " *theme-chomp-%d*" i)))
                   (screen (chomp-screen-create 5 2)))
              (push buffer buffers)
              (with-current-buffer buffer
                (chomp-mode)
                (setq-local chomp--screen screen)
                (setq-local chomp--render (chomp-render-create screen buffer))
                (chomp-screen-set-attr screen :fg 1)
                (chomp-screen-write-char screen ?x))))
          (cl-letf (((symbol-function 'face-foreground)
                     (lambda (face &rest args)
                       (if (eq face 'chomp-color-1)
                           color
                         (apply original-face-foreground face args))))
                    ((symbol-function 'chomp-render-full-reset)
                     (lambda (render)
                       (cl-incf resets)
                       (push (and (cl-every #'null
                                           chomp-render--indexed-color-cache)
                                  (zerop (hash-table-count
                                          chomp-render--attr-face-cache)))
                             cleared)
                       (funcall original-reset render))))
            ;; Seed both caches with the old theme color.
            (should (equal "#111111" (chomp-render--color-to-string 1)))
            (chomp-render--attr-to-face (make-chomp-attr :fg 1))
            (should (> (hash-table-count chomp-render--attr-face-cache) 0))
            (setq color "#222222")
            ;; Invoke the Emacs 29+ hook callback.
            (chomp-render--theme-changed 'test-theme)
            (should (= 2 resets))
            (should (memq t cleared))
            (should (equal "#222222" (chomp-render--color-to-string 1)))
            ;; Exercise the Emacs 28 load-theme advice callback too.
            (setq color "#333333" cleared nil)
            (chomp-render--after-load-theme)
            (should (= 4 resets))
            (should (memq t cleared))
            (should (equal "#333333" (chomp-render--color-to-string 1)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest chomp-test-render-theme-hook-installation-paths ()
  "Theme invalidation installs the modern hook and Emacs 28 advice fallback."
  (let ((original-boundp (symbol-function 'boundp))
        hook advice)
    (cl-letf (((symbol-function 'boundp)
               (lambda (symbol)
                 (if (eq symbol 'enable-theme-functions)
                     t
                   (funcall original-boundp symbol))))
              ((symbol-function 'add-hook)
               (lambda (symbol function &rest _)
                 (setq hook (list symbol function)))))
      (chomp-render--install-theme-invalidation))
    (should (equal '(enable-theme-functions chomp-render--theme-changed)
                   hook))
    (cl-letf (((symbol-function 'boundp)
               (lambda (symbol)
                 (if (eq symbol 'enable-theme-functions)
                     nil
                   (funcall original-boundp symbol))))
              ((symbol-function 'advice-member-p) (lambda (&rest _) nil))
              ((symbol-function 'advice-add)
               (lambda (symbol where function &rest _)
                 (setq advice (list symbol where function)))))
      (chomp-render--install-theme-invalidation))
    (should (equal '(load-theme :after chomp-render--after-load-theme)
                   advice))))

(ert-deftest chomp-test-render-attr-to-face ()
  "Attribute to face conversion works."
  ;; Default attr -> nil
  (should (null (chomp-render--attr-to-face nil)))
  (should (null (chomp-render--attr-to-face (make-chomp-attr))))
  ;; Bold/italic/font compose via :inherit named faces
  (let ((face (chomp-render--attr-to-face (make-chomp-attr :bold t))))
    (should (equal '(chomp-bold) (plist-get face :inherit))))
  (let ((face (chomp-render--attr-to-face
               (make-chomp-attr :bold t :italic t :font 3))))
    (should (equal '(chomp-bold chomp-italic chomp-font-3)
                   (plist-get face :inherit))))
  ;; Foreground color
  (let ((face (chomp-render--attr-to-face (make-chomp-attr :fg 1))))
    (should (equal (chomp-render--color-to-string 1)
                   (plist-get face :foreground))))
  ;; Underline styles map to Emacs underline styles
  (let ((face (chomp-render--attr-to-face
               (make-chomp-attr :underline 'double))))
    (should (equal '(:style double-line) (plist-get face :underline))))
  (let ((face (chomp-render--attr-to-face
               (make-chomp-attr :underline 'dotted :ul-color 1))))
    (should (eq 'dots (plist-get (plist-get face :underline) :style)))
    (should (equal (chomp-render--color-to-string 1)
                   (plist-get (plist-get face :underline) :color)))))

;;;; ---- Phase 2 Tests --------------------------------------------------

(ert-deftest chomp-test-csi-j-k-aliases ()
  "CSI j (CUB alias) and CSI k (CUU alias) work."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; Move to (5, 3)
    (chomp-test-output parser "\e[4;6H")
    (should (equal '(5 . 3) (chomp-test-cursor screen)))
    ;; CSI k = cursor up 1
    (chomp-test-output parser "\e[k")
    (should (equal '(5 . 2) (chomp-test-cursor screen)))
    ;; CSI 2j = cursor left 2
    (chomp-test-output parser "\e[2j")
    (should (equal '(3 . 2) (chomp-test-cursor screen)))))

(ert-deftest chomp-test-sgr-underline-sub-params ()
  "SGR 4:N sub-parameters set correct underline styles."
  (chomp-test-with-screen (:width 20 :height 6)
    ;; 4:0 = off
    (chomp-test-output parser "\e[4:0mA\e[0m")
    (let* ((cell (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 0))
           (attr (chomp-cell-attr cell)))
      ;; 4:0 should set underline to nil
      (should (or (null attr) (null (chomp-attr-underline attr)))))
    ;; 4:1 = line
    (chomp-test-output parser "\e[4:1mB\e[0m")
    (let* ((cell (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 1))
           (attr (chomp-cell-attr cell)))
      (should (eq 'line (chomp-attr-underline attr))))
    ;; 4:3 = curly
    (chomp-test-output parser "\e[4:3mC\e[0m")
    (let* ((cell (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 2))
           (attr (chomp-cell-attr cell)))
      (should (eq 'curly (chomp-attr-underline attr))))
    ;; 4:5 = dashed
    (chomp-test-output parser "\e[4:5mD\e[0m")
    (let* ((cell (aref (chomp-line-cells (chomp-screen-get-line screen 0)) 3))
           (attr (chomp-cell-attr cell)))
      (should (eq 'dashed (chomp-attr-underline attr))))))

(ert-deftest chomp-test-double-width-char ()
  "Double-width (CJK) characters occupy two cells."
  (chomp-test-with-screen (:width 10 :height 3)
    ;; Write a CJK character (U+4E2D, width=2)
    (chomp-screen-write-char screen #x4E2D)
    ;; Should be at cell 0 with width 2
    (let* ((line (chomp-screen-get-line screen 0))
           (cell0 (aref (chomp-line-cells line) 0))
           (cell1 (aref (chomp-line-cells line) 1)))
      (should (= #x4E2D (chomp-cell-char cell0)))
      (should (= 2 (chomp-cell-width cell0)))
      (should (= 0 (chomp-cell-width cell1))))  ; continuation cell
    ;; Cursor should have advanced by 2
    (should (= 2 (chomp-screen-cursor-x screen)))))

(ert-deftest chomp-test-double-width-at-eol ()
  "Double-width char at last column wraps correctly."
  (chomp-test-with-screen (:width 5 :height 3)
    ;; Fill to column 4 (last col, 0-indexed)
    (chomp-test-output parser "XXXX")
    (should (= 4 (chomp-screen-cursor-x screen)))
    ;; Write a double-width char -- doesn't fit at col 4, should wrap
    (chomp-screen-write-char screen #x4E2D)
    ;; Should be on line 1 at col 2
    (should (= 1 (chomp-screen-cursor-y screen)))
    (should (= 2 (chomp-screen-cursor-x screen)))))

(ert-deftest chomp-test-cursor-style ()
  "Cursor style is set by DECSCUSR."
  (chomp-test-with-screen (:width 10 :height 3)
    ;; Default is :block
    (should (eq :block (chomp-screen-cursor-style screen)))
    ;; Set to bar (DECSCUSR 5 = blinking bar, 6 = steady bar)
    (chomp-test-output parser "\e[6 q")
    (should (eq :bar (chomp-screen-cursor-style screen)))
    ;; Set to underline
    (chomp-test-output parser "\e[4 q")
    (should (eq :underline (chomp-screen-cursor-style screen)))
    ;; Set to blinking block
    (chomp-test-output parser "\e[1 q")
    (should (eq :blinking-block (chomp-screen-cursor-style screen)))))

(ert-deftest chomp-test-cursor-blink-mode-12 ()
  "DECSET/DECRST 12 toggles cursor blink."
  (chomp-test-with-screen (:width 10 :height 3)
    (should (null (chomp-screen-cursor-blink screen)))
    ;; DECSET 12
    (chomp-test-output parser "\e[?12h")
    (should (chomp-screen-cursor-blink screen))
    ;; DECRST 12
    (chomp-test-output parser "\e[?12l")
    (should (null (chomp-screen-cursor-blink screen)))))

(ert-deftest chomp-test-parse-sub-params-preserved ()
  "Colon-separated sub-parameters are parsed as lists."
  (let ((params (chomp-parse--parse-params "4:3;1;38:2:255:128:0")))
    ;; First param: (4 3)
    (should (equal '(4 3) (aref params 0)))
    ;; Second param: 1
    (should (= 1 (aref params 1)))
    ;; Third param: (38 2 255 128 0)
    (should (equal '(38 2 255 128 0) (aref params 2)))))

(ert-deftest chomp-test-per-color-faces ()
  "Per-color named faces exist and return correct colors."
  ;; Face exists
  (should (facep 'chomp-color-0))
  (should (facep 'chomp-color-1))
  (should (facep 'chomp-color-255))
  ;; 0-15 inherit Emacs ansi-color faces
  (should (eq 'ansi-color-black
              (face-attribute 'chomp-color-0 :inherit nil 'default)))
  (should (eq 'ansi-color-bright-red
              (face-attribute 'chomp-color-9 :inherit nil 'default)))
  ;; Color lookup returns a string
  (should (stringp (chomp-render--color-to-string 0)))
  (should (stringp (chomp-render--color-to-string 196))))

(ert-deftest chomp-test-inverse-attr-face ()
  "Inverse attribute uses default face colors."
  (let ((face (chomp-render--attr-to-face (make-chomp-attr :inverse t))))
    ;; Should have both :foreground and :background set
    (should (plist-get face :foreground))
    (should (plist-get face :background))
    ;; They should be swapped relative to each other
    (let ((fg (plist-get face :foreground))
          (bg (plist-get face :background)))
      ;; fg should be the default bg, bg should be the default fg
      ;; Just verify they're different (unless the theme has same fg/bg)
      (should (stringp fg))
      (should (stringp bg)))))

(ert-deftest chomp-test-inverse-attr-with-fg ()
  "Inverse with explicit fg swaps correctly."
  (let* ((red (chomp-render--color-to-string 1))
         (face (chomp-render--attr-to-face
                (make-chomp-attr :inverse t :fg 1))))
    ;; fg was palette 1; inverse means it becomes the background
    (should (plist-get face :background))
    (should (equal red (plist-get face :background)))))

(ert-deftest chomp-test-xtsmgraphics-response ()
  "XTSMGRAPHICS query gets a valid response."
  (chomp-test-with-screen (:width 80 :height 24)
    (let ((responses nil))
      (setf (chomp-parser-write-fn parser)
            (lambda (s) (push s responses)))
      ;; Query color register count: CSI ? 1 ; 1 S
      (chomp-test-output parser "\e[?1;1S")
      (should (car responses))
      (should (string-match-p "\\`\e\\[\\?1;0;256S\\'" (car responses)))
      ;; Query graphics geometry: CSI ? 2 ; 1 S
      (setq responses nil)
      (chomp-test-output parser "\e[?2;1S")
      (should (car responses))
      (should (string-match-p "\\`\e\\[\\?2;0;" (car responses))))))

(ert-deftest chomp-test-osc-color-query ()
  "OSC 10/11 ? query returns rgb: format."
  (chomp-test-with-screen (:width 20 :height 6)
    (let ((responses nil))
      (setf (chomp-parser-write-fn parser)
            (lambda (s) (push s responses)))
      ;; Query foreground
      (chomp-test-output parser "\e]10;?\a")
      (should (car responses))
      (should (string-match-p "rgb:" (car responses)))
      ;; Query background
      (setq responses nil)
      (chomp-test-output parser "\e]11;?\a")
      (should (car responses))
      (should (string-match-p "rgb:" (car responses))))))

(ert-deftest chomp-test-osc-4-palette-query ()
  "OSC 4 palette queries return xterm rgb replies."
  (chomp-test-with-screen (:width 20 :height 6)
    (let ((responses nil))
      (setf (chomp-parser-write-fn parser)
            (lambda (s) (push s responses)))
      (chomp-test-output parser "\e]4;1;?\a")
      (should (equal "\e]4;1;rgb:cdcd/0000/0000\e\\" (car responses)))
      (setq responses nil)
      (chomp-test-output parser "\e]4;0;?;15;?\a")
      (should (= 2 (length responses)))
      (should (member "\e]4;0;rgb:0000/0000/0000\e\\" responses))
      (should (member "\e]4;15;rgb:ffff/ffff/ffff\e\\" responses)))))

(ert-deftest chomp-test-osc-52-disabled ()
  "OSC 52 has no clipboard or event side effects by default."
  (should-not (default-value 'chomp-enable-osc52))
  (let* ((screen (chomp-screen-create 20 6))
         responses events clipboard-calls kill-calls
         (kill-ring '("keep"))
         (parser
          (chomp-parse-create
           screen
           (lambda (response) (push response responses))
           (lambda (type &rest args) (push (cons type args) events)))))
    (cl-letf (((symbol-function 'gui-get-selection)
               (lambda (&rest _)
                 (push 'get clipboard-calls)
                 "clipboard"))
              ((symbol-function 'gui-set-selection)
               (lambda (&rest args) (push args clipboard-calls)))
              ((symbol-function 'current-kill)
               (lambda (&rest _)
                 (push 'get kill-calls)
                 "kill"))
              ((symbol-function 'kill-new)
               (lambda (&rest args) (push args kill-calls))))
      (let ((chomp-enable-osc52 nil))
        (chomp-test-output parser "\e]52;c;?\a")
        (chomp-test-output
         parser (concat "\e]52;c;" (base64-encode-string "set" t) "\a"))
        (chomp-test-output parser "\e]52;c;\a")))
    (should-not responses)
    (should-not events)
    (should-not clipboard-calls)
    (should-not kill-calls)
    (should (equal '("keep") kill-ring))))

(ert-deftest chomp-test-osc-52-enabled ()
  "Opting in restores OSC 52 query and set behavior."
  (let* ((screen (chomp-screen-create 20 6))
         responses events selections kills
         (parser
          (chomp-parse-create
           screen
           (lambda (response) (push response responses))
           (lambda (type &rest args) (push (cons type args) events)))))
    (cl-letf (((symbol-function 'gui-get-selection)
               (lambda (&rest _) "clipboard"))
              ((symbol-function 'gui-set-selection)
               (lambda (selection value)
                 (push (list selection value) selections)))
              ((symbol-function 'kill-new)
               (lambda (text &optional _replace) (push text kills))))
      (let ((chomp-enable-osc52 t))
        (chomp-test-output parser "\e]52;c;?\a")
        (should (equal (format "\e]52;c;%s\e\\"
                               (base64-encode-string "clipboard" t))
                       (car responses)))
        (chomp-test-output
         parser (concat "\e]52;c;" (base64-encode-string "set" t) "\a"))))
    (should (equal '("set") kills))
    (should (equal '((CLIPBOARD "set")) selections))
    (should (equal '((clipboard "set")) events))))

(ert-deftest chomp-test-input-backspace-variants ()
  "Input translation handles backspace modifier variants."
  (let ((screen (chomp-screen-create 80 24)))
    ;; Plain backspace
    (should (equal "\x7f" (chomp-input-translate 'backspace screen)))
    ;; M-backspace
    (should (equal "\e\x7f" (chomp-input-translate
                              (event-convert-list '(meta backspace)) screen)))
    ;; C-backspace
    (should (equal "\x08" (chomp-input-translate
                            (event-convert-list '(control backspace)) screen)))
    ;; Eat treats DEL as ordinary backspace input.
    (should (equal "\x7f" (chomp-input-translate ?\C-? screen)))))

(ert-deftest chomp-test-input-function-keys-extended ()
  "F21+ function keys produce escape sequences."
  (let ((screen (chomp-screen-create 80 24)))
    ;; F21
    (should (chomp-input-translate 'f21 screen))
    ;; F36
    (should (chomp-input-translate 'f36 screen))
    ;; F63
    (should (chomp-input-translate 'f63 screen))))

(ert-deftest chomp-test-input-deletechar ()
  "deletechar key translates to ESC[3~."
  (let ((screen (chomp-screen-create 80 24)))
    (should (equal "\e[3~" (chomp-input-translate 'deletechar screen)))))

(ert-deftest chomp-test-send-password ()
  "Password command sends the password followed by return."
  (let (sent)
    (with-temp-buffer
      (setq-local chomp--io 'fake)
      (cl-letf (((symbol-function 'chomp-io-send)
                 (lambda (_io s) (push s sent))))
        (chomp-send-password "secret")))
    (should (equal '("\r" "secret") sent))))

(ert-deftest chomp-test-public-input-api ()
  "Public input commands preserve strings and encode keys and paste mode."
  (let (sent)
    (with-temp-buffer
      (setq major-mode 'chomp-mode)
      (setq-local chomp--screen (chomp-screen-create 10 3))
      (setq-local chomp--io (make-chomp-io :process 'fake))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'chomp-io-send)
                 (lambda (_io string) (push string sent))))
        (chomp-send-string (unibyte-string 0 255 ?x))
        (chomp-send-key "up")
        (chomp-send-key "up" "control, shift")
        (chomp-send-key "c" "ctrl")
        (chomp-paste-string "plain")
        (setf (chomp-screen-bracketed-paste chomp--screen) t)
        (chomp-paste-string "bracketed")))
    (should (equal (list "\e[200~bracketed\e[201~" "plain" "\C-c"
                         "\e[1;6A" "\e[A" (unibyte-string 0 255 ?x))
                   sent))))

(ert-deftest chomp-test-public-input-api-requires-running-terminal ()
  "Public input commands reject non-terminal and stopped terminal buffers."
  (dolist (function '(chomp-send-string chomp-paste-string))
    (with-temp-buffer
      (should-error (funcall function "x") :type 'user-error)
      (setq major-mode 'chomp-mode)
      (should-error (funcall function "x") :type 'user-error)))
  (with-temp-buffer
    (should-error (chomp-send-key "up") :type 'user-error)
    (setq major-mode 'chomp-mode)
    (should-error (chomp-send-key "up") :type 'user-error)))

(ert-deftest chomp-test-io-send-preserves-bytes ()
  "Process writes preserve unibyte data and UTF-8 encode text."
  (let ((io (make-chomp-io :process 'fake))
        sent)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string)
               (lambda (_process string) (push string sent))))
      (chomp-io-send io (unibyte-string 0 255 ?x))
      (chomp-io-send io "λ"))
    (should (equal (encode-coding-string "λ" 'utf-8 t) (car sent)))
    (should (equal (unibyte-string 0 255 ?x) (cadr sent)))))

(ert-deftest chomp-test-io-process-uses-binary-writes ()
  "PTY output is decoded as UTF-8 while input remains byte-preserving."
  (let* ((screen (chomp-screen-create 10 3))
         (parser (chomp-parse-create screen))
         (io (make-chomp-io :screen screen :parser parser))
         coding)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq coding (plist-get args :coding))
                 'fake))
              ((symbol-function 'set-process-window-size) #'ignore))
      (let ((chomp-enable-shell-integration nil))
        (chomp-io-start io "/bin/sh" (current-buffer))))
    (should (equal '(utf-8-unix . no-conversion) coding))))

(ert-deftest chomp-test-io-list-command-keeps-shell-integration ()
  "Shell argv from the program prompt retains startup integration."
  (let* ((dir (make-temp-file "chomp-integration-" t))
         (script (expand-file-name "bash" dir))
         (env (list (concat "CHOMP_SHELL_INTEGRATION_DIR=" dir)))
         command)
    (unwind-protect
        (progn
          (with-temp-file script (insert "# integration\n"))
          (let ((chomp-enable-shell-integration t))
            (setq command
                  (chomp-io--build-command '("/bin/bash" "-i") env)))
          (should (equal "/bin/bash" (nth 0 command)))
          (should (equal "--rcfile" (nth 1 command)))
          (should (file-exists-p (nth 2 command)))
          (should (equal "-i" (nth 3 command))))
      (when (and command (nth 2 command))
        (delete-file (nth 2 command)))
      (delete-directory dir t))))

(ert-deftest chomp-test-io-zsh-bootstrap-environment ()
  "Zsh integration prepends bootstrap environment without changing argv."
  (let* ((dir (make-temp-file "chomp-zsh-integration-" t))
         (script (expand-file-name "zsh" dir))
         (env (list (concat "CHOMP_SHELL_INTEGRATION_DIR=" dir)))
         (command '("/bin/zsh" "-i" "--no-rcs")))
    (unwind-protect
        (progn
          (with-temp-file script (insert "# integration\n"))
          (let ((process-environment '("ZDOTDIR=/user/zsh"))
                (chomp-enable-shell-integration t))
            (let ((prepared (chomp-io--prepare-environment command env)))
              (should (equal (concat "ZDOTDIR="
                                     (expand-file-name "zsh-bootstrap" dir))
                             (car prepared)))
              (should (equal "CHOMP_ZSH_ZDOTDIR=/user/zsh"
                             (cadr prepared)))
              (should (equal "CHOMP_ZSH_ZDOTDIR_SET=1"
                             (nth 2 prepared)))
              (should (equal command (chomp-io--build-command command env)))))
          (let ((process-environment '("ZDOTDIR="))
                (chomp-enable-shell-integration t))
            (let ((prepared (chomp-io--prepare-environment command env)))
              (should (equal "CHOMP_ZSH_ZDOTDIR=" (cadr prepared)))
              (should (equal "CHOMP_ZSH_ZDOTDIR_SET=1"
                             (nth 2 prepared)))))
          (let ((process-environment nil)
                (chomp-enable-shell-integration t))
            (should (equal "CHOMP_ZSH_ZDOTDIR_SET=0"
                           (nth 2 (chomp-io--prepare-environment command env)))))
          (let ((chomp-enable-shell-integration nil))
            (should (equal env (chomp-io--prepare-environment command env)))
            (should (equal command (chomp-io--build-command command env)))))
      (delete-directory dir t))))

(ert-deftest chomp-test-io-zsh-bootstrap-subprocess ()
  "Bundled zsh bootstrap sources user startup files and emits OSC 51."
  (skip-unless (executable-find "zsh"))
  (let* ((home (make-temp-file "chomp-zsh-home-" t))
         (zsh (executable-find "zsh"))
         (default-directory (file-name-as-directory
                             (file-truename default-directory)))
         (process-environment (copy-sequence process-environment))
         (extra-env (chomp-shell-env-vars)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".zshenv" home)
            (insert "print USER_ZSHENV\n"))
          (with-temp-file (expand-file-name ".zshrc" home)
            (insert "print USER_ZSHRC\nPS1='USER> '\n"))
          (setenv "HOME" home)
          (setenv "TERM" "eat-truecolor")
          (setenv "ZDOTDIR" nil)
          (setq process-environment
                (append (chomp-io--prepare-environment
                         (list zsh "-i") extra-env)
                        process-environment))
          (with-temp-buffer
            (insert "exit\n")
            (let ((status (call-process-region
                           (point-min) (point-max) zsh t t nil "-i")))
              (should (zerop status)))
            (should (string-match-p "USER_ZSHENV" (buffer-string)))
            (should (string-match-p "USER_ZSHRC" (buffer-string)))
            (should (string-match-p "\e]51;e;B" (buffer-string)))
            (should (string-match-p "\e]51;e;C" (buffer-string)))))
      (delete-directory home t))))

(ert-deftest chomp-test-io-command-sets-stty-sane ()
  "PTY command wrapper initializes terminal settings like Eat."
  (let ((cmd (chomp-io--wrap-command-with-stty '("/bin/sh") 24 80)))
    (should (equal '("/usr/bin/env" "sh" "-c")
                   (list (nth 0 cmd) (nth 1 cmd) (nth 2 cmd))))
    (should (string-match-p "stty .* rows 24 columns 80 sane" (nth 3 cmd)))
    (should (equal '(".." "/bin/sh") (nthcdr 4 cmd)))))

(defun chomp-test--mouse-event (type point)
  "Return a synthetic mouse event of TYPE at buffer POINT."
  (list type (list nil point '(0 . 0) 0 nil point '(0 . 0) nil)))

(ert-deftest chomp-test-mouse-wheel-left-right ()
  "Mouse wheel-left/right produce correct button codes."
  (let ((screen (chomp-screen-create 80 24)))
    (should (= 66 (chomp-input--mouse-button
                   (chomp-test--mouse-event 'wheel-left 1) screen)))
    (should (= 67 (chomp-input--mouse-button
                   (chomp-test--mouse-event 'wheel-right 1) screen)))))

(ert-deftest chomp-test-mouse-keymap-bindings ()
  "DEC mouse keymap forwards press, release, drag, and movement events."
  (should (eq (lookup-key chomp-mouse-mode-map [down-mouse-1])
              #'chomp-mouse-input))
  (should (eq (lookup-key chomp-mouse-mode-map [drag-mouse-1])
              #'chomp-mouse-input))
  (should (eq (lookup-key chomp-mouse-mode-map [mouse-1])
              #'chomp-mouse-input))
  (should (eq (lookup-key chomp-mouse-mode-map [mouse-movement])
              #'chomp-mouse-input)))

(ert-deftest chomp-test-mouse-sgr-encoding ()
  "Mouse events encode as SGR mouse reports relative to display-begin."
  (let ((screen (chomp-screen-create 10 3)))
    (setf (chomp-screen-mouse-mode screen) 'button-event)
    (setf (chomp-screen-mouse-sgr screen) t)
    (with-temp-buffer
      (insert "scrollback\nabcdefghij\nklmnopqrst\nuvwxyz    \n")
      (let ((display-begin (save-excursion
                             (goto-char (point-min))
                             (forward-line 1)
                             (point))))
        ;; Button 1 press at display row 0, column 1.
        (should (equal "\e[<0;2;1M"
                       (chomp-input-encode-mouse
                        (chomp-test--mouse-event 'down-mouse-1
                                                 (1+ display-begin))
                        screen display-begin)))
        ;; Movement while button 1 is pressed reports button+32.
        (should (equal "\e[<32;3;1M"
                       (chomp-input-encode-mouse
                        (chomp-test--mouse-event 'mouse-movement
                                                 (+ display-begin 2))
                        screen display-begin)))
        ;; Release clears the pressed-button state.
        (should (equal "\e[<0;2;1m"
                       (chomp-input-encode-mouse
                        (chomp-test--mouse-event 'mouse-1 (1+ display-begin))
                        screen display-begin)))
        (should-not (chomp-screen-mouse-pressed screen))
        ;; Clicks in scrollback above the display are not sent to TUIs.
        (should-not (chomp-input-encode-mouse
                     (chomp-test--mouse-event 'down-mouse-1 (point-min))
                     screen display-begin))))))

(ert-deftest chomp-test-render-cursor-type-mapping ()
  "Cursor style maps to correct Emacs cursor-type."
  (should (eq 'box (chomp-render--cursor-type-for-style :block)))
  (should (eq 'box (chomp-render--cursor-type-for-style :blinking-block)))
  (should (equal '(bar . 2) (chomp-render--cursor-type-for-style :bar)))
  (should (equal '(hbar . 2) (chomp-render--cursor-type-for-style :underline))))

(ert-deftest chomp-test-render-cursor-blink-check ()
  "Cursor blink detection works."
  (should (chomp-render--cursor-blink-p :blinking-block))
  (should (chomp-render--cursor-blink-p :blinking-bar))
  (should (chomp-render--cursor-blink-p :blinking-underline))
  (should-not (chomp-render--cursor-blink-p :block))
  (should-not (chomp-render--cursor-blink-p :bar))
  (should-not (chomp-render--cursor-blink-p :underline)))

(ert-deftest chomp-test-render-cursor-only-refresh ()
  "Pure cursor movement updates the rendered cursor overlay."
  (let* ((screen (chomp-screen-create 5 3))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (let ((render (chomp-render-create screen (current-buffer))))
        (chomp-render-refresh render)
        (let ((before (overlay-start (chomp-render-state-cursor-overlay render))))
          (chomp-test-output parser "\e[2;3H")
          (chomp-render-refresh render)
          (let ((after (overlay-start (chomp-render-state-cursor-overlay render))))
            (should (/= before after))
            (save-excursion
              (goto-char (chomp-render-state-display-begin render))
              (forward-line 1)
              (should (= after (+ (point) 2))))))))))

(ert-deftest chomp-test-render-keeps-display-begin-at-viewport-start ()
  "Updating row zero does not move the scrollback/display boundary."
  (let* ((screen (chomp-screen-create 6 4))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (let ((render (chomp-render-create screen (current-buffer))))
        (chomp-render-refresh render)
        (chomp-test-output parser "prompt")
        (chomp-render-refresh render)
        (should (= (point-min)
                   (marker-position
                    (chomp-render-state-display-begin render))))
        ;; A full-screen program must own every viewport row even when the
        ;; shell cursor was previously in the middle of the screen.
        (chomp-test-output
         parser
         "\e[3;1Hmiddle\e[?1049h\e[1;1HAAAAA\e[2;1HBBBBB\e[3;1HCCCCC\e[4;1HDDDDD")
        (chomp-render-refresh render)
        (should (= (point-min)
                   (marker-position
                    (chomp-render-state-display-begin render))))
        (should (equal '("AAAAA" "BBBBB" "CCCCC" "DDDDD")
                       (chomp-test-display-text screen)))
        (should (equal "AAAAA \nBBBBB \nCCCCC \nDDDDD "
                       (buffer-string)))))))

(ert-deftest chomp-test-render-emacs-mode-preserves-view ()
  "Emacs input mode preserves the reading view while the cursor advances."
  (let* ((screen (chomp-screen-create 20 40))
         (parser (chomp-parse-create screen))
         (buffer (generate-new-buffer " *chomp-test-view*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (let ((render (chomp-render-create screen buffer)))
            (chomp-test-output parser "\e[11;1H0123456789")
            (chomp-render-refresh render)
            (setq-local chomp--input-mode 'emacs)
            (goto-char (point-min))
            (forward-line 10)
            (forward-char 3)
            (set-mark (save-excursion (forward-line 2) (point)))
            (setq mark-active t)
            (set-window-start (selected-window)
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line 5)
                                (point)) t)
            (let ((saved-point (point))
                  (saved-point-line (line-number-at-pos))
                  (saved-point-column (current-column))
                  (saved-mark (mark))
                  (saved-mark-line (line-number-at-pos (mark)))
                  (saved-start (window-start))
                  (saved-start-line (line-number-at-pos (window-start)))
                  (saved-char (char-after))
                  (old-cursor (overlay-start
                               (chomp-render-state-cursor-overlay render))))
              ;; Replacing the row containing point must not collapse point or mark.
              (chomp-test-output parser "\e[11;1Habc")
              (chomp-render-refresh render)
              (should (/= old-cursor
                          (overlay-start
                           (chomp-render-state-cursor-overlay render))))
              (should (= saved-point (point)))
              (should (= saved-mark (mark)))
              (should mark-active)
              (should (= saved-start (window-start)))
              (should (= saved-char (char-after)))
              ;; A width change must preserve line/column, not a raw offset.
              (chomp-screen-resize screen 25 40)
              (chomp-render-full-reset render)
              (should (= saved-point-line (line-number-at-pos)))
              (should (= saved-point-column (current-column)))
              (should (= saved-mark-line (line-number-at-pos (mark))))
              (should mark-active)
              (should (= saved-start-line
                         (line-number-at-pos (window-start))))
              (should (= saved-char (char-after))))
            (setq-local chomp--input-mode 'semi-char)
            (chomp-test-output parser "\e[26;4H")
            (chomp-render-refresh render)
            (should (= (point)
                       (overlay-start
                        (chomp-render-state-cursor-overlay render))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest chomp-test-render-scrollback-clear-reconciles-buffer ()
  "ED 3 clears both model scrollback and rendered scrollback."
  (let* ((screen (chomp-screen-create 3 2))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (let ((render (chomp-render-create screen (current-buffer))))
        (chomp-test-output parser "A\e[2;1HB")
        (chomp-screen-index screen)
        (chomp-render-refresh render)
        (should (= 1 (chomp-render-state-scrollback-count render)))
        (should (string-prefix-p "A  \n" (buffer-string)))
        (chomp-screen-erase-in-display screen 3)
        (chomp-render-refresh render)
        (should (= 0 (chomp-render-state-scrollback-count render)))
        (should-not (string-prefix-p "A  \n" (buffer-string)))))))

(ert-deftest chomp-test-io-filter-queues-chunks-without-concat ()
  "Process filter appends chunks instead of growing one pending string."
  (let ((io (make-chomp-io)))
    (chomp-io--filter io nil "abc")
    (chomp-io--filter io nil "def")
    (when (chomp-io-render-timer io)
      (cancel-timer (chomp-io-render-timer io)))
    (should (equal '("abc" "def") (chomp-io-pending-chunks io)))
    (should (= 0 (chomp-io-pending-offset io)))))

(ert-deftest chomp-test-io-binary-flood-does-not-drop-output ()
  "Flood mode still parses pending terminal output."
  (let* ((screen (chomp-screen-create 10 2))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (let* ((render (chomp-render-create screen (current-buffer)))
             (io (make-chomp-io :screen screen :parser parser :render render
                                :buffer (current-buffer) :chunk-size 10)))
        ;; Force the next chunk over the flood threshold, then ensure parsing
        ;; still happens instead of skipping bytes.
        (setf (chomp-io-throughput-time io) (float-time))
        (setf (chomp-io-throughput-bytes io) 1048576)
        (chomp-io--enqueue-output io "OK")
        (chomp-io--process-pending io t)
        (should (chomp-io-binary-flood io))
        (should (equal "OK" (chomp-test-display-line screen 0)))))))

(ert-deftest chomp-test-io-sentinel-emits-in-terminal-buffer ()
  "Process exit events run with the terminal buffer current."
  (let* ((screen (chomp-screen-create 5 2))
         (target-buffer (generate-new-buffer " *chomp-test-sentinel*"))
         (seen-buffer nil))
    (unwind-protect
        (let* ((parser (chomp-parse-create
                        screen nil
                        (lambda (_type &rest _args)
                          (setq seen-buffer (current-buffer)))))
               (io (make-chomp-io :screen screen :parser parser
                                  :buffer target-buffer)))
          (with-temp-buffer
            (chomp-io--sentinel io nil "finished\n"))
          (should (eq seen-buffer target-buffer)))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(ert-deftest chomp-test-interactive-program-arguments ()
  "Interactive program input is split into argv without changing API input."
  (cl-labels
      ((start (program interactive)
         (let (buffer started)
           (unwind-protect
               (save-window-excursion
                 (cl-letf (((symbol-function 'read-shell-command)
                            (lambda (&rest _) program))
                           ((symbol-function 'chomp-io-start)
                            (lambda (_io command _buffer &optional _env)
                              (setq started command))))
                   (setq buffer
                         (if interactive
                             (let ((current-prefix-arg '(16)))
                               (call-interactively #'chomp))
                           (chomp program)))
                   started))
             (when (buffer-live-p buffer)
               (kill-buffer buffer))))))
    (should (equal '("htop" "-d" "2")
                   (start "htop -d 2" t)))
    (should (equal '("printf" "%s %s" "a" "b")
                   (start "printf '%s %s' a b" t)))
    (should-error (start "   " t) :type 'user-error)
    (should-error (start "''" t) :type 'user-error)
    (should (equal "/tmp/program with spaces"
                   (start "/tmp/program with spaces" nil)))
    (should (equal '("printf" "%s" "unchanged")
                   (start '("printf" "%s" "unchanged") nil)))))

;;;; ---- Notification Handler Tests ------------------------------------

(ert-deftest chomp-test-notification-callback-deferred-in-origin-buffer ()
  "Notification callbacks are deferred in their terminal buffer."
  (let ((origin (generate-new-buffer " *chomp-notify-origin*"))
        (other (generate-new-buffer " *chomp-notify-other*"))
        timer seen)
    (unwind-protect
        (with-current-buffer origin
          (let ((chomp-notification-function
                 (lambda (title body)
                   (setq seen (list (current-buffer) title body)))))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest args)
                         (setq timer (cons function args)))))
              (chomp--handle-event 'notification "title" "body")))
          (should-not seen)
          (with-current-buffer other
            (apply (car timer) (cdr timer)))
          (should (equal (list origin "title" "body") seen)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list origin other)))))

(ert-deftest chomp-test-notification-and-progress-callback-errors-isolated ()
  "Terminal callback failures do not escape event handling."
  (with-temp-buffer
    (let ((chomp-notification-function (lambda (&rest _) (error "notify")))
          (chomp-progress-function (lambda (&rest _) (error "progress"))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (apply function args))))
        (should-not (chomp--handle-event 'notification nil "body"))
        (should-not (chomp--handle-event 'progress 'set 10))))))

(ert-deftest chomp-test-progress-callback-and-mode-line-compose ()
  "Progress callbacks receive normalized args and default display composes."
  (with-temp-buffer
    (chomp-mode)
    (chomp-semi-char-mode)
    (let* (seen
           (chomp-progress-function
            (lambda (state percent) (setq seen (list state percent)))))
      (chomp--handle-event 'progress 'pause 25)
      (should (equal '(pause 25) seen)))
    (let ((chomp-progress-function #'chomp--default-progress))
      (chomp--handle-event 'progress 'set 80)
      (should (equal "[Semi] [80%]" (chomp--mode-line-input-mode)))
      (chomp--handle-event 'progress 'remove 0)
      (should (equal "[Semi]" (chomp--mode-line-input-mode))))))

;;;; ---- Mode Line Tests ------------------------------------------------

(ert-deftest chomp-test-mode-line-input-mode-installed ()
  "Chomp mode keeps its major-mode name and updates the input-mode label."
  (with-temp-buffer
    (chomp-mode)
    (should (equal "Chomp" mode-name))
    (should (equal '(" " (:eval (chomp--mode-line-input-mode)))
                   mode-line-process))
    (chomp-semi-char-mode)
    (should (equal "[Semi]" (chomp--mode-line-input-mode)))
    (should (equal "Chomp" mode-name))
    (chomp-char-mode)
    (should (equal "[Char]" (chomp--mode-line-input-mode)))
    (should (equal "Chomp" mode-name))
    (chomp-emacs-mode)
    (should (equal "[Emacs]" (chomp--mode-line-input-mode)))
    (should (equal "Chomp" mode-name))))

;;;; ---- Clear and Copy Tests -------------------------------------------

(ert-deftest chomp-test-clear-and-clear-scrollback ()
  "Clear commands erase the viewport, optionally history, and redraw prompt."
  (dolist (clear '((chomp-clear . nil) (chomp-clear-scrollback . t)))
    (let* ((screen (chomp-screen-create 5 2))
           (parser (chomp-parse-create screen))
           redraw)
      (chomp-test-output parser "abc\r\ndef")
      (setf (chomp-screen-scrollback screen)
            (list (make-chomp-line :text "old  " :cells-valid nil)))
      (setf (chomp-screen-scrollback-length screen) 1)
      (with-temp-buffer
        (insert "x")
        (setq major-mode 'chomp-mode)
        (setq-local chomp--screen screen)
        (setq-local chomp--io (make-chomp-io :process 'fake))
        (setq-local chomp-enable-shell-prompt-annotation nil)
        (setq-local left-margin-width 1)
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'before-string "0")
          (setq-local chomp-shell--prompt-overlays (list overlay))
          (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'chomp-send-key)
                     (lambda (key modifiers)
                       (setq redraw (list key modifiers)))))
            (funcall (car clear)))
          (should-not (overlay-buffer overlay))
          (should-not chomp-shell--prompt-overlays)
          (should-not left-margin-width)))
      (should (equal '(0 . 0) (chomp-test-cursor screen)))
      (should (equal '("" "") (chomp-test-display-text screen)))
      (should (equal '("l" "control") redraw))
      (if (cdr clear)
          (should-not (chomp-screen-scrollback screen))
        (should (= 1 (chomp-screen-scrollback-length screen)))))))

(ert-deftest chomp-test-copy-all-plain-text-soft-wraps ()
  "Copy-all trims padding and omits newlines across soft-wrapped rows."
  (let* ((screen (chomp-screen-create 4 3))
         (parser (chomp-parse-create screen))
         copied)
    (chomp-test-output parser "abcdE\r\nlast")
    (setf (chomp-screen-scrollback screen)
          (list (make-chomp-line :text "old " :cells-valid nil)))
    (setf (chomp-screen-scrollback-length screen) 1)
    (should (equal "old\nabcdE\nlast" (chomp-screen-plain-text screen)))
    (with-temp-buffer
      (setq major-mode 'chomp-mode)
      (setq-local chomp--screen screen)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _) (setq copied text))))
        (should (equal "old\nabcdE\nlast" (chomp-copy-all)))))
    (should (equal "old\nabcdE\nlast" copied))))

;;;; ---- Session Tests --------------------------------------------------

(defun chomp-test--session-buffer (name &optional identity)
  "Create a lightweight Chomp buffer named NAME with IDENTITY."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq major-mode 'chomp-mode)
      (setq-local chomp--session-id (or identity name)))
    buffer))

(ert-deftest chomp-test-buffer-list-sorted-and-filtered ()
  "Session lists are sorted and completion contains only Chomp buffers."
  (let ((z (chomp-test--session-buffer " *chomp-z*"))
        (a (chomp-test--session-buffer " *chomp-a*"))
        (ordinary (generate-new-buffer " *ordinary*")))
    (unwind-protect
        (progn
          (should (equal (list a z) (chomp--buffers)))
          (with-current-buffer a
            (let (collection default)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt choices &rest args)
                           (setq collection choices default (nth 4 args))
                           default))
                        ((symbol-function 'pop-to-buffer-same-window) #'identity))
                (should (eq z (chomp-list-buffers))))
              (should (equal '(" *chomp-a*" " *chomp-z*") collection))
              (should (equal " *chomp-z*" default)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list z a ordinary)))))

(ert-deftest chomp-test-buffer-cycle-wraps ()
  "Next and previous session navigation wrap around."
  (let ((a (chomp-test--session-buffer " *chomp-cycle-a*"))
        (b (chomp-test--session-buffer " *chomp-cycle-b*"))
        (ordinary (generate-new-buffer " *chomp-cycle-ordinary*")))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
          (with-current-buffer ordinary
            (should (eq a (chomp-next)))
            (should (eq b (chomp-previous))))
          (with-current-buffer a
            (should (eq b (chomp-next))))
          (with-current-buffer b
            (should (eq a (chomp-next))))
          (with-current-buffer a
            (should (eq b (chomp-previous)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list a b ordinary)))))

(ert-deftest chomp-test-other-reuses-or-creates ()
  "`chomp-other' chooses another session or creates one."
  (let ((a (chomp-test--session-buffer " *chomp-other-a*"))
        (b (chomp-test--session-buffer " *chomp-other-b*"))
        created)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
          (with-current-buffer a
            (should (eq b (chomp-other))))
          (kill-buffer b)
          (with-current-buffer a
            (cl-letf (((symbol-function 'chomp)
                       (lambda (&optional _) (setq created t))))
              (should (chomp-other))
              (should created))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list a b)))))

(ert-deftest chomp-test-project-reuses-renamed-buffer ()
  "Project terminals use project names and stable identities after OSC titles."
  (let* ((root (file-name-as-directory (make-temp-file "chomp-project-" t)))
         (project 'fake-project)
         buffer started name-called)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) project))
                    ((symbol-function 'project-root) (lambda (_) root))
                    ((symbol-function 'project-prefixed-buffer-name)
                     (lambda (mode) (setq name-called mode) "*fake-chomp*"))
                    ((symbol-function 'chomp-io-start)
                     (lambda (&rest _) (setq started (1+ (or started 0)))))
                    ((symbol-function 'pop-to-buffer-same-window) #'identity))
            (setq buffer (chomp-project))
            (should (equal "chomp" name-called))
            (should (equal "*fake-chomp*" (buffer-name buffer)))
            (should (equal (file-truename root)
                           (buffer-local-value 'chomp--session-id buffer)))
            (with-current-buffer buffer
              (chomp--handle-event 'title "renamed"))
            (should (eq buffer (chomp-project)))
            (should (= 1 started))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

;;;; ---- Bookmark Tests -------------------------------------------------

(ert-deftest chomp-test-bookmark-record-and-renamed-session-reuse ()
  "Bookmark records retain session metadata and reuse renamed buffers."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "chomp-bookmark-" t)))
         (buffer (generate-new-buffer "*bookmark-terminal*"))
         record)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory directory)
            (chomp-mode)
            (setq-local chomp--session-id "stable-id")
            (should (eq #'chomp-bookmark-make-record
                        bookmark-make-record-function))
            (setq record (chomp-bookmark-make-record))
            (rename-buffer "*renamed-terminal*"))
          (should (equal "*bookmark-terminal*" (car record)))
          (should (equal directory
                         (alist-get 'chomp-directory (cdr record))))
          (should (equal "*bookmark-terminal*"
                         (alist-get 'chomp-display-name (cdr record))))
          (should (equal "stable-id"
                         (alist-get 'chomp-session-id (cdr record))))
          (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
            (should (eq buffer (chomp-bookmark-jump (cdr record))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest chomp-test-bookmark-creates-missing-session-in-directory ()
  "A missing bookmarked session is recreated in its saved directory."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "chomp-bookmark-new-" t)))
         (record `((chomp-directory . ,directory)
                   (chomp-display-name . "*saved-terminal*")
                   (chomp-session-id . "missing-id")))
         created seen-directory seen-name)
    (unwind-protect
        (cl-letf (((symbol-function 'chomp)
                   (lambda (&optional _)
                     (setq seen-directory default-directory
                           seen-name chomp-buffer-name
                           created (chomp-test--session-buffer seen-name))))
                  ((symbol-function 'pop-to-buffer-same-window) #'identity))
          (let ((result (chomp-bookmark-jump record)))
            (should (eq created result)))
          (should (equal directory seen-directory))
          (should (equal "*saved-terminal*" seen-name))
          (should (equal "missing-id"
                         (buffer-local-value 'chomp--session-id created))))
      (when (buffer-live-p created) (kill-buffer created))
      (delete-directory directory t))))

(ert-deftest chomp-test-bookmark-reused-session-changes-directory ()
  "Reusing a local session sends a quoted cd and Return via public input."
  (let* ((old-directory (file-name-as-directory
                         (make-temp-file "chomp-bookmark-old-" t)))
         (directory (file-name-as-directory
                     (make-temp-file "chomp bookmark new-" t)))
         (buffer (chomp-test--session-buffer "*bookmark-reuse*" "reuse-id"))
         (record `((chomp-directory . ,directory)
                   (chomp-display-name . "*saved*")
                   (chomp-session-id . "reuse-id")))
         calls)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory old-directory))
          (cl-letf (((symbol-function 'chomp-send-string)
                     (lambda (string) (push (list 'string string) calls)))
                    ((symbol-function 'chomp-send-key)
                     (lambda (key &optional modifiers)
                       (push (list 'key key modifiers) calls)))
                    ((symbol-function 'pop-to-buffer-same-window) #'identity))
            (should (eq buffer (chomp-bookmark-jump record))))
          (should (equal
                   `((key "return" nil)
                     (string ,(concat
                               "cd "
                               (shell-quote-argument
                                (directory-file-name directory)))))
                   calls)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory old-directory t)
      (delete-directory directory t))))

;;;; ---- Shell Integration Tests ----------------------------------------

(require 'chomp-shell)

(ert-deftest chomp-test-shell-base64-decode ()
  "Base64 decode works for valid input."
  (should (equal "hello" (chomp-shell--base64-decode (base64-encode-string "hello"))))
  (should (equal "/home/arthur" (chomp-shell--base64-decode
                                  (base64-encode-string "/home/arthur"))))
  ;; Invalid base64 returns nil
  (should-not (chomp-shell--base64-decode "!!invalid!!")))

(ert-deftest chomp-test-shell-split-payload ()
  "Payload splitting works."
  (should (equal '("foo" "bar") (chomp-shell--split-payload "e;A;foo;bar" 4)))
  (should (equal '("0") (chomp-shell--split-payload "e;H;0" 4)))
  ;; Past end returns nil
  (should-not (chomp-shell--split-payload "e;B" 4)))

(ert-deftest chomp-test-shell-osc51-dispatch ()
  "OSC 51 dispatch parses command letters correctly."
  (let ((events nil))
    (with-temp-buffer
      ;; Set up buffer-local state
      (setq-local chomp-enable-shell-prompt-annotation nil)  ; disable annotation
      (setq-local chomp-enable-directory-tracking nil)       ; disable for test
      (setq-local chomp-shell--pending-events nil)
      (setq-local chomp-shell--command-status 0)
      (setq-local chomp-shell--current-command nil)
      ;; Test exit status
      (chomp-shell-handle-osc51 "e;H;42" nil)
      (should (= 42 chomp-shell--command-status))
      ;; Test command text
      (let ((cmd-b64 (base64-encode-string "ls -la" t)))
        (chomp-shell-handle-osc51 (concat "e;F;" cmd-b64) nil)
        (should (equal "ls -la" chomp-shell--current-command))))))

(ert-deftest chomp-test-shell-message-default-deny ()
  "OSC 51 messages do nothing with the default nil whitelist."
  (should-not (default-value 'chomp-shell-message-handler-alist))
  (let ((called nil)
        (payload (format "e;M;%s;%s"
                         (base64-encode-string "handler" t)
                         (base64-encode-string "argument" t))))
    (with-temp-buffer
      (let ((chomp-shell-message-handler-alist nil))
        (chomp-shell-handle-osc51 payload nil)))
    (should-not called)))

(ert-deftest chomp-test-shell-message-whitelist ()
  "Only valid named OSC 51 messages invoke decoded handlers."
  (let (calls)
    (with-temp-buffer
      (let ((chomp-shell-message-handler-alist
             `(("allowed" . ,(lambda (&rest args) (push args calls)))
               ("error" . ,(lambda (&rest _) (error "handler failed"))))))
        (chomp-shell-handle-osc51
         (format "e;M;%s;%s;%s"
                 (base64-encode-string "allowed" t)
                 (chomp-test-base64 "héllo")
                 (chomp-test-base64 "世界")) nil)
        (chomp-shell-handle-osc51
         (format "e;M;%s;;%s"
                 (base64-encode-string "allowed" t)
                 (base64-encode-string "tail" t)) nil)
        (chomp-shell-handle-osc51
         (format "e;M;%s;%s" (base64-encode-string "unknown" t)
                 (base64-encode-string "ignored" t)) nil)
        (chomp-shell-handle-osc51 "e;M;not-base64" nil)
        (chomp-shell-handle-osc51
         (format "e;M;%s;%s"
                 (base64-encode-string "allowed" t)
                 (base64-encode-string (unibyte-string 255) t)) nil)
        (chomp-shell-handle-osc51
         (format "e;M;%s" (base64-encode-string "error" t)) nil)
        ;; A failed handler must not prevent the next message from parsing.
        (chomp-shell-handle-osc51
         (format "e;M;%s;%s" (base64-encode-string "allowed" t)
                 (base64-encode-string "after" t)) nil)))
    (should (equal '(("after") ("" "tail") ("héllo" "世界"))
                   calls))))

(ert-deftest chomp-test-shell-osc51-cwd ()
  "OSC 51 CWD tracking decodes base64 host and path."
  (with-temp-buffer
    (setq-local chomp-enable-directory-tracking t)
    (setq-local default-directory "/tmp/")
    (let* ((host (system-name))
           (path (temporary-file-directory))
           (payload (format "e;A;%s;%s"
                            (base64-encode-string host t)
                            (base64-encode-string
                             (directory-file-name path) t))))
      (chomp-shell-handle-osc51 payload nil)
      (should (equal (file-name-as-directory (directory-file-name path))
                     default-directory)))))

(ert-deftest chomp-test-shell-indicators ()
  "Prompt annotation indicators are opt-in and have correct faces."
  (should-not (default-value 'chomp-enable-shell-prompt-annotation))
  (let ((running (chomp-shell--running-indicator))
        (success (chomp-shell--status-indicator 0))
        (failure (chomp-shell--status-indicator 1)))
    (should (string= "+" running))
    (should (string= "0" success))
    (should (string= "X" failure))
    ;; Check faces
    (should (memq 'chomp-shell-prompt-annotation-running
                  (get-text-property 0 'face running)))
    (should (memq 'chomp-shell-prompt-annotation-success
                  (get-text-property 0 'face success)))
    (should (memq 'chomp-shell-prompt-annotation-failure
                  (get-text-property 0 'face failure)))))

(ert-deftest chomp-test-shell-absolute-line ()
  "Absolute line calculation works."
  (let ((screen (chomp-screen-create 20 6)))
    ;; Cursor at (0,0) with no scrollback = line 0
    (should (= 0 (chomp-shell--absolute-line screen)))
    ;; Move cursor to row 3
    (setf (chomp-screen-cursor-y screen) 3)
    (should (= 3 (chomp-shell--absolute-line screen)))))

(ert-deftest chomp-test-shell-env-vars ()
  "Shell env vars include integration directory and terminfo."
  (let ((vars (chomp-shell-env-vars)))
    (should (= 3 (length vars)))
    (should (string-prefix-p "CHOMP_SHELL_INTEGRATION_DIR=" (car vars)))
    (should (string-prefix-p "EAT_SHELL_INTEGRATION_DIR=" (cadr vars)))
    (should (string-prefix-p "TERMINFO=" (caddr vars)))))

(ert-deftest chomp-test-shell-prompt-metadata-without-annotations ()
  "Prompt navigation metadata does not require visual annotations."
  (let* ((screen (chomp-screen-create 20 6))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (setq-local chomp-enable-shell-prompt-annotation nil)
      (setq-local chomp-shell--pending-events nil)
      (setq-local chomp-shell--prompt-start-line nil)
      (setq-local chomp-shell--prompt-overlays nil)
      (let ((render (chomp-render-create screen (current-buffer))))
        ;; Queue two complete prompts before one render pass.
        (chomp-shell-handle-osc51 "e;B" screen)
        (chomp-test-output parser "first")
        (chomp-shell-handle-osc51 "e;C" screen)
        (chomp-test-output parser "\r\n\r\n")
        (chomp-shell-handle-osc51 "e;B" screen)
        (chomp-test-output parser "second")
        (chomp-shell-handle-osc51 "e;C" screen)
        (chomp-render-refresh render)
        (chomp-shell-post-render render)
        (let* ((first-begin
                (text-property-any (point-min) (point-max)
                                   'chomp-shell-prompt-begin t))
               (first-end
                (text-property-any (point-min) (point-max)
                                   'chomp-shell-prompt-end t))
               (second-end
                (and first-end
                     (text-property-any (1+ first-end) (point-max)
                                        'chomp-shell-prompt-end t))))
          (should first-begin)
          (should first-end)
          (should second-end)
          ;; Later replacement of a prompt row must retain model metadata.
          (chomp-test-output parser "\e[1;6H!")
          (chomp-render-refresh render)
          (chomp-shell-post-render render)
          (should (get-text-property first-begin 'chomp-shell-prompt-begin))
          (should (get-text-property first-end 'chomp-shell-prompt-end))
          (setq-local chomp--input-mode 'semi-char)
          (goto-char (point-max))
          (chomp-previous-prompt)
          (should (eq chomp--input-mode 'emacs))
          (should (= (point) (1+ second-end)))
          (chomp-previous-prompt)
          (should (= (point) (1+ first-end)))
          (goto-char (point-min))
          (chomp-next-prompt)
          (should (= (point) (1+ first-end)))
          (chomp-next-prompt)
          (should (= (point) (1+ second-end))))
        (should-not chomp-shell--prompt-overlays)
        (should-not chomp-shell--prompt-mark)
        (should-not
         (cl-find-if (lambda (ov)
                       (overlay-get ov 'chomp-shell-prompt))
                     (overlays-in (point-min) (point-max))))))))

(ert-deftest chomp-test-shell-prompt-imenu ()
  "Prompt metadata produces ordered command entries and navigable positions."
  (let* ((screen (chomp-screen-create 30 5))
         (parser (chomp-parse-create screen)))
    (with-temp-buffer
      (chomp-mode)
      (should (eq #'chomp-shell-imenu-create-index
                  imenu-create-index-function))
      (should (eq #'chomp-shell-imenu-goto imenu-default-goto-function))
      (let ((render (chomp-render-create screen (current-buffer))))
        (dolist (prompt-command '(("$ " . "  echo one  ")
                                  ("> " . "printf two")
                                  ("# " . "")))
          (chomp-shell-handle-osc51 "e;B" screen)
          (chomp-test-output parser (car prompt-command))
          (chomp-shell-handle-osc51 "e;C" screen)
          (chomp-test-output parser (concat (cdr prompt-command) "\r\n")))
        (chomp-render-refresh render)
        (let ((index (chomp-shell-imenu-create-index)))
          (should (equal '("echo one" "printf two") (mapcar #'car index)))
          (setq-local chomp--input-mode 'semi-char)
          (chomp-shell-imenu-goto (caar index) (cdar index))
          (should (eq chomp--input-mode 'emacs))
          (should (= (point) (marker-position (cdar index))))
          (should (looking-at-p "  echo one")))
        ;; Once model rows are erased/evicted, their properties yield no index.
        (chomp-screen-erase-in-display screen 2)
        (chomp-render-refresh render)
        (should-not (chomp-shell-imenu-create-index))))))

(ert-deftest chomp-test-shell-pending-events ()
  "OSC 51 B and C sequences queue pending events."
  (let ((screen (chomp-screen-create 20 6)))
    (with-temp-buffer
      (setq-local chomp-enable-shell-prompt-annotation t)
      (setq-local chomp-shell--pending-events nil)
      (setq-local chomp-shell--prompt-start-line nil)
      ;; Prompt start
      (chomp-shell-handle-osc51 "e;B" screen)
      (should (= 1 (length chomp-shell--pending-events)))
      (should (equal '(prompt-start 0 0)
                     (car chomp-shell--pending-events)))
      (should-not chomp-shell--prompt-start-line)
      ;; Prompt end
      (chomp-shell-handle-osc51 "e;C" screen)
      (should (= 2 (length chomp-shell--pending-events)))
      (should (eq 'prompt-end (caar chomp-shell--pending-events))))))

(provide 'chomp-test)
;;; chomp-test.el ends here
