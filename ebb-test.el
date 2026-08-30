;;; ebb-test.el --- Tests for ebb -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for the ebb terminal emulator, focused on the screen model
;; and parser since they are pure data transformers testable without a buffer.

;;; Code:

(require 'ert)
(require 'ebb-term)
(require 'ebb-parse)
(require 'ebb-render)
(require 'ebb-io)
(require 'ebb-trace)
(require 'ebb-input)
(require 'ebb)

(defvar xterm-store-paste-on-kill-ring)

;;;; ---- Test Helpers ---------------------------------------------------

(defmacro ebb-test-with-screen (spec &rest body)
  "Create a test screen per SPEC and evaluate BODY.
SPEC is a plist with :width (default 20) and :height (default 6).
Binds `screen' and `parser' in BODY."
  (declare (indent 1))
  (let ((width (or (plist-get spec :width) 20))
        (height (or (plist-get spec :height) 6)))
    `(let* ((screen (ebb-screen-create ,width ,height))
            (parser (ebb-parse-create screen)))
       (ignore parser)
       ,@body)))

(defun ebb-test-output (parser str)
  "Feed STR through PARSER."
  (ebb-parse-bytes parser str))

(defun ebb-test-base64 (string)
  "Return STRING encoded as UTF-8 base64."
  (base64-encode-string (encode-coding-string string 'utf-8) t))

(defun ebb-test-display-line (screen row)
  "Return the text content of display line ROW as a string."
  (let* ((line (ebb-screen-get-line screen row))
         (cells (ebb-line-cells line))
         (parts nil))
    (dotimes (i (length cells))
      (let ((cell (aref cells i)))
        (unless (zerop (ebb-cell-width cell))
          (push (concat (string (ebb-cell-char cell))
                        (ebb-cell-combining cell))
                parts))))
    ;; Strip trailing spaces
    (string-trim-right (apply #'concat (nreverse parts)))))

(defun ebb-test-display-text (screen)
  "Return all display lines as a list of strings (trailing spaces stripped)."
  (let ((lines nil))
    (dotimes (i (ebb-screen-height screen))
      (push (ebb-test-display-line screen i) lines))
    (nreverse lines)))

(defun ebb-test-cursor (screen)
  "Return cursor position as (X . Y)."
  (cons (ebb-screen-cursor-x screen)
        (ebb-screen-cursor-y screen)))

(ert-deftest ebb-test-window-size-change-callbacks-receive-window ()
  "Buffer-local resize callbacks accept the changed window."
  (let* ((window (selected-window))
         (original-buffer (window-buffer window))
         (buffer (generate-new-buffer " *ebb-resize-test*"))
         (screen (ebb-screen-create 1 1))
         (io (make-ebb-io :screen screen)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local ebb--io io)
            (setq-local ebb-eshell--io io))
          (set-window-buffer window buffer)
          (dolist (resize '(ebb--window-size-change ebb-eshell--resize))
            (ebb-screen-resize screen 1 1)
            (funcall resize window)
            (should (= (window-max-chars-per-line window)
                       (ebb-screen-width screen)))
            (should (= (window-body-height window)
                       (ebb-screen-height screen)))))
      (set-window-buffer window original-buffer)
      (kill-buffer buffer))))

(ert-deftest ebb-test-window-size-change-uses-smallest-visible-window ()
  "Multiple views of one terminal use Emacs's standard process sizing rule."
  (save-window-excursion
    (let* ((buffer (generate-new-buffer " *ebb-resize-windows-test*"))
           (screen (ebb-screen-create 1 1))
           (io (make-ebb-io :screen screen :process 'fake)))
      (unwind-protect
          (progn
            (set-window-buffer (selected-window) buffer)
            (set-window-buffer (split-window-below) buffer)
            (with-current-buffer buffer
              (setq-local ebb--io io))
            (cl-letf (((symbol-function 'process-live-p) (lambda (_) nil)))
              (ebb--window-size-change (selected-window)))
            (let ((size (window-adjust-process-window-size-smallest
                         'fake (get-buffer-window-list buffer nil t))))
              (should (= (car size) (ebb-screen-width screen)))
              (should (= (cdr size) (ebb-screen-height screen)))))
        (kill-buffer buffer)))))

;;;; ---- Screen Model Tests ---------------------------------------------

(ert-deftest ebb-test-screen-create ()
  "Screen creation initializes correctly."
  (let ((s (ebb-screen-create 80 24)))
    (should (= 80 (ebb-screen-width s)))
    (should (= 24 (ebb-screen-height s)))
    (should (= 0 (ebb-screen-cursor-x s)))
    (should (= 0 (ebb-screen-cursor-y s)))
    (should (= 0 (ebb-screen-scroll-top s)))
    (should (= 23 (ebb-screen-scroll-bottom s)))
    (should (ebb-screen-auto-wrap s))
    (should (ebb-screen-cursor-visible s))))

(ert-deftest ebb-test-write-char ()
  "Writing characters places them and advances cursor."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-write-char screen ?H)
    (ebb-screen-write-char screen ?i)
    (should (equal "Hi" (ebb-test-display-line screen 0)))
    (should (equal '(2 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-write-char-replaces-internal-raw-byte ()
  "The screen model never stores Emacs internal raw-byte characters."
  (ebb-test-with-screen (:width 3 :height 1)
    (ebb-screen-write-char screen #x3fffab)
    (should (equal "�" (ebb-test-display-line screen 0)))
    (should (string-prefix-p
             "�" (ebb-render--line-to-string
                   (ebb-screen-get-line screen 0) 3)))))

(ert-deftest ebb-test-combining-mark-preserved ()
  "Combining marks decorate the prior cell without moving the cursor."
  (ebb-test-with-screen (:width 5 :height 1)
    (ebb-test-output parser "ã")
    (let ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0)))
      (should (= ?a (ebb-cell-char cell)))
      (should (equal "̃" (ebb-cell-combining cell))))
    (should (equal "ã" (ebb-test-display-line screen 0)))
    (should (equal '(1 . 0) (ebb-test-cursor screen)))
    (should (equal "ã    "
                   (ebb-render--line-to-string
                    (ebb-screen-get-line screen 0) 5)))))

(ert-deftest ebb-test-zwj-grapheme-preserved ()
  "A ZWJ sequence occupies the first glyph's terminal cells."
  (ebb-test-with-screen (:width 5 :height 1)
    (ebb-test-output parser "👩‍💻")
    (let ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0)))
      (should (= ?👩 (ebb-cell-char cell)))
      (should (equal "‍💻" (ebb-cell-combining cell)))
      (should (= 2 (ebb-cell-width cell))))
    (should (equal "👩‍💻" (ebb-test-display-line screen 0)))
    (should (equal '(2 . 0) (ebb-test-cursor screen)))
    (should (string-prefix-p "👩‍💻"
                             (ebb-render--line-to-string
                              (ebb-screen-get-line screen 0) 5)))))

(ert-deftest ebb-test-vs16-emoji-presentation-widens ()
  "A text-default emoji followed by VS16 occupies two cells."
  (let ((vs "\uFE0F"))
    (ebb-test-with-screen (:width 5 :height 1)
      (ebb-test-output parser (concat "\u2764" vs "b"))
      (let ((cells (ebb-line-cells (ebb-screen-get-line screen 0))))
        (should (= ?\u2764 (ebb-cell-char (aref cells 0))))
        (should (= 2 (ebb-cell-width (aref cells 0))))
        (should (equal vs (ebb-cell-combining (aref cells 0))))
        (should (zerop (ebb-cell-width (aref cells 1))))
        ;; The shifted tail keeps its content.
        (should (= ?b (ebb-cell-char (aref cells 2)))))
      (should (equal (concat "\u2764" vs "b")
                     (ebb-test-display-line screen 0)))
      (should (equal '(3 . 0) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-vs16-at-right-edge-sets-pending-wrap ()
  "Widening a presentation sequence at the right edge wraps safely."
  (let ((vs "\uFE0F"))
    (ebb-test-with-screen (:width 5 :height 2)
      (ebb-test-output parser (concat "abc\u2764" vs))
      (should (equal '(4 . 0) (ebb-test-cursor screen)))
      (should (ebb-screen-pending-wrap screen))
      (ebb-test-output parser "x")
      (should (equal (concat "abc\u2764" vs)
                     (ebb-test-display-line screen 0)))
      (should (equal "x" (ebb-test-display-line screen 1)))
      (should (equal '(1 . 1) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-vs16-after-non-emoji-stays-zero-width ()
  "VS16 after a character outside the presentation table is a suffix."
  (let ((vs "\uFE0F"))
    (ebb-test-with-screen (:width 5 :height 1)
      (ebb-test-output parser (concat "a" vs))
      (let ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0)))
        (should (= ?a (ebb-cell-char cell)))
        (should (= 1 (ebb-cell-width cell)))
        (should (equal vs (ebb-cell-combining cell))))
      (should (equal '(1 . 0) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-vs16-after-wide-emoji-is-suffix ()
  "VS16 after an already double-width emoji does not widen again."
  (let ((vs "\uFE0F"))
    (ebb-test-with-screen (:width 5 :height 1)
      (ebb-test-output parser (concat "\U0001f600" vs))
      (let ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0)))
        (should (= #x1f600 (ebb-cell-char cell)))
        (should (= 2 (ebb-cell-width cell)))
        (should (equal vs (ebb-cell-combining cell))))
      (should (equal '(2 . 0) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-auto-wrap ()
  "Auto-wrap moves to next line at end of line."
  (ebb-test-with-screen (:width 5 :height 3)
    (dotimes (i 7)
      (ebb-screen-write-char screen (+ ?a i)))
    ;; "abcde" on line 0 (wrapped), "fg" on line 1
    (should (equal "abcde" (ebb-test-display-line screen 0)))
    (should (equal "fg" (ebb-test-display-line screen 1)))
    (should (ebb-line-wrapped (ebb-screen-get-line screen 0)))
    (should (equal '(2 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-auto-wrap-lazy-ascii-before-unicode ()
  "Unicode after a lazy ASCII row wraps without accessing nil cells."
  (ebb-test-with-screen (:width 5 :height 2)
    (ebb-test-output parser "abcdeλ")
    (should (equal "abcde" (ebb-test-display-line screen 0)))
    (should (equal "λ" (ebb-test-display-line screen 1)))
    (should (ebb-line-wrapped (ebb-screen-get-line screen 0)))
    (should (equal '(1 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-cursor-move-clamp ()
  "Cursor movement clamps to screen bounds."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-cursor-move screen 'up 100)
    (should (= 0 (ebb-screen-cursor-y screen)))
    (ebb-screen-cursor-move screen 'left 100)
    (should (= 0 (ebb-screen-cursor-x screen)))
    (ebb-screen-cursor-move screen 'down 100)
    (should (= 5 (ebb-screen-cursor-y screen)))
    (ebb-screen-cursor-move screen 'right 100)
    (should (= 19 (ebb-screen-cursor-x screen)))))

(ert-deftest ebb-test-cursor-goto ()
  "Cursor goto positions correctly (0-indexed)."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-cursor-goto screen 3 10)
    (should (equal '(10 . 3) (ebb-test-cursor screen)))
    ;; Clamp to bounds
    (ebb-screen-cursor-goto screen 100 100)
    (should (equal '(19 . 5) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-carriage-return ()
  "CR moves cursor to column 0."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-cursor-goto screen 0 10)
    (ebb-screen-carriage-return screen)
    (should (= 0 (ebb-screen-cursor-x screen)))
    (should (= 0 (ebb-screen-cursor-y screen)))))

(ert-deftest ebb-test-index-scroll ()
  "Index at bottom of scroll region scrolls up."
  (ebb-test-with-screen (:width 10 :height 3)
    ;; Write text on all lines
    (ebb-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "line0"))
    (ebb-screen-cursor-goto screen 1 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "line1"))
    (ebb-screen-cursor-goto screen 2 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "line2"))
    ;; Now at row 2, index should scroll
    (ebb-screen-index screen)
    (should (equal "line1" (ebb-test-display-line screen 0)))
    (should (equal "line2" (ebb-test-display-line screen 1)))
    (should (equal "" (ebb-test-display-line screen 2)))))

(ert-deftest ebb-test-reverse-index ()
  "Reverse index at top of scroll region scrolls down."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "line0"))
    (ebb-screen-cursor-goto screen 1 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "line1"))
    ;; Cursor at row 0
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-reverse-index screen)
    ;; Line 0 should now be blank, old line 0 shifted to line 1
    (should (equal "" (ebb-test-display-line screen 0)))
    (should (equal "line0" (ebb-test-display-line screen 1)))
    (should (equal "line1" (ebb-test-display-line screen 2)))))

(ert-deftest ebb-test-erase-in-display ()
  "Erase in display works for all modes."
  (ebb-test-with-screen (:width 5 :height 3)
    ;; Fill screen
    (dotimes (r 3)
      (ebb-screen-cursor-goto screen r 0)
      (dotimes (_ 5) (ebb-screen-write-char screen ?X)))
    ;; Erase from cursor to end (mode 0)
    (ebb-screen-cursor-goto screen 1 2)
    (ebb-screen-erase-in-display screen 0)
    (should (equal "XXXXX" (ebb-test-display-line screen 0)))
    (should (equal "XX" (ebb-test-display-line screen 1)))
    (should (equal "" (ebb-test-display-line screen 2)))))

(ert-deftest ebb-test-erase-display-preserves-main-viewport-in-history ()
  "ED 2 keeps cleared shell output available as main-screen history."
  (ebb-test-with-screen (:width 8 :height 4)
    (ebb-test-output parser "ls -al\r\nfile\r\n$ ")
    (should (= 0 (ebb-screen-history-row-count screen)))
    (ebb-test-output parser "\e[H\e[2J$ ")
    (should (> (ebb-screen-history-row-count screen) 0))
    (let ((text (ebb-screen-plain-text screen)))
      (should (string-match-p "ls -al" text))
      (should (string-match-p "file" text)))
    (should (equal "$" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-erase-display-skips-duplicate-repaint-history ()
  "Repeated ED 2 repaints do not duplicate an unchanged main-screen frame."
  (ebb-test-with-screen (:width 8 :height 2)
    (ebb-test-output parser "frame")
    (ebb-test-output parser "\e[H\e[2J")
    (let ((history-length (ebb-screen-scrollback-length screen)))
      (should (= 1 history-length))
      (ebb-test-output parser "frame")
      (ebb-test-output parser "\e[H\e[2J")
      (should (= history-length (ebb-screen-scrollback-length screen))))
    ;; Clearing history also clears the duplicate-frame guard.
    (ebb-screen-erase-in-display screen 3)
    (ebb-test-output parser "frame")
    (ebb-test-output parser "\e[H\e[2J")
    (should (= 1 (ebb-screen-scrollback-length screen)))))

(ert-deftest ebb-test-erase-in-line ()
  "Erase in line works for all modes."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "0123456789"))
    ;; Erase from cursor to end (mode 0)
    (ebb-screen-cursor-goto screen 0 5)
    (ebb-screen-erase-in-line screen 0)
    (should (equal "01234" (ebb-test-display-line screen 0)))
    ;; Erase from start to cursor (mode 1)
    (ebb-screen-cursor-goto screen 0 2)
    (ebb-screen-erase-in-line screen 1)
    (should (equal "   34" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-insert-delete-lines ()
  "Insert and delete lines within scroll region."
  (ebb-test-with-screen (:width 5 :height 4)
    (dotimes (r 4)
      (ebb-screen-cursor-goto screen r 0)
      (ebb-screen-write-char screen (+ ?A r)))
    ;; Insert 1 line at row 1
    (ebb-screen-cursor-goto screen 1 0)
    (ebb-screen-insert-lines screen 1)
    (should (equal "A" (ebb-test-display-line screen 0)))
    (should (equal "" (ebb-test-display-line screen 1)))
    (should (equal "B" (ebb-test-display-line screen 2)))
    (should (equal "C" (ebb-test-display-line screen 3)))
    ;; D fell off the bottom

    ;; Delete 1 line at row 1
    (ebb-screen-cursor-goto screen 1 0)
    (ebb-screen-delete-lines screen 1)
    (should (equal "A" (ebb-test-display-line screen 0)))
    (should (equal "B" (ebb-test-display-line screen 1)))
    (should (equal "C" (ebb-test-display-line screen 2)))
    (should (equal "" (ebb-test-display-line screen 3)))))

(ert-deftest ebb-test-insert-delete-lines-move-kitty-placements ()
  "Whole-row insert/delete operations keep graphics attached to moved text."
  (ebb-test-with-screen (:width 5 :height 4)
    (let* ((graphics (ebb-screen-graphics screen))
           (placement (make-ebb-graphics-placement
                       :image-id 1 :row 1 :column 0 :columns 1 :rows 1)))
      (setf (ebb-graphics-state-placements graphics) (list placement))
      (ebb-screen-cursor-goto screen 1 0)
      (ebb-screen-insert-lines screen 1)
      (should (= 2 (ebb-graphics-placement-row placement)))
      (ebb-screen-cursor-goto screen 1 0)
      (ebb-screen-delete-lines screen 1)
      (should (= 1 (ebb-graphics-placement-row placement)))
      ;; A placement spanning the movement boundary cannot be represented
      ;; without splitting, so discard it instead of misaligning it.
      (setf (ebb-graphics-state-placements graphics)
            (list (make-ebb-graphics-placement
                   :image-id 2 :row 0 :column 0 :columns 1 :rows 2)))
      (ebb-screen-insert-lines screen 1)
      (should-not (ebb-graphics-state-placements graphics)))))

(ert-deftest ebb-test-insert-delete-chars ()
  "Insert and delete characters on a line."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "ABCDE"))
    ;; Insert 2 chars at position 2
    (ebb-screen-cursor-goto screen 0 2)
    (ebb-screen-insert-chars screen 2)
    (should (equal "AB  CDE" (ebb-test-display-line screen 0)))
    ;; Delete 2 chars at position 2
    (ebb-screen-delete-chars screen 2)
    (should (equal "ABCDE" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-scroll-region ()
  "Scroll region confines scrolling."
  (ebb-test-with-screen (:width 5 :height 5)
    (dotimes (r 5)
      (ebb-screen-cursor-goto screen r 0)
      (ebb-screen-write-char screen (+ ?A r)))
    ;; Set scroll region to rows 1-3
    (ebb-screen-set-scroll-region screen 1 3)
    ;; Cursor homes to 0,0 after DECSTBM
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    ;; Scroll up within region
    (ebb-screen-cursor-goto screen 3 0)
    (ebb-screen-index screen)
    ;; Row 0 and 4 should be unchanged
    (should (equal "A" (ebb-test-display-line screen 0)))
    (should (equal "C" (ebb-test-display-line screen 1)))
    (should (equal "D" (ebb-test-display-line screen 2)))
    (should (equal "" (ebb-test-display-line screen 3)))
    (should (equal "E" (ebb-test-display-line screen 4)))
    (should-not (ebb-screen-scrollback-lines screen))))

(ert-deftest ebb-test-alt-screen ()
  "Alternate screen saves and restores main screen."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-cursor-goto screen 0 0)
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "main"))
    ;; Enter alt screen
    (ebb-screen-enter-alt screen)
    (should (equal "" (ebb-test-display-line screen 0)))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    ;; Write on alt screen
    (mapc (lambda (c) (ebb-screen-write-char screen c)) (string-to-list "alt"))
    (should (equal "alt" (ebb-test-display-line screen 0)))
    ;; Leave alt screen
    (ebb-screen-leave-alt screen)
    (should (equal "main" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-save-restore-cursor ()
  "DECSC/DECRC preserve position, attributes, modes, and character sets."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-cursor-goto screen 3 10)
    (ebb-screen-set-attr screen :bold t)
    (ebb-screen-designate-charset screen ?\( ?0)
    (setf (ebb-screen-origin-mode screen) t
          (ebb-screen-auto-wrap screen) nil)
    (ebb-screen-save-cursor screen)
    ;; Move elsewhere and change rendition state.
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-reset-attr screen)
    (ebb-screen-designate-charset screen ?\( ?B)
    (setf (ebb-screen-origin-mode screen) nil
          (ebb-screen-auto-wrap screen) t)
    ;; Restore.
    (ebb-screen-restore-cursor screen)
    (should (equal '(10 . 3) (ebb-test-cursor screen)))
    (should (ebb-attr-bold (ebb-screen-current-attr screen)))
    (should (ebb-screen-origin-mode screen))
    (should-not (ebb-screen-auto-wrap screen))
    (should (eq 'dec-graphics (ebb-screen-charset-g0 screen)))
    (ebb-screen-write-char screen ?q)
    (should (= #x2500
               (ebb-cell-char
                (aref (ebb-line-cells (ebb-screen-get-line screen 3)) 10))))))

(ert-deftest ebb-test-alt-screen-preserves-extended-saved-cursor-state ()
  "Alternate-screen DECSC state does not replace the main screen's state."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-designate-charset screen ?\( ?0)
    (ebb-screen-save-cursor screen)
    (ebb-screen-enter-alt screen)
    (ebb-screen-designate-charset screen ?\( ?B)
    (ebb-screen-save-cursor screen)
    (ebb-screen-leave-alt screen)
    (ebb-screen-designate-charset screen ?\( ?B)
    (ebb-screen-restore-cursor screen)
    (should (eq 'dec-graphics (ebb-screen-charset-g0 screen)))))

(ert-deftest ebb-test-tab-stops ()
  "Tab stops work correctly."
  (ebb-test-with-screen (:width 40 :height 3)
    ;; Default tab stops: 8, 16, 24, 32
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-tab-forward screen 1)
    (should (= 8 (ebb-screen-cursor-x screen)))
    (ebb-screen-tab-forward screen 1)
    (should (= 16 (ebb-screen-cursor-x screen)))
    ;; Tab backward
    (ebb-screen-tab-backward screen 1)
    (should (= 8 (ebb-screen-cursor-x screen)))
    ;; A cursor beyond the right margin must not be pulled backward by HT.
    (ebb-screen-set-horizontal-margin-mode screen t)
    (ebb-screen-set-horizontal-margins screen 2 5)
    (ebb-screen-cursor-goto screen 0 7)
    (ebb-screen-tab-forward screen 1)
    (should (= 8 (ebb-screen-cursor-x screen)))))

(ert-deftest ebb-test-reverse-wrap-modes-are-independent ()
  "Resetting one reverse-wrap mode leaves the other mode enabled."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-screen-set-mode screen 1045 t)
    (ebb-screen-set-mode screen 45 nil)
    (should (eq 'extended (ebb--reverse-wrap-mode screen)))
    (ebb-screen-set-mode screen 45 t)
    (should (eq 'extended (ebb--reverse-wrap-mode screen)))
    (ebb-screen-set-mode screen 1045 nil)
    (should (eq 'inline (ebb--reverse-wrap-mode screen)))))

(ert-deftest ebb-test-resize-clamps-cursor ()
  "Resize clamps cursor to new bounds."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-cursor-goto screen 20 70)
    (ebb-screen-resize screen 40 10)
    (should (= 39 (ebb-screen-cursor-x screen)))
    (should (= 9 (ebb-screen-cursor-y screen)))))

(ert-deftest ebb-test-resize-preserves-wrapped-logical-cursor ()
  "Width reflow keeps the cursor at the same logical text offset."
  (ebb-test-with-screen (:width 4 :height 3)
    (ebb-test-output parser "abcdef")
    (should (equal '(2 . 1) (ebb-test-cursor screen)))
    (ebb-screen-resize screen 3 3)
    (should (equal '(2 . 1) (ebb-test-cursor screen)))
    (should (ebb-screen-pending-wrap screen))
    (should (equal '("abc" "def" "") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-resize-preserves-exact-pending-wrap-boundary ()
  "A cursor after the rightmost cell remains pending after narrower reflow."
  (ebb-test-with-screen (:width 4 :height 3)
    (ebb-test-output parser "abcd")
    (should (ebb-screen-pending-wrap screen))
    (ebb-screen-resize screen 2 3)
    (should (equal '(1 . 1) (ebb-test-cursor screen)))
    (should (ebb-screen-pending-wrap screen))
    (should (equal '("ab" "cd" "") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-resize-height-moves-displaced-rows-to-history ()
  "Shrinking the main grid preserves displaced top rows in history."
  (ebb-test-with-screen (:width 4 :height 3)
    ;; Separate parser calls force ordinary semantics rather than CRLF batch.
    (dolist (part '("A\r\n" "B\r\n" "C"))
      (ebb-test-output parser part))
    (ebb-screen-resize screen 4 2)
    (should (= 1 (ebb-screen-scrollback-length screen)))
    (should (equal "A   "
                   (ebb-render--line-to-string-scrollback
                    (ebb-screen-history-render-row screen 0) 4)))
    (should (equal '("B" "C") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-resize-width-one-replaces-wide-glyph ()
  "A glyph wider than the terminal becomes one valid replacement cell."
  (ebb-test-with-screen (:width 2 :height 2)
    (ebb-test-output parser "界")
    (ebb-screen-resize screen 1 2)
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0)))
      (should (= #xfffd (ebb-cell-char cell)))
      (should (= 1 (ebb-cell-width cell))))))

(ert-deftest ebb-test-history-width-one-replaces-wide-glyph ()
  "History reflow never emits an orphan wide-character continuation."
  (ebb-test-with-screen (:width 2 :height 1)
    (ebb-test-output parser "界\r\n")
    (ebb-screen-resize screen 1 1)
    (should (= 1 (ebb-screen-history-row-count screen)))
    (let* ((line (ebb-screen-history-render-row screen 0))
           (cell (aref (ebb-line-cells line) 0)))
      (should (= #xfffd (ebb-cell-char cell)))
      (should (= 1 (ebb-cell-width cell))))))

(ert-deftest ebb-test-resize-preserves-prompt-metadata ()
  "Main-screen reflow carries prompt markers onto their projected rows."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "abc")
    (let ((line (ebb--line-at screen 0)))
      (setf (ebb-line-prompt-begins line) '(0)
            (ebb-line-prompt-ends line) '(3)))
    (ebb-screen-resize screen 2 2)
    (should (equal '(0) (ebb-line-prompt-begins (ebb--line-at screen 0))))
    (should (equal '(1) (ebb-line-prompt-ends (ebb--line-at screen 1))))
    (should (equal '((1 . 1)) (ebb-screen-prompt-end-locations screen)))))

(ert-deftest ebb-test-alt-resize-preserves-saved-main-auto-wrap ()
  "Resizing in the alternate screen does not invent saved pending wrap."
  (ebb-test-with-screen (:width 4 :height 2)
    (setf (ebb-screen-auto-wrap screen) nil)
    (ebb-test-output parser "ab")
    (ebb-screen-enter-alt screen)
    (ebb-screen-resize screen 2 2)
    (ebb-screen-leave-alt screen)
    (should-not (ebb-screen-auto-wrap screen))
    (should-not (ebb-screen-pending-wrap screen))
    (should (equal '(1 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-alt-resize-width-one-replaces-wide-glyph ()
  "Alternate-screen truncation never leaves a width-two cell in one column."
  (ebb-test-with-screen (:width 2 :height 2)
    (ebb-screen-enter-alt screen)
    (ebb-test-output parser "界")
    (ebb-screen-resize screen 1 2)
    (let ((cell (aref (ebb-line-cells (ebb--line-at screen 0)) 0)))
      (should (= #xfffd (ebb-cell-char cell)))
      (should (= 1 (ebb-cell-width cell))))))

(ert-deftest ebb-test-alt-resize-widen-clears-stale-pending-wrap ()
  "Widening an alternate grid continues after the old rightmost cell."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-screen-enter-alt screen)
    (ebb-test-output parser "abcd")
    (should (ebb-screen-pending-wrap screen))
    (ebb-screen-resize screen 6 2)
    (should-not (ebb-screen-pending-wrap screen))
    (should (equal '(4 . 0) (ebb-test-cursor screen)))
    (ebb-test-output parser "Z")
    (should (equal '("abcdZ" "") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-resize-resets-scroll-region ()
  "Resize resets scroll region to full screen."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-set-scroll-region screen 5 15)
    (ebb-screen-resize screen 80 30)
    (should (= 0 (ebb-screen-scroll-top screen)))
    (should (= 29 (ebb-screen-scroll-bottom screen)))))

(ert-deftest ebb-test-resize-marks-all-dirty ()
  "After resize, every line is marked dirty."
  (let ((screen (ebb-screen-create 80 24)))
    (ebb-screen-clear-dirty screen)
    (ebb-screen-resize screen 100 30)
    (should (= 30 (length (ebb-screen-get-dirty screen))))))

(ert-deftest ebb-test-resize-keeps-output-above-blank-rows ()
  "Shrinking the window does not let padding rows discard visible output."
  (ebb-test-with-screen (:width 10 :height 6)
    (ebb-test-output parser "flashprog")
    (ebb-screen-resize screen 10 3)
    (should (equal '("flashprog" "" "") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-resize-keeps-logical-scrollback-width-independent ()
  "Resize reprojects logical history without mutating its stored cells."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "row")
    (let ((display-line (ebb-screen-get-line screen 0)))
      (ebb--line-ensure-cells display-line 4)
      (setf (ebb-screen-scrollback screen)
            (list (make-ebb-line
                   :cells (vconcat (ebb-line-cells display-line))
                   :cells-valid t))
            (ebb-screen-scrollback-length screen) 1)
      (ebb-screen-resize screen 6 2)
      (should (= 1 (ebb-screen-history-row-count screen)))
      (let ((logical (car (ebb-screen-scrollback screen)))
            (rendered (ebb-screen-history-render-row screen 0)))
        (should (ebb-history-line-p logical))
        (should (= 3 (length (ebb-history-line-cells logical))))
        (should (= 6 (length (ebb-line-cells rendered))))
        (should (string-prefix-p
                 "row" (ebb-render--line-to-string-scrollback rendered 6)))))))

(ert-deftest ebb-test-logical-history-reflows-from-row-map ()
  "History changes physical row count without rewriting logical cells."
  (let ((screen (ebb-screen-create 4 2)))
    (setf (ebb-screen-scrollback screen)
          (list (make-ebb-line :text "ef  " :cells-valid nil)
                (make-ebb-line :text "abcd" :cells-valid nil :wrapped t))
          (ebb-screen-scrollback-length screen) 2)
    (should (= 2 (ebb-screen-history-row-count screen)))
    (should (= 1 (ebb-screen-scrollback-length screen)))
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should (equal "abcdef" (ebb-history-line-text logical)))
      (should (zerop (length (ebb-history-line-cells logical)))))
    (ebb-screen-resize screen 2 2)
    (should (= 3 (ebb-screen-history-row-count screen)))
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should (equal "abcdef" (ebb-history-line-text logical)))
      (should (zerop (length (ebb-history-line-cells logical)))))))

(ert-deftest ebb-test-logical-history-plain-render-stays-lazy ()
  "Rendering plain history does not materialize cell structs."
  (let ((screen (ebb-screen-create 4 2)))
    (ebb--history-push-row
     screen (make-ebb-line :text "abc " :cells-valid nil) 4)
    (let* ((logical (car (ebb-screen-scrollback screen)))
           (rendered (ebb-screen-history-render-row screen 0)))
      (should (equal "abc " (ebb-history-line-text logical)))
      (should (= 3 (ebb-history-line-text-length logical)))
      (should (zerop (length (ebb-history-line-cells logical))))
      (should (equal "abc " (ebb-line-text rendered)))
      (should-not (ebb-line-cells-valid rendered)))))

(ert-deftest ebb-test-plain-crlf-history-is-chunked-and-lazy ()
  "Complete plain lines share one chunk and materialize only rendered rows."
  (ebb-test-with-screen (:width 4 :height 2)
    (let ((data (copy-sequence "aa\r\nbb\r\ncc\r\ndd\r\n")))
      (ebb-test-output parser data)
      ;; History owns the retained block rather than aliasing mutable input.
      (aset data 0 ?X))
    (should (= 3 (ebb-screen-scrollback-length screen)))
    (should (= 1 (length (ebb-screen-scrollback screen))))
    (let ((chunk (car (ebb-screen-scrollback screen))))
      (should (ebb-history-chunk-p chunk))
      (should (= 3 (length (ebb-history-chunk-lengths chunk))))
      (should (equal '("aa  " "bb  " "cc  ")
                     (cl-loop for row below 3
                              collect (ebb-line-text
                                       (ebb-screen-history-render-row
                                        screen row)))))
      (should (ebb-history-chunk-p
               (car (ebb-screen-scrollback screen)))))))

(ert-deftest ebb-test-plain-crlf-block-leaves-trailing-text-on-screen ()
  "Batch parsing stops at incomplete trailing text and resumes normally."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "aa\r\nbb\r\ntail")
    (should (= 1 (ebb-screen-scrollback-length screen)))
    (should (ebb-history-chunk-p (car (ebb-screen-scrollback screen))))
    (should (equal '("bb" "tail") (ebb-test-display-text screen)))
    (should (equal '(3 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-plain-crlf-block-dirties-only-written-rows ()
  "A non-scrolling bulk write does not invalidate untouched viewport rows."
  (ebb-test-with-screen (:width 8 :height 4)
    (ebb-screen-clear-dirty screen)
    (ebb-test-output parser "a\r\nb\r\n")
    (should (equal '(0 1) (ebb-screen-get-dirty screen)))))

(ert-deftest ebb-test-plain-crlf-batch-joins-wrapped-logical-line ()
  "Fallback batching does not split the final row of an open wrapped line."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "abcdef\r\nxx\r\n")
    (let ((oldest (car (last (ebb-screen-scrollback screen)))))
      (should (ebb-history-line-p oldest))
      (should (equal "abcdef" (ebb-history-line-text oldest)))
      (should-not (ebb-history-line-open oldest)))))

(ert-deftest ebb-test-plain-crlf-block-keeps-correct-surviving-prefix ()
  "Bulk scrolling copies only existing rows that were not sent to history."
  (let* ((screen (ebb-screen-create 8 24))
         (parser (ebb-parse-create screen)))
    (dotimes (row 20)
      (ebb--set-line-at
       screen row
       (make-ebb-line
        :text (format "old%02d%s" row (make-string 3 ?\s))
        :cells-valid nil)))
    (ebb-screen-cursor-goto screen 20 0)
    (ebb-test-output parser
                     (mapconcat (lambda (char) (format "%c\r\n" char))
                                (number-sequence ?a ?j) ""))
    (should (= 7 (ebb-screen-scrollback-length screen)))
    (should (equal "old00   "
                   (ebb-line-text
                    (ebb-screen-history-render-row screen 0))))
    (let ((display (ebb-test-display-text screen)))
      (should (equal "old07" (nth 0 display)))
      (should (equal "old19" (nth 12 display)))
      (should (equal "a" (nth 13 display)))
      (should (equal "j" (nth 22 display)))
      (should (equal "" (nth 23 display))))))

(ert-deftest ebb-test-plain-history-chunk-reflows-and-preserves-anchors ()
  "Chunk row maps rebuild on resize without expanding logical line objects."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "aa\r\nbb\r\ncc\r\ndd\r\n")
    (should (equal '(1 . 1)
                   (ebb-screen-history-anchor-location screen 1 1)))
    (ebb-screen-resize screen 1 3)
    (should (= 6 (ebb-screen-history-row-count screen)))
    (should (equal '(3 . 0)
                   (ebb-screen-history-anchor-location screen 1 1)))
    ;; A logical-end anchor stays at the previous row's end rather than
    ;; moving to the next logical line.
    (should (equal '(1 . 1)
                   (ebb-screen-history-anchor-location screen 0 2)))
    (should (equal "b" (ebb-line-text
                        (ebb-screen-history-render-row screen 2))))
    (should (ebb-history-chunk-p
             (car (ebb-screen-scrollback screen))))))

(ert-deftest ebb-test-plain-history-chunk-trims-terminal-padding ()
  "Trailing spaces in closed batched rows do not become logical content."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "a  \r\n   \r\nzz\r\nqq\r\n")
    (let ((chunk (car (ebb-screen-scrollback screen))))
      (should (equal [1 0 2] (ebb-history-chunk-lengths chunk))))
    (ebb-screen-resize screen 1 3)
    (should (= 4 (ebb-screen-history-row-count screen)))
    (should (equal '("a" " " "z" "z")
                   (cl-loop for row below 4
                            collect (ebb-line-text
                                     (ebb-screen-history-render-row
                                      screen row)))))))

(ert-deftest ebb-test-plain-history-chunk-trims-logical-lines ()
  "Bounded history compacts a retained suffix of a plain chunk."
  (ebb-test-with-screen (:width 4 :height 2)
    (setf (ebb-screen-scrollback-max screen) 2
          (ebb-screen-scrollback-trim-batch screen) 0)
    (ebb-test-output parser "aa\r\nbb\r\ncc\r\ndd\r\n")
    (let ((chunk (car (ebb-screen-scrollback screen))))
      (should (= 2 (ebb-screen-scrollback-length screen)))
      (should (ebb-history-chunk-p chunk))
      (should (zerop (ebb-history-chunk-first chunk)))
      (should (= 2 (length (ebb-history-chunk-lengths chunk))))
      (should (< (length (ebb-history-chunk-text chunk)) 12))
      (should (equal "bb  "
                     (ebb-line-text
                      (ebb-screen-history-render-row screen 0))))
      (should (equal "cc  "
                     (ebb-line-text
                      (ebb-screen-history-render-row screen 1)))))))

(ert-deftest ebb-test-styled-crlf-history-stays-text-backed ()
  "Single-width styled history retains compact text and attribute runs."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "\e[31maa\r\nbb\r\n")
    (let* ((logical (car (ebb-screen-scrollback screen)))
           (rendered (ebb-screen-history-render-row screen 0))
           (run (car (ebb-history-line-attr-runs logical))))
      (should (ebb-history-line-p logical))
      (should (equal "aa  " (ebb-history-line-text logical)))
      (should (= 2 (ebb-history-line-text-length logical)))
      (should (zerop (length (ebb-history-line-cells logical))))
      (should (equal '(0 2) (cl-subseq run 0 2)))
      (should (= 1 (ebb-attr-fg (nth 2 run))))
      (should (equal '(0 2)
                     (cl-subseq (car (ebb-line-attr-runs rendered)) 0 2))))))

(ert-deftest ebb-test-styled-history-preserves-styled-trailing-space ()
  "A styled blank remains logical history content while padding is trimmed."
  (ebb-test-with-screen (:width 4 :height 1)
    (ebb-test-output parser "\e[31ma \e[0m\r\n")
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should (= 2 (ebb-history-line-text-length logical)))
      (ebb-screen-resize screen 1 1)
      (should (= 2 (ebb-screen-history-row-count screen)))
      (let* ((rendered (ebb-screen-history-render-row screen 1))
             (run (car (ebb-line-attr-runs rendered))))
        (should (equal " " (ebb-line-text rendered)))
        (should (equal '(0 1) (cl-subseq run 0 2)))
        (should (= 1 (ebb-attr-fg (nth 2 run))))))))

(ert-deftest ebb-test-wrapped-styled-history-merges-text-runs ()
  "Styled wrapped rows join lazily without materializing cells."
  (ebb-test-with-screen (:width 4 :height 1)
    (ebb-test-output parser "\e[31mabcdef\r\n")
    (let* ((logical (car (ebb-screen-scrollback screen)))
           (run (car (ebb-history-line-attr-runs logical))))
      (should (equal "abcdef" (ebb-history-line-text logical)))
      (should (= 6 (ebb-history-line-text-length logical)))
      (should (zerop (length (ebb-history-line-cells logical))))
      (should (equal '(0 6) (cl-subseq run 0 2)))
      (should (= 1 (ebb-attr-fg (nth 2 run)))))))

(ert-deftest ebb-test-wide-styled-history-keeps-cell-fallback ()
  "Styled wide characters retain the cell-backed history path."
  (ebb-test-with-screen (:width 4 :height 1)
    (ebb-test-output parser "\e[31m界\r\n")
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should (ebb-history-line-p logical))
      (should-not (ebb-history-line-text logical))
      (should (> (length (ebb-history-line-cells logical)) 0)))))

(ert-deftest ebb-test-uniform-styled-history-keeps-cell-fallback ()
  "Uniform text styling uses cells until its cache semantics are supported."
  (let* ((screen (ebb-screen-create 4 1))
         (attr (make-ebb-attr :bg 1))
         (line (make-ebb-line :text "    " :cells-valid nil
                              :uniform-attr attr)))
    (ebb--history-push-row screen line 4)
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should-not (ebb-history-line-text logical))
      (should (eq attr (ebb-cell-attr (aref (ebb-history-line-cells logical) 0)))))))

(ert-deftest ebb-test-erase-removes-stale-text-attribute-runs ()
  "Erasing styled text removes cached runs from rendering and history."
  (ebb-test-with-screen (:width 4 :height 1)
    (ebb-test-output parser "\e[31mabc\r\e[0m\e[K")
    (let ((line (ebb--line-at screen 0)))
      (should-not (ebb-line-attr-runs line))
      (should (equal "    " (ebb-line-text line))))
    (ebb-test-output parser "\r\n")
    (let ((logical (car (ebb-screen-scrollback screen))))
      (should (zerop (ebb-history-line-text-length logical)))
      (should-not (ebb-history-line-attr-runs logical)))))

(ert-deftest ebb-test-history-prompt-end-stays-on-boundary-row ()
  "Prompt ends at a wrap boundary agree with their rendered row."
  (let ((screen (ebb-screen-create 4 1)))
    (setf (ebb-screen-scrollback screen)
          (list (make-ebb-history-line
                 :id 0 :text "abcdef" :text-length 6 :prompt-ends '(4)))
          (ebb-screen-scrollback-length screen) 1
          (ebb-screen-history-next-id screen) 1
          (ebb-screen-history-logical-p screen) t)
    (should (equal '((0 . 4)) (ebb-screen-prompt-end-locations screen)))
    (should (equal '(4)
                   (ebb-line-prompt-ends
                    (ebb-screen-history-render-row screen 0))))))

(ert-deftest ebb-test-logical-history-appends-row-map-in-place ()
  "Steady output extends the width map without rescanning old history."
  (let ((screen (ebb-screen-create 4 2)))
    (setf (ebb-screen-scrollback screen)
          (cl-loop for id downfrom 19 to 0
                   collect (make-ebb-history-line
                            :id id
                            :cells (vector (make-ebb-cell :char ?x))))
          (ebb-screen-scrollback-length screen) 20
          (ebb-screen-history-next-id screen) 20
          (ebb-screen-history-generation screen) 1
          (ebb-screen-history-logical-p screen) t)
    (let ((map (ebb-screen-history-row-map screen)))
      (ebb--history-push-row
       screen (make-ebb-line :text "y   " :cells-valid nil) 4)
      (should (eq map (ebb-screen-history-row-map screen)))
      (should (= 21 (ebb-screen-history-row-count screen))))))

(ert-deftest ebb-test-render-materializes-bounded-history ()
  "Rendering cost is bounded independently of total logical history."
  (let ((screen (ebb-screen-create 8 3)))
    (setf (ebb-screen-scrollback screen)
          (cl-loop for id below 500
                   collect (make-ebb-history-line
                            :id id
                            :cells (vector (make-ebb-cell :char ?x))))
          (ebb-screen-scrollback-length screen) 500
          (ebb-screen-history-next-id screen) 500
          (ebb-screen-history-generation screen) 1)
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (should (= 500 (ebb-render-state-history-total-rows render)))
        (should (<= (ebb-render-state-scrollback-count render) 72))
        (should (< (count-lines (point-min) (point-max)) 100))))))

(ert-deftest ebb-test-render-resize-preserves-logical-history-anchor ()
  "Width changes preserve the logical cell under point."
  (let ((screen (ebb-screen-create 4 2)))
    (setf (ebb-screen-scrollback screen)
          (list (make-ebb-history-line
                 :id 0
                 :cells (vconcat
                         (mapcar (lambda (char) (make-ebb-cell :char char))
                                 (string-to-list "abcdefgh")))))
          (ebb-screen-scrollback-length screen) 1
          (ebb-screen-history-next-id screen) 1
          (ebb-screen-history-generation screen) 1
          (ebb-screen-history-logical-p screen) t)
    (with-temp-buffer
      (setq-local ebb--input-mode 'emacs)
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (goto-char (ebb-render-state-region-begin render))
        (forward-line 1)
        (forward-char 1)
        (should (equal '(history 0 5)
                       (ebb-render-buffer-anchor render)))
        (ebb-screen-resize screen 3 2)
        (ebb-render-full-reset render)
        (should (equal '(history 0 5)
                       (ebb-render-buffer-anchor render)))
        (should (equal '(1 . 2)
                       (ebb-render-buffer-location render)))))))

(ert-deftest ebb-test-resize-alternate-screen ()
  "Resizing in the alternate screen keeps the saved main screen in sync."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "main")
    (ebb-screen-enter-alt screen)
    (ebb-screen-resize screen 6 3)
    (ebb-screen-leave-alt screen)
    (should (equal '("main" "" "") (ebb-test-display-text screen)))
    (dotimes (row 3)
      (should (= 6 (length (ebb-line-cells
                            (ebb-screen-get-line screen row))))))))

(ert-deftest ebb-test-resize-does-not-reflow-alternate-screen ()
  "Resizing a full-screen application preserves its row layout."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-screen-enter-alt screen)
    (ebb-test-output parser "abcd\r\nxy")
    (ebb-screen-resize screen 2 3)
    (should (equal '("ab" "xy" "") (ebb-test-display-text screen)))))

(ert-deftest ebb-test-reset ()
  "Full reset clears everything."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-cursor-goto screen 3 10)
    (ebb-screen-set-attr screen :bold t)
    (ebb-screen-write-char screen ?X)
    (ebb-screen-reset screen)
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    (should (not (ebb-attr-bold (ebb-screen-current-attr screen))))
    (should (equal "" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-charset-dec-graphics ()
  "DEC graphics charset translates box-drawing characters."
  (ebb-test-with-screen (:width 10 :height 3)
    ;; Switch to DEC graphics
    (ebb-screen-designate-charset screen ?\( ?0)
    (setf (ebb-screen-charset-active screen) 'g0)
    ;; Write 'q' which should translate to horizontal line
    (ebb-screen-write-char screen ?q)
    (should (= #x2500 (ebb-cell-char
                        (aref (ebb-line-cells
                               (ebb-screen-get-line screen 0)) 0))))))

(ert-deftest ebb-test-charset-96-character-set ()
  "ESC - designates the G1 96-character set."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "\e-0\x0eq\x0f")
    (should (= #x2500 (ebb-cell-char
                        (aref (ebb-line-cells
                               (ebb-screen-get-line screen 0)) 0))))))

(ert-deftest ebb-test-charset-designation-recovers ()
  "Unsupported character sets are consumed without disrupting text."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "\e-?ok")
    (should (eq :ground (ebb-parser-state parser)))
    (should (equal "ok" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-scrollback ()
  "Lines scrolled off top go to scrollback."
  (ebb-test-with-screen (:width 5 :height 2)
    ;; Fill lines
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-write-char screen ?A)
    (ebb-screen-cursor-goto screen 1 0)
    (ebb-screen-write-char screen ?B)
    ;; Scroll up (pushes line 0 to scrollback)
    (ebb-screen-cursor-goto screen 1 0)
    (ebb-screen-index screen)
    ;; Check scrollback
    (let ((sb (ebb-screen-scrollback-lines screen)))
      (should (= 1 (length sb)))
      (should (= ?A (ebb-cell-char
                      (aref (ebb-line-cells (car sb)) 0)))))))

;;;; ---- Parser Tests ---------------------------------------------------

(ert-deftest ebb-test-parse-plain-text ()
  "Parser handles plain text."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "Hello")
    (should-not (multibyte-string-p
                 (ebb-line-text (ebb--line-at screen 0))))
    (should (equal "Hello" (ebb-test-display-line screen 0)))
    (should (equal '(5 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-malformed-utf8-as-replacement-character ()
  "Parser replaces Emacs internal eight-bit characters before rendering."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output
     parser (concat "ô" (decode-coding-string
                         (unibyte-string #xff) 'utf-8-unix)))
    (let ((line (ebb-screen-get-line screen 0)))
      (should (equal "ô�" (ebb-test-display-line screen 0)))
      (should (string-prefix-p "ô�"
                               (ebb-render--line-to-string line 20)))
      (should (string-prefix-p "ô�"
                               (ebb-render--line-to-string-scrollback
                                line 20))))
    (should (equal '(2 . 0) (ebb-test-cursor screen)))
    ;; Existing sessions may still contain a raw byte cached before this input
    ;; was normalized.  Rendering repairs that stale cache in place.
    (let* ((raw (decode-coding-string (unibyte-string #xab) 'utf-8-unix))
           (line (make-ebb-line :text (concat raw "  ") :cells-valid nil)))
      (should (equal "�  " (ebb-render--line-to-string line 3)))
      (should (equal "�  " (ebb-line-text line))))))

(ert-deftest ebb-test-parse-ignores-unsupported-c1-controls ()
  "Unsupported C1 controls do not enter the screen model as text."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser (string ?A #x8a ?B))
    (should (equal "AB" (ebb-test-display-line screen 0)))
    (should (equal '(2 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-crlf ()
  "Parser handles CR LF."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "Hello\r\nWorld")
    (should (equal "Hello" (ebb-test-display-line screen 0)))
    (should (equal "World" (ebb-test-display-line screen 1)))))

(ert-deftest ebb-test-parse-cursor-movement ()
  "Parser handles CSI cursor movement."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; CUP to row 3, col 5 (1-indexed in VT)
    (ebb-test-output parser "\e[4;6H")
    (should (equal '(5 . 3) (ebb-test-cursor screen)))
    ;; CUU (up 2)
    (ebb-test-output parser "\e[2A")
    (should (equal '(5 . 1) (ebb-test-cursor screen)))
    ;; CUF (right 3)
    (ebb-test-output parser "\e[3C")
    (should (equal '(8 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-selective-erase-protection ()
  "ISO and DEC protection preserve cells for their selective erase forms."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "ab\eVc\eW\e[1;1H\e[3X")
    (should (equal "  c" (ebb-test-display-line screen 0)))
    (ebb-test-output parser "\e[2Jab\e[1\"qc\e[0\"q\e[1;1H\e[?2K")
    (should (equal "  c" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-horizontal-margin-state-moves-with-lines ()
  "IL moves protected-cell metadata with cells inside horizontal margins."
  (ebb-test-with-screen (:width 6 :height 4)
    (ebb-test-output
     parser "\e[?69h\e[2;5s\e[2;2H\e[1\"qA\e[0\"q\e[2;2H\e[L\e[?2J")
    (should (equal " A" (ebb-test-display-line screen 2)))))

(ert-deftest ebb-test-parse-dec-rectangular-operations ()
  "DEC rectangular copy, fill, and selective erase preserve cursor state."
  (ebb-test-with-screen (:width 8 :height 4)
    (ebb-test-output parser "abcdefgh\r\nijklmnop\r\nqrstuvwx")
    (ebb-test-output parser "\e[4;2H\e[1;2;2;4;1;3;5;1$v")
    (should (equal "qrstbcdx" (ebb-test-display-line screen 2)))
    (should (equal "    jkl" (ebb-test-display-line screen 3)))
    (should (equal '(1 . 3) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[37;1;1;2;2$x")
    (should (equal "%%cdefgh" (ebb-test-display-line screen 0)))
    (ebb-test-output parser "\e[1;1H\e[1\"qP\e[0\"qI\eVZ\eW\e[1;1;1;3${")
    (should (equal "P  defgh" (ebb-test-display-line screen 0)))
    (ebb-test-output parser "\e[1;1;1;1$z")
    (should (equal "   defgh" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-rectangle-operation-clears-wide-boundary ()
  "A rectangle starting on a continuation cell clears the whole wide glyph."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "界\e[37;1;2;1;2$x")
    (let ((cells (ebb-line-cells (ebb-screen-get-line screen 0))))
      (should (= 1 (ebb-cell-width (aref cells 0))))
      (should (= 1 (ebb-cell-width (aref cells 1))))
      (should (= ?% (ebb-cell-char (aref cells 1)))))))

(ert-deftest ebb-test-parse-erase ()
  "Parser handles CSI J and CSI K."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "XXXXXXXXXX")
    (ebb-test-output parser "\e[1;6H")  ; cursor at row 0, col 5
    (ebb-test-output parser "\e[0K")    ; erase to end of line
    (should (equal "XXXXX" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-decaln ()
  "DECALN fills the display with E characters and homes the cursor."
  (ebb-test-with-screen (:width 5 :height 3)
    (ebb-test-output parser "junk\e#")
    (should (eq :escape-intermediate (ebb-parser-state parser)))
    (ebb-test-output parser "8")
    (should (equal '("EEEEE" "EEEEE" "EEEEE")
                   (ebb-test-display-text screen)))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    (should (eq :ground (ebb-parser-state parser)))))

(ert-deftest ebb-test-parse-dec-line-renditions ()
  "DEC line-size sequences select 40-column behavior on an 80-column line."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "\e[1;10H\e#6")
    (should (eq 'double-width
                (ebb-line-rendition (ebb-screen-get-line screen 0))))
    (should (= 5 (ebb-screen-line-width screen)))
    (should (equal '(4 . 0) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[1;1HABCDEF")
    (should (equal "ABCDE" (ebb-test-display-line screen 0)))
    (should (equal "F" (ebb-test-display-line screen 1)))
    (ebb-test-output parser "\e[2;1H\e#3")
    (should (eq 'double-height-top
                (ebb-line-rendition (ebb-screen-get-line screen 1))))
    (ebb-test-output parser "\e#4")
    (should (eq 'double-height-bottom
                (ebb-line-rendition (ebb-screen-get-line screen 1))))
    (ebb-test-output parser "\e#5")
    (should (eq 'normal
                (ebb-line-rendition (ebb-screen-get-line screen 1))))
    ;; The per-character path recomputes the width after wrapping onto a
    ;; normal-width line.
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-set-line-rendition screen 'double-width)
    (mapc (lambda (char) (ebb-screen-write-char screen char))
          (string-to-list "123456"))
    (should (equal "12345" (ebb-test-display-line screen 0)))
    (should (equal "6" (ebb-test-display-line screen 1)))
    (should (equal '(1 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-render-dec-double-width-line ()
  "DEC double-width lines render as expanded half-column strings."
  (ebb-test-with-screen (:width 10 :height 2)
    (ebb-test-output parser "\e#6Hello")
    (let ((rendered (ebb-render--line-to-string
                     (ebb-screen-get-line screen 0) 10)))
      (should (= 5 (length rendered)))
      (should (equal "Hello" rendered))
      (should (equal 'ultra-expanded
                     (plist-get (get-text-property 0 'face rendered) :width))))))

(ert-deftest ebb-test-render-dec-double-width-clips-by-display-columns ()
  "Combining characters do not reduce a double-width line's visible cells."
  (ebb-test-with-screen (:width 10 :height 2)
    (ebb-test-output parser (concat "\e#6" "ãbcde"))
    (let ((rendered (ebb-render--line-to-string
                     (ebb-screen-get-line screen 0) 10)))
      (should (equal "ãbcde" rendered))
      (should (= 5 (string-width rendered)))
      (should (equal 'ultra-expanded
                     (plist-get (get-text-property 0 'face rendered) :width))))))

(ert-deftest ebb-test-dec-double-width-rendition-survives-scrollback ()
  "History preserves and reflows DEC double-width line metadata."
  (ebb-test-with-screen (:width 10 :height 2)
    (ebb-test-output parser "\e#6Hello")
    (ebb-screen-cursor-goto screen 1 0)
    (ebb-screen-index screen)
    (let* ((logical (car (ebb-screen-scrollback screen)))
           (line (ebb-screen-history-render-row screen 0))
           (rendered (ebb-render--line-to-string-scrollback line 10)))
      (should (eq 'double-width (ebb-history-line-rendition logical)))
      (should (eq 'double-width (ebb-line-rendition line)))
      (should (equal "Hello" rendered))
      (should (= 5 (length rendered)))
      (should (equal 'ultra-expanded
                     (plist-get (get-text-property 0 'face rendered) :width))))
    (ebb-screen-resize screen 12 2)
    (let* ((line (ebb-screen-history-render-row screen 0))
           (rendered (ebb-render--line-to-string-scrollback line 12)))
      (should (eq 'double-width (ebb-line-rendition line)))
      (should (equal "Hello " rendered))
      (should (= 6 (length rendered))))))

(ert-deftest ebb-test-restore-cursor-clamps-to-restored-line-width ()
  "DECRC cannot restore the cursor into a double-width line's hidden half."
  (ebb-test-with-screen (:width 10 :height 2)
    (ebb-screen-cursor-goto screen 0 8)
    (ebb-screen-save-cursor screen)
    (ebb-screen-cursor-goto screen 0 0)
    (ebb-screen-set-line-rendition screen 'double-width)
    (ebb-screen-restore-cursor screen)
    (should (equal '(4 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-esc-encoding-selector-is-consumed ()
  "ESC percent encoding selectors do not leak their final byte as text."
  (ebb-test-with-screen (:width 10 :height 2)
    (ebb-test-output parser "\e%Gok")
    (should (equal "ok" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-deccolm ()
  "DECCOLM selects 80/132 columns, clears the display, and homes the cursor."
  (ebb-test-with-screen (:width 100 :height 24)
    (ebb-test-output parser "junk\e[3;20r\e[?3l")
    (should (= 80 (ebb-screen-width screen)))
    (should (= 0 (ebb-screen-scroll-top screen)))
    (should (= 23 (ebb-screen-scroll-bottom screen)))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    (should (equal "" (ebb-test-display-line screen 0)))
    ;; VTTEST deliberately moves beyond column 80 to verify clamping.
    (ebb-test-output parser "\e[23;70H\e[42C+")
    (should (= ?+ (ebb-cell-char
                   (aref (ebb-line-cells
                          (ebb-screen-get-line screen 22)) 79))))
    (ebb-test-output parser "\e[?3h")
    (should (= 132 (ebb-screen-width screen)))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    (should (equal "" (ebb-test-display-line screen 22)))
    (ebb-test-output parser "\ec")
    (should (= 80 (ebb-screen-width screen)))))

(ert-deftest ebb-test-parse-decrqm ()
  "DECRQM reports supported ANSI and DEC mode state."
  (ebb-test-with-screen (:width 10 :height 5)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e[?7$p\e[?25$p")
      (should (equal '("\e[?25;1$y" "\e[?7;1$y") responses))
      (setq responses nil)
      (ebb-test-output parser "\e[65;1\"p\e[4h\e[4$p")
      (should (equal "\e[4;1$y" (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\e[?69h\e[?69$p")
      (should (equal "\e[?69;1$y" (car responses))))))

(ert-deftest ebb-test-parse-synchronized-output ()
  "DEC 2026 synchronized output tracks mode state and answers DECRQM."
  (ebb-test-with-screen (:width 10 :height 5)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (should-not (ebb-screen-sync-output screen))
      (ebb-test-output parser "\e[?2026h\e[?2026$p")
      (should (ebb-screen-sync-output screen))
      (should (equal "\e[?2026;1$y" (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\e[?2026l\e[?2026$p")
      (should-not (ebb-screen-sync-output screen))
      (should (equal "\e[?2026;2$y" (car responses)))
      ;; RIS clears a stuck mode.
      (ebb-test-output parser "\e[?2026h\ec")
      (should-not (ebb-screen-sync-output screen)))))

(ert-deftest ebb-test-parse-decrqss ()
  "DECRQSS reports current rendition and margin state."
  (ebb-test-with-screen (:width 10 :height 5)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser
                       "\e[1;5;8;9;38;5;123;48;2;1;2;3m\eP$qm\e\\")
      (should (equal "\eP1$r0;1;5;8;9;38;5;123;48;2;1;2;3m\e\\"
                     (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\e[2;4r\eP$qr\e\\")
      (should (equal "\eP1$r2;4r\e\\" (car responses)))
      (setq responses nil)
      (setf (ebb-screen-current-attr screen)
            (make-ebb-attr :underline 'curly :font 1 :blink 'fast
                           :conceal t :crossed t :fg 123 :bg '(1 2 3)
                           :ul-color 4))
      (ebb-test-output parser "\eP$qm\e\\")
      (should (equal "\eP1$r0;4:3;11;6;8;9;38;5;123;48;2;1;2;3;58;5;4m\e\\"
                     (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\eP$qbogus\e\\")
      (should (equal "\eP0$r\e\\" (car responses))))))

(ert-deftest ebb-test-parse-ris-clears-parser-owned-status ()
  "RIS clears DECSCL and page-length state used by DECRQSS replies."
  (ebb-test-with-screen (:width 10 :height 5)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (puthash screen 61 ebb-parse--conformance-levels)
      (puthash screen 27 ebb-parse--page-lengths)
      (ebb-test-output parser "\ec")
      (ebb-test-output parser "\eP$q\"p\e\\")
      (should (equal "\eP1$r65;1\"p\e\\" (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\eP$qt\e\\")
      (should (equal "\eP1$r5t\e\\" (car responses))))))

(ert-deftest ebb-test-parse-decrqcra ()
  "DECRQCRA reports the DEC checksum for the requested rectangle."
  (ebb-test-with-screen (:width 5 :height 3)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "A\e[7;0;1;1;1;1*y")
      (should (equal "\eP7!~FFBF\e\\" (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\e[8;0;1;2;1;2*y")
      (should (equal "\eP8!~10000\e\\" (car responses)))
      (setq responses nil)
      (ebb-test-output
       parser "\e[?69h\e[2;5s\e[2;3r\e[?6hB\e[9;0;1;1;1;1*y")
      (should (equal "\eP9!~FFBE\e\\" (car responses))))))

(ert-deftest ebb-test-aborted-dcs-clears-intermediates ()
  "An aborted DCS does not leak its intermediates into the next CSI."
  (ebb-test-with-screen (:width 10 :height 5)
    (ebb-test-output parser "\e[4h\eP!qignored\e[p")
    (should (ebb-screen-insert-mode screen))))

(ert-deftest ebb-test-parse-decstr ()
  "DECSTR resets modes and saved cursor without clearing or moving."
  (ebb-test-with-screen (:width 10 :height 5)
    (ebb-test-output parser "X\e[4h\e[2;4r\e[3;4H\e7\e[!p")
    (should (equal "X" (ebb-test-display-line screen 0)))
    (should (equal '(3 . 2) (ebb-test-cursor screen)))
    (should-not (ebb-screen-insert-mode screen))
    (should (= 0 (ebb-screen-scroll-top screen)))
    (should (= 4 (ebb-screen-scroll-bottom screen)))
    (ebb-test-output parser "\e8")
    (should (equal '(0 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-decscl-resets-terminal ()
  "DECSCL performs a full terminal reset."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "junk\e[3;3H\e[65;1\"p")
    (should (equal "" (ebb-test-display-line screen 0)))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-winops-resize-characters ()
  "CSI 8 t resizes the model and emits a synchronization event."
  (ebb-test-with-screen (:width 10 :height 3)
    (let (events)
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest args) (push (cons type args) events)))
      (ebb-test-output parser "\e[8;25;80t")
      (should (= 80 (ebb-screen-width screen)))
      (should (= 25 (ebb-screen-height screen)))
      (should (equal '(resize-request 80 25) (car events))))))

(ert-deftest ebb-test-parse-sgr-basic ()
  "Parser handles basic SGR attributes."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; Bold + red foreground
    (ebb-test-output parser "\e[1;31mHi\e[0m")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      (should attr)
      (should (ebb-attr-bold attr))
      (should (= 1 (ebb-attr-fg attr))))))  ; red = 1

(ert-deftest ebb-test-private-csi-m-not-sgr ()
  "CSI > 4 ; 1 m (modifyOtherKeys) must not set underline."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[>4;1m\e[1;36mebb\e[0m")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      (should attr)
      (should (ebb-attr-bold attr))
      (should (= 6 (ebb-attr-fg attr)))
      (should-not (ebb-attr-underline attr)))))

(ert-deftest ebb-test-private-csi-u-does-not-restore-cursor ()
  "Kitty keyboard negotiation is not mistaken for cursor restoration."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[3;5H\e[s\e[1;1H\e[u")
    (should (equal '(4 . 2) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[2;2H\e[>13u")
    (should (equal '(1 . 1) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-sgr-256color ()
  "Parser handles 256-color SGR."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[38;5;196mR\e[0m")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      (should (= 196 (ebb-attr-fg attr))))))

(ert-deftest ebb-test-parse-sgr-256color-truncated ()
  "A truncated 256-color sequence does not read padded parameters."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[38;5mR\e[0m")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      ;; Default attribute: no foreground was set from the padded zeros.
      (should (or (null attr) (null (ebb-attr-fg attr)))))))

(ert-deftest ebb-test-parse-cup-extra-semicolon ()
  "CUP with a second semicolon falls back to the generic parser."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "A\e[1;2;3HB")
    (should (equal "AB" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-sgr-256color-followed-by-reset ()
  "A palette color does not consume a following SGR parameter."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[38;5;2;49mG")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      (should (= 2 (ebb-attr-fg attr)))
      (should-not (ebb-attr-bg attr)))))

(ert-deftest ebb-test-parse-sgr-truecolor ()
  "Parser handles truecolor SGR."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[38;2;255;128;0mO\e[0m")
    (let* ((line (ebb-screen-get-line screen 0))
           (cell (aref (ebb-line-cells line) 0))
           (attr (ebb-cell-attr cell)))
      (should (equal '(255 128 0) (ebb-attr-fg attr))))))

(ert-deftest ebb-test-parse-decset-alt-screen ()
  "Parser handles DECSET ?1049 (alt screen)."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "main")
    (ebb-test-output parser "\e[?1049h")  ; enter alt
    (should (equal "" (ebb-test-display-line screen 0)))
    (ebb-test-output parser "alt")
    (should (equal "alt" (ebb-test-display-line screen 0)))
    (ebb-test-output parser "\e[?1049l")  ; leave alt
    (should (equal "main" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-scroll-region ()
  "Parser handles DECSTBM."
  (ebb-test-with-screen (:width 10 :height 5)
    (ebb-test-output parser "\e[2;4r")  ; scroll region rows 2-4 (1-indexed)
    (should (= 1 (ebb-screen-scroll-top screen)))   ; 0-indexed
    (should (= 3 (ebb-screen-scroll-bottom screen)))))

(ert-deftest ebb-test-parse-horizontal-margins ()
  "DECLRMM and DECSLRM constrain margin-aware cursor operations."
  (ebb-test-with-screen (:width 20 :height 10)
    (ebb-test-output parser "\e[?69h\e[5;10s")
    (should (ebb-screen-horizontal-margins-enabled-p screen))
    (should (= 4 (ebb-screen-left-margin screen)))
    (should (= 9 (ebb-screen-right-margin screen)))
    (ebb-test-output parser "\e[1;6H\e[99C")
    (should (equal '(9 . 0) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[1;4H\r")
    (should (equal '(0 . 0) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[1;6H\r")
    (should (equal '(4 . 0) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[s")
    (should (= 0 (ebb-screen-left-margin screen)))
    (should (= 19 (ebb-screen-right-margin screen)))
    (ebb-test-output parser "\e[?69l")
    (should-not (ebb-screen-horizontal-margins-enabled-p screen))))

(ert-deftest ebb-test-cursor-movement-respects-active-region ()
  "Relative movement stops at a margin only when starting inside it."
  (ebb-test-with-screen (:width 20 :height 10)
    (ebb-test-output parser "\e[3;6r\e[4;1H\e[99B")
    (should (equal '(0 . 5) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[8;1H\e[99B")
    (should (equal '(0 . 9) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[?69h\e[5;10s\e[4;6H\e[99D")
    (should (equal '(4 . 3) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[4;3H\e[99D")
    (should (equal '(0 . 3) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-origin-mode-reports-relative-horizontal-margins ()
  "CPR reports coordinates relative to both margins in origin mode."
  (ebb-test-with-screen (:width 20 :height 10)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e[3;8r\e[?69h\e[5;10s\e[?6h\e[2;3H\e[6n")
      (should (equal '(6 . 3) (ebb-test-cursor screen)))
      (should (equal "\e[2;3R" (car responses))))))

(ert-deftest ebb-test-parse-origin-mode ()
  "DECOM makes cursor addressing relative to the scrolling region."
  (ebb-test-with-screen (:width 10 :height 6)
    (ebb-test-output parser "\e[2;5r\e[?6h")
    (should (ebb-screen-origin-mode screen))
    (should (equal '(0 . 1) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[2;3H")
    (should (equal '(2 . 2) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[20A")
    (should (equal '(2 . 1) (ebb-test-cursor screen)))
    (ebb-test-output parser "\e[?6l")
    (should-not (ebb-screen-origin-mode screen))
    (should (equal '(0 . 0) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-private-mode-4-is-not-insert-mode ()
  "DEC smooth-scroll mode does not enable ANSI insert mode."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "\e[?4h")
    (should-not (ebb-screen-insert-mode screen))
    (ebb-test-output parser "\e[4h")
    (should (ebb-screen-insert-mode screen))
    (ebb-test-output parser "\e[4l")
    (should-not (ebb-screen-insert-mode screen))))

(ert-deftest ebb-test-insert-mode-preserves-shifted-cell ()
  "ANSI insert mode shifts rather than aliases the overwritten cell."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "AAAAAAAAAA\e[1;2HB\e[1D\e[4h********\e[4l")
    (should (equal "A********B" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-insert-delete ()
  "Parser handles ICH, DCH, IL, DL."
  (ebb-test-with-screen (:width 10 :height 3)
    (ebb-test-output parser "ABCDE")
    ;; Insert 2 chars at col 2
    (ebb-test-output parser "\e[1;3H")   ; cursor at row 1, col 3 (1-indexed)
    (ebb-test-output parser "\e[2@")     ; ICH 2
    (should (equal "AB  CDE" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-esc-save-restore ()
  "Parser handles ESC 7/8 save/restore cursor."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[3;5H")  ; goto row 3, col 5
    (ebb-test-output parser "\e7")      ; save
    (ebb-test-output parser "\e[1;1H")  ; goto origin
    (ebb-test-output parser "\e8")      ; restore
    (should (equal '(4 . 2) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-osc-title ()
  "Parser handles OSC 0/2 title setting."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e]2;My Title\a")
    (should (equal "My Title" (ebb-screen-title screen)))))

(ert-deftest ebb-test-parse-osc-title-st ()
  "Parser handles OSC with ST (ESC \\) terminator."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e]2;My Title\e\\")
    (should (equal "My Title" (ebb-screen-title screen)))))

(ert-deftest ebb-test-parse-can-aborts-csi ()
  "CAN aborts a pending CSI sequence; the final byte prints."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "A\e[5;3\x18mBX")
    (should (equal "AmBX" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-unknown-csi-intermediate-ignored ()
  "Unknown CSI+intermediate sequences are ignored, not dispatched as plain CSI."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "ABCD\e[1;2H\e[ @")
    (should (equal "ABCD" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-malformed-csi-ignored ()
  "A parameter byte after an intermediate byte ignores the whole sequence."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "AB\e[ 3qCD")
    (should (equal "ABCD" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-bounds-csi-headers ()
  "Overlong CSI parameters and intermediates are ignored without growing."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((overlong 1025))
      (ebb-test-output parser (concat "\e[" (make-string overlong ?1)))
      (should (eq :csi-ignored (ebb-parser-state parser)))
      (should (= 1024 (length (ebb-parser-param-string parser))))
      (ebb-test-output parser "m")
      (ebb-test-output parser (concat "\e[" (make-string overlong ?/)))
      (should (eq :csi-ignored (ebb-parser-state parser)))
      (should (= 1024 (length (ebb-parser-intermediates parser))))
      (ebb-test-output parser "qX")
      (should (equal "X" (ebb-test-display-line screen 0))))))

(ert-deftest ebb-test-parse-fast-csi-respects-header-bound ()
  "The fast CSI path rejects overlong cursor and attribute parameters."
  (ebb-test-with-screen (:width 20 :height 6)
    (dolist (final '(?H ?m))
      (let ((sequence (concat (make-string 1025 ?1) (string final))))
        (should-not (ebb-parse--fast-csi-at
                     parser sequence 0 (length sequence)))))))

(ert-deftest ebb-test-parse-bounds-esc-intermediates ()
  "Overlong ESC intermediates are ignored through their final byte."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((overlong 1025))
      (ebb-test-output parser (concat "\e" (make-string overlong ?#)))
      (should (eq :escape-ignored (ebb-parser-state parser)))
      (should (= 1024 (length (ebb-parser-intermediates parser))))
      (ebb-test-output parser "0X")
      (should (equal "X" (ebb-test-display-line screen 0))))))

(ert-deftest ebb-test-parse-bounds-dcs-headers ()
  "Overlong DCS parameters and intermediates are ignored through ST."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((overlong 1025))
      (ebb-test-output parser (concat "\eP" (make-string overlong ?1)))
      (should (eq :dcs-ignored (ebb-parser-state parser)))
      (should (= 1024 (length (ebb-parser-dcs-params parser))))
      (ebb-test-output parser "\e\\")
      (ebb-test-output parser (concat "\eP" (make-string overlong ?/)))
      (should (eq :dcs-ignored (ebb-parser-state parser)))
      (should (= 1024 (length (ebb-parser-intermediates parser))))
      (ebb-test-output parser "\e\\X")
      (should (equal "X" (ebb-test-display-line screen 0))))))

(ert-deftest ebb-test-parse-c1-st-terminates-osc ()
  "C1 ST (U+009C) terminates a pending OSC string."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser (concat "\e]0;hello" (string #x9c) "X"))
    (should (equal "hello" (ebb-screen-title screen)))
    (should (equal "X" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-c1-st-after-esc-completes-osc ()
  "C1 ST after an ESC inside an OSC string still dispatches the string."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser (concat "\e]0;hello\e" (string #x9c) "X"))
    (should (equal "hello" (ebb-screen-title screen)))
    (should (equal "X" (ebb-test-display-line screen 0)))
    (should (eq :ground (ebb-parser-state parser)))))

(ert-deftest ebb-test-parse-osc-discards-c0 ()
  "C0 controls inside an OSC string are discarded."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e]0;ti\nle\a")
    (should (equal "tile" (ebb-screen-title screen)))))

(ert-deftest ebb-test-parse-dcs-header-ignores-c0 ()
  "C0 controls in the DCS header are ignored and the sequence continues."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\eP\n$qmBODY\e\\")
    (should (equal "" (ebb-test-display-line screen 0)))
    (should (eq :ground (ebb-parser-state parser)))))

(ert-deftest ebb-test-parse-decstbm-ignores-private-marker ()
  "CSI ? Ps r is not DECSTBM; only CSI Pt;Pb r sets the scroll region."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[?5r")
    (should (= 0 (ebb-screen-scroll-top screen)))
    (should (= 5 (ebb-screen-scroll-bottom screen)))
    (ebb-test-output parser "\e[2;4r")
    (should (= 1 (ebb-screen-scroll-top screen)))
    (should (= 3 (ebb-screen-scroll-bottom screen)))))

(ert-deftest ebb-test-osc8-hyperlink-rendering ()
  "OSC 8 URIs and ids become clickable text properties."
  (ebb-test-with-screen (:width 30 :height 2)
    (ebb-test-output parser
                     "\e]8;id=docs;https://example.com\e\\click\e]8;;\e\\ plain")
    (let* ((buffer (generate-new-buffer " *ebb-osc8-test*"))
           (render (ebb-render-create screen buffer)))
      (unwind-protect
          (with-current-buffer buffer
            (ebb-render-refresh render)
            (should (equal "https://example.com"
                           (get-text-property 1 'help-echo)))
            (should (equal "docs" (get-text-property 1 'ebb-link-id)))
            (should (eq ebb-link-map (get-text-property 1 'keymap)))
            (should-not (get-text-property 7 'help-echo)))
        (kill-buffer buffer)))))

(ert-deftest ebb-test-plain-url-and-file-detection ()
  "Plain URLs and existing file references become clickable."
  (let ((file (locate-library "ebb")))
    (with-temp-buffer
      (setq default-directory (file-name-directory file))
      (insert (format "See https://example.com and ./%s:42:3\n"
                      (file-name-nondirectory file)))
      (let ((ebb--render nil) (ebb--screen nil))
        (ebb--detect-plain-links nil))
      (goto-char (point-min))
      (let ((url-pos (progn (search-forward "https://") (- (point) 8))))
        (should (equal "https://example.com"
                       (get-text-property url-pos 'help-echo))))
      (let ((file-pos (progn (search-forward "./") (- (point) 2))))
        (let ((uri (get-text-property file-pos 'help-echo)))
          (should (string-prefix-p "fileref:" uri))
          (should (string-suffix-p ":42:3" uri)))))))

(ert-deftest ebb-test-parse-osc-notifications ()
  "OSC 9 and 777 emit only valid normalized notifications."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (events)
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest args) (push (cons type args) events)))
      (ebb-test-output parser "\e]9;body\a")
      (ebb-test-output parser "\e]777;notify;title;body text\a")
      (ebb-test-output parser "\e]9;\a")
      (ebb-test-output parser "\e]777;notify;;\a")
      (ebb-test-output parser "\e]777;unknown;title;body\a")
      (should (equal '((notification "title" "body text")
                       (notification nil "body"))
                     events)))))

(ert-deftest ebb-test-parse-osc-progress ()
  "OSC 9;4 progress states are normalized and percentages clamped."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (events)
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest args)
              (when (eq type 'progress) (push args events))))
      (dolist (payload '("0;0" "1;150" "2;-5" "3;42" "4;75"
                         "5;10" "bogus"))
        (ebb-test-output parser (format "\e]9;4;%s\a" payload)))
      (should (equal '((pause 75) (indeterminate 42) (error 0)
                       (set 100) (remove 0))
                     events)))))

(ert-deftest ebb-test-parse-sos-does-not-dispatch-kitty-payload ()
  "An SOS string beginning with G is not mistaken for a Kitty APC."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output parser "\eXGa=q,i=7;\e\\")
      (should-not responses))))

(ert-deftest ebb-test-parse-kitty-query-response ()
  "The canonical Kitty support query is answered without storing an image."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Gi=7,s=1,v=1,a=q,t=d,f=24;AAAA\e\\")
      (should (equal '("\e_Gi=7;OK\e\\") responses))
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images (ebb-screen-graphics screen))))))))

(defconst ebb-test-kitty-png-base64
  "iVBORw0KGgoAAAANSUhEUgAAAAIAAAABCAIAAAB7QOjdAAAADUlEQVR4nGP4zwAE/wEHAAH/4iOeWQAAAABJRU5ErkJggg=="
  "A 2x1 RGB PNG (red, blue) as base64.")

(ert-deftest ebb-test-parse-kitty-direct-png-transmit ()
  "A direct PNG transmission is retained with its IHDR dimensions."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((data (base64-decode-string ebb-test-kitty-png-base64))
          responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output parser (format "\e_Ga=t,f=100,i=9;%s\e\\"
                                      ebb-test-kitty-png-base64))
      (let* ((graphics (ebb-screen-graphics screen))
             (image (gethash 9 (ebb-graphics-state-images graphics))))
        (should image)
        (should (= 100 (ebb-graphics-image-format image)))
        (should (= 2 (ebb-graphics-image-width image)))
        (should (= 1 (ebb-graphics-image-height image)))
        (should (equal data (ebb-graphics-image-data image)))
        (should (= (length data) (ebb-graphics-state-byte-count graphics))))
      (should (equal '("\e_Gi=9;OK\e\\") responses))
      ;; Data that is not a PNG is rejected rather than stored.
      (setq responses nil)
      (ebb-test-output parser (format "\e_Ga=t,f=100,i=10;%s\e\\"
                                      (base64-encode-string "nope" t)))
      (should-not (gethash 10 (ebb-graphics-state-images
                               (ebb-screen-graphics screen))))
      (should (equal '("\e_Gi=10;EINVAL:invalid PNG data\e\\") responses)))))

(ert-deftest ebb-test-kitty-anonymous-transmit-does-not-acknowledge ()
  "A transmission without i or I creates an anonymous image without an ACK."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1;AQID\e\\")
      (should (= 1 (hash-table-count
                    (ebb-graphics-state-images (ebb-screen-graphics screen)))))
      (should-not responses))))

(ert-deftest ebb-test-kitty-image-number-response-includes-id-and-number ()
  "An I request is acknowledged with both the allocated i and original I."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,I=13;AQID\e\\")
      (should (equal '("\e_Gi=1,I=13;OK\e\\") responses)))))

(ert-deftest ebb-test-kitty-placement-geometry-follows-kitty ()
  "Unspecified c/r preserve aspect ratio; a complete cell box stretches."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((encoded (base64-encode-string (make-string (* 16 32 3) 0) t))
          (graphics (ebb-screen-graphics screen)))
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest args)
              (when (and (eq type 'pixel-size) (eq (car args) 'cell))
                '(10 . 20))))
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=16,v=32,i=1,C=1;%s\e\\" encoded))
      (let ((natural (car (ebb-graphics-state-placements graphics))))
        (should (= 2 (ebb-graphics-placement-columns natural)))
        (should (= 2 (ebb-graphics-placement-rows natural)))
        (should (= 16 (ebb-graphics-placement-pixel-width natural)))
        (should (= 32 (ebb-graphics-placement-pixel-height natural))))
      (ebb-test-output parser "\e_Ga=p,i=1,c=4,C=1;\e\\")
      (let ((by-columns (car (ebb-graphics-state-placements graphics))))
        (should (= 4 (ebb-graphics-placement-columns by-columns)))
        (should (= 4 (ebb-graphics-placement-rows by-columns)))
        (should (= 40 (ebb-graphics-placement-pixel-width by-columns)))
        (should (= 80 (ebb-graphics-placement-pixel-height by-columns))))
      (ebb-test-output parser "\e_Ga=p,i=1,c=4,r=1,C=1;\e\\")
      (let ((boxed (car (ebb-graphics-state-placements graphics))))
        (should (= 4 (ebb-graphics-placement-columns boxed)))
        (should (= 1 (ebb-graphics-placement-rows boxed)))
        (should (= 10 (ebb-graphics-placement-pixel-width boxed)))
        (should (= 20 (ebb-graphics-placement-pixel-height boxed))))
      ;; Partial final cells are represented by padding in the renderer's
      ;; cell-box-sized image rather than by changing the image aspect ratio.
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=16,v=28,i=2,C=1;%s\e\\"
                      (base64-encode-string (make-string (* 16 28 3) 0) t)))
      (let ((natural (car (ebb-graphics-state-placements graphics))))
        (should (= 2 (ebb-graphics-placement-columns natural)))
        (should (= 2 (ebb-graphics-placement-rows natural)))
        (should (= 16 (ebb-graphics-placement-pixel-width natural)))
        (should (= 28 (ebb-graphics-placement-pixel-height natural))))
      (ebb-test-output parser "\e_Ga=p,i=2,c=2,C=1;\e\\")
      (let ((snapped (car (ebb-graphics-state-placements graphics))))
        (should (= 2 (ebb-graphics-placement-columns snapped)))
        (should (= 2 (ebb-graphics-placement-rows snapped)))
        (should (= 20 (ebb-graphics-placement-pixel-width snapped)))
        (should (= 35 (ebb-graphics-placement-pixel-height snapped)))))))

(ert-deftest ebb-test-parse-kitty-zlib-payload ()
  "An o=z payload is inflated before validation and storage."
  (skip-unless (zlib-available-p))
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((compressed (unibyte-string 120 156 99 100 98 6 0 0 13 0 7))
          responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output
       parser (format "\e_Ga=t,f=24,s=1,v=1,i=5,o=z;%s\e\\"
                      (base64-encode-string compressed t)))
      (should (equal (unibyte-string 1 2 3)
                     (ebb-graphics-image-data
                      (gethash 5 (ebb-graphics-state-images
                                  (ebb-screen-graphics screen))))))
      (should (equal '("\e_Gi=5;OK\e\\") responses)))))

(ert-deftest ebb-test-kitty-zlib-output-is-bounded-before-allocation ()
  "Compressed data expanding past the image limit is rejected by the helper."
  (skip-unless (executable-find "python3"))
  (let ((ebb-kitty-graphics-image-limit 8)
        (compressed
         (with-temp-buffer
           (set-buffer-multibyte nil)
           (call-process "python3" nil t nil "-c"
                         (concat "import sys,zlib;"
                                 "sys.stdout.buffer.write(zlib.compress(b'x'*64))"))
           (buffer-string))))
    (should (equal '(error . "EFBIG:inflated image too large")
                   (ebb-graphics--inflate compressed)))))

(ert-deftest ebb-test-kitty-decoded-dimensions-are-bounded ()
  "Raw and PNG dimensions are rejected before backend image allocation."
  (let ((ebb-kitty-graphics-image-limit 16)
        (raw (make-hash-table :test #'eql))
        (png (make-hash-table :test #'eql)))
    (puthash ?f "32" raw)
    (puthash ?s "1000000" raw)
    (puthash ?v "1000000" raw)
    (should (equal '(error . "EFBIG:decoded image too large")
                   (ebb-graphics--prepare-image raw "x")))
    (puthash ?f "100" png)
    (let ((header (make-string 24 0)))
      (aset header 0 #x89)
      (setf (substring header 1 4) "PNG"
            (substring header 12 16) "IHDR")
      ;; 1000 x 1000, big-endian.
      (aset header 18 3) (aset header 19 232)
      (aset header 22 3) (aset header 23 232)
      (should (equal '(error . "EFBIG:decoded PNG too large")
                     (ebb-graphics--prepare-image png header))))))

(ert-deftest ebb-test-kitty-png-quota-charges-decoded-surface ()
  "Compressed PNG data is charged by decoded surface size."
  (let ((ebb-kitty-graphics-storage-limit 100)
        (state (ebb-graphics-create))
        (params (make-hash-table :test #'eql)))
    (puthash ?i "1" params)
    (should-not (ebb-graphics--store-image state params 100 10 10 "tiny"))
    (should (= 0 (ebb-graphics-state-byte-count state)))))

(ert-deftest ebb-test-kitty-oversized-replacement-is-atomic ()
  "A rejected replacement preserves old data, placements, and accounting."
  (let ((ebb-kitty-graphics-storage-limit 4)
        (state (ebb-graphics-create))
        (params (make-hash-table :test #'eql)))
    (puthash ?i "1" params)
    (should (ebb-graphics--store-image state params 24 1 1 "abc"))
    (let ((placement (make-ebb-graphics-placement
                      :image-id 1 :row 0 :column 0 :columns 1 :rows 1)))
      (push placement (ebb-graphics-state-placements state))
      (should-not (ebb-graphics--store-image state params 24 2 1 "abcde"))
      (should (equal "abc"
                     (ebb-graphics-image-data
                      (gethash 1 (ebb-graphics-state-images state)))))
      (should (eq placement (car (ebb-graphics-state-placements state))))
      (should (= 3 (ebb-graphics-state-byte-count state))))))

(ert-deftest ebb-test-parse-kitty-file-mediums ()
  "File mediums read regular files, delete temporary ones, refuse sensitive paths."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((ebb-kitty-graphics-allow-files t)
           (temporary (make-temp-file "tty-graphics-protocol-ebb"))
           (plain (make-temp-file "ebb-kitty-plain"))
           (data (unibyte-string 9 8 7 6 5 4))
           responses)
      (unwind-protect
          (progn
            (dolist (file (list temporary plain))
              (with-temp-file file
                (set-buffer-multibyte nil)
                (insert data)))
            (setf (ebb-parser-write-fn parser)
                  (lambda (response) (push response responses)))
            (ebb-test-output
             parser (format "\e_Ga=t,f=24,s=1,v=1,i=1,t=t,O=3;%s\e\\"
                            (base64-encode-string temporary t)))
            (should (equal (unibyte-string 6 5 4)
                           (ebb-graphics-image-data
                            (gethash 1 (ebb-graphics-state-images
                                        (ebb-screen-graphics screen))))))
            (should-not (file-exists-p temporary))
            (ebb-test-output
             parser (format "\e_Ga=q,f=24,s=2,v=1,i=2,t=f;%s\e\\"
                            (base64-encode-string plain t)))
            (should (file-exists-p plain))
            (should-not (gethash 2 (ebb-graphics-state-images
                                    (ebb-screen-graphics screen))))
            (ebb-test-output
             parser (format "\e_Ga=t,f=100,i=3,t=f;%s\e\\"
                            (base64-encode-string "/proc/self/status" t)))
            (should (equal '("\e_Gi=3;EPERM:refusing to read from a sensitive path\e\\"
                             "\e_Gi=2;OK\e\\"
                             "\e_Gi=1;OK\e\\")
                           responses)))
        (ignore-errors (delete-file temporary))
        (ignore-errors (delete-file plain))))))

(ert-deftest ebb-test-kitty-temporary-medium-rejects-ordinary-paths ()
  "The temporary-file medium cannot be relabeled to read an arbitrary file."
  (let ((plain (make-temp-file "ebb-kitty-plain")))
    (unwind-protect
        (ebb-test-with-screen (:width 20 :height 6)
          (let (responses)
            (with-temp-file plain
              (set-buffer-multibyte nil)
              (insert (unibyte-string 1 2 3)))
            (setf (ebb-parser-write-fn parser)
                  (lambda (response) (push response responses)))
            (ebb-test-output
             parser (format "\e_Ga=t,f=24,s=1,v=1,i=1,t=t;%s\e\\"
                            (base64-encode-string plain t)))
            (should (file-exists-p plain))
            (should-not (gethash 1 (ebb-graphics-state-images
                                    (ebb-screen-graphics screen))))
            (should (equal '("\e_Gi=1;EPERM:invalid temporary-file path\e\\")
                           responses))))
      (ignore-errors (delete-file plain)))))

(ert-deftest ebb-test-kitty-sensitive-path-resolves-symlinks ()
  "A /dev/shm-style exemption cannot hide a symlink to a sensitive file."
  (let ((link (make-temp-name
               (expand-file-name "ebb-kitty-sensitive-"
                                 temporary-file-directory))))
    (unwind-protect
        (progn
          (make-symbolic-link "/proc/self/status" link)
          (should (ebb-graphics--sensitive-path-p link)))
      (ignore-errors (delete-file link)))))

(ert-deftest ebb-test-kitty-temporary-file-symlink-to-sensitive-is-refused ()
  "A t=t path whose truename resolves to a sensitive area is refused."
  (let ((link (expand-file-name
               (make-temp-name "ebb-kitty-tty-graphics-protocol-")
               temporary-file-directory)))
    (unwind-protect
        (progn
          (make-symbolic-link "/proc/self/status" link)
          (ebb-test-with-screen (:width 20 :height 6)
            (let (responses)
              (setf (ebb-parser-write-fn parser)
                    (lambda (response) (push response responses)))
              (ebb-test-output
               parser (format "\e_Ga=q,f=24,s=1,v=1,i=11,t=t;%s\e\\"
                              (base64-encode-string link t)))
              (should (equal '("\e_Gi=11;EPERM:refusing to read from a sensitive path\e\\")
                             responses)))))
      (ignore-errors (delete-file link)))))

(ert-deftest ebb-test-parse-kitty-shared-memory-medium ()
  "The t=s medium reads and unlinks a local POSIX shared-memory object."
  (skip-unless (file-writable-p "/dev/shm"))
  (let* ((name (format "ebb-kitty-%d" (emacs-pid)))
         (path (expand-file-name name "/dev/shm"))
         (data (unibyte-string 1 2 3)))
    (unwind-protect
        (progn
          (with-temp-file path
            (set-buffer-multibyte nil)
            (insert data))
          (ebb-test-with-screen (:width 20 :height 6)
            (let (responses)
              (setf (ebb-parser-write-fn parser)
                    (lambda (response) (push response responses)))
              (ebb-test-output
               parser (format "\e_Ga=q,f=24,s=1,v=1,i=7,t=s;%s\e\\"
                              (base64-encode-string name t)))
              (should (equal '("\e_Gi=7;OK\e\\") responses))
              (should-not (file-exists-p path)))))
      (ignore-errors (delete-file path)))))

(ert-deftest ebb-test-parse-kitty-rejects-id-with-number ()
  "Specifying both i and I is an error, and query validates its payload."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=q,i=1,I=2;AAAA\e\\")
      (ebb-test-output parser "\e_Ga=q,i=3,f=24,s=2,v=2;AAAA\e\\")
      ;; q=1 silences OK, q=2 silences everything.
      (ebb-test-output parser "\e_Ga=q,i=4,f=24,s=1,v=1,q=1;AAAA\e\\")
      (ebb-test-output parser "\e_Ga=q,i=5,f=24,s=2,v=2,q=1;AAAA\e\\")
      (ebb-test-output parser "\e_Ga=q,i=6,f=24,s=2,v=2,q=2;AAAA\e\\")
      (should (equal '("\e_Gi=5;ENODATA:insufficient image data\e\\"
                       "\e_Gi=3;ENODATA:insufficient image data\e\\"
                       "\e_Gi=1,I=2;EINVAL:cannot specify both image id and number\e\\")
                     responses)))))

(ert-deftest ebb-test-parse-kitty-chunked-rgb-placement ()
  "Chunked raw image data is joined once and can create a placement."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((first (unibyte-string 1 2))
           (last (unibyte-string 3))
           responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses)))
      (ebb-test-output
       parser
       (format "\e_Ga=T,f=24,s=1,v=1,i=4,p=2,c=3,r=2,m=1;%s\e\\"
               (base64-encode-string first t)))
      (should (ebb-graphics-state-upload (ebb-screen-graphics screen)))
      (ebb-test-output
       parser
       (format "\e_Gm=0;%s\e\\" (base64-encode-string last t)))
      (let* ((graphics (ebb-screen-graphics screen))
             (image (gethash 4 (ebb-graphics-state-images graphics)))
             (placement (car (ebb-graphics-state-placements graphics))))
        (should (equal (unibyte-string 1 2 3)
                       (ebb-graphics-image-data image)))
        (should (= 4 (ebb-graphics-placement-image-id placement)))
        (should (= 2 (ebb-graphics-placement-placement-id placement)))
        (should (= 3 (ebb-graphics-placement-columns placement)))
        (should (= 2 (ebb-graphics-placement-rows placement))))
      (should (equal '("\e_Gi=4,p=2;OK\e\\") responses)))))

(ert-deftest ebb-test-kitty-new-transmission-aborts-partial-upload ()
  "A complete new transmission is not appended to an older partial image."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((graphics (ebb-screen-graphics screen)))
      (ebb-test-output parser "\e_Ga=t,f=24,s=2,v=1,i=1,m=1;AAAA\e\\")
      (should (ebb-graphics-state-upload graphics))
      (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,i=2;AQID\e\\")
      (should-not (ebb-graphics-state-upload graphics))
      (should-not (gethash 1 (ebb-graphics-state-images graphics)))
      (should (equal (unibyte-string 1 2 3)
                     (ebb-graphics-image-data
                      (gethash 2 (ebb-graphics-state-images graphics))))))))

(ert-deftest ebb-test-kitty-chunked-query-retains-first-chunk ()
  "Query validation joins all direct-upload chunks before replying."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=q,f=24,s=1,v=1,i=8,m=1;AQI=\e\\")
      (should (ebb-graphics-state-upload (ebb-screen-graphics screen)))
      (ebb-test-output parser "\e_Gm=0;Aw==\e\\")
      (should-not (ebb-graphics-state-upload (ebb-screen-graphics screen)))
      (should (equal '("\e_Gi=8;OK\e\\") responses)))))

(ert-deftest ebb-test-kitty-chunked-placement-uses-final-anchor ()
  "A chunked a=T command uses the final packet's cursor and cell geometry."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output
     parser "\e_Ga=T,f=24,s=1,v=1,i=8,c=2,r=1,m=1;AQI=\e\\")
    (ebb-screen-cursor-goto screen 4 7)
    (ebb-test-output parser "\e_Gm=0;Aw==\e\\")
    (let ((placement (car (ebb-graphics-state-placements
                           (ebb-screen-graphics screen)))))
      (should (= 4 (ebb-graphics-placement-row placement)))
      (should (= 7 (ebb-graphics-placement-column placement)))
      (should (equal '(9 . 4) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-kitty-can-aborts-upload-and-recovers ()
  "CAN inside an APC aborts the upload transaction and returns to text."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,i=1,m=1;AQI=\x18")
    (should-not (ebb-graphics-state-upload (ebb-screen-graphics screen)))
    (should (eq :ground (ebb-parser-state parser)))
    (ebb-test-output parser "X")
    (should (equal "X" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-kitty-duplicate-control-keys-are-rejected ()
  "Duplicate keys fail parsing without mutating graphics state."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output
       parser "\e_Ga=t,f=24,s=1,v=1,i=1,i=2;AQID\e\\")
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images (ebb-screen-graphics screen)))))
      ;; A malformed header cannot reliably provide identifiers for the reply.
      (should (equal '("\e_G;EINVAL:malformed control data\e\\")
                     responses)))))

(ert-deftest ebb-test-kitty-non-kitty-strings-clear-cursor-placement-state ()
  "SOS, PM, and non-Kitty APC strings cannot repeat stale cursor movement."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=3,r=2;AQID\e\\")
    (dolist (sequence '("\eXfoo\e\\" "\e^foo\e\\" "\e_foo\e\\"))
      (ebb-screen-cursor-goto screen 0 0)
      (ebb-test-output parser sequence)
      (should (equal '(0 . 0) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-kitty-invalid-numeric-control-data-is-rejected ()
  "Malformed numeric keys do not allocate images or produce success replies."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,i=abc;AQID\e\\")
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images (ebb-screen-graphics screen)))))
      (should (equal '("\e_G;EINVAL:invalid control data\e\\") responses)))))

(ert-deftest ebb-test-kitty-oversized-apc-replies-with-error ()
  "An oversized Kitty APC is rejected instead of silently disappearing."
  (let ((ebb-kitty-graphics-apc-limit 16384))
    (ebb-test-with-screen (:width 20 :height 6)
      (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output
       parser (concat "\e_Ga=q,i=9;" (make-string 17000 ?A) "\e\\"))
      (should (equal '("\e_Gi=9;EFBIG:APC payload too large\e\\") responses))
      ;; Placement identifiers survive truncation the same way image ids do.
      (ebb-test-output
       parser (concat "\e_Ga=q,i=9,p=7;" (make-string 17000 ?A) "\e\\"))
      (should (equal "\e_Gi=9,p=7;EFBIG:APC payload too large\e\\"
                       (car responses)))
      (ebb-test-output parser "X")
        (should (equal "X" (ebb-test-display-line screen 0)))))))

(ert-deftest ebb-test-kitty-apc-ignores-c0-bytes ()
  "C0 controls embedded in an APC are not included in its base64 payload."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e_Ga=q,f=24,s=1,v=1,i=3;AA\aAA\e\\")
      (should (equal '("\e_Gi=3;OK\e\\") responses)))))

(ert-deftest ebb-test-kitty-zero-cell-size-uses-fallback ()
  "A custom pixel-size callback returning zero cannot drop a placement."
  (ebb-test-with-screen (:width 20 :height 6)
    (setf (ebb-parser-emit-fn parser)
          (lambda (type &rest _)
            (when (eq type 'pixel-size) '(0 . 0))))
    (ebb-test-output parser "\e_Ga=T,f=24,s=1,v=1,i=1;AQID\e\\")
    (should (ebb-graphics-state-placements (ebb-screen-graphics screen)))))

(ert-deftest ebb-test-kitty-remote-file-media-is-disabled ()
  "A remote child cannot make the local Emacs host read a file."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((ebb-kitty-graphics-allow-files t)
          responses)
      (setf (ebb-parser-graphics-file-media-enabled parser) nil
            (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output
       parser (format "\e_Ga=q,f=100,i=4,t=f;%s\e\\"
                      (base64-encode-string "/tmp/image.png" t)))
      (should (equal
               '("\e_Gi=4;EPERM:file media disabled for remote terminals\e\\")
               responses)))))

(ert-deftest ebb-test-kitty-negative-z-index-is-rejected ()
  "Unsupported under-text placements fail rather than replacing the text."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser
                       "\e_Ga=T,f=24,s=1,v=1,i=1,z=-1;AQID\e\\")
      (should-not (ebb-graphics-state-placements (ebb-screen-graphics screen)))
      (should (equal
               '("\e_Gi=1;EINVAL:invalid placement parameters\e\\")
               responses)))))

(ert-deftest ebb-test-kitty-quota-keeps-referenced-images ()
  "Quota eviction never blanks an image referenced by a placement."
  (let ((ebb-kitty-graphics-storage-limit 4)
        (state (ebb-graphics-create)))
    (let ((first (make-hash-table :test #'eql))
          (second (make-hash-table :test #'eql)))
      (puthash ?i "1" first)
      (puthash ?i "2" second)
      (should (ebb-graphics--store-image state first 24 1 1 "abc"))
      (push (make-ebb-graphics-placement :image-id 1 :row 0 :rows 1)
            (ebb-graphics-state-placements state))
      (should-not (ebb-graphics--store-image state second 24 1 1 "def"))
      (should (gethash 1 (ebb-graphics-state-images state))))))

(ert-deftest ebb-test-kitty-placement-resources-are-bounded ()
  "Surface dimensions and retained placement count have independent limits."
  (let ((ebb-kitty-graphics-placement-limit 2)
        (ebb-kitty-graphics-surface-limit 4096))
    (ebb-test-with-screen (:width 20 :height 6)
      (let (responses)
        (setf (ebb-parser-write-fn parser)
              (lambda (response) (push response responses)))
        (ebb-test-output
         parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=1000,r=1000;AQID\e\\")
        (should-not (gethash 1 (ebb-graphics-state-images
                                (ebb-screen-graphics screen))))
        (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,i=2;AQID\e\\")
        (ebb-test-output parser "\e_Ga=p,i=2,C=1;\e\\")
        (ebb-test-output parser "\e_Ga=p,i=2,C=1;\e\\")
        (ebb-test-output parser "\e_Ga=p,i=2,C=1;\e\\")
        (should (= 2 (length (ebb-graphics-state-placements
                              (ebb-screen-graphics screen)))))
        (should (string-match-p "EINVAL:invalid placement parameters"
                                (car responses)))))))

(ert-deftest ebb-test-kitty-failed-display-replacement-is-atomic ()
  "An invalid a=T placement cannot replace existing image data or placements."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((graphics (ebb-screen-graphics screen))
           (old-placement nil))
      (ebb-test-output parser "\e_Ga=T,f=24,s=1,v=1,i=1,C=1;AQID\e\\")
      (setq old-placement (car (ebb-graphics-state-placements graphics)))
      (ebb-test-output parser "\e_Ga=T,f=24,s=1,v=1,i=1,z=-1;BAUG\e\\")
      (should (equal (unibyte-string 1 2 3)
                     (ebb-graphics-image-data
                      (gethash 1 (ebb-graphics-state-images graphics)))))
      (should (eq old-placement (car (ebb-graphics-state-placements graphics)))))))

(ert-deftest ebb-test-kitty-noop-scroll-keeps-generation ()
  "Scrolls that cannot move a placement do not invalidate renderer caches."
  (let* ((state (ebb-graphics-create))
         (virtual (make-ebb-graphics-placement :image-id 1 :virtual t)))
    (setf (ebb-graphics-state-placements state) (list virtual))
    (ebb-graphics-scroll state 'up 0 10 1)
    (should (= 0 (ebb-graphics-state-generation state)))))

(ert-deftest ebb-test-kitty-invalid-placement-parameters-are-atomic ()
  "Placement validation fails before image storage or lookup side effects."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((encoded (base64-encode-string (unibyte-string 1 2 3) t))
          responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=1,v=1,i=1,C=2;%s\e\\" encoded))
      (should-not (gethash 1 (ebb-graphics-state-images
                              (ebb-screen-graphics screen))))
      (ebb-test-output parser "\e_Ga=p,i=1,C=2;\e\\")
      (should (equal '("\e_Gi=1;EINVAL:invalid control data\e\\"
                       "\e_Gi=1;EINVAL:invalid control data\e\\")
                     responses)))))

(ert-deftest ebb-test-kitty-unsupported-geometry-is-rejected ()
  "Unsupported source rectangles, offsets, and relative placement never ACK."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (dolist (control '("x=1" "y=1" "w=1" "h=1" "X=1" "Y=1"
                         "P=1" "Q=1" "H=1" "V=1"))
        (ebb-test-output
         parser (format "\e_Ga=T,f=24,s=1,v=1,i=1,%s;AQID\e\\"
                        control)))
      (should (= 10 (length responses)))
      (should (cl-every
               (lambda (response)
                 (string-match-p "EINVAL:invalid control data" response))
               responses))
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images
                     (ebb-screen-graphics screen))))))))

(ert-deftest ebb-test-kitty-non-transmit-command-aborts-upload ()
  "An interleaved placement command discards an incomplete direct upload."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((graphics (ebb-screen-graphics screen)))
      (ebb-test-output parser "\e_Ga=t,f=24,s=1,v=1,i=1,m=1;AQ==\e\\")
      (should (ebb-graphics-state-upload graphics))
      (ebb-test-output parser "\e_Ga=p,i=99;\e\\")
      (should-not (ebb-graphics-state-upload graphics)))))

(ert-deftest ebb-test-kitty-placement-cursor-policy ()
  "Kitty placements move the cursor unless C=1 suppresses movement."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((encoded (base64-encode-string (unibyte-string 1 2 3) t)))
      (ebb-test-output
       parser
       (format "\e_Ga=T,f=24,s=1,v=1,i=1,c=3,r=2;%s\e\\" encoded))
      ;; Like kitty: past the right edge, on the placement's last row.
      (should (equal '(3 . 1) (ebb-test-cursor screen)))
      (ebb-screen-cursor-goto screen 0 0)
      (ebb-test-output parser "\e_Ga=p,i=1,c=2,r=1,C=1;\e\\")
      (should (equal '(0 . 0) (ebb-test-cursor screen))))))

(ert-deftest ebb-test-kitty-placements-scroll-and-clip-with-region ()
  "Graphics placements follow terminal scrolling and retain slice offsets."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-screen-enter-alt screen)
    (let* ((graphics (ebb-screen-graphics screen))
           (placement (make-ebb-graphics-placement
                       :image-id 1 :row 1 :column 0 :columns 2 :rows 3)))
      (setf (ebb-graphics-state-placements graphics) (list placement))
      (ebb-screen-scroll screen 'up 2)
      (should (= 0 (ebb-graphics-placement-row placement)))
      (should (= 2 (ebb-graphics-placement-rows placement)))
      (should (= 1 (ebb-graphics-placement-row-offset placement))))))

(ert-deftest ebb-test-render-kitty-clipping-preserves-backing-canvas ()
  "Clipping visible rows does not shrink the image object's backing canvas."
  (let* ((screen (ebb-screen-create 4 2))
         (render (make-ebb-render-state
                  :screen screen
                  :graphics-cache (make-hash-table :test #'equal)
                  :graphics-cache-sizes (make-hash-table :test #'equal)))
         (image (make-ebb-graphics-image
                 :id 1 :format 24 :width 1 :height 1 :data "abc"))
         (placement (make-ebb-graphics-placement
                     :image-id 1 :columns 2 :rows 2
                     :box-columns 2 :box-rows 3
                     :pixel-width 8 :pixel-height 48 :row-offset 1))
         svg)
    (cl-letf (((symbol-function 'ebb-render-cell-pixel-size)
               (lambda (_) '(8 . 16)))
              ((symbol-function 'ebb-render--graphics-background)
               (lambda () '(0 0 0)))
              ((symbol-function 'ebb-render--graphics-raw-png-helper)
               (lambda (_) "png"))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (data &rest _)
                 (setq svg data)
                 'mock-image)))
      (ebb-render--graphics-image-object render image placement))
    (should (string-match-p "height='48'" svg))))

(ert-deftest ebb-test-kitty-main-scroll-preserves-history-anchor ()
  "A placement entering main-screen history keeps its combined row anchor."
  (ebb-test-with-screen (:width 4 :height 2)
    (let* ((data (unibyte-string 1 2 3))
           (encoded (base64-encode-string data t)))
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=1,v=1,i=1,c=1,r=1;%s\e\\" encoded))
      (let ((placement (car (ebb-graphics-state-placements
                             (ebb-screen-graphics screen)))))
        (should (= 0 (ebb-graphics-placement-row placement)))
        (ebb-test-output parser "A\r\nB\r\n")
        (should (> (ebb-screen-history-row-count screen) 0))
        (should (= 0 (ebb-graphics-placement-row placement)))))))

(ert-deftest ebb-test-kitty-delete-row-selectors-use-viewport-coordinates ()
  "The y selector is offset by scrollback rather than deleting history rows."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "A\r\nB\r\n")
    (let* ((graphics (ebb-screen-graphics screen))
           (base (ebb-screen-history-row-count screen))
           (history (make-ebb-graphics-placement
                     :image-id 1 :row 0 :column 0 :columns 1 :rows 1))
           (viewport (make-ebb-graphics-placement
                      :image-id 2 :row base :column 0 :columns 1 :rows 1)))
      (setf (ebb-graphics-state-placements graphics)
            (list viewport history))
      (ebb-test-output parser "\e_Ga=d,d=y,y=1;\e\\")
      (should (equal (list history)
                     (ebb-graphics-state-placements graphics))))))

(ert-deftest ebb-test-kitty-delete-all-keeps-scrollback-placements ()
  "The d=a selector removes only placements visible in the viewport."
  (ebb-test-with-screen (:width 4 :height 2)
    (ebb-test-output parser "A\r\nB\r\n")
    (let* ((graphics (ebb-screen-graphics screen))
           (base (ebb-screen-history-row-count screen))
           (history (make-ebb-graphics-placement
                     :image-id 1 :row 0 :column 0 :columns 1 :rows 1))
           (viewport (make-ebb-graphics-placement
                      :image-id 2 :row base :column 0 :columns 1 :rows 1)))
      (setf (ebb-graphics-state-placements graphics)
            (list viewport history))
      (ebb-test-output parser "\e_Ga=d,d=a;\e\\")
      (should (equal (list history)
                     (ebb-graphics-state-placements graphics))))))

(ert-deftest ebb-test-kitty-top-region-scroll-keeps-history-graphics ()
  "A top-anchored partial scroll preserves anchors entering history."
  (ebb-test-with-screen (:width 4 :height 4)
    (let* ((graphics (ebb-screen-graphics screen))
           (top (make-ebb-graphics-placement
                 :image-id 1 :row 0 :column 0 :columns 1 :rows 1))
           (below (make-ebb-graphics-placement
                   :image-id 2 :row 3 :column 0 :columns 1 :rows 1))
           (crossing (make-ebb-graphics-placement
                      :image-id 3 :row 1 :column 0 :columns 1 :rows 2)))
      (setf (ebb-graphics-state-placements graphics)
            (list below crossing top)
            (ebb-screen-scroll-top screen) 0
            (ebb-screen-scroll-bottom screen) 1)
      (ebb-screen-scroll screen 'up 1)
      (should (= 0 (ebb-graphics-placement-row top)))
      (should (= 4 (ebb-graphics-placement-row below)))
      (should-not (memq crossing
                        (ebb-graphics-state-placements graphics))))))

(ert-deftest ebb-test-kitty-main-and-alt-screens-have-separate-state ()
  "Alternate-screen graphics start empty and main graphics return on exit."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((main (ebb-screen-graphics screen)))
      (puthash 1 (make-ebb-graphics-image :id 1 :format 100 :data "main")
               (ebb-graphics-state-images main))
      (ebb-screen-enter-alt screen)
      (should-not (eq main (ebb-screen-graphics screen)))
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images (ebb-screen-graphics screen)))))
      (puthash 2 (make-ebb-graphics-image :id 2 :format 100 :data "alt")
               (ebb-graphics-state-images (ebb-screen-graphics screen)))
      (ebb-screen-leave-alt screen)
      (should (eq main (ebb-screen-graphics screen)))
      (should (gethash 1 (ebb-graphics-state-images main)))
      (should-not (gethash 2 (ebb-graphics-state-images main))))))

(ert-deftest ebb-test-kitty-main-and-alt-share-storage-budget ()
  "Main and alternate image stores cannot each consume the full quota."
  (let ((ebb-kitty-graphics-storage-limit 4))
    (ebb-test-with-screen (:width 20 :height 6)
      (let ((params (make-hash-table :test #'eql))
            (main (ebb-screen-graphics screen)))
        (puthash ?i "1" params)
        (should (ebb-graphics--store-image main params 24 1 1 "abc"))
        (ebb-screen-enter-alt screen)
        (puthash ?i "2" params)
        (should-not
         (ebb-graphics--store-image
          (ebb-screen-graphics screen) params 24 1 1 "def"))
        (should (= 3 (ebb-graphics-state-byte-count main)))
        (ebb-screen-leave-alt screen)
        (should (gethash 1 (ebb-graphics-state-images main)))
        (should (= 3 (ebb-graphics-state-byte-count main)))))))

(ert-deftest ebb-test-kitty-alt-screen-round-trip-restores-budget ()
  "A successful alternate-screen store is fully refunded on exit."
  (let ((ebb-kitty-graphics-storage-limit 10))
    (ebb-test-with-screen (:width 20 :height 6)
      (let ((params (make-hash-table :test #'eql))
            (main (ebb-screen-graphics screen)))
        (puthash ?i "1" params)
        (should (ebb-graphics--store-image main params 24 1 1 "abc"))
        (ebb-screen-enter-alt screen)
        (puthash ?i "2" params)
        (should (ebb-graphics--store-image
                  (ebb-screen-graphics screen) params 24 1 1 "def"))
        (should (= 6 (ebb-graphics-state-byte-count main)))
        (ebb-screen-leave-alt screen)
        (should (gethash 1 (ebb-graphics-state-images main)))
        (should (= 3 (ebb-graphics-state-byte-count main)))))))

(ert-deftest ebb-test-kitty-eviction-is-per-screen ()
  "One screen never evicts images stored by another screen.
The shared quota is conservative: a full main screen leaves an
 alternate-screen upload with ENOSPC instead of deleting main's
 unreferenced images, which a sibling screen may still display."
  (let ((ebb-kitty-graphics-storage-limit 4))
    (ebb-test-with-screen (:width 20 :height 6)
      (let ((params (make-hash-table :test #'eql))
            (main (ebb-screen-graphics screen)))
        (puthash ?i "1" params)
        (should (ebb-graphics--store-image main params 24 1 1 "abc"))
        (ebb-screen-enter-alt screen)
        (let ((alt (ebb-screen-graphics screen)))
          (puthash ?i "2" params)
          (should-not (ebb-graphics--store-image alt params 24 1 1 "def"))
          (should (= 0 (hash-table-count
                          (ebb-graphics-state-images alt))))
          (should (gethash 1 (ebb-graphics-state-images main))))
        (ebb-screen-leave-alt screen)
        (should (= 3 (ebb-graphics-state-byte-count main)))))))

(ert-deftest ebb-test-render-kitty-layout-cache-is-bounded ()
  "Scrolling without graphics changes cannot grow the layout cache."
  (let ((ebb-kitty-graphics-layout-cache-limit 4))
    (ebb-test-with-screen (:width 6 :height 2)
      (let ((render (make-ebb-render-state
                     :screen screen
                     :graphics-cache (make-hash-table :test #'equal)
                     :graphics-generation -1)))
        (ebb-test-output
         parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=1,r=1,C=1;AQID\e\\")
        (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                  ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                  ((symbol-function 'frame-char-height) (lambda (&rest _) 16)))
          (dotimes (_ 12)
            (ebb-test-output parser "x\r\n")
            (ebb-render--apply-graphics render 1 "      "))
          (should (<= (hash-table-count
                       (ebb-render-state-graphics-layout-cache render))
                      4))
          ;; An evicted row intersecting the placement recomputes correctly.
          (ebb-render--apply-graphics render 0 "      " t)
          (let ((owners (gethash (cons 0 6)
                                 (ebb-render-state-graphics-layout-cache render))))
            (should (= 1 (ebb-graphics-placement-image-id (aref owners 0))))))))))

(ert-deftest ebb-test-render-kitty-screen-switch-invalidates-layout-cache ()
  "Equal generations on distinct main/alternate states cannot share owners."
  (ebb-test-with-screen (:width 6 :height 2)
    (let ((render (make-ebb-render-state
                   :screen screen
                   :graphics-cache (make-hash-table :test #'equal)
                   :graphics-generation -1)))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=1,r=1,C=1;AQID\e\\")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (ebb-render--apply-graphics render 0 "      ")
        (ebb-screen-enter-alt screen)
        (ebb-test-output parser "\e[4G")
        (ebb-test-output
         parser "\e_Ga=T,f=24,s=1,v=1,i=2,c=1,r=1,C=1;AQID\e\\")
        (ebb-render--apply-graphics render 0 "      ")
        (ebb-screen-leave-alt screen)
        (ebb-render--apply-graphics render 0 "      ")
        (let ((owners (gethash (cons 0 6)
                               (ebb-render-state-graphics-layout-cache render))))
          (should (= 1 (ebb-graphics-placement-image-id (aref owners 0))))
          (should-not (aref owners 3)))))))

(ert-deftest ebb-test-kitty-delete-selectors-and-data-lifetime ()
  "Lowercase deletes retain image data; uppercase deletes may free it."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((encoded (base64-encode-string (unibyte-string 1 2 3) t))
           (graphics (ebb-screen-graphics screen)))
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=1,v=1,i=1,p=7;%s\e\\" encoded))
      (ebb-test-output parser "\e_Ga=d,d=i,i=1,p=7;\e\\")
      (should-not (ebb-graphics-state-placements graphics))
      (should (gethash 1 (ebb-graphics-state-images graphics)))
      (ebb-test-output parser "\e_Ga=p,i=1,p=8,C=1;\e\\")
      (should (ebb-graphics-state-placements graphics))
      (ebb-test-output parser "\e_Ga=d,d=I,i=1;\e\\")
      (should-not (gethash 1 (ebb-graphics-state-images graphics)))
      (should-not (ebb-graphics-state-placements graphics)))))

(ert-deftest ebb-test-kitty-delete-selector-coverage ()
  "Coordinate, z-index, placement, and image-range selectors are covered."
  (cl-labels
      ((make-state
        ()
        (let ((state (ebb-graphics-create)))
          (setf (ebb-graphics-state-image-order state) '(2 1)
                (ebb-graphics-state-placements state)
                (list
                 (make-ebb-graphics-placement
                  :image-id 2 :placement-id 22 :row 2 :column 2
                  :columns 2 :rows 2 :z-index 2)
                 (make-ebb-graphics-placement
                  :image-id 1 :placement-id 11 :row 0 :column 0
                  :columns 2 :rows 2 :z-index 1)
                 (make-ebb-graphics-placement
                  :image-id 1 :placement-id 11 :virtual t
                  :columns 2 :rows 2 :z-index 1)))
          state))
       (apply-delete
        (command &optional row column)
        (let* ((state (make-state))
               (params (ebb-graphics--parse-params command)))
          (ebb-graphics--handle-delete
           state params (or row 0) (or column 0) 0 10 10)
          (ebb-graphics-state-placements state))))
    (dolist (case '(("d=c" 0 0)
                    ("d=p,i=1,p=11" 0 0)
                    ("d=p,x=1,y=1" 0 0)
                    ("d=q,x=1,y=1,z=1" 0 0)
                    ("d=x,x=1" 0 0)
                    ("d=y,y=1" 0 0)
                    ("d=z,z=1" 0 0)))
      (let ((remaining
             (apply-delete (nth 0 case) (nth 1 case) (nth 2 case))))
        (should (= 2 (length remaining)))
        (should (cl-find-if #'ebb-graphics-placement-virtual remaining))))
    ;; Image ranges intentionally include virtual placements.
    (let ((remaining (apply-delete "d=r,x=1,y=1")))
      (should (= 1 (length remaining)))
      (should (= 2 (ebb-graphics-placement-image-id (car remaining)))))
    ;; Protocol coordinates are one-based and invalid values are not clamped.
    (should (= 3 (length (apply-delete "d=x,x=0"))))
    ;; Irrelevant coordinate keys do not suppress non-coordinate selectors.
    (should (= 1 (length (apply-delete "d=a,x=0,y=0"))))
    (let ((remaining (apply-delete "d=i,i=1,x=0,y=0")))
      (should (= 1 (length remaining)))
      (should (= 2 (ebb-graphics-placement-image-id (car remaining)))))))

(ert-deftest ebb-test-kitty-deleting-non-first-placement-bumps-generation ()
  "Deleting a placement from the middle invalidates renderer caches."
  (let* ((state (ebb-graphics-create))
         (first (make-ebb-graphics-placement :image-id 1 :placement-id 1))
         (middle (make-ebb-graphics-placement :image-id 2 :placement-id 2))
         (last (make-ebb-graphics-placement :image-id 3 :placement-id 3)))
    (setf (ebb-graphics-state-placements state) (list first middle last))
    (let ((generation (ebb-graphics-state-generation state)))
      (ebb-graphics--remove-placements
       state (lambda (placement) (eq placement middle)) nil)
      (should (= (1+ generation) (ebb-graphics-state-generation state)))
      (should (equal (list first last)
                     (ebb-graphics-state-placements state))))))

(ert-deftest ebb-test-kitty-ed2-clears-placements-but-retains-images ()
  "ED 2 removes visible graphics without discarding reusable image data."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((graphics (ebb-screen-graphics screen)))
      (puthash 1 (make-ebb-graphics-image :id 1 :format 100 :data "png")
               (ebb-graphics-state-images graphics))
      (setf (ebb-graphics-state-placements graphics)
            (list (make-ebb-graphics-placement :image-id 1 :row 0 :rows 1)))
      (ebb-screen-erase-in-display screen 2)
      ;; ED 2 preserves the old viewport in Emacs-mode history, including its
      ;; image placement, while the new viewport is clear.
      (should (= 1 (length (ebb-graphics-state-placements graphics))))
      (should (> (ebb-screen-history-row-count screen) 0))
      (should (gethash 1 (ebb-graphics-state-images graphics))))))

(ert-deftest ebb-test-kitty-partial-ed-leaves-independent-placements ()
  "ED 0 and ED 1 erase text without deleting Kitty placement objects."
  (dolist (mode '(0 1))
    (ebb-test-with-screen (:width 10 :height 3)
      (let* ((graphics (ebb-screen-graphics screen))
             (placement (make-ebb-graphics-placement
                         :image-id 1 :row 0 :column 0 :columns 2 :rows 2)))
        (setf (ebb-graphics-state-placements graphics) (list placement))
        (ebb-screen-erase-in-display screen mode)
        (should (eq placement
                    (car (ebb-graphics-state-placements graphics))))))))

(ert-deftest ebb-test-parse-winops-reports-pixel-geometry ()
  "XTWINOPS pixel queries use geometry supplied by the display callback."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser) (lambda (response) (push response responses))
            (ebb-parser-emit-fn parser)
            (lambda (type kind)
              (when (eq type 'pixel-size)
                (pcase kind ('text-area '(800 . 480)) ('cell '(10 . 20))))))
      (ebb-test-output parser "\e[14t\e[16t")
      (should (equal '("\e[6;20;10t" "\e[4;480;800t") responses)))))

(ert-deftest ebb-test-parse-winops-pixel-geometry-reports-unknown ()
  "XTWINOPS pixel queries report zero rather than invented dimensions."
  (ebb-test-with-screen (:width 20 :height 6)
    (let (responses)
      (setf (ebb-parser-write-fn parser)
            (lambda (response) (push response responses)))
      (ebb-test-output parser "\e[14t\e[16t")
      (should (equal '("\e[6;0;0t" "\e[4;0;0t") responses)))))

(ert-deftest ebb-test-render-kitty-placement-as-row-slices ()
  "Static Kitty placements render as cell-sized image slices per row."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((data (unibyte-string 1 2 3))
           (encoded (base64-encode-string data t))
           (render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser
       (format "\e_Ga=T,f=24,s=1,v=1,i=4,c=3,r=2;%s\e\\" encoded))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p)
                 (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (let ((top (ebb-render--apply-graphics render 0 ""))
              (bottom (ebb-render--apply-graphics render 1 "")))
          (should (= 3 (length top)))
          (should (equal '((slice 0 0 24 16) mock-image)
                         (get-text-property 0 'display top)))
          (should (equal '((slice 0 16 24 16) mock-image)
                         (get-text-property 0 'display bottom))))))))

(ert-deftest ebb-test-render-kitty-slices-have-real-cell-box-geometry ()
  "GUI redisplay gives every requested row the complete placement width."
  (skip-unless (and (display-graphic-p)
                    (image-type-available-p 'svg)
                    (fboundp 'string-pixel-width)))
  (ebb-test-with-screen (:width 10 :height 3)
    (let* ((render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1))
           (cell-width (frame-char-width)))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=3,r=2,C=1;AQID\e\\")
      (let* ((top (ebb-render--apply-graphics render 0 "   "))
             (bottom (ebb-render--apply-graphics render 1 "   "))
             (object (cadr (get-text-property 0 'display top))))
        (should (equal (cons (* 3 cell-width) (* 2 (frame-char-height)))
                       (image-size object t)))
        (should (= (* 3 cell-width) (string-pixel-width top)))
        (should (= (* 3 cell-width) (string-pixel-width bottom)))))))

(ert-deftest ebb-test-render-kitty-static-placement-follows-combining-cell ()
  "Static placement columns are mapped past combining characters."
  (ebb-test-with-screen (:width 6 :height 2)
    (let ((render (make-ebb-render-state
                   :screen screen
                   :graphics-cache (make-hash-table :test #'equal)
                   :graphics-generation -1)))
      (ebb-test-output parser "e\u0301")
      (ebb-test-output parser "\e_Ga=T,f=24,s=1,v=1,i=1,C=1;AQID\e\\")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (let* ((line (ebb-screen-get-line screen 0))
               (string (ebb-render--line-to-string line 6))
               (row (ebb-render--apply-graphics render 0 string)))
          (should-not (get-text-property 1 'display row))
          (should (get-text-property 2 'display row)))))))

(ert-deftest ebb-test-render-kitty-static-placement-follows-wide-scrollback-cell ()
  "Static placement columns map through compact wide history characters."
  (ebb-test-with-screen (:width 6 :height 2)
    (ebb-test-output parser "中\r\nx\r\n")
    (let* ((graphics (ebb-screen-graphics screen))
           (image (make-ebb-graphics-image
                   :id 1 :format 24 :width 1 :height 1
                   :data (unibyte-string 1 2 3)))
           (placement (make-ebb-graphics-placement
                       :image-id 1 :row 0 :column 2 :columns 1 :rows 1
                       :pixel-width 8 :pixel-height 16))
           (render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1))
           (line (ebb-screen-history-render-row screen 0))
           (string (ebb-render--line-to-string-scrollback line 6)))
      (puthash 1 image (ebb-graphics-state-images graphics))
      (setf (ebb-graphics-state-placements graphics) (list placement))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (let ((row (ebb-render--apply-graphics render 0 string t)))
          (should (get-text-property 1 'display row)))))))

(ert-deftest ebb-test-render-kitty-placeholder-renders-in-scrollback ()
  "A diacritic-addressed Unicode placeholder keeps its image in history."
  (ebb-test-with-screen (:width 4 :height 2)
    (let* ((placeholder
            (string #x10eeee
                    (aref ebb-render--placeholder-diacritics 0)
                    (aref ebb-render--placeholder-diacritics 0)))
           (render (make-ebb-render-state
                    :screen screen
                    :history-cache (make-hash-table :test #'equal)
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=42,U=1,c=1,r=1,C=1;AQID\e\\")
      (ebb-test-output parser (concat "\e[38;2;0;0;42m" placeholder
                                      "\e[39m\r\nx\r\n"))
      (should (> (ebb-screen-history-row-count screen) 0))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (let ((history (ebb-render--history-row-string render 0)))
          (should (get-text-property 0 'display history)))))))

(ert-deftest ebb-test-render-kitty-placeholder-inherits-row-in-scrollback ()
  "Compact placeholder cells inherit omitted coordinates in history."
  (ebb-test-with-screen (:width 4 :height 2)
    (let* ((first
            (string #x10eeee
                    (aref ebb-render--placeholder-diacritics 0)
                    (aref ebb-render--placeholder-diacritics 0)))
           (placeholder (string #x10eeee))
           (render (make-ebb-render-state
                    :screen screen
                    :history-cache (make-hash-table :test #'equal)
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=42,U=1,c=2,r=1,C=1;AQID\e\\")
      (ebb-test-output
       parser (concat "\e[38;2;0;0;42m" first placeholder
                      "\e[39m\r\nx\r\n"))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image))
                ((symbol-function 'ebb-render--graphics-raw-png-helper)
                 (lambda (_) "png")))
        (let* ((history (ebb-render--history-row-string render 0))
               (second (string-search placeholder history 1)))
          (should (get-text-property 0 'display history))
          (should second)
          (should (get-text-property second 'display history)))))))

(ert-deftest ebb-test-render-kitty-raw-images-use-png-in-svg ()
  "Raw RGB/RGBA sources are embedded using a librsvg-supported PNG URI."
  (skip-unless (executable-find "python3"))
  (let* ((screen (ebb-screen-create 2 1))
         (render (make-ebb-render-state
                  :screen screen
                  :graphics-cache (make-hash-table :test #'equal)
                  :graphics-cache-sizes (make-hash-table :test #'equal)))
         (image (make-ebb-graphics-image
                 :id 1 :format 24 :width 1 :height 1
                 :data (unibyte-string 255 0 0)))
         (placement (make-ebb-graphics-placement
                     :image-id 1 :columns 1 :rows 1
                     :pixel-width 8 :pixel-height 16))
         svg)
    (cl-letf (((symbol-function 'ebb-render-cell-pixel-size)
               (lambda (_) '(8 . 16)))
              ((symbol-function 'ebb-render--graphics-background)
               (lambda () '(0 0 0)))
              ((symbol-function 'image-type-available-p) (lambda (_) t))
              ((symbol-function 'create-image)
               (lambda (data &rest _)
                 (setq svg data)
                 'mock-image)))
      (ebb-render--graphics-image-object render image placement))
    (should (string-search "data:image/png;base64," svg))
    (should-not (string-search "image/x-portable-pixmap" svg))))

(ert-deftest ebb-test-render-kitty-placeholder-decodes-high-image-id-byte ()
  "The third placeholder diacritic contributes the image ID's high byte."
  (let* ((cell (make-ebb-cell
                :char #x10eeee :width 1
                :combining
                (string (aref ebb-render--placeholder-diacritics 1)
                        (aref ebb-render--placeholder-diacritics 2)
                        (aref ebb-render--placeholder-diacritics 7))))
         (decoded (ebb-render--placeholder-coordinates cell)))
    (should (equal '(1 2 7) decoded))))

(ert-deftest ebb-test-render-kitty-overlapping-placements ()
  "A newer placement covers an older one; the visible remainder keeps its offset."
  (ebb-test-with-screen (:width 20 :height 6)
    (let* ((data (unibyte-string 1 2 3))
           (encoded (base64-encode-string data t))
           (render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=1,v=1,i=1,c=6,r=1,C=1;%s\e\\" encoded))
      (ebb-test-output parser "\e[3G")
      (ebb-test-output
       parser (format "\e_Ga=T,f=24,s=2,v=1,i=2,c=2,r=1,z=1,C=1;%s\e\\"
                      (base64-encode-string (unibyte-string 1 2 3 4 5 6) t)))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p)
                 (lambda (&rest _) t))
                ((symbol-function 'create-image)
                 (lambda (data &rest _) (intern (format "image-%d" (length data))))))
        (let ((row (ebb-render--apply-graphics render 0 "")))
          (should (= 6 (length row)))
          (let ((left (get-text-property 0 'display row))
                (middle (get-text-property 2 'display row))
                (right (get-text-property 4 'display row)))
            (should (equal '(slice 0 0 16 16) (car left)))
          (should (equal '(slice 0 0 16 16) (car middle)))
          (should (equal '(slice 32 0 16 16) (car right)))
            (should (eq (cadr left) (cadr right)))
            (should-not (eq (cadr left) (cadr middle)))))))))

(ert-deftest ebb-test-render-kitty-generation-keeps-image-object-cache ()
  "Moving placements does not recomposite unchanged image data."
  (ebb-test-with-screen (:width 10 :height 3)
    (let* ((render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1))
           (creates 0))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=1,r=1,C=1;AQID\e\\")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image)
                 (lambda (&rest _)
                   (cl-incf creates)
                   'mock-image)))
        (ebb-render--apply-graphics render 0 " ")
        (cl-incf (ebb-graphics-state-generation
                  (ebb-screen-graphics screen)))
        (ebb-render--apply-graphics render 0 " ")
        (should (= 1 creates))))))

(ert-deftest ebb-test-render-kitty-object-cache-has-byte-budget ()
  "Scaled image objects are evicted under the configured render-cache budget."
  (let* ((ebb-kitty-graphics-render-cache-limit 1000)
         (screen (ebb-screen-create 4 2))
         (render (make-ebb-render-state
                  :screen screen
                  :graphics-cache (make-hash-table :test #'equal)
                  :graphics-cache-sizes (make-hash-table :test #'equal)))
         (placement (make-ebb-graphics-placement
                     :image-id 1 :columns 1 :rows 1
                     :pixel-width 8 :pixel-height 16))
         (first (make-ebb-graphics-image
                 :id 1 :format 24 :width 1 :height 1 :data "abc"))
         (second (make-ebb-graphics-image
                  :id 2 :format 24 :width 1 :height 1 :data "def"))
         (creates 0))
    (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 8))
              ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
              ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
              ((symbol-function 'create-image)
               (lambda (&rest _)
                 (cl-incf creates)
                 (list 'image creates))))
      (ebb-render--graphics-image-object render first placement)
      (setf (ebb-graphics-placement-image-id placement) 2)
      (ebb-render--graphics-image-object render second placement)
      (setf (ebb-graphics-placement-image-id placement) 1)
      (ebb-render--graphics-image-object render first placement)
      (should (= 3 creates))
      (should (= 1 (hash-table-count
                    (ebb-render-state-graphics-cache render))))
      (should (<= (ebb-render-state-graphics-cache-bytes render)
                  ebb-kitty-graphics-render-cache-limit)))))

(ert-deftest ebb-test-render-kitty-z-ties-put-higher-image-id-on-top ()
  "Equal z-index placements follow Kitty's effective image-ID z-order."
  (ebb-test-with-screen (:width 6 :height 2)
    (let ((render (make-ebb-render-state
                   :screen screen
                   :graphics-cache (make-hash-table :test #'equal)
                   :graphics-generation -1)))
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=1,c=4,r=1,C=1;AQID\e\\")
      (ebb-test-output parser "\e[2G")
      (ebb-test-output
       parser "\e_Ga=T,f=24,s=1,v=1,i=2,c=2,r=1,C=1;AQID\e\\")
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (ebb-render--apply-graphics render 0 "      ")
        (let ((owners (gethash (cons 0 6)
                               (ebb-render-state-graphics-layout-cache render))))
          (should (= 2 (ebb-graphics-placement-image-id (aref owners 1)))))))))

(ert-deftest ebb-test-render-only-history-graphics-rebuild-scrollback ()
  "Viewport-only graphics changes do not rebuild the materialized history."
  (ebb-test-with-screen (:width 10 :height 4)
    (ebb-test-output parser "a\r\nb\r\nc\r\nd\r\n")
    (with-temp-buffer
      (let* ((graphics (ebb-screen-graphics screen))
             (render (ebb-render-create screen (current-buffer)))
             (original (symbol-function 'ebb-render--rebuild-scrollback))
             (rebuilds 0))
        (ebb-render--update-scrollback render)
        (cl-incf (ebb-graphics-state-generation graphics))
        (cl-letf (((symbol-function 'ebb-render--rebuild-scrollback)
                   (lambda (&rest args)
                     (cl-incf rebuilds)
                     (apply original args))))
          (ebb-render--update-scrollback render)
          (should (= 0 rebuilds))
          (let ((image (make-ebb-graphics-image
                        :id 1 :format 24 :width 1 :height 1 :data "abc")))
            (puthash 1 image (ebb-graphics-state-images graphics))
            (push (make-ebb-graphics-placement
                   :image-id 1 :row 0 :column 0 :columns 1 :rows 1
                   :pixel-width 8 :pixel-height 16)
                  (ebb-graphics-state-placements graphics))
            (cl-incf (ebb-graphics-state-generation graphics)))
          (ebb-render--update-scrollback render))
        (should (= 1 rebuilds))))))

(ert-deftest ebb-test-render-kitty-unicode-placeholders ()
  "Virtual placements render U+10EEEE cells as image tiles."
  (ebb-test-with-screen (:width 10 :height 4)
    (let* ((data (unibyte-string 1 2 3))
           (encoded (base64-encode-string data t))
           (placeholder (string #x10eeee))
           (render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser
       (format "\e_Ga=T,f=24,s=1,v=1,i=42,c=2,r=2,U=1;%s\e\\" encoded))
      (should (equal '(0 . 0) (ebb-test-cursor screen)))
      (ebb-test-output
       parser (concat "\e[38;5;42m" placeholder placeholder "\r\n"
                      placeholder placeholder))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        (let ((top (ebb-render--apply-graphics
                    render 0 (concat placeholder placeholder)))
              (bottom (ebb-render--apply-graphics
                       render 1 (concat placeholder placeholder))))
          (should (equal '((slice 0 0 8 16) mock-image)
                         (get-text-property 0 'display top)))
          (should (equal '((slice 8 0 8 16) mock-image)
                         (get-text-property 1 'display top)))
          (should (equal '((slice 0 16 8 16) mock-image)
                         (get-text-property 0 'display bottom))))))))

(ert-deftest ebb-test-render-kitty-virtual-graphics-pads-trimmed-carrier ()
  "A carrier shorter than its reserved columns cannot break placeholder rendering."
  (ebb-test-with-screen (:width 4 :height 2)
    (let* ((data (unibyte-string 1 2 3))
           (encoded (base64-encode-string data t))
           (placeholder (string #x10eeee))
           (render (make-ebb-render-state
                    :screen screen
                    :graphics-cache (make-hash-table :test #'equal)
                    :graphics-generation -1)))
      (ebb-test-output
       parser
       (format "\e_Ga=T,f=24,s=1,v=1,i=42,c=2,r=1,U=1;%s\e\\" encoded))
      (ebb-test-output
       parser (concat "\e[38;5;42m" placeholder placeholder "\r\n"))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'frame-char-width) (lambda (&rest _) 8))
                ((symbol-function 'frame-char-height) (lambda (&rest _) 16))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'ebb-render--graphics-raw-png-helper)
                 (lambda (_) "png"))
                ((symbol-function 'create-image) (lambda (&rest _) 'mock-image)))
        ;; One-character carrier on a row with two placeholder cells: the
        ;; second column addresses past the end of the string.
        (let ((result (ebb-render--apply-graphics render 0 placeholder)))
          (should (>= (length result) 2))
          (should (get-text-property 0 'display result))
          (should (get-text-property 1 'display result)))))))

(ert-deftest ebb-test-render-kitty-placeholders-separate-placement-ids ()
  "Placeholder tile rows do not bleed between placements of one image."
  (ebb-test-with-screen (:width 4 :height 3)
    (let* ((graphics (ebb-screen-graphics screen))
           (first (make-ebb-graphics-placement
                   :image-id 42 :placement-id 1 :virtual t :columns 1 :rows 2))
           (second (make-ebb-graphics-placement
                    :image-id 42 :placement-id 2 :virtual t :columns 1 :rows 2))
           (render (make-ebb-render-state :screen screen)))
      (setf (ebb-graphics-state-placements graphics) (list second first))
      (setf (ebb-line-cells (ebb-screen-get-line screen 0))
            (vector (make-ebb-cell
                     :char #x10eeee :width 1
                     :attr (make-ebb-attr :fg 42 :ul-color 1))))
      (should (= 1 (ebb-render--placeholder-tile-row
                    render screen graphics 1 first)))
      (should (= 0 (ebb-render--placeholder-tile-row
                    render screen graphics 1 second))))))

(ert-deftest ebb-test-screen-reset-clears-kitty-graphics ()
  "RIS clears stored Kitty image data and partial uploads."
  (ebb-test-with-screen (:width 20 :height 6)
    (progn
      (ebb-test-output parser (format "\e_Ga=t,f=100,i=3;%s\e\\"
                                      ebb-test-kitty-png-base64))
      (should (gethash 3 (ebb-graphics-state-images
                          (ebb-screen-graphics screen))))
      (ebb-screen-reset screen)
      (should (= 0 (hash-table-count
                    (ebb-graphics-state-images
                     (ebb-screen-graphics screen))))))))

(ert-deftest ebb-test-parse-error-recovery ()
  "Parser recovers from malformed sequences."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; Malformed CSI (no final byte, then normal text)
    (ebb-test-output parser "\e[999ZHello")
    ;; Should not crash; Hello might not appear (depends on unknown handler)
    ;; The key test is that we don't error
    (should t)))

(ert-deftest ebb-test-parse-partial-sequence ()
  "Parser handles sequences split across chunks."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; Send CSI CUP in two parts
    (ebb-test-output parser "\e[")
    (ebb-test-output parser "3;5H")
    (should (equal '(4 . 2) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-parse-bell ()
  "Parser emits bell event."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((bell-called nil))
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest _args)
              (when (eq type 'bell)
                (setq bell-called t))))
      (ebb-test-output parser "\a")
      (should bell-called))))

(ert-deftest ebb-test-parse-backspace ()
  "Parser handles backspace."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "AB\bC")
    ;; B is overwritten by C
    (should (equal "AC" (ebb-test-display-line screen 0)))))

(ert-deftest ebb-test-parse-tab ()
  "Parser handles horizontal tab."
  (ebb-test-with-screen (:width 40 :height 3)
    (ebb-test-output parser "A\tB")
    (should (= ?A (ebb-cell-char
                    (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0))))
    (should (= ?B (ebb-cell-char
                    (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 8))))))

(ert-deftest ebb-test-parse-hts ()
  "ESC H sets a horizontal tab stop at the cursor."
  (ebb-test-with-screen (:width 20 :height 3)
    (ebb-test-output parser "\e[3g\e[1;7H\eH\e[1;1H\t*")
    (should (equal "      *" (ebb-test-display-line screen 0)))
    (should (equal '(7 . 0) (ebb-test-cursor screen)))))

;;;; ---- Render Tests ---------------------------------------------------

(ert-deftest ebb-test-render-color-conversion ()
  "Color conversion produces correct hex strings."
  ;; ANSI 0-15 come from theme-backed faces (ansi-color-*)
  (should (stringp (ebb-render--color-to-string 0)))
  (should (stringp (ebb-render--color-to-string 1)))
  ;; 256-color
  (should (equal "#ff0000" (ebb-render--color-to-string 196)))
  ;; Truecolor
  (should (equal "#ff8000" (ebb-render--color-to-string '(255 128 0))))
  ;; Grayscale
  (should (string-match-p "#[0-9a-f]+" (ebb-render--color-to-string 240)))
  ;; nil
  (should (null (ebb-render--color-to-string nil))))

(ert-deftest ebb-test-render-theme-cache-invalidation ()
  "Theme changes clear color caches and redraw every live Ebb render state."
  (let ((buffers nil)
        (color "#111111")
        (resets 0)
        cleared
        (original-face-foreground (symbol-function 'face-foreground))
        (original-reset (symbol-function 'ebb-render-full-reset)))
    (unwind-protect
        (progn
          (fillarray ebb-render--indexed-color-cache nil)
          (clrhash ebb-render--attr-face-cache)
          (dotimes (i 2)
            (let* ((buffer (generate-new-buffer (format " *theme-ebb-%d*" i)))
                   (screen (ebb-screen-create 5 2)))
              (push buffer buffers)
              (with-current-buffer buffer
                (ebb-mode)
                (setq-local ebb--screen screen)
                (setq-local ebb--render (ebb-render-create screen buffer))
                (ebb-screen-set-attr screen :fg 1)
                (ebb-screen-write-char screen ?x))))
          (cl-letf (((symbol-function 'face-foreground)
                     (lambda (face &rest args)
                       (if (eq face 'ebb-color-1)
                           color
                         (apply original-face-foreground face args))))
                    ((symbol-function 'ebb-render-full-reset)
                     (lambda (render)
                       (cl-incf resets)
                       (push (and (cl-every #'null
                                           ebb-render--indexed-color-cache)
                                  (zerop (hash-table-count
                                          ebb-render--attr-face-cache)))
                             cleared)
                       (funcall original-reset render))))
            ;; Seed both caches with the old theme color.
            (should (equal "#111111" (ebb-render--color-to-string 1)))
            (ebb-render--attr-to-face (make-ebb-attr :fg 1))
            (should (> (hash-table-count ebb-render--attr-face-cache) 0))
            (setq color "#222222")
            ;; Invoke the theme-change hook callback.
            (ebb-render--theme-changed 'test-theme)
            (should (= 2 resets))
            (should (memq t cleared))
            (should (equal "#222222" (ebb-render--color-to-string 1)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            buffers))))

(ert-deftest ebb-test-render-theme-hook-installation ()
  "Theme invalidation installs the theme-change hook."
  (let (hook)
    (cl-letf (((symbol-function 'add-hook)
               (lambda (symbol function &rest _)
                 (setq hook (list symbol function)))))
      (ebb-render--install-theme-invalidation))
    (should (equal '(enable-theme-functions ebb-render--theme-changed)
                   hook))))

(ert-deftest ebb-test-render-attr-to-face ()
  "Attribute to face conversion works."
  ;; Default attr -> nil
  (should (null (ebb-render--attr-to-face nil)))
  (should (null (ebb-render--attr-to-face (make-ebb-attr))))
  ;; Bold/italic/font compose via :inherit named faces
  (let ((face (ebb-render--attr-to-face (make-ebb-attr :bold t))))
    (should (equal '(ebb-bold) (plist-get face :inherit))))
  (let ((face (ebb-render--attr-to-face
               (make-ebb-attr :bold t :italic t :font 3))))
    (should (equal '(ebb-bold ebb-italic ebb-font-3)
                   (plist-get face :inherit))))
  ;; Foreground color
  (let ((face (ebb-render--attr-to-face (make-ebb-attr :fg 1))))
    (should (equal (ebb-render--color-to-string 1)
                   (plist-get face :foreground))))
  ;; Underline styles map to Emacs underline styles
  (let ((face (ebb-render--attr-to-face
               (make-ebb-attr :underline 'double))))
    (should (equal '(:style double-line) (plist-get face :underline))))
  (let ((face (ebb-render--attr-to-face
               (make-ebb-attr :underline 'dotted :ul-color 1))))
    (should (eq 'dots (plist-get (plist-get face :underline) :style)))
    (should (equal (ebb-render--color-to-string 1)
                   (plist-get (plist-get face :underline) :color)))))

;;;; ---- Phase 2 Tests --------------------------------------------------

(ert-deftest ebb-test-csi-j-k-aliases ()
  "CSI j (CUB alias) and CSI k (CUU alias) work."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; Move to (5, 3)
    (ebb-test-output parser "\e[4;6H")
    (should (equal '(5 . 3) (ebb-test-cursor screen)))
    ;; CSI k = cursor up 1
    (ebb-test-output parser "\e[k")
    (should (equal '(5 . 2) (ebb-test-cursor screen)))
    ;; CSI 2j = cursor left 2
    (ebb-test-output parser "\e[2j")
    (should (equal '(3 . 2) (ebb-test-cursor screen)))))

(ert-deftest ebb-test-sgr-underline-sub-params ()
  "SGR 4:N sub-parameters set correct underline styles."
  (ebb-test-with-screen (:width 20 :height 6)
    ;; 4:0 = off
    (ebb-test-output parser "\e[4:0mA\e[0m")
    (let* ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0))
           (attr (ebb-cell-attr cell)))
      ;; 4:0 should set underline to nil
      (should (or (null attr) (null (ebb-attr-underline attr)))))
    ;; 4:1 = line
    (ebb-test-output parser "\e[4:1mB\e[0m")
    (let* ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 1))
           (attr (ebb-cell-attr cell)))
      (should (eq 'line (ebb-attr-underline attr))))
    ;; 4:3 = curly
    (ebb-test-output parser "\e[4:3mC\e[0m")
    (let* ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 2))
           (attr (ebb-cell-attr cell)))
      (should (eq 'curly (ebb-attr-underline attr))))
    ;; 4:5 = dashed
    (ebb-test-output parser "\e[4:5mD\e[0m")
    (let* ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 3))
           (attr (ebb-cell-attr cell)))
      (should (eq 'dashed (ebb-attr-underline attr))))))

(ert-deftest ebb-test-sgr-colon-colors ()
  "Colon-form SGR colors accept mixed semicolon-separated attributes."
  (ebb-test-with-screen (:width 20 :height 6)
    (ebb-test-output parser "\e[4:3;38:5:2;48:2::3:4:5;58:5:6mX")
    (let* ((cell (aref (ebb-line-cells (ebb-screen-get-line screen 0)) 0))
           (attr (ebb-cell-attr cell)))
      (should (eq 'curly (ebb-attr-underline attr)))
      (should (= 2 (ebb-attr-fg attr)))
      (should (equal '(3 4 5) (ebb-attr-bg attr)))
      (should (= 6 (ebb-attr-ul-color attr))))))

(ert-deftest ebb-test-double-width-char ()
  "Double-width (CJK) characters occupy two cells."
  (ebb-test-with-screen (:width 10 :height 3)
    ;; Write a CJK character (U+4E2D, width=2)
    (ebb-screen-write-char screen #x4E2D)
    ;; Should be at cell 0 with width 2
    (let* ((line (ebb-screen-get-line screen 0))
           (cell0 (aref (ebb-line-cells line) 0))
           (cell1 (aref (ebb-line-cells line) 1)))
      (should (= #x4E2D (ebb-cell-char cell0)))
      (should (= 2 (ebb-cell-width cell0)))
      (should (= 0 (ebb-cell-width cell1))))  ; continuation cell
    ;; Cursor should have advanced by 2
    (should (= 2 (ebb-screen-cursor-x screen)))))

(ert-deftest ebb-test-double-width-at-eol ()
  "Double-width char at last column wraps correctly."
  (ebb-test-with-screen (:width 5 :height 3)
    ;; Fill to column 4 (last col, 0-indexed)
    (ebb-test-output parser "XXXX")
    (should (= 4 (ebb-screen-cursor-x screen)))
    ;; Write a double-width char -- doesn't fit at col 4, should wrap
    (ebb-screen-write-char screen #x4E2D)
    ;; Should be on line 1 at col 2
    (should (= 1 (ebb-screen-cursor-y screen)))
    (should (= 2 (ebb-screen-cursor-x screen)))))

(ert-deftest ebb-test-cursor-style ()
  "Cursor style is set by DECSCUSR."
  (ebb-test-with-screen (:width 10 :height 3)
    ;; Default is :block
    (should (eq :block (ebb-screen-cursor-style screen)))
    ;; Set to bar (DECSCUSR 5 = blinking bar, 6 = steady bar)
    (ebb-test-output parser "\e[6 q")
    (should (eq :bar (ebb-screen-cursor-style screen)))
    ;; Set to underline
    (ebb-test-output parser "\e[4 q")
    (should (eq :underline (ebb-screen-cursor-style screen)))
    ;; Set to blinking block
    (ebb-test-output parser "\e[1 q")
    (should (eq :blinking-block (ebb-screen-cursor-style screen)))))

(ert-deftest ebb-test-cursor-blink-mode-12 ()
  "DECSET/DECRST 12 toggles cursor blink."
  (ebb-test-with-screen (:width 10 :height 3)
    (should (null (ebb-screen-cursor-blink screen)))
    ;; DECSET 12
    (ebb-test-output parser "\e[?12h")
    (should (ebb-screen-cursor-blink screen))
    ;; DECRST 12
    (ebb-test-output parser "\e[?12l")
    (should (null (ebb-screen-cursor-blink screen)))))

(ert-deftest ebb-test-parse-sub-params-preserved ()
  "Colon-separated sub-parameters are parsed as lists."
  (let ((params (ebb-parse--parse-params "4:3;1;38:2:255:128:0")))
    ;; First param: (4 3)
    (should (equal '(4 3) (aref params 0)))
    ;; Second param: 1
    (should (= 1 (aref params 1)))
    ;; Third param: (38 2 255 128 0)
    (should (equal '(38 2 255 128 0) (aref params 2)))))

(ert-deftest ebb-test-per-color-faces ()
  "Per-color named faces exist and return correct colors."
  ;; Face exists
  (should (facep 'ebb-color-0))
  (should (facep 'ebb-color-1))
  (should (facep 'ebb-color-255))
  ;; 0-15 inherit Emacs ansi-color faces
  (should (eq 'ansi-color-black
              (face-attribute 'ebb-color-0 :inherit nil 'default)))
  (should (eq 'ansi-color-bright-red
              (face-attribute 'ebb-color-9 :inherit nil 'default)))
  ;; Color lookup returns a string
  (should (stringp (ebb-render--color-to-string 0)))
  (should (stringp (ebb-render--color-to-string 196))))

(ert-deftest ebb-test-inverse-attr-face ()
  "Inverse attribute uses default face colors."
  (let ((face (ebb-render--attr-to-face (make-ebb-attr :inverse t))))
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

(ert-deftest ebb-test-inverse-attr-with-fg ()
  "Inverse with explicit fg swaps correctly."
  (let* ((red (ebb-render--color-to-string 1))
         (face (ebb-render--attr-to-face
                (make-ebb-attr :inverse t :fg 1))))
    ;; fg was palette 1; inverse means it becomes the background
    (should (plist-get face :background))
    (should (equal red (plist-get face :background)))))

(ert-deftest ebb-test-xtsmgraphics-response ()
  "XTSMGRAPHICS query gets a valid response."
  (ebb-test-with-screen (:width 80 :height 24)
    (let ((responses nil))
      (setf (ebb-parser-write-fn parser)
            (lambda (s) (push s responses)))
      ;; Query color register count: CSI ? 1 ; 1 S
      (ebb-test-output parser "\e[?1;1S")
      (should (car responses))
      (should (string-match-p "\\`\e\\[\\?1;0;256S\\'" (car responses)))
      ;; Query graphics geometry: CSI ? 2 ; 1 S
      (setq responses nil)
      (ebb-test-output parser "\e[?2;1S")
      (should (car responses))
      (should (string-match-p "\\`\e\\[\\?2;0;" (car responses))))))

(ert-deftest ebb-test-osc-color-query ()
  "OSC 10/11 ? query returns rgb: format."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((responses nil))
      (setf (ebb-parser-write-fn parser)
            (lambda (s) (push s responses)))
      ;; Query foreground
      (ebb-test-output parser "\e]10;?\a")
      (should (car responses))
      (should (string-match-p "rgb:" (car responses)))
      ;; Query background
      (setq responses nil)
      (ebb-test-output parser "\e]11;?\a")
      (should (car responses))
      (should (string-match-p "rgb:" (car responses))))))

(ert-deftest ebb-test-osc-4-palette-query ()
  "OSC 4 palette queries return xterm rgb replies."
  (ebb-test-with-screen (:width 20 :height 6)
    (let ((responses nil))
      (setf (ebb-parser-write-fn parser)
            (lambda (s) (push s responses)))
      (ebb-test-output parser "\e]4;1;?\a")
      (should (equal "\e]4;1;rgb:cdcd/0000/0000\e\\" (car responses)))
      (setq responses nil)
      (ebb-test-output parser "\e]4;0;?;15;?\a")
      (should (= 2 (length responses)))
      (should (member "\e]4;0;rgb:0000/0000/0000\e\\" responses))
      (should (member "\e]4;15;rgb:ffff/ffff/ffff\e\\" responses)))))

(ert-deftest ebb-test-osc-52-disabled ()
  "OSC 52 has no clipboard or event side effects by default."
  (should-not (default-value 'ebb-enable-osc52))
  (let* ((screen (ebb-screen-create 20 6))
         responses events clipboard-calls kill-calls
         (kill-ring '("keep"))
         (parser
          (ebb-parse-create
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
      (let ((ebb-enable-osc52 nil))
        (ebb-test-output parser "\e]52;c;?\a")
        (ebb-test-output
         parser (concat "\e]52;c;" (base64-encode-string "set" t) "\a"))
        (ebb-test-output parser "\e]52;c;\a")))
    (should-not responses)
    (should-not events)
    (should-not clipboard-calls)
    (should-not kill-calls)
    (should (equal '("keep") kill-ring))))

(ert-deftest ebb-test-osc-52-enabled ()
  "Opting in restores OSC 52 query and set behavior."
  (let* ((screen (ebb-screen-create 20 6))
         responses events selections kills
         (parser
          (ebb-parse-create
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
      (let ((ebb-enable-osc52 t))
        (ebb-test-output parser "\e]52;c;?\a")
        (should (equal (format "\e]52;c;%s\e\\"
                               (base64-encode-string "clipboard" t))
                       (car responses)))
        (ebb-test-output
         parser (concat "\e]52;c;" (base64-encode-string "set" t) "\a"))))
    (should (equal '("set") kills))
    (should (equal '((CLIPBOARD "set")) selections))
    (should (equal '((clipboard "set")) events))))

(ert-deftest ebb-test-input-backspace-variants ()
  "Input translation handles backspace modifier variants."
  (let ((screen (ebb-screen-create 80 24)))
    ;; Plain backspace
    (should (equal "\x7f" (ebb-input-translate 'backspace screen)))
    ;; M-backspace
    (should (equal "\e\x7f" (ebb-input-translate
                              (event-convert-list '(meta backspace)) screen)))
    ;; C-backspace
    (should (equal "\x08" (ebb-input-translate
                            (event-convert-list '(control backspace)) screen)))
    ;; Eat treats DEL as ordinary backspace input.
    (should (equal "\x7f" (ebb-input-translate ?\C-? screen)))))

(ert-deftest ebb-test-input-function-keys-extended ()
  "Function keys translate and are forwarded in default semi-char mode."
  (let ((screen (ebb-screen-create 80 24)))
    (should (equal "\eOP" (ebb-input-translate 'f1 screen)))
    (should (ebb-input-translate 'f21 screen))
    (should (ebb-input-translate 'f36 screen))
    (should (ebb-input-translate 'f63 screen)))
  (should (eq #'ebb-self-input
              (lookup-key ebb-semi-char-mode-map [f1])))
  (should (eq #'ebb-self-input
              (lookup-key ebb-semi-char-mode-map [f10])))
  (let ((minor-mode-map-alist
         `((ebb--semi-char-mode . ,(make-sparse-keymap)))))
    (ebb-update-semi-char-mode-map)
    (should (eq #'ebb-self-input
                (lookup-key (cdr (assq 'ebb--semi-char-mode
                                       minor-mode-map-alist))
                            [f5])))))

(ert-deftest ebb-test-input-deletechar ()
  "deletechar key translates to ESC[3~."
  (let ((screen (ebb-screen-create 80 24)))
    (should (equal "\e[3~" (ebb-input-translate 'deletechar screen)))))

(ert-deftest ebb-test-input-committed-text ()
  "Committed input-method text is sent unchanged."
  (let (sent)
    (with-temp-buffer
      (setq-local ebb--io 'fake)
      (setq-local ebb--screen (ebb-screen-create 10 3))
      (cl-letf (((symbol-function 'ebb-io-send)
                 (lambda (_io string) (push string sent))))
        (ebb-self-input 1 "日本")))
    (should (equal '("日本") sent))))

(ert-deftest ebb-test-send-password ()
  "Password command sends the password followed by return."
  (let (sent)
    (with-temp-buffer
      (setq-local ebb--io 'fake)
      (cl-letf (((symbol-function 'ebb-io-send)
                 (lambda (_io s) (push s sent))))
        (ebb-send-password "secret")))
    (should (equal '("\r" "secret") sent))))

(ert-deftest ebb-test-password-prompt-detect ()
  "Cursor-row password regex detection."
  (let ((screen (ebb-screen-create 40 3)))
    (with-temp-buffer
      (setq-local ebb--screen screen)
      (setq-local ebb--io 'fake)
      (let ((s "[sudo] password for arthur: "))
        (ebb-screen-write-string screen s 0 (length s)))
      (should (ebb--password-prompt-detected-p))
      (ebb-screen-cursor-goto screen 0 1)
      (let ((s "hello"))
        (ebb-screen-write-string screen s 0 (length s)))
      (should-not (ebb--password-prompt-detected-p)))))

(ert-deftest ebb-test-password-prompt-row-supports-unicode-cells ()
  "Password detection handles a cell-backed row containing Unicode."
  (let ((screen (ebb-screen-create 40 3)))
    (with-temp-buffer
      (setq-local ebb--screen screen)
      (setq-local ebb--io 'fake)
      (let ((s "λ [sudo] password for arthur: "))
        (ebb-screen-write-string screen s 0 (length s)))
      (should (ebb--password-prompt-detected-p)))))

(ert-deftest ebb-test-password-prompt-send ()
  "Detected password prompt sends via source chain."
  (let ((screen (ebb-screen-create 40 3))
        sent)
    (with-temp-buffer
      (setq-local ebb--screen screen)
      (setq-local ebb--io 'fake)
      (setq-local ebb-password-prompt-functions
                  (list (lambda (_row) "secret")))
      (cl-letf (((symbol-function 'ebb-io-send)
                 (lambda (_io s) (push s sent))))
        (let ((s "Password: "))
          (ebb-screen-write-string screen s 0 (length s)))
        (ebb--prompt-password))
      ;; The newline is sent separately and `clear-string' wipes the
      ;; only string holding the secret after it has been sent.
      (should (equal "\r" (car sent)))
      (should (equal (make-string 6 ?\0) (cadr sent)))
      (should-not ebb--password-mode-p)
      (should (eql 0 ebb--password-handled-y)))))

(ert-deftest ebb-test-public-input-api ()
  "Public input commands preserve strings and encode keys and paste mode."
  (let (sent)
    (with-temp-buffer
      (setq major-mode 'ebb-mode)
      (setq-local ebb--screen (ebb-screen-create 10 3))
      (setq-local ebb--io (make-ebb-io :process 'fake))
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'ebb-io-send)
                 (lambda (_io string) (push string sent))))
        (ebb-send-string (unibyte-string 0 255 ?x))
        (ebb-send-key "up")
        (ebb-send-key "up" "control, shift")
        (ebb-send-key "c" "ctrl")
        (ebb-paste-string "plain")
        (setf (ebb-screen-bracketed-paste ebb--screen) t)
        (ebb-paste-string "bracketed")))
    (should (equal (list "\e[200~bracketed\e[201~" "plain" "\C-c"
                         "\e[1;6A" "\e[A" (unibyte-string 0 255 ?x))
                   sent))))

(ert-deftest ebb-test-yank-paste-bindings ()
  "Yank commands and host-terminal paste events route through Ebb."
  (should (eq (lookup-key ebb-semi-char-mode-map (kbd "C-y")) #'ebb-yank))
  (should (eq (lookup-key ebb-semi-char-mode-map (kbd "S-<insert>")) #'ebb-yank))
  (should (eq (lookup-key ebb-semi-char-mode-map [remap yank]) #'ebb-yank))
  (should (eq (lookup-key ebb-mode-map [xterm-paste]) #'ebb-xterm-paste))
  (should (eq (lookup-key ebb-mode-map [XF86Paste]) #'ebb-yank)))

(ert-deftest ebb-test-xterm-paste-forwards-to-process ()
  "An xterm-paste event is sent to the child and optionally saved as a kill."
  (let ((kill-ring nil)
        (kill-ring-yank-pointer nil)
        (xterm-store-paste-on-kill-ring t)
        (save-interprogram-paste-before-kill t)
        clipboard-writes
        (clipboard-reads 0)
        sent)
    (with-temp-buffer
      (setq major-mode 'ebb-mode)
      (setq-local ebb--screen (ebb-screen-create 10 3))
      (setq-local ebb--io (make-ebb-io :process 'fake))
      (setf (ebb-screen-bracketed-paste ebb--screen) t)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'ebb-io-send)
                 (lambda (_io string) (setq sent string)))
                (interprogram-cut-function
                 (lambda (text) (push text clipboard-writes)))
                ;; Incoming paste is not a yank: pasting must not import
                ;; whatever the desktop clipboard currently holds.
                (interprogram-paste-function
                 (lambda () (cl-incf clipboard-reads) "desktop-clipboard")))
        (ebb-xterm-paste '(xterm-paste "hello"))))
    (should (equal sent "\e[200~hello\e[201~"))
    (should (equal (car kill-ring) "hello"))
    (should (equal (length kill-ring) 1))
    (should (equal clipboard-reads 0))
    (should-not clipboard-writes)))

(ert-deftest ebb-test-public-input-api-requires-running-terminal ()
  "Public input commands reject non-terminal and stopped terminal buffers."
  (dolist (function '(ebb-send-string ebb-paste-string))
    (with-temp-buffer
      (should-error (funcall function "x") :type 'user-error)
      (setq major-mode 'ebb-mode)
      (should-error (funcall function "x") :type 'user-error)))
  (with-temp-buffer
    (should-error (ebb-send-key "up") :type 'user-error)
    (setq major-mode 'ebb-mode)
    (should-error (ebb-send-key "up") :type 'user-error)))

(ert-deftest ebb-test-io-create-terminal-applies-scrollback-option ()
  "The standalone I/O constructor applies valid scrollback limits only."
  (let ((buffer (generate-new-buffer " *ebb-io-create-test*")))
    (unwind-protect
        (progn
          (let ((ebb-scrollback-lines 37))
            (let ((io (ebb-io-create-terminal buffer #'ignore)))
              (should (= 37 (ebb-screen-scrollback-max
                             (ebb-io-screen io))))))
          (let ((ebb-scrollback-lines -1))
            (should-error (ebb-io-create-terminal buffer #'ignore))))
      (kill-buffer buffer))))

(ert-deftest ebb-test-io-send-preserves-bytes ()
  "Process writes preserve unibyte data and UTF-8 encode text."
  (let ((io (make-ebb-io :process 'fake))
        sent)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string)
               (lambda (_process string) (push string sent))))
      (ebb-io-send io (unibyte-string 0 255 ?x))
      (ebb-io-send io "λ"))
    (should (equal (encode-coding-string "λ" 'utf-8 t) (car sent)))
    (should (equal (unibyte-string 0 255 ?x) (cadr sent)))))

(ert-deftest ebb-test-io-interrupt-discards-stale-output ()
  "C-c clears queued output and incomplete parser state before sending."
  (let* ((screen (ebb-screen-create 10 3))
         (parser (ebb-parse-create screen))
         (chunks (list "stale output"))
         (io (make-ebb-io :process 'fake
                          :parser parser
                          :pending-chunks chunks
                          :pending-tail chunks
                          :pending-offset 4
                          :first-chunk-time (current-time)))
         sent)
    (setf (ebb-parser-state parser) :osc-string
          (ebb-parser-osc-parts parser) (list "l" "a" "i" "t" "r" "a" "p")
          (ebb-parser-osc-length parser) 7)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'process-send-string)
               (lambda (_process string) (setq sent string))))
      (ebb-io-send io "\C-c"))
    (should (equal "\C-c" sent))
    (should-not (ebb-io-pending-chunks io))
    (should-not (ebb-io-pending-tail io))
    (should (zerop (ebb-io-pending-offset io)))
    (should-not (ebb-io-first-chunk-time io))
    (should (eq :ground (ebb-parser-state parser)))
    (should-not (ebb-parser-osc-parts parser))
    (should (zerop (ebb-parser-osc-length parser)))))

(ert-deftest ebb-test-io-process-uses-binary-writes ()
  "PTY output is decoded as UTF-8 while input remains byte-preserving."
  (let* ((screen (ebb-screen-create 10 3))
         (parser (ebb-parse-create screen))
         (io (make-ebb-io :screen screen :parser parser))
         coding)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq coding (plist-get args :coding))
                 'fake))
              ((symbol-function 'set-process-window-size) #'ignore))
      (let ((ebb-enable-shell-integration nil))
        (ebb-io-start io "/bin/sh" (current-buffer))))
    (should (equal '(utf-8-unix . no-conversion) coding))))

(ert-deftest ebb-test-io-resize-drains-old-size-output-first ()
  "Resize parses queued output before changing terminal dimensions."
  (let* ((screen (ebb-screen-create 10 3))
         (io (make-ebb-io :screen screen :pending-chunks '("output")))
         events)
    (cl-letf (((symbol-function 'ebb-io--process-pending)
               (lambda (_io _all) (push (ebb-screen-width screen) events)
                 (setf (ebb-io-pending-chunks io) nil)))
              ((symbol-function 'ebb-screen-resize)
               (lambda (_screen width _height) (push width events)
                 (setf (ebb-screen-width screen) width))))
      (ebb-io-handle-resize io 20 3))
    (should (equal '(20 10) events))))

(ert-deftest ebb-test-io-resize-notifies-pty ()
  "A resize propagates to the PTY, so the child is told about the new size."
  (let* ((screen (ebb-screen-create 10 3))
         (proc (make-process :name "ebb-test-pty" :buffer nil
                             :command '("cat") :connection-type 'pty
                             :noquery t))
         (io (make-ebb-io :screen screen :process proc))
         sizes)
    (unwind-protect
        (progn
          ;; `process-type' answers `real' for a PTY subprocess, never `pty'.
          (should (ebb-io--pty-process-p proc))
          (cl-letf (((symbol-function 'set-process-window-size)
                     (lambda (_proc height width) (push (cons width height) sizes)))
                    ((symbol-function 'ebb-screen-resize) #'ignore))
            (ebb-io-handle-resize io 20 5))
          (should (equal '((20 . 5)) sizes)))
      (delete-process proc))))

(ert-deftest ebb-test-io-resize-reaches-child ()
  "The child shell reports the resized dimensions, not its startup size."
  (skip-unless (file-executable-p "/bin/sh"))
  (let ((buffer (generate-new-buffer " *ebb-resize*")))
    (unwind-protect
        (with-current-buffer buffer
          (let* ((screen (ebb-screen-create 120 24))
                 (parser (ebb-parse-create screen))
                 (io (make-ebb-io :screen screen :parser parser
                                  :render (ebb-render-create screen buffer)
                                  :buffer buffer :min-latency 0
                                  :max-latency 0.01))
                 (ebb-enable-shell-integration nil))
            (unwind-protect
                (progn
                  (ebb-io-start io "/bin/sh" buffer)
                  (dotimes (_ 10) (accept-process-output nil 0.05))
                  (ebb-io-handle-resize io 40 12)
                  (ebb-io-send io "stty size\n")
                  (let ((deadline (+ (float-time) 5)))
                    (while (and (< (float-time) deadline)
                                (not (string-match-p
                                      "12 40" (ebb-screen-plain-text screen))))
                      (accept-process-output nil 0.05)))
                  (should (string-match-p "12 40" (ebb-screen-plain-text screen))))
              (ebb-io-stop io))))
      (kill-buffer buffer))))

(ert-deftest ebb-test-io-list-command-keeps-shell-integration ()
  "Shell argv from the program prompt retains startup integration."
  (let* ((dir (make-temp-file "ebb-integration-" t))
         (script (expand-file-name "bash" dir))
         (env (list (concat "EBB_SHELL_INTEGRATION_DIR=" dir)))
         command)
    (unwind-protect
        (progn
          (with-temp-file script (insert "# integration\n"))
          (let ((ebb-enable-shell-integration t))
            (setq command
                  (ebb-io--build-command '("/bin/bash" "-i") env)))
          (should (equal "/bin/bash" (nth 0 command)))
          (should (equal "--rcfile" (nth 1 command)))
          (should (file-exists-p (nth 2 command)))
          (should (equal "-i" (nth 3 command))))
      (when (and command (nth 2 command))
        (delete-file (nth 2 command)))
      (delete-directory dir t))))

(ert-deftest ebb-test-io-zsh-bootstrap-environment ()
  "Zsh integration prepends bootstrap environment without changing argv."
  (let* ((dir (make-temp-file "ebb-zsh-integration-" t))
         (script (expand-file-name "zsh" dir))
         (env (list (concat "EBB_SHELL_INTEGRATION_DIR=" dir)))
         (command '("/bin/zsh" "-i" "--no-rcs")))
    (unwind-protect
        (progn
          (with-temp-file script (insert "# integration\n"))
          (let ((process-environment '("ZDOTDIR=/user/zsh"))
                (ebb-enable-shell-integration t))
            (let ((prepared (ebb-io--prepare-environment command env)))
              (should (equal (concat "ZDOTDIR="
                                     (expand-file-name "zsh-bootstrap" dir))
                             (car prepared)))
              (should (equal "EBB_ZSH_ZDOTDIR=/user/zsh"
                             (cadr prepared)))
              (should (equal "EBB_ZSH_ZDOTDIR_SET=1"
                             (nth 2 prepared)))
              (should (equal command (ebb-io--build-command command env)))))
          (let ((process-environment '("ZDOTDIR="))
                (ebb-enable-shell-integration t))
            (let ((prepared (ebb-io--prepare-environment command env)))
              (should (equal "EBB_ZSH_ZDOTDIR=" (cadr prepared)))
              (should (equal "EBB_ZSH_ZDOTDIR_SET=1"
                             (nth 2 prepared)))))
          (let ((process-environment nil)
                (ebb-enable-shell-integration t))
            (should (equal "EBB_ZSH_ZDOTDIR_SET=0"
                           (nth 2 (ebb-io--prepare-environment command env)))))
          (let ((ebb-enable-shell-integration nil))
            (should (equal env (ebb-io--prepare-environment command env)))
            (should (equal command (ebb-io--build-command command env)))))
      (delete-directory dir t))))

(ert-deftest ebb-test-io-zsh-bootstrap-subprocess ()
  "Bundled zsh bootstrap sources user startup files and emits OSC 51."
  (skip-unless (executable-find "zsh"))
  (let* ((home (make-temp-file "ebb-zsh-home-" t))
         (zsh (executable-find "zsh"))
         (default-directory (file-name-as-directory
                             (file-truename default-directory)))
         (process-environment (copy-sequence process-environment))
         (extra-env (ebb-shell-env-vars)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".zshenv" home)
            (insert "print USER_ZSHENV\n"))
          (with-temp-file (expand-file-name ".zshrc" home)
            (insert "print USER_ZSHRC\nPS1='USER> '\n"))
          (setenv "HOME" home)
          (setenv "TERM" "ebb-truecolor")
          (setenv "ZDOTDIR" nil)
          (setq process-environment
                (append (ebb-io--prepare-environment
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

(ert-deftest ebb-test-io-command-sets-stty-sane ()
  "PTY command wrapper initializes terminal settings like Eat."
  (let ((cmd (ebb-io--wrap-command-with-stty '("/bin/sh") 24 80)))
    (should (equal '("/usr/bin/env" "sh" "-c")
                   (list (nth 0 cmd) (nth 1 cmd) (nth 2 cmd))))
    (should (string-match-p "stty .* rows 24 columns 80 sane" (nth 3 cmd)))
    (should (equal '(".." "/bin/sh") (nthcdr 4 cmd)))))

(defun ebb-test--mouse-event (type point)
  "Return a synthetic mouse event of TYPE at buffer POINT."
  (list type (list nil point '(0 . 0) 0 nil point '(0 . 0) nil)))

(ert-deftest ebb-test-mouse-wheel-left-right ()
  "Mouse wheel-left/right produce correct button codes."
  (let ((screen (ebb-screen-create 80 24)))
    (should (= 66 (ebb-input--mouse-button
                   (ebb-test--mouse-event 'wheel-left 1) screen)))
    (should (= 67 (ebb-input--mouse-button
                   (ebb-test--mouse-event 'wheel-right 1) screen)))))

(ert-deftest ebb-test-mouse-keymap-bindings ()
  "DEC mouse keymap forwards press, release, drag, and movement events."
  (should (eq (lookup-key ebb-mouse-mode-map [down-mouse-1])
              #'ebb-mouse-input))
  (should (eq (lookup-key ebb-mouse-mode-map [drag-mouse-1])
              #'ebb-mouse-input))
  (should (eq (lookup-key ebb-mouse-mode-map [mouse-1])
              #'ebb-mouse-input))
  (should (eq (lookup-key ebb-mouse-mode-map [mouse-movement])
              #'ebb-mouse-input)))

(ert-deftest ebb-test-mouse-sgr-encoding ()
  "Mouse events encode as SGR mouse reports relative to display-begin."
  (let ((screen (ebb-screen-create 10 3)))
    (setf (ebb-screen-mouse-mode screen) 'button-event)
    (setf (ebb-screen-mouse-sgr screen) t)
    (with-temp-buffer
      (insert "scrollback\nabcdefghij\nklmnopqrst\nuvwxyz    \n")
      (let ((display-begin (save-excursion
                             (goto-char (point-min))
                             (forward-line 1)
                             (point))))
        ;; Button 1 press at display row 0, column 1.
        (should (equal "\e[<0;2;1M"
                       (ebb-input-encode-mouse
                        (ebb-test--mouse-event 'down-mouse-1
                                                 (1+ display-begin))
                        screen display-begin)))
        ;; Movement while button 1 is pressed reports button+32.
        (should (equal "\e[<32;3;1M"
                       (ebb-input-encode-mouse
                        (ebb-test--mouse-event 'mouse-movement
                                                 (+ display-begin 2))
                        screen display-begin)))
        ;; Release clears the pressed-button state.
        (should (equal "\e[<0;2;1m"
                       (ebb-input-encode-mouse
                        (ebb-test--mouse-event 'mouse-1 (1+ display-begin))
                        screen display-begin)))
        (should-not (ebb-screen-mouse-pressed screen))
        ;; Clicks in scrollback above the display are not sent to TUIs.
        (should-not (ebb-input-encode-mouse
                     (ebb-test--mouse-event 'down-mouse-1 (point-min))
                     screen display-begin))))))

(ert-deftest ebb-test-mouse-trimmed-row-recovers-window-column ()
  "Clicks past trimmed text use the window-relative visual column."
  (let ((screen (ebb-screen-create 10 3))
        used-window)
    (with-temp-buffer
      (insert "abc\n\n")
      (cl-letf (((symbol-function 'posn-col-row)
                 (lambda (_posn &optional use-window)
                   (setq used-window use-window)
                   '(7 . 0))))
        (should (equal '(7 . 0)
                       (ebb-input--mouse-coordinates
                        (ebb-test--mouse-event 'down-mouse-1 4)
                        screen (point-min))))
        (should used-window)))))

(ert-deftest ebb-test-render-cursor-type-mapping ()
  "Cursor style maps to correct Emacs cursor-type."
  (should (eq 'box (ebb-render--cursor-type-for-style :block)))
  (should (eq 'box (ebb-render--cursor-type-for-style :blinking-block)))
  (should (equal '(bar . 2) (ebb-render--cursor-type-for-style :bar)))
  (should (equal '(hbar . 2) (ebb-render--cursor-type-for-style :underline))))

(ert-deftest ebb-test-render-trims-trailing-padding ()
  "Viewport rows end at their last real cell, not at the grid width."
  (let* ((screen (ebb-screen-create 10 3))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "abc")
        (ebb-render-refresh render)
        (should (equal "abc\n\n" (buffer-string)))
        ;; Motion therefore stops at real content.
        (goto-char (point-min))
        (end-of-line)
        (should (= 3 (current-column)))))))

(ert-deftest ebb-test-render-keeps-styled-trailing-cells ()
  "Styled blank cells keep their padding; only default blanks are trimmed."
  (let* ((screen (ebb-screen-create 10 3))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "\e[41m   \e[0mab")
        (ebb-render-refresh render)
        (should (equal "   ab\n\n" (buffer-string)))
        (should (get-text-property 0 'face (buffer-substring
                                            (point-min) (+ (point-min) 3))))))))

(ert-deftest ebb-test-render-trims-double-width-padding ()
  "DEC double-size width faces do not preserve empty row padding."
  (let* ((screen (ebb-screen-create 10 2))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "\e#6Hi")
        (ebb-render-refresh render)
        (should (equal "Hi\n" (buffer-string)))
        (should (equal 'ultra-expanded
                       (plist-get (get-text-property (point-min) 'face)
                                  :width)))))))

(ert-deftest ebb-test-render-virtual-cursor-past-eol ()
  "A cursor right of trimmed content is drawn with an `after-string'."
  (let* ((screen (ebb-screen-create 10 3))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "abc\e[1;8H")
        (ebb-render-refresh render)
        (let* ((ov (ebb-render-state-cursor-overlay render))
               (after (overlay-get ov 'after-string)))
          (should after)
          ;; Overlay anchors at EOL of the cursor row; the string bridges
          ;; columns 3..7 and paints column 7 with the cursor face.
          (save-excursion
            (goto-char (ebb-render-state-display-begin render))
            (should (= (overlay-start ov)
                       (+ (point-min) 3)))
            (should (= (length after) 5))
            (should (string-suffix-p " " (substring-no-properties after)))
            (should (get-text-property (1- (length after)) 'face after))
            (should-not (get-text-property 0 'face after))))))))

(ert-deftest ebb-test-render-virtual-cursor-keeps-line-rendition ()
  "Virtual cursor cells retain DEC double-size line rendering."
  (let* ((screen (ebb-screen-create 10 2))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "\e#6Hi\e[1;5H")
        (ebb-render-refresh render)
        (let* ((after (overlay-get
                       (ebb-render-state-cursor-overlay render)
                       'after-string))
               (cursor-face (get-text-property
                             (1- (length after)) 'face after)))
          (should (= (length after) 3))
          (should (equal 'ultra-expanded
                         (plist-get (get-text-property 0 'face after)
                                    :width)))
          (should (memq 'ebb-cursor cursor-face))
          (should (member '(:width ultra-expanded) cursor-face)))))))

(ert-deftest ebb-test-render-cursor-only-refresh ()
  "Pure cursor movement updates the rendered cursor overlay."
  (let* ((screen (ebb-screen-create 5 3))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (let ((before (overlay-start (ebb-render-state-cursor-overlay render))))
          (ebb-test-output parser "\e[2;3H")
          (ebb-render-refresh render)
          ;; Row 2 is empty, so the cursor sits on a virtual cell drawn via
          ;; `after-string' at EOL.
          (let ((after (overlay-start (ebb-render-state-cursor-overlay render))))
            (should (/= before after))
            (should (overlay-get
                     (ebb-render-state-cursor-overlay render) 'after-string))
            (save-excursion
              (goto-char (ebb-render-state-display-begin render))
              (forward-line 1)
              (should (= after (point))))))))))

(ert-deftest ebb-test-render-keeps-display-begin-at-viewport-start ()
  "Updating row zero does not move the scrollback/display boundary."
  (let* ((screen (ebb-screen-create 6 4))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-render-refresh render)
        (ebb-test-output parser "prompt")
        (ebb-render-refresh render)
        (should (= (point-min)
                   (marker-position
                    (ebb-render-state-display-begin render))))
        ;; A full-screen program must own every viewport row even when the
        ;; shell cursor was previously in the middle of the screen.
        (ebb-test-output
         parser
         "\e[3;1Hmiddle\e[?1049h\e[1;1HAAAAA\e[2;1HBBBBB\e[3;1HCCCCC\e[4;1HDDDDD")
        (ebb-render-refresh render)
        (should (= (point-min)
                   (marker-position
                    (ebb-render-state-display-begin render))))
        (should (equal '("AAAAA" "BBBBB" "CCCCC" "DDDDD")
                       (ebb-test-display-text screen)))
        (should (equal "AAAAA\nBBBBB\nCCCCC\nDDDDD"
                       (buffer-string)))))))

(ert-deftest ebb-test-render-emacs-mode-preserves-view ()
  "Emacs input mode preserves the reading view while the cursor advances."
  (let* ((screen (ebb-screen-create 20 40))
         (parser (ebb-parse-create screen))
         (buffer (generate-new-buffer " *ebb-test-view*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (let ((render (ebb-render-create screen buffer)))
            (ebb-test-output parser "\e[11;1H0123456789")
            (ebb-render-refresh render)
            (setq-local ebb--input-mode 'emacs)
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
                               (ebb-render-state-cursor-overlay render))))
              ;; Replacing the row containing point must not collapse point or mark.
              (ebb-test-output parser "\e[11;1Habc")
              (ebb-render-refresh render)
              (should (/= old-cursor
                          (overlay-start
                           (ebb-render-state-cursor-overlay render))))
              (should (= saved-point (point)))
              (should (= saved-mark (mark)))
              (should mark-active)
              (should (= saved-start (window-start)))
              (should (= saved-char (char-after)))
              ;; A width change must preserve line/column, not a raw offset.
              (ebb-screen-resize screen 25 40)
              (ebb-render-full-reset render)
              (should (= saved-point-line (line-number-at-pos)))
              (should (= saved-point-column (current-column)))
              (should (= saved-mark-line (line-number-at-pos (mark))))
              (should mark-active)
              (should (= saved-start-line
                         (line-number-at-pos (window-start))))
              (should (= saved-char (char-after))))
            (setq-local ebb--input-mode 'semi-char)
            (ebb-test-output parser "\e[26;4H")
            (ebb-render-refresh render)
            (should (= (point)
                       (overlay-start
                        (ebb-render-state-cursor-overlay render))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ebb-test-render-emacs-mode-history-point-stops-following ()
  "A history point prevents a viewport-anchored window from following output."
  (let* ((screen (ebb-screen-create 8 3))
         (buffer (generate-new-buffer " *ebb-test-history-follow*")))
    (setf (ebb-screen-scrollback screen)
          (cl-loop for id downfrom 9 to 0
                   collect (make-ebb-history-line
                            :id id :text (format "%03d" id)
                            :text-length 3))
          (ebb-screen-scrollback-length screen) 10
          (ebb-screen-history-next-id screen) 10
          (ebb-screen-history-generation screen) 1
          (ebb-screen-history-logical-p screen) t)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (setq-local ebb--input-mode 'emacs)
          (let ((render (ebb-render-create screen buffer)))
            (ebb-render--rebuild-scrollback render 0 10 10 1)
            (ebb-render-goto-location render 9 0 t)
            ;; Model the transient state after C-p moved point into history,
            ;; but before redisplay moved a bottom-following window-start.
            (set-window-start (selected-window)
                              (ebb-render-state-display-begin render) t)
            (let ((point-anchor (ebb-render-buffer-anchor render)))
              (push (make-ebb-history-line :id 10 :text "010" :text-length 3)
                    (ebb-screen-scrollback screen))
              (setf (ebb-screen-scrollback-length screen) 11
                    (ebb-screen-history-next-id screen) 11
                    (ebb-screen-history-generation screen) 2
                    (ebb-screen-scrollback-dirty screen) t)
              (ebb-render-refresh render)
              (should (equal point-anchor
                             (ebb-render-buffer-anchor render)))
              (should (equal point-anchor
                             (ebb-render-buffer-anchor
                              render (window-start))))
              ;; Once normalized to a history anchor, later output preserves
              ;; both point and the reading window.
              (let ((start-anchor
                     (ebb-render-buffer-anchor render (window-start))))
                (push (make-ebb-history-line
                       :id 11 :text "011" :text-length 3)
                      (ebb-screen-scrollback screen))
                (setf (ebb-screen-scrollback-length screen) 12
                      (ebb-screen-history-next-id screen) 12
                      (ebb-screen-history-generation screen) 3
                      (ebb-screen-scrollback-dirty screen) t)
                (ebb-render-refresh render)
                (should (equal point-anchor
                               (ebb-render-buffer-anchor render)))
                (should (equal start-anchor
                               (ebb-render-buffer-anchor
                                render (window-start))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ebb-test-render-nonselected-window-point-survives-refresh ()
  "Window-point in history survives a scrollback rebuild while unselected."
  (let* ((screen (ebb-screen-create 20 6))
         (parser (ebb-parse-create screen))
         (buffer (generate-new-buffer " *ebb-test-wp*"))
         (other (generate-new-buffer " *ebb-test-wp-other*")))
    (cl-flet ((point-line ()
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char (window-point
                                (get-buffer-window buffer)))
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer buffer)
            (setq-local ebb--input-mode 'emacs)
            (let ((render (ebb-render-create screen buffer)))
              (setq-local ebb--render render)
              (dotimes (i 200)
                (ebb-test-output parser (format "line-%04d\r\n" i)))
              (ebb-render-refresh render)
              ;; Read history in this window, then work elsewhere while
              ;; output keeps arriving.
              (ebb-render-scroll-history render -60)
              (let ((line (point-line)))
                (select-window (split-window))
                (switch-to-buffer other)
                (dotimes (i 20)
                  (ebb-test-output parser (format "more-%04d\r\n" i)))
                (with-current-buffer buffer
                  (ebb-render-refresh render))
                (should (equal line (point-line))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (buffer-live-p other) (kill-buffer other))))))

(ert-deftest ebb-test-render-scroll-keeps-distant-window-point ()
  "Scrolling one window preserves another window's distant history point.
Slab sizing must cover window points as well as window starts, or a
nonselected point parked far from its window start loses its anchor
and falls back to the window start."
  (let* ((screen (ebb-screen-create 20 6))
         (parser (ebb-parse-create screen))
         (buffer (generate-new-buffer " *ebb-test-distant-point*")))
    (cl-flet ((line-at (position)
                (with-current-buffer buffer
                  (save-excursion
                    (goto-char position)
                    (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (switch-to-buffer buffer)
            (setq-local ebb--input-mode 'emacs)
            (let ((render (ebb-render-create screen buffer))
                  (other (split-window)))
              (setq-local ebb--render render)
              (set-window-buffer other buffer)
              (dotimes (i 400)
                (ebb-test-output parser (format "line-%04d\r\n" i)))
              (ebb-render-refresh render)
              (let ((total (ebb-screen-history-row-count screen)))
                ;; Materialize the whole history so both positions exist,
                ;; then park the other window reading near the bottom with
                ;; its point far above the reading position.
                (ebb-render--rebuild-scrollback
                 render 0 total total
                 (ebb-screen-history-generation screen))
                (let ((region-begin
                       (ebb-render-state-region-begin render)))
                  (set-window-start
                   other
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char region-begin)
                       (forward-line 330)
                       (point)))
                   t)
                  (set-window-point
                   other
                   (with-current-buffer buffer
                     (save-excursion
                       (goto-char region-begin)
                       (forward-line 90)
                       (point)))))
                (set-window-start (selected-window)
                                  (ebb-render-state-display-begin render) t)
                (let ((line (line-at (window-point other))))
                  ;; Scroll the selected window to a region that excludes
                  ;; the other window's point unless points are sized in.
                  (should (= 200 (ebb-render-scroll-history
                                  render (- 200 total))))
                  (should (equal line (line-at (window-point other))))))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest ebb-test-render-cursor-follow-avoids-slab-seam ()
  "Following the cursor never lets a window render across the slab seam.
A far-back scroll materializes a bounded slab that does not extend to
the viewport.  When output arrives in a live input mode, the window
must snap to the viewport top rather than letting redisplay scroll
across the unmaterialized gap."
  (let* ((screen (ebb-screen-create 20 6))
         (parser (ebb-parse-create screen))
         (buffer (generate-new-buffer " *ebb-test-seam*")))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (setq-local ebb--input-mode 'semi-char)
          (let ((render (ebb-render-create screen buffer))
                (window (selected-window)))
            (setq-local ebb--render render)
            (dotimes (i 2000)
              (ebb-test-output parser (format "line-%04d\r\n" i)))
            (ebb-render-refresh render)
            ;; Scroll far enough back that the slab cannot cover the
            ;; range up to the viewport.
            (ebb-render-scroll-history render -1500)
            (let ((total (ebb-screen-history-row-count screen))
                  (slab-end (+ (ebb-render-state-history-start-row render)
                               (ebb-render-state-scrollback-count render))))
              (should (< slab-end total)))
            ;; New output: the cursor update must move the reading window
            ;; to the viewport, not leave it for redisplay to drag across
            ;; the seam.
            (ebb-test-output parser "tail-line\r\n")
            (ebb-render-refresh render)
            (should (= (window-start window)
                       (marker-position
                        (ebb-render-state-display-begin render))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ebb-test-ebb-numeric-prefix-switches-to-existing ()
  "A numeric prefix switches to the Nth session instead of erroring."
  (let ((buffer (generate-new-buffer "*ebb-test-prefix*")))
    (unwind-protect
        (cl-letf (((symbol-function 'ebb--buffers)
                   (lambda () (list buffer))))
          (save-window-excursion
            (let ((current-prefix-arg 1))
              (ebb)
              (should (eq (current-buffer) buffer)))))
      (kill-buffer buffer))))

(ert-deftest ebb-test-render-emacs-mode-trimmed-point-keeps-viewport ()
  "A trimmed history point does not pull a live window into scrollback."
  (let* ((screen (ebb-screen-create 8 3))
         (buffer (generate-new-buffer " *ebb-test-trimmed-history-point*")))
    (setf (ebb-screen-scrollback screen)
          (cl-loop for id downfrom 9 to 0
                   collect (make-ebb-history-line
                            :id id :text (format "%03d" id)
                            :text-length 3))
          (ebb-screen-scrollback-length screen) 10
          (ebb-screen-history-next-id screen) 10
          (ebb-screen-history-generation screen) 1
          (ebb-screen-history-logical-p screen) t)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (setq-local ebb--input-mode 'emacs)
          (let ((render (ebb-render-create screen buffer)))
            (ebb-render--rebuild-scrollback render 0 10 10 1)
            (ebb-render-goto-location render 0 0 t)
            (set-window-start (selected-window)
                              (ebb-render-state-display-begin render) t)
            ;; Replace the bounded history with newer lines, trimming the
            ;; logical line that anchors point.
            (setf (ebb-screen-scrollback screen)
                  (cl-loop for id downfrom 19 to 10
                           collect (make-ebb-history-line
                                    :id id :text (format "%03d" id)
                                    :text-length 3))
                  (ebb-screen-scrollback-length screen) 10
                  (ebb-screen-history-next-id screen) 20
                  (ebb-screen-history-generation screen) 2
                  (ebb-screen-scrollback-dirty screen) t)
            (ebb-render-refresh render)
            (should (equal '(viewport 0 0)
                           (ebb-render-buffer-anchor
                            render (window-start))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ebb-test-render-emacs-mode-preserves-separated-history-anchors ()
  "Refreshing history preserves point, mark, and a distant reading window."
  (let* ((screen (ebb-screen-create 8 3))
         (buffer (generate-new-buffer " *ebb-test-history-view*")))
    (setf (ebb-screen-scrollback screen)
          (cl-loop for id downfrom 199 to 0
                   collect (make-ebb-history-line
                            :id id :text (format "%03d" id)
                            :text-length 3))
          (ebb-screen-scrollback-length screen) 200
          (ebb-screen-history-next-id screen) 200
          (ebb-screen-history-generation screen) 1
          (ebb-screen-history-logical-p screen) t)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (setq-local ebb--input-mode 'emacs)
          (let ((render (ebb-render-create screen buffer)))
            (ebb-render--rebuild-scrollback render 10 160 200 1)
            (ebb-render-goto-location render 150 1 t)
            (set-mark (save-excursion
                        (ebb-render-goto-location render 100 2 t)
                        (point)))
            (setq mark-active t)
            (save-excursion
              (ebb-render-goto-location render 20 0 t)
              (set-window-start (selected-window) (point) t))
            (let ((point-anchor (ebb-render-buffer-anchor render))
                  (mark-anchor (ebb-render-buffer-anchor render (mark t)))
                  (start-anchor
                   (ebb-render-buffer-anchor render (window-start))))
              (cl-incf (ebb-screen-history-generation screen))
              (setf (ebb-screen-scrollback-dirty screen) t)
              (ebb-render-refresh render)
              (should (equal point-anchor
                             (ebb-render-buffer-anchor render)))
              (should (equal mark-anchor
                             (ebb-render-buffer-anchor render (mark t))))
              (should (equal start-anchor
                             (ebb-render-buffer-anchor
                              render (window-start)))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest ebb-test-render-ed2-shows-viewport-top ()
  "ED 2 makes Ctrl-L and clear show the top of the live viewport."
  (save-window-excursion
    (let* ((screen (ebb-screen-create 5 2))
           (parser (ebb-parse-create screen))
           (buffer (generate-new-buffer " *ebb-ed2-test*")))
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (setq-local ebb--input-mode 'semi-char)
            (let ((render (ebb-render-create screen buffer)))
              (setf (ebb-screen-scrollback screen)
                    (list (make-ebb-line :text "old  " :cells-valid nil))
                    (ebb-screen-scrollback-length screen) 1
                    (ebb-screen-scrollback-dirty screen) t)
              (ebb-render-refresh render)
              (set-window-start (selected-window) (point-min) t)
              (ebb-test-output parser "\e[H\e[2Jprompt")
              (ebb-render-refresh render)
              (should (= (window-start)
                         (marker-position
                          (ebb-render-state-display-begin render))))
              (ebb-screen-resize screen 6 3)
              (ebb-render-full-reset render)
              (should (= (window-start)
                         (marker-position
                          (ebb-render-state-display-begin render))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ebb-test-render-height-resize-keeps-live-viewport-visible ()
  "A minibuffer-style height resize keeps a live terminal out of scrollback."
  (save-window-excursion
    (let* ((screen (ebb-screen-create 5 4))
           (parser (ebb-parse-create screen))
           (buffer (generate-new-buffer " *ebb-height-follow-test*")))
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (setq-local ebb--input-mode 'semi-char)
            (let ((render (ebb-render-create screen buffer)))
              (setf (ebb-screen-scrollback screen)
                    (list (make-ebb-line :text "old  " :cells-valid nil))
                    (ebb-screen-scrollback-length screen) 1
                    (ebb-screen-scrollback-dirty screen) t)
              (ebb-render-refresh render)
              (ebb-test-output parser "\e[H\e[2Jprompt")
              (ebb-render-refresh render)
              (let ((display-begin
                     (ebb-render-state-display-begin render)))
                (should (= (window-start) (marker-position display-begin)))
                ;; Model Emacs moving only window-start during a temporary
                ;; shrink while window-point remains at the live cursor.
                (set-window-start (selected-window) (point-min) t)
                (should (< (window-start) (marker-position display-begin)))
                (ebb-screen-resize screen 5 3)
                (ebb-render-resize-height render)
                (should (= (window-start)
                           (marker-position display-begin))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ebb-test-render-height-resize-preserves-emacs-history-view ()
  "Emacs mode keeps an intentional history view across a height resize."
  (save-window-excursion
    (let* ((screen (ebb-screen-create 5 4))
           (buffer (generate-new-buffer " *ebb-height-emacs-test*")))
      (unwind-protect
          (with-current-buffer buffer
            (switch-to-buffer buffer)
            (setq-local ebb--input-mode 'emacs)
            (let ((render (ebb-render-create screen buffer)))
              (setf (ebb-screen-scrollback screen)
                    (list (make-ebb-line :text "old  " :cells-valid nil))
                    (ebb-screen-scrollback-length screen) 1
                    (ebb-screen-scrollback-dirty screen) t)
              (ebb-render-refresh render)
              (goto-char (ebb-render-state-display-begin render))
              (set-window-point (selected-window) (point))
              (set-window-start (selected-window) (point-min) t)
              (let ((start-anchor
                     (ebb-render-buffer-anchor render (window-start))))
                (should (eq 'history (car start-anchor)))
                (ebb-screen-resize screen 5 3)
                (ebb-render-resize-height render)
                (should (equal start-anchor
                               (ebb-render-buffer-anchor
                                render (window-start)))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest ebb-test-render-scrollback-clear-reconciles-buffer ()
  "ED 3 clears both model scrollback and rendered scrollback."
  (let* ((screen (ebb-screen-create 3 2))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (let ((render (ebb-render-create screen (current-buffer))))
        (ebb-test-output parser "A\e[2;1HB")
        (ebb-screen-index screen)
        (ebb-render-refresh render)
        (should (= 1 (ebb-render-state-scrollback-count render)))
        (should (string-prefix-p "A\n" (buffer-string)))
        (ebb-screen-erase-in-display screen 3)
        (ebb-render-refresh render)
        (should (= 0 (ebb-render-state-scrollback-count render)))
        (should-not (string-prefix-p "A\n" (buffer-string)))))))

(ert-deftest ebb-test-render-region-preserves-surrounding-buffer ()
  "A bounded render region never rewrites surrounding Eshell text."
  (let ((screen (ebb-screen-create 5 2)))
    (with-temp-buffer
      (insert "before\nold\nafter")
      (let ((begin (copy-marker (point-min)))
            end)
        (goto-char (point-min))
        (forward-line 1)
        (set-marker begin (point))
        (forward-line 1)
        (setq end (copy-marker (point)))
        (let ((render (ebb-render-create screen (current-buffer) begin end)))
          (ebb-screen-write-string screen "hello" 0 5)
          (ebb-render-refresh render)
          (should (string-prefix-p "before\nhello" (buffer-string)))
          (should (string-suffix-p "after" (buffer-string)))
          (ebb-screen-resize screen 4 3)
          (ebb-render-full-reset render)
          (should (string-prefix-p "before\nhell" (buffer-string)))
          (should (string-suffix-p "after" (buffer-string)))
          (ebb-render-destroy render))))))

(ert-deftest ebb-test-io-attach-keeps-existing-process ()
  "Attaching uses an existing process instead of spawning one."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         (io (make-ebb-io :screen screen :parser parser)))
    (with-temp-buffer
      (should (eq 'existing (ebb-io-attach io 'existing (current-buffer))))
      (should (eq 'existing (ebb-io-process io)))
      (should (eq (current-buffer) (ebb-io-buffer io))))))

(ert-deftest ebb-test-io-attach-disables-file-media-remotely ()
  "Attaching an Eshell-style remote process preserves the TRAMP boundary."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         (io (make-ebb-io :screen screen :parser parser)))
    (with-temp-buffer
      (setq default-directory "/ssh:example.invalid:/tmp/")
      (ebb-io-attach io 'existing (current-buffer))
      (should (ebb-io-remote io))
      (should-not (ebb-parser-graphics-file-media-enabled parser)))))

(ert-deftest ebb-test-trace-records-process-output ()
  "Tracing uses the terminal buffer instead of the process context."
  (let ((terminal (generate-new-buffer " *ebb-trace-terminal*"))
        (trace (generate-new-buffer " *ebb-trace-output*")))
    (unwind-protect
        (with-current-buffer terminal
          (setq-local ebb-trace--buffer trace)
          (let ((io (make-ebb-io :buffer terminal)))
            (ebb-trace--filter-advice #'ignore io nil "\e[31mred")
            (with-current-buffer trace
              (should (string-match-p "output.*31mred" (buffer-string))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list terminal trace)))))

(ert-deftest ebb-test-io-filter-queues-chunks-without-concat ()
  "Process filter appends chunks instead of growing one pending string."
  (let ((io (make-ebb-io)))
    (ebb-io--filter io nil "abc")
    (ebb-io--filter io nil "def")
    (when (ebb-io-render-timer io)
      (cancel-timer (ebb-io-render-timer io)))
    (should (equal '("abc" "def") (ebb-io-pending-chunks io)))
    (should (= 0 (ebb-io-pending-offset io)))))

(ert-deftest ebb-test-io-pixel-size-updates-are-coalesced ()
  "Repeated resizes leave only one pending pixel-size helper launch.
Rows and columns are still applied synchronously on every call; only
the pixel helper is coalesced."
  (let* ((io (make-ebb-io :process 'pty :render 'render))
         scheduled
         (cancelled 0)
         (spawns 0)
         (resizes 0)
         helper-command)
    (cl-letf (((symbol-function 'set-process-window-size)
               (lambda (&rest _) (cl-incf resizes)))
              ((symbol-function 'display-graphic-p) (lambda () t))
              ((symbol-function 'ebb-render-cell-pixel-size)
               (lambda (_) '(10 . 20)))
              ((symbol-function 'process-tty-name) (lambda (_) "/dev/pts/1"))
              ((symbol-function 'ebb-io--pixel-size-helper)
               (lambda () '("helper")))
              ((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))
                 (list 'timer args)))
              ((symbol-function 'cancel-timer)
               (lambda (_) (cl-incf cancelled)))
              ((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (cl-incf spawns)
                 (setq helper-command (plist-get args :command))
                 'helper-process)))
      (ebb-io-set-window-size io 'pty 20 80)
      (ebb-io-set-window-size io 'pty 21 81)
      (should (= 1 cancelled))
      (should (= 0 spawns))
      ;; The base resize is synchronous even while the pixel helper waits.
      (should (= 2 resizes))
      (apply (car scheduled) (cdr scheduled))
      (should (= 1 spawns))
      (should (equal '("helper" "/dev/pts/1" "21" "81" "10" "20")
                     helper-command)))))

(ert-deftest ebb-test-io-pixel-size-failure-disables-retries ()
  "The first helper failure disables later launches for the terminal."
  (let ((io (make-ebb-io :process 'pty :render 'render))
        scheduled)
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 1))
              ((symbol-function 'message) #'ignore))
      (ebb-io--pixel-size-sentinel io 'helper "failed"))
    (should (ebb-io-pixel-size-error io))
    (cl-letf (((symbol-function 'set-process-window-size) #'ignore)
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq scheduled t))))
      (ebb-io-set-window-size io 'pty 20 80))
    (should-not scheduled)))

(ert-deftest ebb-test-io-runs-post-render-functions ()
  "Post-render integration uses the documented abnormal hook."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         seen)
    (with-temp-buffer
      (let* ((render (ebb-render-create screen (current-buffer)))
             (io (make-ebb-io :screen screen :parser parser :render render
                              :buffer (current-buffer)))
             (ebb-io-after-render-functions
              (list (lambda (value) (setq seen value)))))
        (ebb-io--enqueue-output io "OK")
        (ebb-io--process-pending io t)
        (should (eq seen render))))))

(ert-deftest ebb-test-io-holds-frame-during-synchronized-output ()
  "DEC 2026 defers rendering until the application releases the frame."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         (renders 0))
    (with-temp-buffer
      (let* ((render (ebb-render-create screen (current-buffer)))
             (io (make-ebb-io :screen screen :parser parser :render render
                              :buffer (current-buffer))))
        (cl-letf (((symbol-function 'ebb-render-refresh)
                   (lambda (_) (cl-incf renders)))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'timer))
                  ((symbol-function 'cancel-timer) #'ignore))
          ;; Output inside an open synchronized-update window is parsed
          ;; but not rendered.
          (ebb-io--enqueue-output io "\e[?2026hOK")
          (ebb-io--process-pending io)
          (should (ebb-screen-sync-output screen))
          (should (= renders 0))
          (should (ebb-io-sync-timer io))
          ;; The release flushes the held frame.
          (ebb-io--enqueue-output io "\e[?2026l")
          (ebb-io--process-pending io t)
          (should (= renders 1))
          (should-not (ebb-io-sync-timer io)))))))

(ert-deftest ebb-test-io-synchronized-output-timeout-forces-refresh ()
  "A stuck synchronized-output window only delays its redraw."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         (renders 0))
    (with-temp-buffer
      (let* ((render (ebb-render-create screen (current-buffer)))
             (io (make-ebb-io :screen screen :parser parser :render render
                              :buffer (current-buffer))))
        (cl-letf (((symbol-function 'ebb-render-refresh)
                   (lambda (_) (cl-incf renders))))
          (ebb-io--enqueue-output io "\e[?2026hOK")
          (ebb-io--process-pending io)
          (should (= renders 0))
          ;; The safety flush renders even though the mode is still set.
          (ebb-io--sync-timeout io)
          (should (= renders 1))
          (should-not (ebb-io-sync-timer io)))))))

(ert-deftest ebb-test-io-processing-errors-are-counted ()
  "Repeated asynchronous processing errors reach the error hook with a count."
  (let* ((screen (ebb-screen-create 5 2))
         (parser (ebb-parse-create screen))
         counts)
    (with-temp-buffer
      (let* ((render (ebb-render-create screen (current-buffer)))
             (io (make-ebb-io :screen screen :parser parser :render render
                              :buffer (current-buffer)))
             (ebb-io-processing-error-functions
              (list (lambda (_io _error count) (push count counts)))))
        (ebb-io--enqueue-output io "bad")
        (cl-letf (((symbol-function 'ebb-parse-bytes)
                   (lambda (&rest _) (error "test failure"))))
          (ebb-io--process-pending io t)
          (ebb-io--process-pending io t))
        (should (equal '(2 1) counts))))))

(ert-deftest ebb-test-io-sentinel-emits-in-terminal-buffer ()
  "Process exit events run with the terminal buffer current."
  (let* ((screen (ebb-screen-create 5 2))
         (target-buffer (generate-new-buffer " *ebb-test-sentinel*"))
         (seen-buffer nil))
    (unwind-protect
        (let* ((parser (ebb-parse-create
                        screen nil
                        (lambda (_type &rest _args)
                          (setq seen-buffer (current-buffer)))))
               (io (make-ebb-io :screen screen :parser parser
                                  :buffer target-buffer)))
          (with-temp-buffer
            (ebb-io--sentinel io nil "finished\n"))
          (should (eq seen-buffer target-buffer)))
      (when (buffer-live-p target-buffer)
        (kill-buffer target-buffer)))))

(ert-deftest ebb-test-interactive-program-arguments ()
  "Interactive program input is split into argv without changing API input."
  (cl-labels
      ((start (program interactive)
         (let (buffer started)
           (unwind-protect
               (save-window-excursion
                 (cl-letf (((symbol-function 'read-shell-command)
                            (lambda (&rest _) program))
                           ((symbol-function 'ebb-io-start)
                            (lambda (_io command _buffer &optional _env)
                              (setq started command))))
                   (setq buffer
                         (if interactive
                             (let ((current-prefix-arg '(16)))
                               (call-interactively #'ebb))
                           (ebb program)))
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

(ert-deftest ebb-test-notification-callback-deferred-in-origin-buffer ()
  "Notification callbacks are deferred in their terminal buffer."
  (let ((origin (generate-new-buffer " *ebb-notify-origin*"))
        (other (generate-new-buffer " *ebb-notify-other*"))
        timer seen)
    (unwind-protect
        (with-current-buffer origin
          (let ((ebb-notification-function
                 (lambda (title body)
                   (setq seen (list (current-buffer) title body)))))
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (_time _repeat function &rest args)
                         (setq timer (cons function args)))))
              (ebb--handle-event 'notification "title" "body")))
          (should-not seen)
          (with-current-buffer other
            (apply (car timer) (cdr timer)))
          (should (equal (list origin "title" "body") seen)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list origin other)))))

(ert-deftest ebb-test-notification-and-progress-callback-errors-isolated ()
  "Terminal callback failures do not escape event handling."
  (with-temp-buffer
    (let ((ebb-notification-function (lambda (&rest _) (error "notify")))
          (ebb-progress-function (lambda (&rest _) (error "progress"))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat function &rest args)
                   (apply function args))))
        (should-not (ebb--handle-event 'notification nil "body"))
        (should-not (ebb--handle-event 'progress 'set 10))))))

(ert-deftest ebb-test-progress-callback-and-mode-line-compose ()
  "Progress callbacks receive normalized args and default display composes."
  (with-temp-buffer
    (ebb-mode)
    (ebb-semi-char-mode)
    (let* (seen
           (ebb-progress-function
            (lambda (state percent) (setq seen (list state percent)))))
      (ebb--handle-event 'progress 'pause 25)
      (should (equal '(pause 25) seen)))
    (let ((ebb-progress-function #'ebb--default-progress))
      (ebb--handle-event 'progress 'set 80)
      (should (equal "[Semi] [80%]" (ebb--mode-line-input-mode)))
      (ebb--handle-event 'progress 'remove 0)
      (should (equal "[Semi]" (ebb--mode-line-input-mode))))))

;;;; ---- Mode Line Tests ------------------------------------------------

(ert-deftest ebb-test-mode-disables-undo-history ()
  "Generated terminal output does not accumulate undo history."
  (with-temp-buffer
    (ebb-mode)
    (should (eq buffer-undo-list t))
    (let ((inhibit-read-only t))
      (insert "streamed output")
      (delete-region (point-min) (point-max)))
    (should (eq buffer-undo-list t))))

(ert-deftest ebb-test-mode-disables-fontification ()
  "Ebb prevents deferred fontification from scanning terminal scrollback."
  (let (emojify-argument)
    (cl-letf (((symbol-function 'emojify-mode)
               (lambda (argument) (setq emojify-argument argument))))
      (with-temp-buffer
        (ebb-mode)
        (should-not font-lock-mode)
        (should (equal -1 emojify-argument))))))

(ert-deftest ebb-test-mode-line-input-mode-installed ()
  "Ebb mode keeps its name and refreshes the cursor when input mode changes."
  (with-temp-buffer
    (ebb-mode)
    (setq-local ebb--render 'render)
    (let (refreshed)
      (cl-letf (((symbol-function 'ebb-render--update-cursor)
                 (lambda (_) (push ebb--input-mode refreshed))))
        (should (equal "Ebb" mode-name))
        (should (equal '(" " (:eval (ebb--mode-line-input-mode)))
                       mode-line-process))
        (ebb-semi-char-mode)
        (should (equal "[Semi]" (ebb--mode-line-input-mode)))
        (should (equal "Ebb" mode-name))
        (ebb-char-mode)
        (should (equal "[Char]" (ebb--mode-line-input-mode)))
        (should (equal "Ebb" mode-name))
        (ebb-emacs-mode)
        (should (equal "[Emacs]" (ebb--mode-line-input-mode)))
        (should (equal "Ebb" mode-name))
        (should (equal '(emacs char semi-char) refreshed))))))

;;;; ---- Clear and Copy Tests -------------------------------------------

(ert-deftest ebb-test-clear-and-clear-scrollback ()
  "Clear commands erase the viewport, optionally history, and redraw prompt."
  (dolist (clear '((ebb-clear . nil) (ebb-clear-scrollback . t)))
    (let* ((screen (ebb-screen-create 5 2))
           (parser (ebb-parse-create screen))
           redraw)
      (ebb-test-output parser "abc\r\ndef")
      (setf (ebb-screen-scrollback screen)
            (list (make-ebb-line :text "old  " :cells-valid nil)))
      (setf (ebb-screen-scrollback-length screen) 1)
      (with-temp-buffer
        (insert "x")
        (setq major-mode 'ebb-mode)
        (setq-local ebb--screen screen)
        (setq-local ebb--io (make-ebb-io :process 'fake))
        (setq-local ebb-enable-shell-prompt-annotation nil)
        (setq-local left-margin-width 1)
        (let ((overlay (make-overlay (point-min) (point-max))))
          (overlay-put overlay 'before-string "0")
          (setq-local ebb-shell--prompt-overlays (list overlay))
          (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                    ((symbol-function 'ebb-send-key)
                     (lambda (key modifiers)
                       (setq redraw (list key modifiers)))))
            (funcall (car clear)))
          (should-not (overlay-buffer overlay))
          (should-not ebb-shell--prompt-overlays)
          (should-not left-margin-width)))
      (should (equal '(0 . 0) (ebb-test-cursor screen)))
      (should (equal '("" "") (ebb-test-display-text screen)))
      (should (equal '("l" "control") redraw))
      (if (cdr clear)
          (should-not (ebb-screen-scrollback screen))
        ;; Existing history plus the two meaningful viewport rows.
        (should (= 3 (ebb-screen-scrollback-length screen)))))))

(ert-deftest ebb-test-copy-all-plain-text-soft-wraps ()
  "Copy-all trims padding and omits newlines across soft-wrapped rows."
  (let* ((screen (ebb-screen-create 4 3))
         (parser (ebb-parse-create screen))
         copied)
    (ebb-test-output parser "abcdE\r\nlast")
    (setf (ebb-screen-scrollback screen)
          (list (make-ebb-line :text "old " :cells-valid nil)))
    (setf (ebb-screen-scrollback-length screen) 1)
    (should (equal "old\nabcdE\nlast" (ebb-screen-plain-text screen)))
    (with-temp-buffer
      (setq major-mode 'ebb-mode)
      (setq-local ebb--screen screen)
      (cl-letf (((symbol-function 'kill-new)
                 (lambda (text &optional _) (setq copied text))))
        (should (equal "old\nabcdE\nlast" (ebb-copy-all)))))
    (should (equal "old\nabcdE\nlast" copied))))

(ert-deftest ebb-test-copy-all-plain-text-preserves-graphemes ()
  "Copy-all preserves combining marks and ZWJ sequences."
  (let* ((screen (ebb-screen-create 10 2))
         (parser (ebb-parse-create screen)))
    (ebb-test-output parser "ã 👩‍💻")
    (should (equal "ã 👩‍💻" (ebb-screen-plain-text screen)))))

(ert-deftest ebb-test-copy-range-reads-unmaterialized-history ()
  "Region copying reads logical history rather than rendered buffer rows."
  (let ((screen (ebb-screen-create 4 2)))
    (setf (ebb-screen-scrollback screen)
          (list (make-ebb-history-line
                 :id 2 :cells (vector (make-ebb-cell :char ?c)
                                      (make-ebb-cell :char ?c)))
                (make-ebb-history-line
                 :id 1 :cells (vector (make-ebb-cell :char ?b)
                                      (make-ebb-cell :char ?b)))
                (make-ebb-history-line
                 :id 0 :cells (vector (make-ebb-cell :char ?a)
                                      (make-ebb-cell :char ?a))))
          (ebb-screen-scrollback-length screen) 3
          (ebb-screen-history-next-id screen) 3
          (ebb-screen-history-generation screen) 1)
    (should (equal "a\nbb\nc"
                   (ebb-screen-text-range screen '(0 . 1) '(2 . 1))))))

;;;; ---- Session Tests --------------------------------------------------

(defun ebb-test--session-buffer (name &optional identity)
  "Create a lightweight Ebb buffer named NAME with IDENTITY."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq major-mode 'ebb-mode)
      (setq-local ebb--session-id (or identity name)))
    buffer))

(ert-deftest ebb-test-buffer-list-sorted-and-filtered ()
  "Session lists are sorted and completion contains only Ebb buffers."
  (let ((z (ebb-test--session-buffer " *ebb-z*"))
        (a (ebb-test--session-buffer " *ebb-a*"))
        (ordinary (generate-new-buffer " *ordinary*")))
    (unwind-protect
        (progn
          (should (equal (list a z) (ebb--buffers)))
          (with-current-buffer a
            (let (collection default)
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (_prompt choices &rest args)
                           (setq collection choices default (nth 4 args))
                           default))
                        ((symbol-function 'pop-to-buffer-same-window) #'identity))
                (should (eq z (ebb-list-buffers))))
              (should (equal '(" *ebb-a*" " *ebb-z*") collection))
              (should (equal " *ebb-z*" default)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list z a ordinary)))))

(ert-deftest ebb-test-buffer-cycle-wraps ()
  "Next and previous session navigation wrap around."
  (let ((a (ebb-test--session-buffer " *ebb-cycle-a*"))
        (b (ebb-test--session-buffer " *ebb-cycle-b*"))
        (ordinary (generate-new-buffer " *ebb-cycle-ordinary*")))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
          (with-current-buffer ordinary
            (should (eq a (ebb-next)))
            (should (eq b (ebb-previous))))
          (with-current-buffer a
            (should (eq b (ebb-next))))
          (with-current-buffer b
            (should (eq a (ebb-next))))
          (with-current-buffer a
            (should (eq b (ebb-previous)))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list a b ordinary)))))

(ert-deftest ebb-test-other-reuses-or-creates ()
  "`ebb-other' chooses another session or creates one."
  (let ((a (ebb-test--session-buffer " *ebb-other-a*"))
        (b (ebb-test--session-buffer " *ebb-other-b*"))
        created)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
          (with-current-buffer a
            (should (eq b (ebb-other))))
          (kill-buffer b)
          (with-current-buffer a
            (cl-letf (((symbol-function 'ebb)
                       (lambda (&optional _) (setq created t))))
              (should (ebb-other))
              (should created))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list a b)))))

(ert-deftest ebb-test-other-window-starts-at-destination-size ()
  "Other-window terminals start only after reaching their destination window."
  (let (buffer target started-size)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (setq target (split-window-right))
          (let ((ebb-buffer-name " *ebb-other-window-start*"))
            (cl-letf (((symbol-function 'switch-to-buffer-other-window)
                       (lambda (buf &optional _norecord)
                         (select-window target)
                         (set-window-buffer target buf)
                         buf))
                      ((symbol-function 'ebb-io-start)
                       (lambda (io _shell buf &optional _env)
                         (should (eq target (selected-window)))
                         (should (eq buf (window-buffer target)))
                         (setq started-size
                               (cons (ebb-screen-width (ebb-io-screen io))
                                     (ebb-screen-height (ebb-io-screen io)))))))
              (setq buffer (ebb-other-window '("/bin/true")))))
          (should (equal
                   (cons (window-max-chars-per-line target)
                         (window-body-height target))
                   started-size)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest ebb-test-project-reuses-renamed-buffer ()
  "Project terminals use project names and stable identities after OSC titles."
  (let* ((root (file-name-as-directory (make-temp-file "ebb-project-" t)))
         (project 'fake-project)
         buffer started name-called)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'project-current) (lambda (&rest _) project))
                    ((symbol-function 'project-root) (lambda (_) root))
                    ((symbol-function 'project-prefixed-buffer-name)
                     (lambda (mode) (setq name-called mode) "*fake-ebb*"))
                    ((symbol-function 'ebb-io-start)
                     (lambda (&rest _) (setq started (1+ (or started 0)))))
                    ((symbol-function 'pop-to-buffer-same-window) #'identity))
            (setq buffer (ebb-project))
            (should (equal "ebb" name-called))
            (should (equal "*fake-ebb*" (buffer-name buffer)))
            (should (equal (file-truename root)
                           (buffer-local-value 'ebb--session-id buffer)))
            (with-current-buffer buffer
              (ebb--handle-event 'title "renamed"))
            (should (eq buffer (ebb-project)))
            (should (= 1 started))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t))))

(ert-deftest ebb-test-buffer-name-strips-local-host ()
  "Local user@host is stripped; remote host kept; directory names abbreviate."
  (let ((host (system-name))
        (user (user-login-name)))
    (should (equal (format "*ebb: ~/src/ebb$*")
                   (ebb-buffer-name-by-title
                    (format "%s@%s:~/src/ebb$" user host))))
    (should (equal (format "*ebb: alice@otherbox:~/x*")
                   (ebb-buffer-name-by-title "alice@otherbox:~/x")))
    (let ((ebb-buffer-name-title-prefix "ssh: "))
      (should (equal "*ssh: alice@otherbox:~/x*"
                     (ebb-buffer-name-by-title "alice@otherbox:~/x"))))
    (let ((default-directory (expand-file-name "src/ebb/" "~")))
      (should (equal "*ebb: ~/src/ebb*"
                     (ebb-buffer-name-by-directory nil))))
    (let ((default-directory "/ssh:remote:/home/arthur/src/"))
      (should (string-match-p "remote"
                              (ebb-buffer-name-by-directory nil))))))

;;;; ---- Bookmark Tests -------------------------------------------------

(ert-deftest ebb-test-bookmark-record-and-renamed-session-reuse ()
  "Bookmark records retain session metadata and reuse renamed buffers."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "ebb-bookmark-" t)))
         (buffer (generate-new-buffer "*bookmark-terminal*"))
         record)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory directory)
            (ebb-mode)
            (setq-local ebb--session-id "stable-id")
            (should (eq #'ebb-bookmark-make-record
                        bookmark-make-record-function))
            (setq record (ebb-bookmark-make-record))
            (rename-buffer "*renamed-terminal*"))
          (should (equal "*bookmark-terminal*" (car record)))
          (should (equal directory
                         (alist-get 'ebb-directory (cdr record))))
          (should (equal "*bookmark-terminal*"
                         (alist-get 'ebb-display-name (cdr record))))
          (should (equal "stable-id"
                         (alist-get 'ebb-session-id (cdr record))))
          (cl-letf (((symbol-function 'pop-to-buffer-same-window) #'identity))
            (should (eq buffer (ebb-bookmark-jump (cdr record))))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory directory t))))

(ert-deftest ebb-test-bookmark-creates-missing-session-in-directory ()
  "A missing bookmarked session is recreated in its saved directory."
  (let* ((directory (file-name-as-directory
                     (make-temp-file "ebb-bookmark-new-" t)))
         (record `((ebb-directory . ,directory)
                   (ebb-display-name . "*saved-terminal*")
                   (ebb-session-id . "missing-id")))
         created seen-directory seen-name)
    (unwind-protect
        (cl-letf (((symbol-function 'ebb)
                   (lambda (&optional _)
                     (setq seen-directory default-directory
                           seen-name ebb-buffer-name
                           created (ebb-test--session-buffer seen-name))))
                  ((symbol-function 'pop-to-buffer-same-window) #'identity))
          (let ((result (ebb-bookmark-jump record)))
            (should (eq created result)))
          (should (equal directory seen-directory))
          (should (equal "*saved-terminal*" seen-name))
          (should (equal "missing-id"
                         (buffer-local-value 'ebb--session-id created))))
      (when (buffer-live-p created) (kill-buffer created))
      (delete-directory directory t))))

(ert-deftest ebb-test-bookmark-reused-session-changes-directory ()
  "Reusing a local session sends a quoted cd and Return via public input."
  (let* ((old-directory (file-name-as-directory
                         (make-temp-file "ebb-bookmark-old-" t)))
         (directory (file-name-as-directory
                     (make-temp-file "ebb bookmark new-" t)))
         (buffer (ebb-test--session-buffer "*bookmark-reuse*" "reuse-id"))
         (record `((ebb-directory . ,directory)
                   (ebb-display-name . "*saved*")
                   (ebb-session-id . "reuse-id")))
         calls)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory old-directory))
          (cl-letf (((symbol-function 'ebb-send-string)
                     (lambda (string) (push (list 'string string) calls)))
                    ((symbol-function 'ebb-send-key)
                     (lambda (key &optional modifiers)
                       (push (list 'key key modifiers) calls)))
                    ((symbol-function 'pop-to-buffer-same-window) #'identity))
            (should (eq buffer (ebb-bookmark-jump record))))
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

(require 'ebb-shell)

(ert-deftest ebb-test-shell-base64-decode ()
  "Base64 decode works for valid input."
  (should (equal "hello" (ebb-shell--base64-decode (base64-encode-string "hello"))))
  (should (equal "/home/arthur" (ebb-shell--base64-decode
                                  (base64-encode-string "/home/arthur"))))
  ;; Invalid base64 returns nil
  (should-not (ebb-shell--base64-decode "!!invalid!!")))

(ert-deftest ebb-test-shell-split-payload ()
  "Payload splitting works."
  (should (equal '("foo" "bar") (ebb-shell--split-payload "e;A;foo;bar" 4)))
  (should (equal '("0") (ebb-shell--split-payload "e;H;0" 4)))
  ;; Past end returns nil
  (should-not (ebb-shell--split-payload "e;B" 4)))

(ert-deftest ebb-test-shell-osc51-dispatch ()
  "OSC 51 dispatch parses command letters correctly."
  (with-temp-buffer
      ;; Set up buffer-local state
      (setq-local ebb-enable-shell-prompt-annotation nil)  ; disable annotation
      (setq-local ebb-enable-directory-tracking nil)       ; disable for test
      (setq-local ebb-shell--pending-events nil)
      (setq-local ebb-shell--command-status 0)
      (setq-local ebb-shell--current-command nil)
      ;; Test exit status
      (ebb-shell-handle-osc51 "e;H;42" nil)
      (should (= 42 ebb-shell--command-status))
      ;; Test command text
      (let ((cmd-b64 (base64-encode-string "ls -la" t)))
        (ebb-shell-handle-osc51 (concat "e;F;" cmd-b64) nil)
        (should (equal "ls -la" ebb-shell--current-command)))))

(ert-deftest ebb-test-shell-message-default-deny ()
  "OSC 51 messages do nothing with the default nil whitelist."
  (should-not (default-value 'ebb-shell-message-handler-alist))
  (let ((called nil)
        (payload (format "e;M;%s;%s"
                         (base64-encode-string "handler" t)
                         (base64-encode-string "argument" t))))
    (with-temp-buffer
      (let ((ebb-shell-message-handler-alist nil))
        (ebb-shell-handle-osc51 payload nil)))
    (should-not called)))

(ert-deftest ebb-test-shell-message-whitelist ()
  "Only valid named OSC 51 messages invoke decoded handlers."
  (let (calls)
    (with-temp-buffer
      (let ((ebb-shell-message-handler-alist
             `(("allowed" . ,(lambda (&rest args) (push args calls)))
               ("error" . ,(lambda (&rest _) (error "handler failed"))))))
        (ebb-shell-handle-osc51
         (format "e;M;%s;%s;%s"
                 (base64-encode-string "allowed" t)
                 (ebb-test-base64 "héllo")
                 (ebb-test-base64 "世界")) nil)
        (ebb-shell-handle-osc51
         (format "e;M;%s;;%s"
                 (base64-encode-string "allowed" t)
                 (base64-encode-string "tail" t)) nil)
        (ebb-shell-handle-osc51
         (format "e;M;%s;%s" (base64-encode-string "unknown" t)
                 (base64-encode-string "ignored" t)) nil)
        (ebb-shell-handle-osc51 "e;M;not-base64" nil)
        (ebb-shell-handle-osc51
         (format "e;M;%s;%s"
                 (base64-encode-string "allowed" t)
                 (base64-encode-string (unibyte-string 255) t)) nil)
        (ebb-shell-handle-osc51
         (format "e;M;%s" (base64-encode-string "error" t)) nil)
        ;; A failed handler must not prevent the next message from parsing.
        (ebb-shell-handle-osc51
         (format "e;M;%s;%s" (base64-encode-string "allowed" t)
                 (base64-encode-string "after" t)) nil)))
    (should (equal '(("after") ("" "tail") ("héllo" "世界"))
                   calls))))

(ert-deftest ebb-test-shell-osc51-cwd ()
  "OSC 51 CWD tracking decodes base64 host and path."
  (with-temp-buffer
    (setq-local ebb-enable-directory-tracking t)
    (setq-local default-directory "/tmp/")
    (let* ((host (system-name))
           (path (temporary-file-directory))
           (payload (format "e;A;%s;%s"
                            (base64-encode-string host t)
                            (base64-encode-string
                             (directory-file-name path) t))))
      (ebb-shell-handle-osc51 payload nil)
      (should (equal (file-name-as-directory (directory-file-name path))
                     default-directory)))))

(ert-deftest ebb-test-shell-indicators ()
  "Prompt annotation indicators are opt-in and have correct faces."
  (should-not (default-value 'ebb-enable-shell-prompt-annotation))
  (let ((running (ebb-shell--running-indicator))
        (success (ebb-shell--status-indicator 0))
        (failure (ebb-shell--status-indicator 1)))
    (should (string= "+" running))
    (should (string= "0" success))
    (should (string= "X" failure))
    ;; Check faces
    (should (memq 'ebb-shell-prompt-annotation-running
                  (get-text-property 0 'face running)))
    (should (memq 'ebb-shell-prompt-annotation-success
                  (get-text-property 0 'face success)))
    (should (memq 'ebb-shell-prompt-annotation-failure
                  (get-text-property 0 'face failure)))))

(ert-deftest ebb-test-shell-absolute-line ()
  "Absolute line calculation works."
  (let ((screen (ebb-screen-create 20 6)))
    ;; Cursor at (0,0) with no scrollback = line 0
    (should (= 0 (ebb-shell--absolute-line screen)))
    ;; Move cursor to row 3
    (setf (ebb-screen-cursor-y screen) 3)
    (should (= 3 (ebb-shell--absolute-line screen)))))

(ert-deftest ebb-test-shell-env-vars ()
  "Shell env vars include integration directory and terminfo."
  (let ((vars (ebb-shell-env-vars)))
    (should (= 3 (length vars)))
    (should (string-prefix-p "EBB_SHELL_INTEGRATION_DIR=" (car vars)))
    (should (string-prefix-p "EAT_SHELL_INTEGRATION_DIR=" (cadr vars)))
    (should (string-prefix-p "TERMINFO=" (caddr vars)))))

(ert-deftest ebb-test-shell-prompt-metadata-without-annotations ()
  "Prompt navigation metadata does not require visual annotations."
  (let* ((screen (ebb-screen-create 20 6))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (setq-local ebb-enable-shell-prompt-annotation nil)
      (setq-local ebb-shell--pending-events nil)
      (setq-local ebb-shell--prompt-start-line nil)
      (setq-local ebb-shell--prompt-overlays nil)
      (let ((render (ebb-render-create screen (current-buffer))))
        ;; Queue two complete prompts before one render pass.
        (ebb-shell-handle-osc51 "e;B" screen)
        (ebb-test-output parser "first")
        (ebb-shell-handle-osc51 "e;C" screen)
        (ebb-test-output parser "\r\n\r\n")
        (ebb-shell-handle-osc51 "e;B" screen)
        (ebb-test-output parser "second")
        (ebb-shell-handle-osc51 "e;C" screen)
        (ebb-render-refresh render)
        (ebb-shell-post-render render)
        (let* ((first-begin
                (text-property-any (point-min) (point-max)
                                   'ebb-shell-prompt-begin t))
               (first-end
                (text-property-any (point-min) (point-max)
                                   'ebb-shell-prompt-end t))
               (second-end
                (and first-end
                     (text-property-any (1+ first-end) (point-max)
                                        'ebb-shell-prompt-end t))))
          (should first-begin)
          (should first-end)
          (should second-end)
          ;; Later replacement of a prompt row must retain model metadata.
          ;; Note the replacement may change the row's rendered length
          ;; (rows are trimmed), so recapture positions afterwards.
          (ebb-test-output parser "\e[1;6H!")
          (ebb-render-refresh render)
          (ebb-shell-post-render render)
          (should (get-text-property first-begin 'ebb-shell-prompt-begin))
          (setq first-begin
                (text-property-any (point-min) (point-max)
                                   'ebb-shell-prompt-begin t))
          (setq first-end
                (text-property-any (point-min) (point-max)
                                   'ebb-shell-prompt-end t))
          (setq second-end
                (and first-end
                     (text-property-any (1+ first-end) (point-max)
                                        'ebb-shell-prompt-end t)))
          (setq-local ebb--input-mode 'semi-char)
          (goto-char (point-max))
          (ebb-previous-prompt)
          (should (eq ebb--input-mode 'emacs))
          (should (= (point) (1+ second-end)))
          (ebb-previous-prompt)
          (should (= (point) (1+ first-end)))
          (goto-char (point-min))
          (ebb-next-prompt)
          (should (= (point) (1+ first-end)))
          (ebb-next-prompt)
          (should (= (point) (1+ second-end))))
        (should-not ebb-shell--prompt-overlays)
        (should-not ebb-shell--prompt-mark)
        (should-not
         (cl-find-if (lambda (ov)
                       (overlay-get ov 'ebb-shell-prompt))
                     (overlays-in (point-min) (point-max))))))))

(ert-deftest ebb-test-shell-prompt-imenu ()
  "Prompt metadata produces ordered command entries and navigable positions."
  (let* ((screen (ebb-screen-create 30 5))
         (parser (ebb-parse-create screen)))
    (with-temp-buffer
      (ebb-mode)
      (should (eq #'ebb-shell-imenu-create-index
                  imenu-create-index-function))
      (should (eq #'ebb-shell-imenu-goto imenu-default-goto-function))
      (let ((render (ebb-render-create screen (current-buffer))))
        (dolist (prompt-command '(("$ " . "  echo one  ")
                                  ("> " . "printf two")
                                  ("# " . "")))
          (ebb-shell-handle-osc51 "e;B" screen)
          (ebb-test-output parser (car prompt-command))
          (ebb-shell-handle-osc51 "e;C" screen)
          (ebb-test-output parser (concat (cdr prompt-command) "\r\n")))
        (ebb-render-refresh render)
        (let ((index (ebb-shell-imenu-create-index)))
          (should (equal '("echo one" "printf two") (mapcar #'car index)))
          (setq-local ebb--input-mode 'semi-char)
          (ebb-shell-imenu-goto (caar index) (cdar index))
          (should (eq ebb--input-mode 'emacs))
          (should (= (point) (marker-position (cdar index))))
          (should (looking-at-p "  echo one")))
        ;; Clearing the viewport retains command metadata in scrollback.
        (ebb-screen-erase-in-display screen 2)
        (ebb-render-refresh render)
        (should (equal '("echo one" "printf two")
                       (mapcar #'car (ebb-shell-imenu-create-index))))))))

(ert-deftest ebb-test-shell-pending-events ()
  "OSC 51 B and C sequences queue pending events."
  (let ((screen (ebb-screen-create 20 6)))
    (with-temp-buffer
      (setq-local ebb-enable-shell-prompt-annotation t)
      (setq-local ebb-shell--pending-events nil)
      (setq-local ebb-shell--prompt-start-line nil)
      ;; Prompt start
      (ebb-shell-handle-osc51 "e;B" screen)
      (should (= 1 (length ebb-shell--pending-events)))
      (should (equal '(prompt-start 0 0)
                     (car ebb-shell--pending-events)))
      (should-not ebb-shell--prompt-start-line)
      ;; Prompt end
      (ebb-shell-handle-osc51 "e;C" screen)
      (should (= 2 (length ebb-shell--pending-events)))
      (should (eq 'prompt-end (caar ebb-shell--pending-events))))))

(ert-deftest ebb-test-eshell-inline-paste-routes-to-process ()
  "Inline Eshell input uses the attached terminal process."
  (let ((io (make-ebb-io :process 'process))
        sent)
    (with-temp-buffer
      (setq-local ebb-eshell--io io)
      (setq-local ebb--io io)
      (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
                ((symbol-function 'ebb-io-send)
                 (lambda (_io string) (setq sent string))))
        (ebb-paste-string "paste")
        (should (equal sent "paste"))))))

(ert-deftest ebb-test-eshell-cleanup-restores-cursor ()
  "Finishing an inline terminal restores Eshell's native cursor."
  (with-temp-buffer
    (let* ((screen (ebb-screen-create 5 2))
           (render (ebb-render-create screen (current-buffer)))
           (io (make-ebb-io :process 'process :render render))
           (process-mark (copy-marker (point-min))))
      (setq-local ebb-eshell--io io)
      (setq-local ebb--io io)
      (setq-local eshell-last-output-start (copy-marker (point-min)))
      (setq-local eshell-last-output-end (copy-marker (point-min)))
      (setq-local cursor-type nil)
      (cl-letf (((symbol-function 'process-mark) (lambda (_) process-mark))
                ((symbol-function 'ebb-io-stop) #'ignore)
                ((symbol-function 'ebb-shell-cleanup) #'ignore)
                ((symbol-function 'ebb-eshell--semi-char-mode) #'ignore)
                ((symbol-function 'ebb-eshell--char-mode) #'ignore)
                ((symbol-function 'ebb-eshell--running-mode) #'ignore)
                ((symbol-function 'ebb--mouse-mode) #'ignore))
        (ebb-eshell--cleanup 'process))
      (should (eq cursor-type t)))))

(ert-deftest ebb-test-eshell-sentinel-cleans-before-eshell ()
  "Ebb releases its region before Eshell writes its exit status."
  (let (events)
    (with-temp-buffer
      (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                ((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                ((symbol-function 'ebb-eshell--cleanup)
                 (lambda (_) (push 'cleanup events))))
        (ebb-eshell--sentinel
         (lambda (&rest _) (push 'eshell events)) 'process "finished\n")
        (should (equal events '(eshell cleanup)))))))

;;;; ---- TRAMP integration ----------------------------------------------

(ert-deftest ebb-test-cwd-to-path ()
  "OSC 7 reports map to plain paths locally and TRAMP paths remotely."
  (with-temp-buffer
    (setq-local default-directory "/tmp/")
    ;; Local host: plain path.
    (should (equal "/home/u" (ebb--cwd-to-path "/home/u" (system-name))))
    (should (equal "/home/u" (ebb--cwd-to-path "/home/u" "")))
    ;; Remote host, no existing prefix: build via default method.
    (let ((ebb-tramp-default-method "ssh"))
      (should (equal "/ssh:box:/home/u"
                     (ebb--cwd-to-path "/home/u" "box"))))
    ;; Empty or nil dir: nil.
    (should-not (ebb--cwd-to-path "" "box"))
    (should-not (ebb--cwd-to-path nil nil)))
  (with-temp-buffer
    (cl-letf (((symbol-function 'system-name)
               (lambda () "local.example.org")))
      ;; Remote buffer: reuse the full prefix (method, user, multi-hop).
      (setq-local default-directory "/ssh:user@box:/tmp/")
      (should (equal "/ssh:user@box:/home/u"
                     (ebb--cwd-to-path "/home/u" "box")))
      ;; Empty host in a remote buffer is the remote shell reporting.
      (should (equal "/ssh:user@box:/home/u"
                     (ebb--cwd-to-path "/home/u" "")))
      ;; Localhost aliases in a remote buffer also mean the remote shell
      ;; (containers often report 127.0.0.1).
      (should (equal "/ssh:user@box:/home/u"
                     (ebb--cwd-to-path "/home/u" "127.0.0.1")))
      ;; A different host means the user ssh'd onward: fresh TRAMP path.
      (let ((ebb-tramp-default-method "ssh"))
        (should (equal "/ssh:other:/home/u"
                       (ebb--cwd-to-path "/home/u" "other"))))
      ;; The report names this machine again: the user left ssh and the
      ;; local shell is reporting, so the path is plain local.
      (should (equal "/home/u"
                     (ebb--cwd-to-path "/home/u" (system-name))))
      ;; ...also when `system-name' is a FQDN and the report is short.
      (should (equal "/home/u"
                     (ebb--cwd-to-path "/home/u" "local")))))
  (with-temp-buffer
    (cl-letf (((symbol-function 'system-name)
               (lambda () "box.example.org")))
      ;; A remote session targeting this machine remains remote.
      (setq-local default-directory "/ssh:user@box.example.org:/tmp/")
      (should (equal "/ssh:user@box.example.org:/home/u"
                     (ebb--cwd-to-path "/home/u" "box.example.org")))
      (should (equal "/ssh:user@box.example.org:/home/u"
                     (ebb--cwd-to-path "/home/u" "box")))
      ;; Equal first labels do not make distinct FQDNs the same host.
      (let ((ebb-tramp-default-method "ssh"))
        (should (equal "/ssh:box.example.net:/home/u"
                       (ebb--cwd-to-path "/home/u"
                                         "box.example.net")))))))

(ert-deftest ebb-test-this-host-excludes-localhost-aliases ()
  "localhost aliases are not `this machine' in a remote buffer.
A remote shell may report them about itself, so `ebb--this-host-p'
must not treat them as the local name even when `(system-name)' is a
localhost alias; only a real host name reverts the TRAMP prefix."
  (should-not (ebb--this-host-p "localhost"))
  (should-not (ebb--this-host-p "127.0.0.1"))
  (should-not (ebb--this-host-p "::1"))
  ;; They still count as local for `ebb--local-host-p'.
  (should (ebb--local-host-p "localhost"))
  ;; The machine's own names still match.
  (should (ebb--this-host-p (system-name)))
  (should (ebb--this-host-p
           (car (split-string (system-name) "\\."))))
  (should-not (ebb--this-host-p ""))
  (should-not (ebb--this-host-p nil)))

(ert-deftest ebb-test-remote-login-shell ()
  "`getent passwd' replies are accepted only when they name one user."
  (cl-letf (((symbol-function 'process-file-shell-command)
             (lambda (cmd _in buf &rest _)
               (should (string-match-p "\\\"\$LOGNAME\\\"" cmd))
               (with-current-buffer buf
                 (insert "u:x:1000:1000:U:/home/u:/usr/bin/fish\n"))
               0)))
    (should (equal "/usr/bin/fish" (ebb--remote-login-shell))))
  ;; Whole-database dump ($LOGNAME unset): refuse to pick root's shell.
  (cl-letf (((symbol-function 'process-file-shell-command)
             (lambda (_cmd _in buf &rest _)
               (with-current-buffer buf
                 (insert "root:x:0:0:R:/root:/bin/bash\n"
                         "u:x:1000:1000:U:/home/u:/usr/bin/fish\n"))
               0)))
    (should-not (ebb--remote-login-shell))))

(ert-deftest ebb-test-remote-shell-resolution ()
  "`ebb--remote-shell' honors `ebb-tramp-shells' per TRAMP method."
  (cl-letf (((symbol-function 'ebb--remote-login-shell)
             (lambda () "/usr/bin/fish")))
    ;; login-shell detection + login+interactive default args.
    (let ((default-directory "/ssh:box:/tmp/"))
      (should (equal '("/usr/bin/fish" "-l" "-i") (ebb--remote-shell))))
    ;; Explicit shell, no default args for unrecognized shells.
    (let ((default-directory "/docker:c:/"))
      (should (equal '("/bin/sh") (ebb--remote-shell))))
    ;; Unlisted method falls back to /bin/sh.
    (let ((default-directory "/sudo:root@localhost:/"))
      (should (equal '("/bin/sh") (ebb--remote-shell))))
    ;; Explicit args override the default.
    (let ((default-directory "/ssh:box:/tmp/")
          (ebb-tramp-shells '(("ssh" "/bin/bash" nil "-i"))))
      (should (equal '("/bin/bash" "-i") (ebb--remote-shell)))))
  ;; Detection failure uses the FALLBACK slot.
  (cl-letf (((symbol-function 'ebb--remote-login-shell) #'ignore))
    (let ((default-directory "/ssh:box:/tmp/")
          (ebb-tramp-shells '(("ssh" login-shell "/bin/zsh"))))
      (should (equal '("/bin/zsh" "-l" "-i") (ebb--remote-shell))))))

(ert-deftest ebb-test-remote-command-wrapper ()
  "The remote wrapper probes TERM on the remote and execs the shell."
  (let ((cmd (ebb-io--remote-command '("/bin/bash" "-l" "-i") 24 80)))
    (should (equal '("/bin/sh" "-c") (list (nth 0 cmd) (nth 1 cmd))))
    (let ((script (nth 2 cmd)))
      (should (string-match-p "infocmp ebb-truecolor" script))
      (should (string-match-p "TERM=xterm-256color" script))
      (should (string-match-p "export TERM COLORTERM" script))
      (should (string-match-p "rows 24 columns 80" script))
      (should (string-suffix-p "exec /bin/bash -l -i" script)))))

(ert-deftest ebb-test-osc7-emits-host ()
  "OSC 7 emits both path and reporting host."
  (ebb-test-with-screen ()
    (let (got)
      (setf (ebb-parser-emit-fn parser)
            (lambda (type &rest args)
              (when (eq type 'cwd) (setq got args))))
      (ebb-test-output parser "\e]7;file://box/home/u\e\\")
      (should (equal '("/home/u" "box") got))
      (should (equal "/home/u" (ebb-screen-cwd screen))))
    ;; Percent-encoded paths are decoded.
    (ebb-test-with-screen ()
      (let (got)
        (setf (ebb-parser-emit-fn parser)
              (lambda (type &rest args)
                (when (eq type 'cwd) (setq got args))))
        (ebb-test-output parser "\e]7;file://box/home/u/my%20dir%C3%A9\e\\")
        (should (equal '("/home/u/my diré" "box") got))))))

;;;; ---- Glyph Fitting --------------------------------------------------

(defmacro ebb-test-with-glyph-metrics (metrics &rest body)
  "Run BODY with glyph measurement stubbed by METRICS.
METRICS is an alist of (CHAR . (WIDTH . HEIGHT)) pixel sizes; a space
cell is 9x18 unless overridden."
  (declare (indent 1))
  `(let ((ebb-render--glyph-cache (make-hash-table :test #'eql))
         (ebb-render--glyph-stamp nil)
         (ebb-render--cell-pixel-width 0)
         (ebb-render--cell-pixel-height 0)
         (ebb-fit-glyphs t))
     (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
               ((symbol-function 'ebb-render--string-pixel-size)
                (lambda (string)
                  (or (cdr (assq (aref string 0) ,metrics)) '(9 . 18)))))
       ,@body)))

(ert-deftest ebb-test-fit-glyphs-constrains-overwide-glyph ()
  "A Nerd Font icon wider than its cell is pinned and scaled down."
  (ebb-test-with-glyph-metrics '((?\N{U+F15B} . (15 . 18)))
    (let* ((line (concat "a" (string ?\N{U+F15B}) "b"))
           (fitted (ebb-render--fit-glyphs line))
           (spec (get-text-property 1 'display fitted)))
      ;; The original string is left untouched.
      (should-not (get-text-property 1 'display line))
      (should (equal '(min-width (1)) (nth 0 spec)))
      (should (< (nth 1 (nth 1 spec)) 1.0))
      ;; ASCII neighbours stay untouched.
      (should-not (get-text-property 0 'display fitted))
      (should-not (get-text-property 2 'display fitted)))))

(ert-deftest ebb-test-fit-glyphs-ignores-fitting-glyphs ()
  "Characters that already fill their cell exactly are left alone."
  (ebb-test-with-glyph-metrics '((?é . (9 . 18)))
    (let ((fitted (ebb-render--fit-glyphs "aéb")))
      (should-not (get-text-property 1 'display fitted))
      ;; No adjustment means no copy is made.
      (should-not (text-properties-at 1 fitted)))))

(ert-deftest ebb-test-fit-glyphs-uses-cell-width-of-wide-chars ()
  "Wide cells are measured against two columns, via spacer or cell width."
  (ebb-test-with-glyph-metrics '((?\N{U+1F310} . (19 . 18)))
    ;; Viewport rows mark the extra column with an invisible spacer.
    (let* ((line (concat (string ?\N{U+1F310})
                         (propertize " " 'invisible t 'ebb-wide-spacer t)))
           (spec (get-text-property 0 'display (ebb-render--fit-glyphs line))))
      (should (equal '(min-width (2)) (nth 0 spec))))
    ;; Trimmed scrollback rows carry the cell width instead.
    (let* ((line (propertize (string ?\N{U+1F310}) 'ebb-cell-width 2))
           (spec (get-text-property 0 'display (ebb-render--fit-glyphs line))))
      (should (equal '(min-width (2)) (nth 0 spec))))))

(ert-deftest ebb-test-fit-glyphs-pads-narrow-glyph ()
  "A glyph narrower than its cell is padded, not scaled."
  (ebb-test-with-glyph-metrics '((?中 . (13 . 18)))
    (let* ((line (propertize (string ?中) 'ebb-cell-width 2))
           (spec (get-text-property 0 'display (ebb-render--fit-glyphs line))))
      (should (equal '((min-width (2))) spec)))))

(ert-deftest ebb-test-fit-glyphs-scales-tall-glyph ()
  "A glyph taller than the cell is scaled so the row height is unchanged."
  (ebb-test-with-glyph-metrics '((?中 . (13 . 20)))
    (let* ((line (propertize (string ?中) 'ebb-cell-width 2))
           (spec (get-text-property 0 'display (ebb-render--fit-glyphs line))))
      (should (equal '(min-width (2)) (nth 0 spec)))
      (should (<= (nth 1 (nth 1 spec)) (/ 18.0 20))))))

(ert-deftest ebb-test-fit-glyphs-disabled ()
  "`ebb-fit-glyphs' nil, or a text terminal, leaves rows untouched."
  (ebb-test-with-glyph-metrics '((?\N{U+F15B} . (15 . 18)))
    (let ((ebb-fit-glyphs nil))
      (should-not (get-text-property
                   0 'display (ebb-render--fit-glyphs
                               (string ?\N{U+F15B})))))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
      (should-not (get-text-property
                   0 'display (ebb-render--fit-glyphs
                               (string ?\N{U+F15B})))))))

(ert-deftest ebb-test-fit-glyphs-marks-wide-scrollback-cells ()
  "Trimmed scrollback rows record the width of wide cells."
  (ebb-test-with-screen ()
    (ebb-test-output parser "a中b")
    (let* ((line (ebb--line-at screen 0))
           (s (ebb-render--cells-to-string-scrollback-fast
               (ebb-line-cells line) (ebb-screen-width screen))))
      (should (equal "a中b" (substring-no-properties s 0 3)))
      (should (equal 2 (get-text-property 1 'ebb-cell-width s)))
      (should-not (get-text-property 0 'ebb-cell-width s)))))

(provide 'ebb-test)
;;; ebb-test.el ends here
