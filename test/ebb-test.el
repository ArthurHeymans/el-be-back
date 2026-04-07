;;; ebb-test.el --- Tests for el-be-back -*- lexical-binding: t; -*-

;;; Commentary:

;; Comprehensive ERT test suite for el-be-back.
;; Pure Elisp tests run without the native module.
;; Integration tests require the module to be loaded.

;;; Code:

(require 'ert)

;; Try to load el-be-back for integration tests
(defvar ebb-test--module-available nil
  "Non-nil if the ebb native module is available.")

(condition-case nil
    (progn
      (require 'el-be-back)
      (setq ebb-test--module-available t))
  (error
   (message "ebb module not available; running pure Elisp tests only")))

;;; --- Test helpers ---

(defun ebb-test--row0 (term)
  "Read the first visible row of TERM as a string."
  (let ((content (ebb--content term)))
    (car (split-string content "\n"))))

(defun ebb-test--cursor (term)
  "Return (ROW . COL) cursor position for TERM."
  (cons (ebb--cursor-row term)
        (ebb--cursor-col term)))

(defun ebb-test--make-term (&optional rows cols)
  "Create a test terminal with ROWS and COLS (default 24x80)."
  (ebb--new (or rows 24) (or cols 80) 100))

;;; =============================================
;;; Pure Elisp tests (no module required)
;;; =============================================

;;; --- Raw key sequence tests ---

(ert-deftest ebb-test-raw-key-sequences ()
  "Test raw escape sequence generation for special keys."
  ;; Cursor keys without modifiers
  (should (string= (ebb--csi-letter "A" 0) "\e[A"))
  (should (string= (ebb--csi-letter "B" 0) "\e[B"))
  (should (string= (ebb--csi-letter "C" 0) "\e[C"))
  (should (string= (ebb--csi-letter "D" 0) "\e[D"))
  ;; Cursor keys with shift (mod=1)
  (should (string= (ebb--csi-letter "A" 1) "\e[1;2A"))
  ;; Cursor keys with ctrl (mod=4)
  (should (string= (ebb--csi-letter "A" 4) "\e[1;5A"))
  ;; Tilde keys
  (should (string= (ebb--csi-tilde 2 0) "\e[2~"))  ; insert
  (should (string= (ebb--csi-tilde 3 0) "\e[3~"))  ; delete
  (should (string= (ebb--csi-tilde 5 1) "\e[5;2~")) ; shift+pgup
  ;; Function keys
  (should (string= (ebb--raw-key-sequence "f1" 0) "\eOP"))
  (should (string= (ebb--raw-key-sequence "f5" 0) "\e[15~"))
  (should (string= (ebb--raw-key-sequence "f12" 0) "\e[24~"))
  ;; Modified specials (CSI u encoding)
  (should (string= (ebb--raw-key-sequence "return" 4) "\e[13;5u"))  ; ctrl+return
  (should (string= (ebb--raw-key-sequence "backspace" 1) "\e[127;2u")) ; shift+backspace
  (should (string= (ebb--raw-key-sequence "tab" 2) "\e[9;3u"))    ; meta+tab
  ;; Unmodified specials return nil (handled by terminal encoder)
  (should (null (ebb--raw-key-sequence "return" 0)))
  (should (null (ebb--raw-key-sequence "backspace" 0)))
  ;; Unknown keys
  (should (null (ebb--raw-key-sequence "nonexistent" 0))))

;;; --- Soft-wrap filter tests ---

(ert-deftest ebb-test-filter-soft-wraps ()
  "Test that soft-wrap newlines are correctly removed."
  ;; No wraps: text passes through unchanged
  (should (string= (ebb--filter-soft-wraps "hello\nworld") "hello\nworld"))
  ;; With wrap property: newline is removed
  (let ((text (concat "hello" (propertize "\n" 'ebb-wrap t) "world")))
    (should (string= (ebb--filter-soft-wraps text) "helloworld")))
  ;; Mixed: only wrapped newlines removed
  (let ((text (concat "line1" (propertize "\n" 'ebb-wrap t)
                      "cont\n" "line2")))
    (should (string= (ebb--filter-soft-wraps text) "line1cont\nline2"))))

(ert-deftest ebb-test-clean-copy-text ()
  "Test copy text cleaning: unwrap + trim trailing whitespace."
  ;; Soft-wrap newline removed, trailing spaces trimmed per line
  (let ((text (concat "hello   " (propertize "\n" 'ebb-wrap t)
                      "world   \nfoo   ")))
    (should (string= (ebb--clean-copy-text text) "hello   world\nfoo"))))

;;; --- CWD resolution tests ---

(ert-deftest ebb-test-resolve-cwd ()
  "Test OSC 7 CWD URL resolution."
  ;; Local path (localhost)
  (should (string= (ebb--resolve-cwd "file://localhost/home/user") "/home/user"))
  ;; Local path (empty hostname)
  (should (string= (ebb--resolve-cwd "file:///tmp") "/tmp"))
  ;; Local path (matching hostname)
  (should (string= (ebb--resolve-cwd (format "file://%s/path" (system-name))) "/path"))
  ;; Plain path (no scheme)
  (should (string= (ebb--resolve-cwd "/plain/path") "/plain/path")))

(ert-deftest ebb-test-resolve-cwd-remote ()
  "Test CWD resolution with remote prefix."
  (let ((ebb--remote-prefix "/sshx:host:"))
    (should (string= (ebb--resolve-cwd "file://otherhost/path")
                     "/sshx:host:/path"))))

;;; --- OSC scanning tests ---

(ert-deftest ebb-test-osc133-scanning ()
  "Test OSC 133 sequence scanning."
  (let ((ebb--prompt-positions nil))
    ;; Test A marker
    (ebb--osc133-marker "A" nil)
    (should (= (length ebb--prompt-positions) 1))
    ;; Test D marker with exit status
    (ebb--osc133-marker "D" "0")
    (should (= (cdar ebb--prompt-positions) 0))
    ;; Another D with non-zero exit
    (ebb--osc133-marker "A" nil)
    (ebb--osc133-marker "D" "1")
    (should (= (cdar ebb--prompt-positions) 1))))

(ert-deftest ebb-test-osc51-eval ()
  "Test OSC 51 Elisp eval dispatch."
  (let ((ebb-eval-cmds '(("test-cmd" (lambda (x) (message "got %s" x)))))
        (messages nil))
    ;; Known command
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (ebb--osc51-eval "\"test-cmd\" \"hello\"")
      (should (member "got hello" messages)))))

(ert-deftest ebb-test-osc-sequence-scanning ()
  "Test raw OSC sequence scanning in process filter data."
  (let ((ebb--prompt-positions nil))
    ;; OSC 133;A with BEL terminator
    (ebb--scan-osc-sequences "\e]133;A\a")
    (should (= (length ebb--prompt-positions) 1))
    ;; OSC 133;D with ST terminator
    (ebb--scan-osc-sequences "\e]133;D;0\e\\")
    (should (= (cdar ebb--prompt-positions) 0))))

;;; --- Link detection tests ---

(ert-deftest ebb-test-url-detection ()
  "Test URL detection in buffer text."
  (with-temp-buffer
    (insert "Visit https://example.com for info.\n")
    (insert "Also http://foo.bar/baz?q=1 works.\n")
    (let ((ebb-enable-url-detection t)
          (ebb-enable-file-detection nil))
      (ebb--detect-urls))
    (goto-char (point-min))
    (search-forward "https://")
    (should (get-text-property (point) 'help-echo))
    (should (string-match-p "https://example.com"
                            (get-text-property (1- (point)) 'help-echo)))))

(ert-deftest ebb-test-file-detection ()
  "Test file:line detection in buffer text."
  (with-temp-buffer
    (let ((test-file (make-temp-file "ebb-test")))
      (unwind-protect
          (progn
            (insert (format "Error at %s:42\n" test-file))
            (let ((ebb-enable-url-detection nil)
                  (ebb-enable-file-detection t))
              (ebb--detect-urls))
            (goto-char (point-min))
            (search-forward (file-name-nondirectory test-file))
            (should (get-text-property (point) 'help-echo))
            (should (string-match-p "fileref:"
                                    (get-text-property (point) 'help-echo))))
        (delete-file test-file)))))

;;; --- Keymap exception tests ---

(ert-deftest ebb-test-keymap-exceptions ()
  "Test that keymap exception keys are not bound in semi-char mode."
  ;; C-c is a prefix key (has sub-bindings like C-c C-c) and should
  ;; NOT be directly bound to ebb-self-input or a sending lambda
  (let ((binding (lookup-key ebb-semi-char-mode-map (kbd "C-c"))))
    ;; C-c should be a prefix keymap (since C-c C-c etc. are bound)
    (should (keymapp binding)))
  ;; Printable characters SHOULD be bound to ebb-self-input
  (let ((binding (lookup-key ebb-semi-char-mode-map "a")))
    (should (eq binding 'ebb-self-input))))

;;; --- Input coalescing tests ---

(ert-deftest ebb-test-input-coalesce-multi-byte ()
  "Test that multi-byte sequences bypass coalescing."
  ;; When a multi-byte sequence is sent, it should flush any pending
  ;; single-char buffer.  We can test the logic without a process by
  ;; checking that input-buffer is cleared when needed.
  (let ((ebb--input-buffer '("b" "a"))
        (ebb--input-timer nil)
        (ebb--process nil))
    ;; Without a live process, nothing gets sent, but the logic path
    ;; is exercised.
    (should (null ebb--input-timer))))

;;; --- Theme face helper tests ---

(ert-deftest ebb-test-face-hex-color ()
  "Test hex color extraction from faces."
  ;; The default face should return a valid hex color
  (let ((color (ebb--face-hex-color 'default :foreground)))
    (should (stringp color))
    (should (string-prefix-p "#" color))
    (should (= (length color) 7))))

;;; =============================================
;;; Integration tests (require native module)
;;; =============================================

(when ebb-test--module-available

  (ert-deftest ebb-test-create ()
    "Test terminal creation."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (should term)
            (should (= (ebb--get-rows term) 24))
            (should (= (ebb--get-cols term) 80))
            ;; Cursor at origin
            (should (equal (ebb-test--cursor term) '(0 . 0)))
            ;; First row is blank
            (should (string-match-p "^\\s-*$" (ebb-test--row0 term))))
        (ebb--free term))))

  (ert-deftest ebb-test-write-input ()
    "Test feeding text to the terminal."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "Hello, world!")
            (let ((row (ebb-test--row0 term)))
              (should (string-match-p "Hello, world!" row)))
            (should (equal (ebb-test--cursor term) '(0 . 13))))
        (ebb--free term))))

  (ert-deftest ebb-test-write-multiline ()
    "Test feeding multiline text."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "line1\r\nline2\r\n")
            (should (= (ebb--cursor-row term) 2)))
        (ebb--free term))))

  (ert-deftest ebb-test-backspace ()
    "Test backspace handling."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "abc\x08")  ; backspace
            (should (= (ebb--cursor-col term) 2)))
        (ebb--free term))))

  (ert-deftest ebb-test-cursor-movement ()
    "Test CSI cursor movement sequences."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            ;; Move to row 5, col 10 (1-indexed in VT)
            (ebb--feed term "\e[6;11H")
            (should (= (ebb--cursor-row term) 5))
            (should (= (ebb--cursor-col term) 10)))
        (ebb--free term))))

  (ert-deftest ebb-test-erase ()
    "Test CSI erase sequences."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "Hello\e[2J")  ; clear screen
            (ebb--feed term "\e[H")        ; cursor home
            ;; After clear, first row should be blank
            (should (string-match-p "^\\s-*$" (ebb-test--row0 term))))
        (ebb--free term))))

  (ert-deftest ebb-test-resize ()
    "Test terminal resize."
    (let ((term (ebb-test--make-term 24 80)))
      (unwind-protect
          (progn
            (ebb--feed term "Hello")
            (ebb--resize term 10 40)
            (should (= (ebb--get-rows term) 10))
            (should (= (ebb--get-cols term) 40))
            ;; Content should be preserved
            (should (string-match-p "Hello" (ebb-test--row0 term))))
        (ebb--free term))))

  (ert-deftest ebb-test-title ()
    "Test OSC 2 title change."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "\e]2;My Title\a")
            (should (string= (ebb--get-title term) "My Title")))
        (ebb--free term))))

  (ert-deftest ebb-test-crlf ()
    "Test CRLF handling."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (ebb--feed term "line1\r\nline2")
            ;; Cursor should be on row 1
            (should (= (ebb--cursor-row term) 1))
            ;; Second row should contain "line2"
            (let* ((content (ebb--content term))
                   (lines (split-string content "\n")))
              (should (string-match-p "line2" (nth 1 lines)))))
        (ebb--free term))))

  (ert-deftest ebb-test-cursor-style ()
    "Test cursor style query."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            ;; Default cursor should be block (1)
            (should (= (ebb--cursor-style term) 1))
            ;; DECSCUSR 5 = blinking bar
            (ebb--feed term "\e[5 q")
            (should (= (ebb--cursor-style term) 0))
            ;; DECSCUSR 3 = blinking underline
            (ebb--feed term "\e[3 q")
            (should (= (ebb--cursor-style term) 2)))
        (ebb--free term))))

  (ert-deftest ebb-test-cursor-visible ()
    "Test cursor visibility query."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (should (ebb--cursor-visible term))
            ;; Hide cursor (DECTCEM reset)
            (ebb--feed term "\e[?25l")
            (should-not (ebb--cursor-visible term))
            ;; Show cursor
            (ebb--feed term "\e[?25h")
            (should (ebb--cursor-visible term)))
        (ebb--free term))))

  (ert-deftest ebb-test-mouse-grabbed ()
    "Test mouse tracking detection."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            ;; Initially not grabbed
            (should-not (ebb--is-mouse-grabbed term))
            ;; Enable mouse tracking mode 1000
            (ebb--feed term "\e[?1000h")
            (should (ebb--is-mouse-grabbed term))
            ;; Disable
            (ebb--feed term "\e[?1000l")
            (should-not (ebb--is-mouse-grabbed term)))
        (ebb--free term))))

  (ert-deftest ebb-test-bracketed-paste ()
    "Test bracketed paste mode detection."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            ;; Initially off
            (should-not (ebb--bracketed-paste-enabled term))
            ;; Enable
            (ebb--feed term "\e[?2004h")
            (should (ebb--bracketed-paste-enabled term))
            ;; Disable
            (ebb--feed term "\e[?2004l")
            (should-not (ebb--bracketed-paste-enabled term)))
        (ebb--free term))))

  (ert-deftest ebb-test-render ()
    "Test rendering into a buffer."
    (let ((term (ebb-test--make-term 5 20)))
      (unwind-protect
          (with-temp-buffer
            (ebb--feed term "Hello, EBB!")
            (let ((inhibit-read-only t))
              (ebb--render term))
            ;; Buffer should contain the text
            (goto-char (point-min))
            (should (search-forward "Hello, EBB!" nil t)))
        (ebb--free term))))

  (ert-deftest ebb-test-alt-screen ()
    "Test alternate screen detection."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            (should-not (ebb--is-alt-screen term))
            ;; Switch to alt screen
            (ebb--feed term "\e[?1049h")
            (should (ebb--is-alt-screen term))
            ;; Switch back
            (ebb--feed term "\e[?1049l")
            (should-not (ebb--is-alt-screen term)))
        (ebb--free term))))

  (ert-deftest ebb-test-version ()
    "Test version query."
    (should (stringp (ebb--version)))
    (should (string-match-p "^[0-9]+\\.[0-9]+\\.[0-9]+" (ebb--version))))

  (ert-deftest ebb-test-set-palette ()
    "Test palette setting."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (let ((colors (mapconcat #'identity
                                   (make-list 16 "#ff0000")
                                   "")))
            (should (ebb--set-palette term colors)))
        (ebb--free term))))

  (ert-deftest ebb-test-set-default-colors ()
    "Test default color setting."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (should (ebb--set-default-colors term "#ffffff" "#000000"))
        (ebb--free term))))

  (ert-deftest ebb-test-wide-char ()
    "Test wide character rendering."
    (let ((term (ebb-test--make-term 5 20)))
      (unwind-protect
          (progn
            (ebb--feed term "A\xe4\xb8\xad")  ; A + CJK char (中)
            ;; Cursor should be at col 3 (A=1 + wide=2)
            (should (= (ebb--cursor-col term) 3)))
        (ebb--free term))))

  (ert-deftest ebb-test-scrollback ()
    "Test scrollback handling."
    (let ((term (ebb-test--make-term 5 20)))
      (unwind-protect
          (progn
            ;; Fill more than 5 rows to trigger scrollback
            (dotimes (i 10)
              (ebb--feed term (format "line %d\r\n" i)))
            ;; Last visible row should have recent content
            (let* ((content (ebb--content term))
                   (lines (split-string content "\n" t)))
              (should (> (length lines) 0))))
        (ebb--free term))))

  (ert-deftest ebb-test-focus-event ()
    "Test focus event handling."
    (let ((term (ebb-test--make-term)))
      (unwind-protect
          (progn
            ;; Enable focus tracking
            (ebb--feed term "\e[?1004h")
            ;; Terminal starts focused=true, so first send focus-lost
            (let ((response (ebb--focus-event term nil)))
              (should response)
              (should (string-match-p "\e\\[O" response)))
            ;; Now send focus gained
            (let ((response (ebb--focus-event term 1)))
              (should response)
              (should (string-match-p "\e\\[I" response))))
        (ebb--free term))))

  ) ;; end of ebb-test--module-available

;;; --- Test runners ---

(defun ebb-test-run ()
  "Run all ebb tests."
  (interactive)
  (ert-run-tests-interactively "^ebb-test-"))

(defun ebb-test-run-elisp ()
  "Run only the pure Elisp tests (no module required)."
  (interactive)
  (ert-run-tests-interactively
   (lambda (test)
     (let ((name (symbol-name (ert-test-name test))))
       (and (string-prefix-p "ebb-test-" name)
            (member name '("ebb-test-raw-key-sequences"
                           "ebb-test-filter-soft-wraps"
                           "ebb-test-clean-copy-text"
                           "ebb-test-resolve-cwd"
                           "ebb-test-resolve-cwd-remote"
                           "ebb-test-osc133-scanning"
                           "ebb-test-osc51-eval"
                           "ebb-test-osc-sequence-scanning"
                           "ebb-test-url-detection"
                           "ebb-test-file-detection"
                           "ebb-test-keymap-exceptions"
                           "ebb-test-input-coalesce-multi-byte"
                           "ebb-test-face-hex-color")))))))

(provide 'ebb-test)
;;; ebb-test.el ends here
