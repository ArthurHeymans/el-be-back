;;; el-be-back.el --- Terminal emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: terminals
;; URL: https://github.com/arthur/el-be-back

;;; Commentary:

;; el-be-back is a terminal emulator for Emacs built on wezterm's terminal
;; emulation engine, exposed as a Rust dynamic module.

;;; Code:

(require 'cl-lib)

;;; --- Customization ---

(defgroup el-be-back nil
  "Terminal emulator built on wezterm."
  :group 'terminals
  :prefix "ebb-")

(defcustom ebb-shell-name (or (bound-and-true-p explicit-shell-file-name)
                              (getenv "SHELL")
                              "/bin/sh")
  "Shell to run in the terminal."
  :type 'string
  :group 'el-be-back)

(defcustom ebb-term-environment-variable "xterm-256color"
  "Value of TERM environment variable for the shell process."
  :type 'string
  :group 'el-be-back)

(defcustom ebb-max-scrollback 10000
  "Maximum number of scrollback lines."
  :type 'integer
  :group 'el-be-back)

(defcustom ebb-minimum-latency 0.008
  "Minimum time in seconds between terminal redraws."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-maximum-latency 0.033
  "Maximum time in seconds before a forced redraw."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-always-compile-module t
  "If non-nil, compile the module automatically if not found."
  :type 'boolean
  :group 'el-be-back)

;;; --- Module loading ---

(defconst ebb--module-name "ebb-module"
  "Name of the dynamic module.")

(defconst ebb--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing el-be-back.")

(defun ebb--module-file ()
  "Return the expected path of the compiled module."
  (expand-file-name (concat ebb--module-name ".so") ebb--directory))

(defun ebb-compile-module ()
  "Compile the Rust dynamic module."
  (interactive)
  (let ((default-directory ebb--directory))
    (message "[ebb] Compiling module (this may take a minute)...")
    (let ((status (call-process
                   "cargo" nil "*ebb-compile*" t
                   "build" "--release")))
      (unless (= status 0)
        (pop-to-buffer "*ebb-compile*")
        (error "[ebb] Module compilation failed (exit code %d)" status)))
    (let* ((target-dir (expand-file-name "target/release/"))
           (lib-name (cond
                      ((eq system-type 'darwin)
                       (concat "lib" ebb--module-name ".dylib"))
                      (t
                       (concat "lib" ebb--module-name ".so"))))
           (src (expand-file-name lib-name target-dir))
           (dst (ebb--module-file)))
      (unless (file-exists-p src)
        (error "[ebb] Built library not found at %s" src))
      (copy-file src dst t)
      (message "[ebb] Module compiled successfully."))))

(unless (require 'ebb-module nil t)
  (if ebb-always-compile-module
      (progn
        (ebb-compile-module)
        (require 'ebb-module))
    (error "[ebb] Module not found. Run M-x ebb-compile-module")))

;;; --- Buffer-local variables ---

(defvar-local ebb--terminal nil
  "The terminal instance (Rust user-ptr).")

(defvar-local ebb--process nil
  "The shell process.")

(defvar-local ebb--pending-chunks nil
  "List of output chunks waiting to be fed to the terminal.")

(defvar-local ebb--render-timer nil
  "Timer for latency-bounded rendering.")

(defvar-local ebb--first-chunk-time nil
  "Timestamp of the first pending output chunk.")

;;; --- Major mode ---

(define-derived-mode ebb-mode fundamental-mode "EBB"
  "Major mode for el-be-back terminal emulator."
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  ;; We manage faces ourselves
  (setq-local font-lock-defaults '(nil t))
  (add-hook 'kill-buffer-hook #'ebb--kill-buffer-hook nil t)
  (add-hook 'window-size-change-functions #'ebb--window-size-change))

;;; --- Process management ---

(defun ebb--process-filter (process output)
  "Process filter: queue OUTPUT from PROCESS and schedule rendering."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (and ebb--terminal (not (string-empty-p output)))
        (push output ebb--pending-chunks)
        (unless ebb--first-chunk-time
          (setq ebb--first-chunk-time (current-time)))
        ;; Cancel existing timer
        (when ebb--render-timer
          (cancel-timer ebb--render-timer)
          (setq ebb--render-timer nil))
        ;; Schedule render
        (let ((elapsed (float-time
                        (time-subtract nil ebb--first-chunk-time))))
          (if (>= elapsed ebb-maximum-latency)
              ;; Past max latency -- render immediately
              (ebb--flush-output)
            ;; Schedule within bounds
            (setq ebb--render-timer
                  (run-with-timer
                   (min (- ebb-maximum-latency elapsed)
                        ebb-minimum-latency)
                   nil #'ebb--flush-output-in-buffer
                   (current-buffer)))))))))

(defun ebb--flush-output-in-buffer (buffer)
  "Flush output in BUFFER if it is still alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ebb--flush-output))))

(defun ebb--flush-output ()
  "Process all pending output chunks and render."
  ;; Cancel pending timer
  (when ebb--render-timer
    (cancel-timer ebb--render-timer)
    (setq ebb--render-timer nil))
  (when (and ebb--terminal ebb--pending-chunks)
    (let ((chunks (nreverse ebb--pending-chunks)))
      (setq ebb--pending-chunks nil
            ebb--first-chunk-time nil)
      ;; Feed all chunks to the terminal
      (dolist (chunk chunks)
        (ebb--feed ebb--terminal chunk))
      ;; Render the screen
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t)
            (inhibit-quit t)
            (buffer-undo-list t))
        (ebb--render-screen))
      ;; Drain terminal responses and send to PTY
      (ebb--drain-and-send)
      ;; Process title/CWD/bell alerts
      (ebb--process-alerts))))

(defun ebb--render-screen ()
  "Render the terminal screen into the current buffer.
Must be called within the performance-trinity let bindings.
The Rust render function erases the buffer, inserts styled text,
and positions the cursor."
  (when ebb--terminal
    (ebb--render ebb--terminal)))

(defun ebb--drain-and-send ()
  "Drain captured terminal output and send to the PTY."
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((response (ebb--drain-output ebb--terminal)))
      (when response
        (process-send-string ebb--process response)))))

(defun ebb--process-alerts ()
  "Process pending alerts (title, CWD, bell) from the terminal."
  (when ebb--terminal
    ;; Title change
    (let ((title (ebb--poll-title ebb--terminal)))
      (when title
        (rename-buffer (format "*ebb: %s*" title) t)))
    ;; CWD change
    (let ((cwd (ebb--poll-cwd ebb--terminal)))
      (when cwd
        ;; Strip file:// prefix and hostname if present
        (let ((dir (if (string-prefix-p "file://" cwd)
                       (replace-regexp-in-string "^file://[^/]*" "" cwd)
                     cwd)))
          (when (file-directory-p dir)
            (setq-local default-directory
                        (file-name-as-directory dir))))))
    ;; Bell
    (when (ebb--poll-bell ebb--terminal)
      (ding t))))

(defun ebb--process-sentinel (process event)
  "Handle PROCESS state change EVENT."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "\n[Process %s]\n" (string-trim event)))))))

(defun ebb--kill-buffer-hook ()
  "Clean up when the terminal buffer is killed."
  (when ebb--render-timer
    (cancel-timer ebb--render-timer))
  (when (and ebb--process (process-live-p ebb--process))
    (delete-process ebb--process))
  (when ebb--terminal
    (ebb--free ebb--terminal)
    (setq ebb--terminal nil)))

;;; --- Resize handling ---

(defun ebb--window-size-change (frame)
  "Handle window resize for terminal buffers in FRAME."
  (dolist (window (window-list frame))
    (let ((buf (window-buffer window)))
      (when (and (buffer-live-p buf)
                 (eq (buffer-local-value 'major-mode buf) 'ebb-mode)
                 (buffer-local-value 'ebb--terminal buf))
        (with-current-buffer buf
            (let ((rows (window-body-height window))
                (cols (window-body-width window))
                (cur-rows (ebb--get-rows ebb--terminal))
                (cur-cols (ebb--get-cols ebb--terminal)))
            (when (or (/= rows cur-rows)
                      (/= cols cur-cols))
              (ebb--resize ebb--terminal rows cols)
              (when (process-live-p ebb--process)
                (set-process-window-size ebb--process rows cols))
              ;; Re-render after resize
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t)
                    (buffer-undo-list t))
                (ebb--render-screen)))))))))

;;; --- Input handling ---

(defun ebb-send-raw (string)
  "Send STRING directly to the terminal's PTY."
  (interactive "sSend: ")
  (when (and ebb--process (process-live-p ebb--process))
    (process-send-string ebb--process string)))

(defun ebb--send-key (key-name &optional shift ctrl meta)
  "Send KEY-NAME to the terminal via wezterm key encoding.
KEY-NAME is a string like \"a\", \"return\", \"up\", etc.
Falls back to raw send for unrecognized keys."
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    ;; Pass 1 for true, nil for false (Rust expects Option<i64>)
    (ebb--key-down ebb--terminal key-name
                   (if shift 1 nil)
                   (if ctrl 1 nil)
                   (if meta 1 nil))
    ;; Drain encoded key bytes and send to PTY
    (ebb--drain-and-send)))

(defun ebb-self-input ()
  "Send the last input event to the terminal."
  (interactive)
  (let* ((event last-input-event)
         (mods (event-modifiers event))
         (basic (event-basic-type event))
         (shift (and (memq 'shift mods) t))
         (ctrl (and (memq 'control mods) t))
         (meta (and (memq 'meta mods) t))
         (key-name (cond
                    ;; Character event (printable, no ctrl/meta)
                    ((and (characterp basic) (not ctrl) (not meta))
                     (string (if shift (upcase basic) basic)))
                    ;; Character with ctrl/meta
                    ((characterp basic)
                     (string basic))
                    ;; Named key (symbol)
                    ((symbolp basic)
                     (symbol-name basic))
                    (t nil))))
    (when key-name
      (ebb--send-key key-name shift ctrl meta))))

;; --- Semi-char mode keymap ---
;; Most keys are forwarded; a few Emacs prefix keys are preserved.

(defvar ebb-semi-char-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Printable ASCII -> forward to terminal
    (let ((i 32))
      (while (<= i 126)
        (define-key map (string i) #'ebb-self-input)
        (setq i (1+ i))))
    ;; Named keys
    (define-key map (kbd "RET")        (lambda () (interactive) (ebb--send-key "return")))
    (define-key map (kbd "<return>")   (lambda () (interactive) (ebb--send-key "return")))
    (define-key map (kbd "TAB")        (lambda () (interactive) (ebb--send-key "tab")))
    (define-key map (kbd "<tab>")      (lambda () (interactive) (ebb--send-key "tab")))
    (define-key map (kbd "DEL")        (lambda () (interactive) (ebb--send-key "backspace")))
    (define-key map (kbd "<backspace>")(lambda () (interactive) (ebb--send-key "backspace")))
    (define-key map (kbd "<delete>")   (lambda () (interactive) (ebb--send-key "delete")))
    (define-key map (kbd "<deletechar>")(lambda () (interactive) (ebb--send-key "delete")))
    (define-key map (kbd "ESC")        (lambda () (interactive) (ebb--send-key "escape")))
    (define-key map (kbd "<escape>")   (lambda () (interactive) (ebb--send-key "escape")))
    (define-key map (kbd "<up>")       (lambda () (interactive) (ebb--send-key "up")))
    (define-key map (kbd "<down>")     (lambda () (interactive) (ebb--send-key "down")))
    (define-key map (kbd "<left>")     (lambda () (interactive) (ebb--send-key "left")))
    (define-key map (kbd "<right>")    (lambda () (interactive) (ebb--send-key "right")))
    (define-key map (kbd "<home>")     (lambda () (interactive) (ebb--send-key "home")))
    (define-key map (kbd "<end>")      (lambda () (interactive) (ebb--send-key "end")))
    (define-key map (kbd "<prior>")    (lambda () (interactive) (ebb--send-key "prior")))
    (define-key map (kbd "<next>")     (lambda () (interactive) (ebb--send-key "next")))
    (define-key map (kbd "<insert>")   (lambda () (interactive) (ebb--send-key "insert")))
    ;; Function keys
    (dotimes (i 12)
      (let ((fn-name (format "f%d" (1+ i))))
        (define-key map (kbd (format "<f%d>" (1+ i)))
          (let ((name fn-name))
            (lambda () (interactive) (ebb--send-key name))))))
    ;; Ctrl keys that should go to terminal
    (define-key map (kbd "C-a") (lambda () (interactive) (ebb--send-key "a" nil t nil)))
    (define-key map (kbd "C-b") (lambda () (interactive) (ebb--send-key "b" nil t nil)))
    (define-key map (kbd "C-d") (lambda () (interactive) (ebb--send-key "d" nil t nil)))
    (define-key map (kbd "C-e") (lambda () (interactive) (ebb--send-key "e" nil t nil)))
    (define-key map (kbd "C-f") (lambda () (interactive) (ebb--send-key "f" nil t nil)))
    (define-key map (kbd "C-k") (lambda () (interactive) (ebb--send-key "k" nil t nil)))
    (define-key map (kbd "C-l") (lambda () (interactive) (ebb--send-key "l" nil t nil)))
    (define-key map (kbd "C-n") (lambda () (interactive) (ebb--send-key "n" nil t nil)))
    (define-key map (kbd "C-p") (lambda () (interactive) (ebb--send-key "p" nil t nil)))
    (define-key map (kbd "C-r") (lambda () (interactive) (ebb--send-key "r" nil t nil)))
    (define-key map (kbd "C-t") (lambda () (interactive) (ebb--send-key "t" nil t nil)))
    (define-key map (kbd "C-w") (lambda () (interactive) (ebb--send-key "w" nil t nil)))
    (define-key map (kbd "C-y") (lambda () (interactive) (ebb--send-key "y" nil t nil)))
    (define-key map (kbd "C-z") (lambda () (interactive) (ebb--send-key "z" nil t nil)))
    (define-key map (kbd "C-\\") (lambda () (interactive) (ebb--send-key "\\" nil t nil)))
    (define-key map (kbd "C-_") (lambda () (interactive) (ebb--send-key "_" nil t nil)))
    ;; C-c C-c sends interrupt
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (ebb--send-key "c" nil t nil)))
    ;; Preserved Emacs keys (NOT forwarded):
    ;; C-c (prefix), C-x (prefix), C-g, C-h, M-x, C-u are inherited from ebb-mode-map
    map)
  "Keymap for semi-char mode: most keys forwarded, Emacs prefixes preserved.")

(define-minor-mode ebb-semi-char-mode
  "Minor mode that forwards most keys to the terminal."
  :lighter " Semi"
  :keymap ebb-semi-char-mode-map)

;; --- ebb-mode-map: base keymap (emacs mode) ---
;; Standard Emacs keys work. Only special ebb commands are bound.
(define-key ebb-mode-map (kbd "C-c C-j") #'ebb-semi-char-mode)

;;; --- Entry point ---

;;;###autoload
(defun ebb ()
  "Start a terminal."
  (interactive)
  (let* ((buf (generate-new-buffer "*ebb*")))
    (with-current-buffer buf
      (ebb-mode)
      ;; Create terminal with initial window dimensions
      (let ((rows (max 1 (window-body-height)))
            (cols (max 1 (window-body-width))))
        (setq ebb--terminal (ebb--new rows cols ebb-max-scrollback))
        ;; Start shell process
        (let ((process-environment
               (append
                (list (concat "TERM=" ebb-term-environment-variable)
                      "COLORTERM=truecolor"
                      (format "INSIDE_EMACS=%s,ebb" emacs-version))
                process-environment)))
          (setq ebb--process
                (make-process
                 :name "ebb"
                 :buffer buf
                 :command (list ebb-shell-name "-l")
                 :coding 'binary
                 :filter #'ebb--process-filter
                 :sentinel #'ebb--process-sentinel
                 :connection-type 'pty)))
        ;; Set initial PTY size
        (set-process-window-size ebb--process rows cols)
        ;; Enter semi-char mode by default
        (ebb-semi-char-mode 1)))
    (pop-to-buffer-same-window buf)))

;;; --- Public API ---

(defun ebb-version ()
  "Return the el-be-back version."
  (ebb--version))

(provide 'el-be-back)
;;; el-be-back.el ends here
