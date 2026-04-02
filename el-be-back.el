;;; el-be-back.el --- Terminal emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: terminals
;; URL: https://github.com/ArthurHeymans/el-be-back

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

(defcustom ebb-always-compile-module nil
  "If non-nil, compile the module automatically if not found.
When nil, an error is raised with instructions to run
\\[ebb-compile-module] manually."
  :type 'boolean
  :group 'el-be-back)

(defcustom ebb-tramp-method "sshx"
  "TRAMP method to use when constructing paths for remote hosts.
Used when the shell reports a CWD on an unknown remote host
\(e.g. after typing `ssh host' inside ebb)."
  :type 'string
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
  "Compile the Rust dynamic module.
On NixOS, uses `nix shell' to provide a working Rust toolchain
\(rustup's linker paths are broken on NixOS).  Otherwise uses
`cargo build --release' directly."
  (interactive)
  (let ((default-directory ebb--directory))
    (cond
     ;; NixOS: always use nix shell (rustup's linker is broken)
     ((file-exists-p "/etc/NIXOS")
      (ebb--compile-module-nix))
     ;; Has cargo: use it directly
     ((executable-find "cargo")
      (ebb--compile-module-cargo))
     ;; No cargo but has nix: use nix shell
     ((executable-find "nix")
      (ebb--compile-module-nix))
     (t
      (error "[ebb] Neither cargo nor nix found.  Install Rust or Nix to compile the module")))))

(defun ebb--compile-module-nix ()
  "Compile the module using a nix-provided Rust toolchain.
Runs `nix shell nixpkgs#cargo nixpkgs#rustc nixpkgs#gcc' to get
a working toolchain, then `cargo build --release' inside it.
Works in any directory (does not require a git repo or flake)."
  (let ((default-directory ebb--directory))
    (message "[ebb] Compiling module via nix shell (this may take a few minutes)...")
    (let ((status (call-process
                   "nix" nil "*ebb-compile*" t
                   "shell" "nixpkgs#cargo" "nixpkgs#rustc" "nixpkgs#gcc"
                   "-c" "cargo" "build" "--release")))
      (unless (= status 0)
        (pop-to-buffer "*ebb-compile*")
        (error "[ebb] nix cargo build failed (exit code %d)" status)))
    (ebb--install-built-module)))

(defun ebb--compile-module-cargo ()
  "Compile the module using `cargo build --release'."
  (let ((default-directory ebb--directory))
    (message "[ebb] Compiling module via cargo (this may take a minute)...")
    (let ((status (call-process
                   "cargo" nil "*ebb-compile*" t
                   "build" "--release")))
      (unless (= status 0)
        (pop-to-buffer "*ebb-compile*")
        (error "[ebb] cargo build failed (exit code %d)" status)))
    (ebb--install-built-module)))

(defun ebb--install-built-module ()
  "Copy the built module from target/release/ to the package directory."
  (let* ((target-dir (expand-file-name "target/release/" ebb--directory))
         ;; Cargo converts hyphens to underscores in library filenames
         (crate-name (replace-regexp-in-string "-" "_" ebb--module-name))
         (lib-name (cond
                    ((eq system-type 'darwin)
                     (concat "lib" crate-name ".dylib"))
                    (t
                     (concat "lib" crate-name ".so"))))
         (src (expand-file-name lib-name target-dir))
         (dst (ebb--module-file)))
    (unless (file-exists-p src)
      (error "[ebb] Built library not found at %s" src))
    (copy-file src dst t)
    (message "[ebb] Module compiled successfully.")))

(add-to-list 'load-path ebb--directory)
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

(defvar-local ebb--copy-mode nil
  "Non-nil when copy mode is active (terminal output paused).")

(defvar-local ebb--remote-prefix nil
  "TRAMP remote prefix for SSH sessions (e.g. \"/rpc:root@host:\").
When non-nil, CWD changes from OSC 7 are converted to TRAMP paths.")

;;; --- CWD resolution ---

(defun ebb--resolve-cwd (cwd)
  "Convert an OSC 7 CWD URL to a local or TRAMP path.
CWD is typically file://hostname/path.  Compares the hostname to
the local machine; if different, constructs a TRAMP path (using
`ebb--remote-prefix' when available, or /ssh:host: otherwise)."
  (let* ((hostname (and (string-match "^file://\\([^/]*\\)" cwd)
                        (match-string 1 cwd)))
         (path (if (string-prefix-p "file://" cwd)
                   (replace-regexp-in-string "^file://[^/]*" "" cwd)
                 cwd))
         (local-host-p (or (null hostname)
                           (string= hostname "")
                           (string-equal hostname (system-name))
                           (string-equal hostname
                                         (car (split-string (system-name) "\\."))))))
    (cond
     (local-host-p path)
     (ebb--remote-prefix (concat ebb--remote-prefix path))
     (t (format "/%s:%s:%s" ebb-tramp-method hostname path)))))

;;; --- Hyperlink support ---

(defvar ebb-hyperlink-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ebb-follow-link-at-mouse)
    (define-key map [mouse-2] #'ebb-follow-link-at-mouse)
    (define-key map (kbd "RET") #'ebb-follow-link-at-point)
    map)
  "Keymap active on OSC 8 hyperlinks in the terminal buffer.")

(defun ebb-follow-link-at-point ()
  "Follow the hyperlink at point."
  (interactive)
  (let ((url (get-text-property (point) 'ebb-url)))
    (if url
        (browse-url url)
      (message "No link at point"))))

(defun ebb-follow-link-at-mouse (event)
  "Follow the hyperlink at the mouse click EVENT."
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (url (and pos (get-text-property pos 'ebb-url))))
    (if url
        (browse-url url)
      (message "No link at click position"))))

;;; --- Copy mode ---

(defvar ebb-copy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; In copy mode, standard Emacs navigation works.
    ;; Only bind the exit key.
    (define-key map (kbd "C-c C-c") #'ebb-copy-mode-exit)
    (define-key map (kbd "q") #'ebb-copy-mode-exit)
    map)
  "Keymap for copy mode (terminal paused, Emacs navigation).")

(define-minor-mode ebb-copy-mode
  "Minor mode for navigating terminal output.
Terminal output is paused; standard Emacs keys work for navigation,
search, and copying."
  :lighter " Copy"
  :keymap ebb-copy-mode-map
  (if ebb-copy-mode
      (progn
        (setq ebb--copy-mode t)
        ;; Disable semi-char mode while in copy mode
        (when (bound-and-true-p ebb-semi-char-mode)
          (ebb-semi-char-mode -1))
        (message "Copy mode: navigate with Emacs keys, q or C-c C-c to exit"))
    (setq ebb--copy-mode nil)
    ;; Re-enable semi-char mode
    (ebb-semi-char-mode 1)
    ;; Flush any output that arrived while paused
    (ebb--flush-output)))

(defun ebb-copy-mode-exit ()
  "Exit copy mode and resume terminal."
  (interactive)
  (ebb-copy-mode -1))

;;; --- Major mode ---

(define-derived-mode ebb-mode fundamental-mode "EBB"
  "Major mode for el-be-back terminal emulator."
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  ;; Larger PTY read buffer -- fewer process-filter calls during heavy output.
  (setq-local read-process-output-max (* 64 1024))
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  ;; Disable font-lock completely -- we manage faces ourselves.
  ;; font-lock/jit-lock will otherwise refontify the buffer and strip our
  ;; face properties. We must be aggressive here because frameworks like
  ;; Doom Emacs forcibly re-enable font-lock.
  (setq-local font-lock-defaults nil)
  (setq-local font-lock-function #'ignore)
  (setq-local font-lock-keywords nil)
  (font-lock-mode -1)
  (when (bound-and-true-p jit-lock-mode)
    (jit-lock-mode nil))
  ;; Disable indent-bars and similar visual modes
  (when (bound-and-true-p indent-bars-mode)
    (indent-bars-mode -1))
  ;; Header line showing terminal title
  (setq-local header-line-format
              '(:eval (ebb--header-line)))
  (add-hook 'kill-buffer-hook #'ebb--kill-buffer-hook nil t)
  ;; Buffer-local: fires only for windows showing this buffer, receives
  ;; the window as argument, and is automatically removed when the
  ;; buffer is killed.
  (add-hook 'window-state-change-functions #'ebb--window-state-change nil t))

(defun ebb--header-line ()
  "Generate the header line for the terminal buffer."
  (let ((title (and ebb--terminal (ebb--get-title ebb--terminal)))
        (cwd (and ebb--terminal (ebb--get-cwd ebb--terminal)))
        (size (format "%dx%d"
                      (if ebb--terminal (ebb--get-rows ebb--terminal) 0)
                      (if ebb--terminal (ebb--get-cols ebb--terminal) 0))))
    (concat " EBB"
            (when ebb--copy-mode " [COPY]")
            (when title (format " | %s" title))
            (when cwd
              (format " | %s" (abbreviate-file-name
                                (ebb--resolve-cwd cwd))))
            (format " | %s" size))))

;;; --- Process management ---

(defun ebb--process-filter (process output)
  "Process filter: queue OUTPUT from PROCESS and schedule rendering."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (and ebb--terminal (not (string-empty-p output)))
        ;; Always queue the output (even in copy mode, so we don't lose data)
        (push output ebb--pending-chunks)
        (unless ebb--first-chunk-time
          (setq ebb--first-chunk-time (current-time)))
        ;; Cancel existing timer
        (when ebb--render-timer
          (cancel-timer ebb--render-timer)
          (setq ebb--render-timer nil))
        ;; Schedule render
        ;; Don't schedule render in copy mode -- just queue
        (unless ebb--copy-mode
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
                     (current-buffer))))))))))

(defun ebb--flush-output-in-buffer (buffer)
  "Flush output in BUFFER if it is still alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ebb--flush-output))))

(defun ebb--flush-output ()
  "Process all pending output chunks and render.
Drains as much immediately available PTY data as possible before
rendering, so that a single render covers the maximum amount of
output.  This prevents render overhead from throttling throughput."
  ;; Cancel pending timer
  (when ebb--render-timer
    (cancel-timer ebb--render-timer)
    (setq ebb--render-timer nil))
  (when (and ebb--terminal ebb--pending-chunks)
    ;; Feed current pending chunks.
    (let ((chunks (nreverse ebb--pending-chunks)))
      (setq ebb--pending-chunks nil
            ebb--first-chunk-time nil)
      (ebb--feed ebb--terminal (apply #'concat chunks)))
    ;; Drain any immediately available additional data before rendering.
    ;; Each accept-process-output call runs the process filter which
    ;; queues more chunks.  We feed them and repeat until there is no
    ;; more data or we've spent ebb-maximum-latency total.
    (let ((deadline (+ (float-time) ebb-maximum-latency)))
      (while (and ebb--process
                  (process-live-p ebb--process)
                  (< (float-time) deadline)
                  (accept-process-output ebb--process 0 nil t)
                  ebb--pending-chunks)
        (when ebb--render-timer
          (cancel-timer ebb--render-timer)
          (setq ebb--render-timer nil))
        (let ((chunks (nreverse ebb--pending-chunks)))
          (setq ebb--pending-chunks nil
                ebb--first-chunk-time nil)
          (ebb--feed ebb--terminal (apply #'concat chunks)))))
    ;; Render the final screen state once.
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (inhibit-quit t)
          (buffer-undo-list t))
      (ebb--render-screen))
    ;; Drain terminal responses and send to PTY
    (ebb--drain-and-send)
    ;; Process title/CWD/bell alerts
    (ebb--process-alerts)))

(defun ebb--render-screen (&optional window)
  "Render the terminal screen into the current buffer.
Must be called within the performance-trinity let bindings.
The Rust render function updates scrollback and display rows.
WINDOW, if non-nil, is the window to pin.  Otherwise the window
displaying the current buffer is used.  This avoids corrupting
other windows (e.g. magit) when a render timer fires while a
different window is selected."
  (when ebb--terminal
    (ebb--render ebb--terminal)
    ;; Pin the window to show the display region (the last `rows' lines
    ;; of the buffer), like a real terminal screen.  Scrollback above is
    ;; only visible in copy-mode.
    (unless ebb-copy-mode
      (let ((win (or window (get-buffer-window (current-buffer)))))
        (when win
          (let ((rows (ebb--get-rows ebb--terminal)))
            (save-excursion
              (goto-char (point-max))
              (forward-line (- (1- rows)))
              (set-window-start win (point) t))))))))

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
    ;; CWD change -- resolve OSC 7 URL to local or TRAMP path.
    (let ((cwd (ebb--poll-cwd ebb--terminal)))
      (when cwd
        (let ((dir (ebb--resolve-cwd cwd)))
          (when (or (file-remote-p dir) (file-directory-p dir))
            (setq-local default-directory
                        (file-name-as-directory dir))))))
    ;; Bell
    (when (ebb--poll-bell ebb--terminal)
      (ding t))))

(defun ebb--process-sentinel (process event)
  "Handle PROCESS state change EVENT.
When the process exits, kill the buffer."
  (when (buffer-live-p (process-buffer process))
    (let ((buf (process-buffer process)))
      (if (memq (process-status process) '(exit signal))
          (kill-buffer buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\n[Process %s]\n" (string-trim event)))))))))

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

(defun ebb--window-state-change (window)
  "Handle state change (including resize) for WINDOW.
This is a buffer-local `window-state-change-functions' hook, so it
fires only for windows displaying this ebb buffer."
  (when (and ebb--terminal (window-live-p window))
    (let ((rows (window-body-height window))
          (cols (window-body-width window))
          (cur-rows (ebb--get-rows ebb--terminal))
          (cur-cols (ebb--get-cols ebb--terminal)))
      (when (or (/= rows cur-rows)
                (/= cols cur-cols))
        (ebb--resize ebb--terminal rows cols)
        (when (and ebb--process (process-live-p ebb--process))
          (set-process-window-size ebb--process rows cols))
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (ebb--render-screen window))))))

;;; --- Input handling ---

(defun ebb-send-raw (string)
  "Send STRING directly to the terminal's PTY."
  (interactive "sSend: ")
  (when (and ebb--process (process-live-p ebb--process))
    (process-send-string ebb--process string)))

(defconst ebb--simple-key-bytes
  '(("return"    . "\r")
    ("backspace" . "\x7f")
    ("tab"       . "\t")
    ("escape"    . "\x1b")
    ("DEL"       . "\x7f")
    ("delete"    . "\x1b[3~")
    ("deletechar" . "\x1b[3~")
    ("RET"       . "\r")
    ("TAB"       . "\t")
    ("ESC"       . "\x1b"))
  "Direct byte mappings for keys that KeyCode::encode() returns empty for.")

(defun ebb--send-key (key-name &optional shift ctrl meta)
  "Send KEY-NAME to the terminal via wezterm key encoding.
KEY-NAME is a string like \"a\", \"return\", \"up\", etc.
Uses terminal.key_down() which reads all internal mode state
\(DECCKM, newline mode, keyboard encoding) for correct output."
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((encoded (or
                    ;; Primary: terminal.key_down() -- reads DECCKM,
                    ;; newline mode, etc. from internal terminal state.
                    (let ((result (ebb--key-down ebb--terminal key-name
                                                (if shift 1 nil)
                                                (if ctrl 1 nil)
                                                (if meta 1 nil))))
                      (and result (not (string-empty-p result)) result))
                    ;; Fallback: simple key byte table
                    (let ((entry (assoc key-name ebb--simple-key-bytes)))
                      (when entry
                        (let ((bytes (cdr entry)))
                          (if meta (concat "\x1b" bytes) bytes))))
                    ;; Fallback: ctrl+letter
                    (when (and ctrl (= (length key-name) 1))
                      (let* ((ch (aref key-name 0))
                             (code (- (downcase ch) ?a -1)))
                        (when (and (>= code 1) (<= code 26))
                          (let ((bytes (string code)))
                            (if meta (concat "\x1b" bytes) bytes)))))
                    ;; Fallback: plain character
                    (when (and (= (length key-name) 1) (not ctrl) (not meta))
                      key-name))))
      (when encoded
        (process-send-string ebb--process encoded)))))

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

(defun ebb-yank ()
  "Paste the kill ring content into the terminal.
Uses bracketed paste if the terminal has it enabled."
  (interactive)
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((text (current-kill 0)))
      (when text
        (ebb--send-paste ebb--terminal text)
        (ebb--drain-and-send)))))

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
    (define-key map (kbd "C-y") #'ebb-yank)
    (define-key map (kbd "C-z") (lambda () (interactive) (ebb--send-key "z" nil t nil)))
    (define-key map (kbd "C-\\") (lambda () (interactive) (ebb--send-key "\\" nil t nil)))
    (define-key map (kbd "C-_") (lambda () (interactive) (ebb--send-key "_" nil t nil)))
    ;; C-c C-c sends interrupt
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (ebb--send-key "c" nil t nil)))
    ;; C-c C-k enters copy mode
    (define-key map (kbd "C-c C-k") #'ebb-copy-mode)
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
(define-key ebb-mode-map (kbd "C-c C-k") #'ebb-copy-mode)

;;; --- Entry point ---

;;;###autoload
(defun ebb ()
  "Start a terminal.
When `default-directory' is a remote TRAMP path (e.g. /ssh:host:/path/),
opens an SSH session to the remote host instead of a local shell."
  (interactive)
  (let* ((buf (generate-new-buffer "*ebb*")))
    (with-current-buffer buf
      (ebb-mode)
      ;; Create terminal with initial window dimensions
      (let ((rows (max 1 (window-body-height)))
            (cols (max 1 (window-body-width))))
        (setq ebb--terminal (ebb--new rows cols ebb-max-scrollback))
        ;; Remember the TRAMP prefix for remote CWD tracking.
        (setq ebb--remote-prefix (file-remote-p default-directory))
        (let ((process-environment
               (append
                (list (concat "TERM=" ebb-term-environment-variable)
                      "COLORTERM=truecolor"
                      (format "INSIDE_EMACS=%s,ebb" emacs-version))
                process-environment))
              (inhibit-eol-conversion t))
          (setq ebb--process
                (make-process
                 :name "ebb"
                 :buffer buf
                 :command (ebb--build-shell-command rows cols)
                 :filter #'ebb--process-filter
                 :sentinel #'ebb--process-sentinel
                 :connection-type 'pty)))
        ;; For SSH sessions, inject PROMPT_COMMAND for directory tracking
        ;; after the remote shell starts.  Leading space keeps it out of
        ;; shell history.
        (when ebb--remote-prefix
          (let ((proc ebb--process))
            (run-at-time 1 nil
              (lambda ()
                (when (process-live-p proc)
                  (process-send-string proc
                    (concat
                     " PROMPT_COMMAND='printf \"\\033]7;file://%s%s\\033\\\\\\\\\" \"$(hostname)\" \"$(pwd)\"'\n"
                     " clear\n")))))))
        ;; Enter semi-char mode by default
        (ebb-semi-char-mode 1)))
    (pop-to-buffer-same-window buf)))

(defun ebb--build-shell-command (rows cols)
  "Build the command list to start a shell.
For local directories, starts `ebb-shell-name' with stty initialisation.
For remote TRAMP directories (ssh/sshx/scp), starts a local ssh process
connecting to the remote host."
  (let ((remote (file-remote-p default-directory)))
    (if (not remote)
        ;; Local shell
        `("/usr/bin/env" "sh" "-c"
          ,(format "stty -nl echo rows %d columns %d sane 2>/dev/null; exec \"$@\""
                   rows cols)
          "--" ,ebb-shell-name "-l")
      ;; Remote: start a local ssh command to the remote host.
      ;; TRAMP's make-process with :file-handler doesn't provide a proper
      ;; PTY, so terminal emulators must use a local ssh client instead.
      (require 'tramp)
      (let* ((dissected (tramp-dissect-file-name default-directory))
             (method (tramp-file-name-method dissected))
             (user (tramp-file-name-user dissected))
             (host (tramp-file-name-host dissected))
             (port (tramp-file-name-port dissected))
             (localname (tramp-file-name-localname dissected)))
        (if (member method '("sudo" "su" "doas"))
            ;; Local privilege escalation: open a local shell.
            `("/usr/bin/env" "sh" "-c"
              ,(format "stty -nl echo rows %d columns %d sane 2>/dev/null; exec \"$@\""
                       rows cols)
              "--" ,ebb-shell-name "-l")
          ;; Any other remote method with a host: connect via SSH.
          ;; This covers ssh, sshx, scp, rsync, and custom methods
          ;; like tramp-rpc that ultimately reach the host over SSH.
          ;; Wrap with stty to set PTY dimensions so the SSH client
          ;; negotiates the correct terminal size with the remote.
          (let ((ssh-args (list "-t")))
            (when port
              (push "-p" ssh-args)
              (push (if (numberp port) (number-to-string port) port)
                    ssh-args))
            (push (if user (format "%s@%s" user host) host) ssh-args)
            ;; Build a remote command: cd to directory and start login shell.
            (let ((cd-cmd (if (and localname
                                   (not (string= localname "/"))
                                   (not (string= localname "")))
                              (format "cd %s; " (shell-quote-argument localname))
                            "")))
              (setq ssh-args
                    (append ssh-args
                            (list (format "%sexec $SHELL -l" cd-cmd)))))
            `("/usr/bin/env" "sh" "-c"
              ,(format "stty rows %d columns %d sane 2>/dev/null; exec \"$@\""
                       rows cols)
              "--" "ssh" ,@ssh-args)))))))

;;; --- Public API ---

(defun ebb-version ()
  "Return the el-be-back version."
  (ebb--version))

(provide 'el-be-back)
;;; el-be-back.el ends here
