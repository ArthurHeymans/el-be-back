;;; chomp.el --- Terminal emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: terminals, processes
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Chomp is a terminal emulator for Emacs.  It uses a separated screen
;; model (pure data structure) and async I/O to ensure the main thread
;; never hangs.
;;
;; Usage:  M-x chomp

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'project)
(require 'bookmark)
(require 'face-remap)
(require 'chomp-term)
(require 'chomp-parse)
(require 'chomp-render)
(require 'chomp-input)
(require 'chomp-io)
(require 'chomp-shell)

(declare-function notifications-notify "notifications")

;;;; ---- Customization --------------------------------------------------

(defgroup chomp nil
  "Terminal emulator."
  :group 'processes
  :prefix "chomp-")

(defcustom chomp-buffer-name "*chomp*"
  "Default buffer name for chomp terminals."
  :type 'string
  :group 'chomp)

(defcustom chomp-default-shell nil
  "Shell to run.  nil means use `$SHELL' or `shell-file-name'.
For best shell integration, set this to your interactive shell
\(e.g., \"/usr/bin/fish\") if it differs from `$SHELL'."
  :type '(choice (const nil) string)
  :group 'chomp)

(defcustom chomp-tramp-shells
  '(("ssh" login-shell)
    ("sshx" login-shell)
    ("scp" login-shell)
    ("docker" "/bin/sh"))
  "Shell to use for remote TRAMP connections, per method.
Each entry is (TRAMP-METHOD SHELL [FALLBACK ARG...]).  TRAMP-METHOD
is a method string such as \"ssh\", or t as a catch-all default.

SHELL is a path string, or the symbol `login-shell' to auto-detect
the remote user's login shell via `getent passwd'.  FALLBACK, when
present, is used when login-shell detection fails.  Any elements
after FALLBACK are extra shell arguments; when none are given,
recognized shells (bash, zsh, fish) start login+interactive
\(`-l -i') so they source the user's rc/profile files."
  :type '(alist :key-type (choice string (const t)) :value-type sexp)
  :group 'chomp)

(defcustom chomp-tramp-default-method nil
  "TRAMP method for remote paths built from OSC 7 directory reports.
Used when the shell reports a non-local hostname and the buffer's
`default-directory' has no existing remote prefix.  nil means use
`tramp-default-method'."
  :type '(choice (const :tag "Use tramp-default-method" nil) string)
  :group 'chomp)

(defvar tramp-default-method)

(defcustom chomp-kill-buffer-on-exit t
  "If non-nil, kill the buffer when the shell process exits.
With the default of t, Ctrl-D / shell exit closes the chomp buffer."
  :type 'boolean
  :group 'chomp)

(defcustom chomp-detect-password-prompts t
  "If non-nil, open `read-passwd' when the cursor row looks like a password prompt."
  :type 'boolean
  :group 'chomp)

(defcustom chomp-password-prompt-regex comint-password-prompt-regexp
  "Regex matched against the cursor row to detect a password prompt."
  :type 'regexp
  :group 'chomp)

(defcustom chomp-password-prompt-debounce 0.2
  "Seconds to wait after detecting a password prompt before opening `read-passwd'."
  :type 'number
  :group 'chomp)

(defcustom chomp-password-prompt-functions
  '(chomp--default-password-source)
  "Sources tried in order when a password is needed.
Each function receives the cursor-row text (or nil) and should return a
password string or nil to try the next source."
  :type 'hook
  :group 'chomp)

(defcustom chomp-query-before-kill 'auto
  "Whether to query before killing a buffer with a running process.
`auto' queries only if the process is running.  t always queries."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'chomp)

(defcustom chomp-show-title t
  "If non-nil, track OSC titles for buffer naming when using title mode."
  :type 'boolean
  :group 'chomp)

(defcustom chomp-buffer-name-function #'chomp-buffer-name-by-directory
  "Function that maps OSC title + `default-directory' to a buffer name.
Called with the latest OSC title string (may be nil).  Return a name, or
nil to leave the buffer name alone.  Set to nil to disable auto-rename.

Defaults to directory naming like ghostel users typically want: local
buffers show an abbreviated path without the hostname; remote paths keep
the TRAMP host."
  :type '(choice (const :tag "Disabled" nil)
                 (function-item :tag "By directory"
                                chomp-buffer-name-by-directory)
                 (function-item :tag "By title"
                                chomp-buffer-name-by-title)
                 function)
  :group 'chomp)

(defcustom chomp-scrollback-lines 10000
  "Maximum number of scrollback lines."
  :type 'integer
  :group 'chomp)

(defcustom chomp-default-input-mode 'semi-char
  "Default input mode when opening a terminal.
Options: `char', `semi-char', `emacs'."
  :type '(choice (const char) (const semi-char) (const emacs))
  :group 'chomp)

(defcustom chomp-notification-function #'chomp--default-notification
  "Function called asynchronously with notification TITLE and BODY."
  :type '(choice (const nil) function)
  :group 'chomp)

(defcustom chomp-progress-function #'chomp--default-progress
  "Function called with normalized progress STATE and PERCENT."
  :type '(choice (const nil) function)
  :group 'chomp)

;;;; ---- Buffer-local Variables -----------------------------------------

(defvar-local chomp--io nil "The chomp-io instance for this buffer.")
(defvar-local chomp--screen nil "The chomp-screen for this buffer.")
(defvar-local chomp--parser nil "The chomp-parser for this buffer.")
(defvar-local chomp--render nil "The chomp-render-state for this buffer.")
(defvar-local chomp--input-mode nil "Current input mode symbol.")
(defvar-local chomp--session-id nil "Stable identity of this terminal session.")
(defvar-local chomp--progress nil "Current terminal progress mode-line string.")
(defvar-local chomp--mouse-drag-transient-map-exit nil
  "Function that exits the active mouse drag transient map.")

(defvar-local chomp--password-mode-p nil
  "Non-nil while a password prompt is active.")
(defvar-local chomp--password-handled-y nil
  "Display row of the last handled password prompt, or nil.")
(defvar-local chomp--password-confirm-timer nil
  "Pending debounce timer for password prompt detection.")
(defvar-local chomp--password-prompt-active nil
  "Non-nil while the password source chain is running.")
(defvar-local chomp--title nil
  "Last OSC title string reported by the shell.")
(defvar-local chomp--managed-buffer-name nil
  "Last buffer name set by chomp auto-rename, or nil if unmanaged.")

(defvar chomp--focus-change-installed nil
  "Non-nil when `chomp--focus-change' is installed globally.")

;;;; ---- Minor Modes for Input ------------------------------------------

(define-minor-mode chomp--semi-char-mode
  "Minor mode for semi-char input."
  :lighter " Semi"
  :keymap chomp-semi-char-mode-map)

(define-minor-mode chomp--char-mode
  "Minor mode for char input."
  :lighter " Char"
  :keymap chomp-char-mode-map)

(define-minor-mode chomp--mouse-mode
  "Minor mode for forwarding DEC mouse events to the terminal."
  :lighter nil
  :keymap chomp-mouse-mode-map)

;;;; ---- Input Mode Switching -------------------------------------------

(defun chomp-semi-char-mode ()
  "Switch to semi-char input mode."
  (interactive)
  (chomp--char-mode -1)
  (chomp--semi-char-mode 1)
  (setq chomp--input-mode 'semi-char)
  (force-mode-line-update))

(defun chomp-char-mode ()
  "Switch to char input mode (all keys to terminal)."
  (interactive)
  (chomp--semi-char-mode -1)
  (chomp--char-mode 1)
  (setq chomp--input-mode 'char)
  (force-mode-line-update))

(defun chomp-emacs-mode ()
  "Switch to Emacs input mode (normal Emacs keys)."
  (interactive)
  (chomp--semi-char-mode -1)
  (chomp--char-mode -1)
  (setq chomp--input-mode 'emacs)
  (force-mode-line-update))

;;;; ---- Self-Input Command ---------------------------------------------

(defun chomp--require-running-terminal ()
  "Return the current terminal I/O state or signal `user-error'."
  (unless (or (eq major-mode 'chomp-mode)
              (bound-and-true-p chomp-eshell--io))
    (user-error "Not in a Chomp buffer"))
  (let ((process (and chomp--io (chomp-io-process chomp--io))))
    (unless (and process (process-live-p process))
      (user-error "Process not running")))
  chomp--io)

;;;###autoload
(defun chomp-send-string (string)
  "Send STRING unchanged to the current terminal."
  (interactive "sSend string: ")
  (chomp-io-send (chomp--require-running-terminal) string))

(defun chomp--key-event (key modifiers)
  "Return an Emacs key event for KEY and comma-separated MODIFIERS."
  (let* ((basic (cond ((characterp key) key)
                      ((and (stringp key) (= (length key) 1)) (aref key 0))
                      ((stringp key) (intern (downcase key)))
                      ((symbolp key) key)
                      (t (user-error "Invalid key: %S" key))))
         (mods (mapcar
                (lambda (name)
                  (let ((modifier (intern (downcase (string-trim name)))))
                    (cond ((eq modifier 'alt) 'meta)
                          ((eq modifier 'ctrl) 'control)
                          ((memq modifier '(control meta shift super hyper))
                           modifier)
                          (t (user-error "Unknown modifier: %s" name)))))
                (if (or (null modifiers) (string-empty-p modifiers))
                    nil
                  (split-string modifiers "," t "[[:space:]]*")))))
    (if mods (event-convert-list (append mods (list basic))) basic)))

;;;###autoload
(defun chomp-send-key (key &optional modifiers)
  "Send named KEY with comma-separated MODIFIERS to the current terminal."
  (interactive (list (read-string "Key: ")
                     (read-string "Modifiers (comma-separated): ")))
  (let* ((io (chomp--require-running-terminal))
         (sequence (chomp-input-translate
                    (chomp--key-event key modifiers) chomp--screen)))
    (unless sequence
      (user-error "Unsupported key: %s" key))
    (chomp-io-send io sequence)))

;;;###autoload
(defun chomp-paste-string (string)
  "Paste STRING, using bracketed paste when terminal mode 2004 is active."
  (interactive "sPaste string: ")
  (let ((io (chomp--require-running-terminal)))
    (chomp-io-send
     io
     (if (and chomp--screen (chomp-screen-bracketed-paste chomp--screen))
         (concat (chomp-input-bracketed-paste-start) string
                 (chomp-input-bracketed-paste-end))
       string))))

(defun chomp-self-input (n &optional e)
  "Send the key event E to the terminal N times.
N defaults to 1, E defaults to `last-command-event'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (let ((keys (this-command-keys)))
           (if (and (> (length keys) 1)
                    (eq (aref keys (- (length keys) 2))
                        meta-prefix-char))
               ;; Reconstruct Meta modifier from ESC prefix
               (cond
                ((eq last-command-event meta-prefix-char)
                 last-command-event)
                ((characterp last-command-event)
                 (aref (kbd (format "M-%c" last-command-event)) 0))
                ((symbolp last-command-event)
                 (aref (kbd (format "M-<%S>" last-command-event)) 0))
                (t last-command-event))
             last-command-event))))
  (when chomp--io
    (let ((seq (chomp-input-translate (or e last-command-event) chomp--screen)))
      (when seq
        (dotimes (_ (or n 1))
          (chomp-io-send chomp--io seq))))))

(defun chomp-quoted-input ()
  "Read the next key event literally and send it to the terminal."
  (interactive)
  (when chomp--io
    (let* ((key (read-event "Send key: "))
           (seq (chomp-input-translate key chomp--screen)))
      (when seq
        (chomp-io-send chomp--io seq)))))

(defun chomp-yank ()
  "Yank (paste) from the kill ring into the terminal.
If bracketed paste mode is active, wraps in bracketed paste sequences."
  (interactive)
  (when chomp--io
    (let ((text (current-kill 0)))
      (when text
        (chomp-paste-string text)))))

(defun chomp-yank-pop ()
  "Yank-pop: replace the last yank with the next kill ring entry."
  (interactive)
  (when chomp--io
    (current-kill 1)
    (let ((text (current-kill 0)))
      (when text
        (chomp-paste-string text)))))

(defun chomp-send-password (&optional password)
  "Read PASSWORD from the minibuffer and send it to the terminal.
Interactively, this uses `read-passwd' so password keystrokes do not go through
normal terminal input handling or appear in `view-lossage'."
  (interactive)
  (unless chomp--io
    (user-error "Process not running"))
  (let ((password (or password (read-passwd "Password: "))))
    (when password
      (chomp-io-send chomp--io password)
      (chomp-io-send chomp--io "\r"))))

;;;; ---- Password prompt detection -------------------------------------

(defun chomp--cursor-row-text ()
  "Return trimmed text of the terminal cursor row, or nil."
  (when chomp--screen
    (let* ((y (chomp-screen-cursor-y chomp--screen))
           (line (chomp-screen-get-line chomp--screen y))
           (width (chomp-screen-width chomp--screen))
           (text (or (chomp-line-text line)
                     (and (chomp-line-cells line)
                          (let ((cells (chomp-line-cells line))
                                (out (make-string width ?\s)))
                            (dotimes (i (min width (length cells)))
                              (aset out i (chomp-cell-char (aref cells i))))
                            out)))))
      (when text
        (let ((row (string-trim-right text)))
          (and (not (string-empty-p row)) row))))))

(defun chomp--password-prompt-detected-p ()
  "Return non-nil if the cursor row matches `chomp-password-prompt-regex'."
  (when-let* ((row (chomp--cursor-row-text))
              (case-fold-search t))
    (string-match-p chomp-password-prompt-regex row)))

(defun chomp--default-password-source (row)
  "Prompt with `read-passwd', labeling with ROW when available."
  (read-passwd (concat (or row "Password:") " ")))

(defun chomp--cancel-password-confirm-timer ()
  "Cancel a pending password-prompt debounce timer."
  (when chomp--password-confirm-timer
    (cancel-timer chomp--password-confirm-timer)
    (setq chomp--password-confirm-timer nil)))

(defun chomp--confirm-and-prompt (buf)
  "Re-check password detection in BUF, then open the password minibuffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq chomp--password-confirm-timer nil)
      (when (and chomp--password-mode-p
                 (chomp--password-prompt-detected-p))
        (chomp--prompt-password)))))

(defun chomp--prompt-password ()
  "Run `chomp-password-prompt-functions' and send the result to the PTY."
  (let ((pwd nil)
        (row (chomp--cursor-row-text))
        (y (and chomp--screen (chomp-screen-cursor-y chomp--screen))))
    (setq chomp--password-prompt-active t)
    (unwind-protect
        (setq pwd (run-hook-with-args-until-success
                   'chomp-password-prompt-functions row))
      (setq chomp--password-prompt-active nil)
      (when (and pwd chomp--io)
        ;; Send before clear-string; process-send-string may keep the string.
        (chomp-io-send chomp--io (concat pwd "\r"))
        (clear-string pwd))
      (setq chomp--password-handled-y y
            chomp--password-mode-p nil)
      (force-mode-line-update))))

(defun chomp--detect-password-prompt ()
  "Watch the cursor row and open `read-passwd' on a password prompt.
Called after each render.  Debounced so short-lived matches don't flash."
  (when (and chomp-detect-password-prompts chomp--screen chomp--io
             (not (eq chomp--input-mode 'emacs))
             (not chomp--password-prompt-active))
    (let ((now (chomp--password-prompt-detected-p))
          (y (chomp-screen-cursor-y chomp--screen)))
      (cond
       ((not now)
        (chomp--cancel-password-confirm-timer)
        (when (or chomp--password-mode-p chomp--password-handled-y)
          (setq chomp--password-mode-p nil
                chomp--password-handled-y nil)
          (force-mode-line-update)))
       (chomp--password-mode-p nil)
       ((and chomp--password-handled-y
             (= y chomp--password-handled-y))
        nil)
       (t
        (setq chomp--password-mode-p t
              chomp--password-handled-y nil)
        (force-mode-line-update)
        (chomp--cancel-password-confirm-timer)
        (setq chomp--password-confirm-timer
              (run-at-time chomp-password-prompt-debounce nil
                           #'chomp--confirm-and-prompt
                           (current-buffer))))))))

(defun chomp-mouse-input (event)
  "Send mouse EVENT to the terminal when DEC mouse tracking is active."
  (interactive "e")
  (when-let ((win (posn-window (event-start event))))
    (when (windowp win)
      (select-window win)))
  (when (and chomp--io chomp--screen chomp--render
             (chomp-screen-mouse-mode chomp--screen))
    (when (and (memq 'down (event-modifiers event))
               (not chomp--mouse-drag-transient-map-exit))
      (let ((old-track-mouse track-mouse)
            (buffer (current-buffer)))
        (setq track-mouse 'dragging)
        (setq chomp--mouse-drag-transient-map-exit
              (set-transient-map
               chomp-mouse-mode-map
               #'always
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq track-mouse old-track-mouse)
                     (setq chomp--mouse-drag-transient-map-exit nil))))))))
    (when-let ((seq (chomp-input-encode-mouse
                     event chomp--screen
                     (marker-position
                      (chomp-render-state-display-begin chomp--render)))))
      (chomp-io-send chomp--io seq))
    (when (and chomp--mouse-drag-transient-map-exit
               (chomp-input--mouse-release-p event))
      (funcall chomp--mouse-drag-transient-map-exit)
      (setq chomp--mouse-drag-transient-map-exit nil))))

;;;; ---- Display Commands -----------------------------------------------

(defun chomp--clear-screen (scrollback)
  "Clear the viewport, and history too when SCROLLBACK is non-nil."
  (chomp--require-running-terminal)
  (chomp-screen-cursor-goto chomp--screen 0 0)
  (chomp-screen-erase-in-display chomp--screen 2)
  (when scrollback
    (chomp-screen-erase-in-display chomp--screen 3))
  (chomp-shell-cleanup)
  (chomp-shell-setup-margins)
  (when chomp--render
    (chomp-render-refresh chomp--render))
  (chomp-send-key "l" "control"))

;;;###autoload
(defun chomp-clear ()
  "Clear the viewport, retaining scrollback, and request a prompt redraw."
  (interactive)
  (chomp--clear-screen nil))

;;;###autoload
(defun chomp-clear-scrollback ()
  "Clear the viewport and scrollback, then request a prompt redraw."
  (interactive)
  (chomp--clear-screen t))

;;;###autoload
(defun chomp-copy-all ()
  "Copy all terminal scrollback and viewport text to the kill ring."
  (interactive)
  (unless (and (eq major-mode 'chomp-mode) chomp--screen)
    (user-error "Not in a Chomp buffer"))
  (let ((text (chomp-screen-plain-text chomp--screen)))
    (kill-new text)
    text))

;;;; ---- Process Management Commands ------------------------------------

(defun chomp-kill-process ()
  "Kill the terminal process."
  (interactive)
  (when-let ((proc (and chomp--io (chomp-io-process chomp--io))))
    (when (process-live-p proc)
      (kill-process proc))))

(defun chomp-reset ()
  "Reset the terminal to its initial state."
  (interactive)
  (when chomp--screen
    (chomp-screen-reset chomp--screen)
    (when chomp--render
      (chomp-render-full-reset chomp--render))))

(defun chomp-previous-prompt (&optional n)
  "Enter Emacs mode and move to the Nth previous shell prompt."
  (interactive "p")
  (unless (eq chomp--input-mode 'emacs)
    (chomp-emacs-mode))
  (chomp-shell-previous-prompt n))

(defun chomp-next-prompt (&optional n)
  "Enter Emacs mode and move to the Nth next shell prompt."
  (interactive "p")
  (unless (eq chomp--input-mode 'emacs)
    (chomp-emacs-mode))
  (chomp-shell-next-prompt n))

;;;; ---- Notifications and Progress -------------------------------------

(defun chomp--default-notification (title body)
  "Display a desktop notification with TITLE and BODY."
  (require 'notifications)
  (notifications-notify :title (or title "Terminal") :body body))

(defun chomp--default-progress (state percent)
  "Display normalized progress STATE and PERCENT in the mode line."
  (setq chomp--progress
        (pcase state
          ('remove nil)
          ('set (format "[%d%%]" percent))
          ('error (format "[Error %d%%]" percent))
          ('indeterminate "[…]")
          ('pause (format "[Paused %d%%]" percent))))
  (force-mode-line-update))

(defun chomp--run-callback (buffer function args)
  "Run FUNCTION with ARGS in BUFFER, isolating callback errors."
  (when (and (buffer-live-p buffer) function)
    (with-current-buffer buffer
      (condition-case error-data
          (apply function args)
        (error
         (message "[chomp] Callback error: %S" error-data)
         nil)))))

(defun chomp--defer-callback (function &rest args)
  "Run FUNCTION later in the current buffer with ARGS."
  (run-at-time 0 nil #'chomp--run-callback
               (current-buffer) function args))

;;;; ---- Event Handler --------------------------------------------------

(defun chomp--handle-event (type &rest args)
  "Handle events emitted by the parser."
  (pcase type
    ('bell (ding t))
    ('title
     (when chomp-show-title
       (chomp--set-title (car args))))
    ('cwd
     (let ((path (chomp--cwd-to-path (car args) (cadr args))))
       ;; Remote: trust the shell's report; a `file-directory-p' here
       ;; would open a synchronous TRAMP connection on every cd.
       (when (and path (if (file-remote-p path) t (file-directory-p path)))
         (setq default-directory (file-name-as-directory path)
               list-buffers-directory default-directory)
         (when chomp-buffer-name-function
           (chomp--rename-managed
            (funcall chomp-buffer-name-function chomp--title))))))
    ('cursor-style
     ;; Could update cursor display here
     nil)
    ('mode-set
     (let ((mode (car args)))
       ;; Handle modes that need buffer-level action
       (pcase mode
         ((or 1000 1002 1003)
          (chomp--mouse-mode (if (chomp-screen-mouse-mode chomp--screen) 1 -1))
          (unless (chomp-screen-mouse-mode chomp--screen)
            (setf (chomp-screen-mouse-pressed chomp--screen) nil)
            (when chomp--mouse-drag-transient-map-exit
              (funcall chomp--mouse-drag-transient-map-exit)
              (setq chomp--mouse-drag-transient-map-exit nil))))
         (1004
          ;; Focus events -- could enable focus tracking here
          nil)
         (_ nil))))
    ('process-exit
     (let ((event (car args)))
       (when (buffer-live-p (current-buffer))
         (with-current-buffer (current-buffer)
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "\n\n[Process %s]\n"
                             (string-trim event))))
           ;; Switch to emacs mode
           (chomp-emacs-mode)
           (when chomp-kill-buffer-on-exit
             (run-at-time 1 nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (kill-buffer buf)))
                          (current-buffer)))))))
    ('osc-51
     (let ((payload (car args)))
       (chomp-shell-handle-osc51 payload chomp--screen)))
    ('notification
     (when chomp-notification-function
       (apply #'chomp--defer-callback chomp-notification-function args)))
    ('progress
     (chomp--run-callback (current-buffer) chomp-progress-function args))
    ('reset nil)
    (_ nil)))

;;;; ---- Focus Events ---------------------------------------------------

(defun chomp--focus-change ()
  "Handle focus changes for chomp buffers."
  (dolist (frame (frame-list))
    (let ((focused (frame-focus-state frame)))
      (dolist (win (window-list frame 'no-minibuf))
        (let ((buf (window-buffer win)))
          (when (buffer-local-value 'chomp--io buf)
            (with-current-buffer buf
              (when (and chomp--io chomp--screen
                         (chomp-screen-focus-events chomp--screen))
                (chomp-io-send chomp--io
                               (if focused
                                   (chomp-input-focus-in)
                                 (chomp-input-focus-out)))))))))))

(defun chomp--ensure-focus-change-hook ()
  "Install global focus tracking once."
  (unless chomp--focus-change-installed
    (add-function :after after-focus-change-function #'chomp--focus-change)
    (setq chomp--focus-change-installed t)))

(defun chomp--maybe-remove-focus-change-hook ()
  "Remove global focus tracking when no other chomp buffers remain."
  (when (and chomp--focus-change-installed
             (not (cl-some (lambda (buf)
                             (and (not (eq buf (current-buffer)))
                                  (buffer-live-p buf)
                                  (eq (buffer-local-value 'major-mode buf)
                                      'chomp-mode)))
                           (buffer-list))))
    (remove-function after-focus-change-function #'chomp--focus-change)
    (setq chomp--focus-change-installed nil)))

;;;; ---- Resize Hook ----------------------------------------------------

(defun chomp--window-size-change (window)
  "Handle a size change for WINDOW showing a Chomp buffer."
  (let ((buffer (window-buffer window)))
    (when (buffer-local-value 'chomp--io buffer)
      (with-current-buffer buffer
        (let ((new-width (window-max-chars-per-line window))
              (new-height (window-body-height window)))
          (when (and (> new-width 0) (> new-height 0))
            (chomp-io-handle-resize chomp--io new-width new-height)))))))

;;;; ---- Major Mode -----------------------------------------------------

(define-derived-mode chomp-mode fundamental-mode "Chomp"
  "Major mode for the chomp terminal emulator."
  (setq-local buffer-read-only t)
  (setq-local buffer-undo-list t)
  (setq-local truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1)
  (setq-local auto-hscroll-mode nil)
  (setq-local hscroll-margin 0)
  ;; Disable features that conflict with terminal display
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local show-trailing-whitespace nil)
  (setq-local display-line-numbers nil)
  (face-remap-add-relative 'default 'chomp-default)
  (setq-local bookmark-make-record-function #'chomp-bookmark-make-record)
  (setq-local imenu-create-index-function #'chomp-shell-imenu-create-index)
  (setq-local imenu-default-goto-function #'chomp-shell-imenu-goto)
  (setq-local mode-line-process
              '(" " (:eval (chomp--mode-line-input-mode))))
  ;; Set up shell integration margins
  (chomp-shell-setup-margins)
  ;; Buffer-local window-size-change hooks receive a window, not a frame.
  (add-hook 'window-size-change-functions #'chomp--window-size-change nil t)
  ;; Add focus tracking
  (chomp--ensure-focus-change-hook)
  ;; Clean up shell state on kill
  (add-hook 'kill-buffer-hook #'chomp-shell-cleanup nil t)
  ;; Kill the (possibly remote) shell process with the buffer; the
  ;; process has no buffer of its own, so Emacs won't reap it.
  (add-hook 'kill-buffer-hook
            (lambda () (when chomp--io (chomp-io-stop chomp--io)))
            nil t)
  (add-hook 'kill-buffer-hook #'chomp--maybe-remove-focus-change-hook nil t)
  ;; Query before kill
  (when chomp-query-before-kill
    (add-hook 'kill-buffer-query-functions #'chomp--kill-buffer-query nil t)))

(defun chomp--kill-buffer-query ()
  "Query before killing a chomp buffer with a live process."
  (or (not chomp--io)
      (not (chomp-io-process chomp--io))
      (not (process-live-p (chomp-io-process chomp--io)))
      (yes-or-no-p "Terminal process is running.  Kill buffer? ")))

;;;; ---- Entry Points ---------------------------------------------------

(defun chomp--buffers ()
  "Return live Chomp buffers sorted by name."
  (sort (seq-filter (lambda (buffer)
                      (eq (buffer-local-value 'major-mode buffer) 'chomp-mode))
                    (buffer-list))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

(defun chomp--find-session (identity)
  "Return the Chomp buffer with stable IDENTITY, if any."
  (seq-find (lambda (buffer)
              (equal identity
                     (buffer-local-value 'chomp--session-id buffer)))
            (chomp--buffers)))

(defun chomp--cycle (step)
  "Switch STEP places through the sorted Chomp buffer list, wrapping."
  (let ((buffers (chomp--buffers)))
    (unless buffers
      (user-error "No Chomp sessions"))
    (let* ((position (cl-position (current-buffer) buffers))
           (target (if position
                       (nth (mod (+ position step) (length buffers)) buffers)
                     (if (> step 0) (car buffers) (car (last buffers))))))
      (pop-to-buffer-same-window target)
      target)))

;;;###autoload
(defun chomp-next ()
  "Switch to the next Chomp session, wrapping at the end."
  (interactive)
  (chomp--cycle 1))

;;;###autoload
(defun chomp-previous ()
  "Switch to the previous Chomp session, wrapping at the beginning."
  (interactive)
  (chomp--cycle -1))

;;;###autoload
(defun chomp-list-buffers ()
  "Choose a Chomp session, defaulting to the next one."
  (interactive)
  (let* ((buffers (chomp--buffers))
         (next (and buffers
                    (nth (mod (1+ (or (cl-position (current-buffer) buffers) -1))
                              (length buffers))
                         buffers))))
    (unless buffers
      (user-error "No Chomp sessions"))
    (pop-to-buffer-same-window
     (get-buffer
      (completing-read "Chomp session: "
                       (mapcar #'buffer-name buffers) nil t nil nil
                       (buffer-name next))))))

;;;###autoload
(defun chomp-other (&optional program)
  "Switch to another Chomp session, or create one with PROGRAM."
  (interactive)
  (if-let ((other (seq-find (lambda (buffer) (not (eq buffer (current-buffer))))
                            (chomp--buffers))))
      (pop-to-buffer-same-window other)
    (chomp program)))

(defun chomp--cwd-to-path (dir host)
  "Return OSC 7 report DIR as a usable path, given the reporting HOST.
Reports from a non-local HOST become TRAMP paths.  The buffer's remote
prefix is reused when it targets the reported host (preserves method,
user, multi-hop); a different host means the user ssh'd onward from
this buffer's host, so a fresh path is built via
`chomp-tramp-default-method'.  A local-looking HOST (or none) in a
remote buffer is the remote shell reporting on itself."
  (when (and dir (not (string-empty-p dir)))
    (let ((prefix (file-remote-p default-directory)))
      (cond
       ((not (chomp--local-host-p host))
        (if (and prefix
                 (equal (downcase host)
                        (downcase (or (file-remote-p default-directory 'host)
                                      ""))))
            (concat prefix dir)
          (progn
            (require 'tramp)
            (format "/%s:%s:%s"
                    (or chomp-tramp-default-method tramp-default-method)
                    host dir))))
       (prefix (concat prefix dir))
       (t dir)))))

(defun chomp--remote-login-shell ()
  "Return the remote user's login shell via `getent passwd', or nil.
Runs on the host of `default-directory' through TRAMP."
  (with-temp-buffer
    (when (eq 0 (ignore-errors
                  (process-file-shell-command "getent passwd \"$LOGNAME\""
                                              nil (current-buffer))))
      ;; With $LOGNAME unset, bare `getent passwd' dumps the whole
      ;; database with exit 0 -- only a single-line reply is a user.
      (when (= (count-lines (point-min) (point-max)) 1)
        (let ((shell (nth 6 (split-string (string-trim (buffer-string)) ":"))))
          (and shell (not (string-empty-p shell)) shell))))))

(defun chomp--remote-shell ()
  "Return the shell argv list to run for a remote `default-directory'.
Resolves per TRAMP method via `chomp-tramp-shells'; falls back to
/bin/sh when nothing resolves."
  (let* ((method (file-remote-p default-directory 'method))
         (spec (cdr (or (assoc method chomp-tramp-shells)
                        (assoc t chomp-tramp-shells))))
         (program (or (if (eq (car spec) 'login-shell)
                          (or (chomp--remote-login-shell) (cadr spec))
                        (car spec))
                      "/bin/sh"))
         (args (or (cddr spec)
                   (and (memq (chomp-io--detect-shell program)
                              '(bash zsh fish))
                        '("-l" "-i")))))
    (cons program args)))

;;;###autoload
(defun chomp (&optional program)
  "Start a terminal emulator.
With a numeric prefix argument N, switch to the Nth existing chomp
session (creating it if needed).  With \\[universal-argument] \\[universal-argument],
prompt for the program to run.  PROGRAM defaults to `chomp-default-shell'
or `$SHELL'.

When `default-directory' is remote, spawns the remote shell via TRAMP
(see `chomp-tramp-shells').  Shell integration is not deployed to the
remote host: cwd tracking there requires the remote rc files to emit
OSC 7/51 themselves (e.g. by sourcing the scripts in chomp's
`integration/' directory)."
  (interactive
   (list
    (cond
     ;; C-u C-u: prompt for program
     ((equal current-prefix-arg '(16))
      (let ((command
             (read-shell-command "Run program: "
                                 (or chomp-default-shell
                                     (getenv "SHELL")
                                     shell-file-name))))
        (let ((argv (split-string-shell-command command)))
          (when (or (null argv) (string-empty-p (car argv)))
            (user-error "Program cannot be empty"))
          argv)))
     (t nil))))
  ;; With numeric prefix: switch to Nth chomp buffer
  (when (and (numberp current-prefix-arg) (not program))
    (let* ((n current-prefix-arg)
           (chomp-bufs (chomp--buffers))
           (existing (nth (1- n) chomp-bufs)))
      (when existing
        (pop-to-buffer-same-window existing)
        (cl-return-from chomp existing))))
  (let* ((shell (or program
                    (and (file-remote-p default-directory)
                         (chomp--remote-shell))
                    chomp-default-shell
                    (getenv "SHELL")
                    shell-file-name
                    "/bin/sh"))
         (buf (generate-new-buffer chomp-buffer-name)))
    ;; Set up mode first
    (with-current-buffer buf
      (chomp-mode)
      (setq chomp--session-id (buffer-name buf))
      ;; Determine initial size from a window
      (let* ((win (or (get-buffer-window buf)
                      (selected-window)))
             (width (max (window-max-chars-per-line win) 10))
             (height (max (window-body-height win) 3)))
        ;; Create screen model
        (setq chomp--screen (chomp-screen-create width height))
        (setf (chomp-screen-scrollback-max chomp--screen) chomp-scrollback-lines)
        ;; Create renderer
        (setq chomp--render (chomp-render-create chomp--screen buf))
        ;; Create parser
        (setq chomp--parser (chomp-parse-create chomp--screen nil
                                                #'chomp--handle-event))
        ;; Create I/O
        (setq chomp--io (make-chomp-io
                         :screen chomp--screen
                         :parser chomp--parser
                         :render chomp--render
                         :buffer buf
                         :chunk-size chomp-chunk-size
                         :min-latency chomp-minimum-latency
                         :max-latency chomp-maximum-latency))
        ;; Start process with shell integration env vars
        (chomp-io-start chomp--io shell buf
                        (chomp-shell-env-vars))
        ;; Initial directory-based name (before any OSC title).
        ;; Leave custom/project buffer names alone until OSC updates.
        (when (and chomp-buffer-name-function
                   (or (equal (buffer-name) "*chomp*")
                       (string-match-p "\\`\\*chomp\\*<[0-9]+>\\'"
                                       (buffer-name))))
          (chomp--rename-managed
           (funcall chomp-buffer-name-function nil)))
        ;; Set default input mode
        (pcase chomp-default-input-mode
          ('char (chomp-char-mode))
          ('emacs (chomp-emacs-mode))
          (_ (chomp-semi-char-mode)))))
    ;; Display buffer
    (pop-to-buffer-same-window buf)
    ;; Resize to match actual window
    (when chomp--io
      (let ((win (get-buffer-window buf)))
        (when win
          (chomp-io-handle-resize
           chomp--io
           (window-max-chars-per-line win)
           (window-body-height win)))))
    buf))

;;;###autoload
(defun chomp-other-window (&optional program)
  "Start a terminal in another window."
  (interactive)
  (let ((buf (chomp program)))
    (when buf
      (switch-to-buffer-other-window buf))))

;;;###autoload
(defun chomp-project ()
  "Switch to or start a terminal in the current project root."
  (interactive)
  (let* ((project (project-current))
         (root (and project (file-truename (project-root project))))
         (existing (and root (chomp--find-session root))))
    (if existing
        (pop-to-buffer-same-window existing)
      (let ((default-directory (or root default-directory))
            (chomp-buffer-name (if project
                                   (project-prefixed-buffer-name "chomp")
                                 chomp-buffer-name)))
        (let ((buffer (chomp)))
          (when root
            (with-current-buffer buffer
              (setq chomp--session-id root)))
          buffer)))))

;;;###autoload
(defun chomp-project-other-window ()
  "Start a terminal in the current project root in another window."
  (interactive)
  (let ((default-directory
         (or (when-let ((proj (project-current)))
               (project-root proj))
             default-directory)))
    (chomp-other-window)))

;;;; ---- Bookmarks ------------------------------------------------------

(defun chomp-bookmark-make-record ()
  "Return a bookmark record for the current local terminal session."
  (when (file-remote-p default-directory)
    (user-error "Remote Chomp bookmarks are unsupported"))
  (cons (buffer-name)
        `((handler . chomp-bookmark-jump)
          (chomp-directory . ,default-directory)
          (chomp-display-name . ,(buffer-name))
          (chomp-session-id . ,chomp--session-id))))

(defun chomp--bookmark-property (record property)
  "Return custom PROPERTY from bookmark RECORD."
  (alist-get property (if (stringp (car-safe record)) (cdr record) record)))

;;;###autoload
(defun chomp-bookmark-jump (record)
  "Jump to the terminal session described by bookmark RECORD."
  (let* ((directory (chomp--bookmark-property record 'chomp-directory))
         (display-name (chomp--bookmark-property record 'chomp-display-name))
         (identity (chomp--bookmark-property record 'chomp-session-id)))
    (when (or (null directory) (file-remote-p directory))
      (user-error "Remote Chomp bookmarks are unsupported"))
    (let ((buffer (chomp--find-session identity)))
      (if buffer
          (with-current-buffer buffer
            (unless (file-equal-p default-directory directory)
              (chomp-send-string
               (concat "cd "
                       (shell-quote-argument (directory-file-name directory))))
              (chomp-send-key "return")))
        (let ((default-directory directory)
              (chomp-buffer-name (or display-name chomp-buffer-name)))
          (setq buffer (chomp))
          (with-current-buffer buffer
            (setq chomp--session-id identity))))
      (pop-to-buffer-same-window buffer)
      buffer)))

;;;; ---- Buffer naming --------------------------------------------------

(defun chomp--local-host-p (host)
  "Return non-nil if HOST is this machine or empty/localhost."
  (or (null host)
      (string-empty-p host)
      (member (downcase host) '("localhost" "127.0.0.1" "::1"))
      (eq t (compare-strings host nil nil (system-name) nil nil t))
      (eq t (compare-strings host nil nil
                             (car (split-string (system-name) "\\."))
                             nil nil t))))

(defun chomp--format-title-for-buffer (title)
  "Return TITLE with local user@host: stripped; keep remote host."
  (when (and title (not (string-empty-p title)))
    (let* ((trimmed (if (> (length title) 60)
                        (concat (substring title 0 57) "...")
                      title))
           (local-user (user-login-name))
           (hosts (delq nil
                        (list (system-name)
                              (car (split-string (system-name) "\\."))))))
      (or (cl-loop for host in hosts
                   for re = (concat "\\`"
                                    (regexp-quote local-user)
                                    "@"
                                    (regexp-quote host)
                                    ":")
                   when (string-match re trimmed)
                   return (substring trimmed (match-end 0)))
          trimmed))))

(defun chomp-buffer-name-by-title (title)
  "Return \"*chomp: TITLE*\", stripping local user@host from TITLE."
  (when-let ((pretty (chomp--format-title-for-buffer title)))
    (format "*chomp: %s*" pretty)))

(defun chomp-buffer-name-by-directory (&optional _title)
  "Return \"*chomp: DIR*\" from abbreviated `default-directory'.
Local paths omit the hostname; remote TRAMP paths keep the host."
  (format "*chomp: %s*"
          (abbreviate-file-name
           (directory-file-name default-directory))))

(defun chomp--rename-managed (new-name)
  "Rename buffer to NEW-NAME unless the user renamed it manually."
  (when (and new-name
             (or (null chomp--managed-buffer-name)
                 (equal (buffer-name) chomp--managed-buffer-name))
             (not (equal new-name (buffer-name))))
    (rename-buffer new-name t)
    (setq chomp--managed-buffer-name (buffer-name))))

(defun chomp--set-title (title)
  "Record OSC TITLE and rename via `chomp-buffer-name-function'."
  (setq chomp--title title)
  (when chomp-buffer-name-function
    (chomp--rename-managed
     (funcall chomp-buffer-name-function title))))

;;;; ---- Mode Line ------------------------------------------------------

(defun chomp--mode-line-input-mode ()
  "Return mode-line string for current input mode and terminal progress."
  (string-join
   (delq nil
         (list (pcase chomp--input-mode
                 ('char "[Char]")
                 ('semi-char "[Semi]")
                 ('emacs "[Emacs]")
                 (_ nil))
               (and chomp--password-mode-p
                    (propertize "🔒Password" 'face 'warning))
               chomp--progress))
   " "))

;; Add to mode-line
(put 'chomp--input-mode 'risky-local-variable t)

(require 'chomp-eshell)

(provide 'chomp)
;;; chomp.el ends here
