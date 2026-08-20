;;; ebb.el --- Terminal emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: terminals, processes
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ebb is a terminal emulator for Emacs.  It uses a separated screen
;; model (pure data structure) and async I/O to ensure the main thread
;; never hangs.
;;
;; Usage:  M-x ebb

;;; Code:

(require 'cl-lib)
(require 'comint)
(require 'project)
(require 'bookmark)
(require 'face-remap)
(require 'browse-url)
(require 'ebb-term)
(require 'ebb-parse)
(require 'ebb-render)
(require 'ebb-input)
(require 'ebb-io)
(require 'ebb-shell)

(declare-function notifications-notify "notifications")
(defvar xterm-store-paste-on-kill-ring)

;;;; ---- Customization --------------------------------------------------

(defcustom ebb-buffer-name "*ebb*"
  "Default buffer name for ebb terminals."
  :type 'string
  :group 'ebb)

(defcustom ebb-default-shell nil
  "Shell to run.  nil means use `$SHELL' or `shell-file-name'.
For best shell integration, set this to your interactive shell
\(e.g., \"/usr/bin/fish\") if it differs from `$SHELL'."
  :type '(choice (const nil) string)
  :group 'ebb)

(defcustom ebb-tramp-shells
  '(("ssh" login-shell)
    ("sshx" login-shell)
    ("scp" login-shell)
    ("rpc" login-shell)
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
  :group 'ebb)

(defcustom ebb-tramp-default-method nil
  "TRAMP method for remote paths built from OSC 7 directory reports.
Used when the shell reports a non-local hostname and the buffer's
`default-directory' has no existing remote prefix.  nil means use
`tramp-default-method'."
  :type '(choice (const :tag "Use tramp-default-method" nil) string)
  :group 'ebb)

(defvar tramp-default-method)

(defcustom ebb-kill-buffer-on-exit t
  "If non-nil, kill the buffer when the shell process exits.
With the default of t, Ctrl-D / shell exit closes the ebb buffer."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-detect-password-prompts t
  "If non-nil, open `read-passwd' when the cursor row looks like a password prompt."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-password-prompt-regex comint-password-prompt-regexp
  "Regex matched against the cursor row to detect a password prompt."
  :type 'regexp
  :group 'ebb)

(defcustom ebb-password-prompt-debounce 0.2
  "Seconds to wait after detecting a password prompt before opening `read-passwd'."
  :type 'number
  :group 'ebb)

(defcustom ebb-password-prompt-functions
  '(ebb--default-password-source)
  "Sources tried in order when a password is needed.
Each function receives the cursor-row text (or nil) and should return a
password string or nil to try the next source."
  :type 'hook
  :group 'ebb)

(defcustom ebb-query-before-kill 'auto
  "Whether to query before killing a buffer with a running process.
`auto' queries only if the process is running.  t always queries."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'ebb)

(defcustom ebb-show-title t
  "If non-nil, track OSC titles for buffer naming when using title mode."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-buffer-name-function #'ebb-buffer-name-by-directory
  "Function that maps OSC title + `default-directory' to a buffer name.
Called with the latest OSC title string (may be nil).  Return a name, or
nil to leave the buffer name alone.  Set to nil to disable auto-rename.

Defaults to directory naming like ghostel users typically want: local
buffers show an abbreviated path without the hostname; remote paths keep
the TRAMP host.  The `ebb-buffer-name-by-title' choice names buffers
from OSC titles via `ebb-buffer-name-title-prefix'."
  :type '(choice (const :tag "Disabled" nil)
                 (function-item :tag "By directory"
                                ebb-buffer-name-by-directory)
                 (function-item :tag "By title"
                                ebb-buffer-name-by-title)
                 function)
  :group 'ebb)

(defcustom ebb-buffer-name-title-prefix "ebb: "
  "Label prepended to OSC titles by `ebb-buffer-name-by-title'.
The buffer is named \"*PREFIX TITLE*\"; e.g. \"ssh: \" groups remote
sessions as \"*ssh: user@host*\" for easy filtering in `ibuffer'."
  :type 'string
  :group 'ebb)

(defcustom ebb-default-input-mode 'semi-char
  "Default input mode when opening a terminal.
Options: `char', `semi-char', `emacs'."
  :type '(choice (const char) (const semi-char) (const emacs))
  :group 'ebb)

(defcustom ebb-notification-function #'ebb--default-notification
  "Function called asynchronously with notification TITLE and BODY."
  :type '(choice (const nil) function)
  :group 'ebb)

(defcustom ebb-progress-function #'ebb--default-progress
  "Function called with normalized progress STATE and PERCENT."
  :type '(choice (const nil) function)
  :group 'ebb)

(defcustom ebb-enable-url-detection t
  "Automatically make plain HTTP and HTTPS URLs clickable."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-enable-file-detection t
  "Automatically make existing file paths and file:line references clickable."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-file-detection-path-regex
  "[~[:alnum:]_.-]*/[^] \t\n\r:\"<>(){}[`']+"
  "Regexp matching paths considered for plain-text link detection."
  :type 'regexp
  :group 'ebb)

;;;; ---- Buffer-local Variables -----------------------------------------

(defvar-local ebb--io nil "The ebb-io instance for this buffer.")
(defvar-local ebb--screen nil "The ebb-screen for this buffer.")
(defvar-local ebb--parser nil "The ebb-parser for this buffer.")
(defvar-local ebb--render nil "The ebb-render-state for this buffer.")
(defvar-local ebb--input-mode nil "Current input mode symbol.")
(defvar-local ebb--session-id nil "Stable identity of this terminal session.")
(defvar-local ebb--progress nil "Current terminal progress mode-line string.")
(defvar-local ebb--mouse-drag-transient-map-exit nil
  "Function that exits the active mouse drag transient map.")

(defvar-local ebb--password-mode-p nil
  "Non-nil while a password prompt is active.")
(defvar-local ebb--password-handled-y nil
  "Display row of the last handled password prompt, or nil.")
(defvar-local ebb--password-confirm-timer nil
  "Pending debounce timer for password prompt detection.")
(defvar-local ebb--password-prompt-active nil
  "Non-nil while the password source chain is running.")
(defvar-local ebb--title nil
  "Last OSC title string reported by the shell.")
(defvar-local ebb--managed-buffer-name nil
  "Last buffer name set by ebb auto-rename, or nil if unmanaged.")

(defvar ebb--focus-change-installed nil
  "Non-nil when `ebb--focus-change' is installed globally.")

;;;; ---- Minor Modes for Input ------------------------------------------

(define-minor-mode ebb--semi-char-mode
  "Minor mode for semi-char input."
  :lighter " Semi"
  :keymap ebb-semi-char-mode-map)

(define-minor-mode ebb--char-mode
  "Minor mode for char input."
  :lighter " Char"
  :keymap ebb-char-mode-map)

(define-minor-mode ebb--mouse-mode
  "Minor mode for forwarding DEC mouse events to the terminal."
  :lighter nil
  :keymap ebb-mouse-mode-map)

;;;; ---- Input Mode Switching -------------------------------------------

(defun ebb--refresh-input-cursor ()
  "Update native and terminal cursor visibility after an input-mode change."
  (when ebb--render
    (ebb-render--update-cursor ebb--render)))

(defun ebb--switch-input-mode (mode)
  "Enable the minor modes for input MODE and record it."
  (ebb--semi-char-mode (if (eq mode 'semi-char) 1 -1))
  (ebb--char-mode (if (eq mode 'char) 1 -1))
  (setq ebb--input-mode mode)
  (ebb--refresh-input-cursor)
  (force-mode-line-update))

(defun ebb-semi-char-mode ()
  "Switch to semi-char input mode."
  (interactive)
  (ebb--switch-input-mode 'semi-char))

(defun ebb-char-mode ()
  "Switch to char input mode (all keys to terminal)."
  (interactive)
  (ebb--switch-input-mode 'char))

(defun ebb-emacs-mode ()
  "Switch to Emacs input mode (normal Emacs keys)."
  (interactive)
  (ebb--switch-input-mode 'emacs))

;;;; ---- Self-Input Command ---------------------------------------------

(defun ebb--require-running-terminal ()
  "Return the current terminal I/O state or signal `user-error'."
  (unless (or (eq major-mode 'ebb-mode)
              (bound-and-true-p ebb-eshell--io))
    (user-error "Not in a Ebb buffer"))
  (let ((process (and ebb--io (ebb-io-process ebb--io))))
    (unless (and process (process-live-p process))
      (user-error "Process not running")))
  ebb--io)

;;;###autoload
(defun ebb-send-string (string)
  "Send STRING unchanged to the current terminal."
  (interactive "sSend string: ")
  (ebb-io-send (ebb--require-running-terminal) string))

(defun ebb--key-event (key modifiers)
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
(defun ebb-send-key (key &optional modifiers)
  "Send named KEY with comma-separated MODIFIERS to the current terminal."
  (interactive (list (read-string "Key: ")
                     (read-string "Modifiers (comma-separated): ")))
  (let* ((io (ebb--require-running-terminal))
         (sequence (ebb-input-translate
                    (ebb--key-event key modifiers) ebb--screen)))
    (unless sequence
      (user-error "Unsupported key: %s" key))
    (ebb-io-send io sequence)))

;;;###autoload
(defun ebb-paste-string (string)
  "Paste STRING, using bracketed paste when terminal mode 2004 is active."
  (interactive "sPaste string: ")
  (let ((io (ebb--require-running-terminal)))
    (ebb-io-send
     io
     (if (and ebb--screen (ebb-screen-bracketed-paste ebb--screen))
         (concat (ebb-input-bracketed-paste-start) string
                 (ebb-input-bracketed-paste-end))
       string))))

(defun ebb-self-input (n &optional e)
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
  (when ebb--io
    (let ((seq (ebb-input-translate (or e last-command-event) ebb--screen)))
      (when seq
        (dotimes (_ (or n 1))
          (ebb-io-send ebb--io seq))))))

(defun ebb-quoted-input ()
  "Read the next key event literally and send it to the terminal."
  (interactive)
  (when ebb--io
    (let* ((key (read-event "Send key: "))
           (seq (ebb-input-translate key ebb--screen)))
      (when seq
        (ebb-io-send ebb--io seq)))))

(defun ebb-yank (&optional rotate)
  "Yank (paste) from the kill ring into the terminal.
With ROTATE non-nil, replace the last yank with the next kill ring
entry.  If bracketed paste mode is active, wraps in bracketed paste
sequences."
  (interactive)
  (when ebb--io
    (when rotate (current-kill 1))
    (let ((text (current-kill 0)))
      (when text
        (ebb-paste-string text)))))

(defun ebb-yank-pop ()
  "Yank-pop: replace the last yank with the next kill ring entry."
  (interactive)
  (ebb-yank 'rotate))

(defun ebb-xterm-paste (event)
  "Forward an xterm-paste EVENT to the terminal process.
The normal `xterm-paste' command inserts into the renderer-owned buffer,
so the child process never receives the pasted text."
  (interactive "e")
  (unless (eq (car-safe event) 'xterm-paste)
    (error "This command must be bound to an xterm-paste event"))
  (when-let* ((text (nth 1 event)))
    (when (bound-and-true-p xterm-store-paste-on-kill-ring)
      ;; This is incoming clipboard data, not a new copy operation.  Avoid
      ;; sending it back through `interprogram-cut-function'.
      (let ((interprogram-cut-function nil))
        (kill-new text)))
    (ebb-paste-string text)))

(defun ebb-send-password (&optional password)
  "Read PASSWORD from the minibuffer and send it to the terminal.
Interactively, this uses `read-passwd' so password keystrokes do not go through
normal terminal input handling or appear in `view-lossage'."
  (interactive)
  (unless ebb--io
    (user-error "Process not running"))
  (let ((password (or password (read-passwd "Password: "))))
    (when password
      (ebb-io-send ebb--io password)
      (ebb-io-send ebb--io "\r"))))

;;;; ---- Password prompt detection -------------------------------------

(defun ebb--cursor-row-text ()
  "Return trimmed text of the terminal cursor row, or nil."
  (when ebb--screen
    (let* ((y (ebb-screen-cursor-y ebb--screen))
           (line (ebb-screen-get-line ebb--screen y))
           (width (ebb-screen-width ebb--screen))
           (text
            (or (ebb-line-text line)
                (and (ebb-line-cells line)
                     (let ((cells (ebb-line-cells line)))
                       ;; `aset' cannot populate a string with non-ASCII
                       ;; character codes.  Build the row through `string'
                       ;; so cell-backed Unicode remains valid text.
                       (apply #'string
                              (cl-loop for i below (min width (length cells))
                                       collect (ebb-cell-char
                                                (aref cells i)))))))))
      (when text
        (let ((row (string-trim-right text)))
          (and (not (string-empty-p row)) row))))))

(defun ebb--password-prompt-detected-p ()
  "Return non-nil if the cursor row matches `ebb-password-prompt-regex'."
  (when-let* ((row (ebb--cursor-row-text))
              (case-fold-search t))
    (string-match-p ebb-password-prompt-regex row)))

(defun ebb--default-password-source (row)
  "Prompt with `read-passwd', labeling with ROW when available."
  (read-passwd (concat (or row "Password:") " ")))

(defun ebb--cancel-password-confirm-timer ()
  "Cancel a pending password-prompt debounce timer."
  (when ebb--password-confirm-timer
    (cancel-timer ebb--password-confirm-timer)
    (setq ebb--password-confirm-timer nil)))

(defun ebb--confirm-and-prompt (buf)
  "Re-check password detection in BUF, then open the password minibuffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq ebb--password-confirm-timer nil)
      (when (and ebb--password-mode-p
                 (ebb--password-prompt-detected-p))
        (ebb--prompt-password)))))

(defun ebb--prompt-password ()
  "Run `ebb-password-prompt-functions' and send the result to the PTY."
  (let ((pwd nil)
        (row (ebb--cursor-row-text))
        (y (and ebb--screen (ebb-screen-cursor-y ebb--screen))))
    (setq ebb--password-prompt-active t)
    (unwind-protect
        (setq pwd (run-hook-with-args-until-success
                   'ebb-password-prompt-functions row))
      (setq ebb--password-prompt-active nil)
      (when (and pwd ebb--io)
        ;; Send password and newline separately so `clear-string' wipes
        ;; the only string holding the secret; a concatenation would
        ;; keep an uncleared copy behind.
        (ebb-io-send ebb--io pwd)
        (ebb-io-send ebb--io "\r")
        (clear-string pwd))
      (setq ebb--password-handled-y y
            ebb--password-mode-p nil)
      (force-mode-line-update))))

(defun ebb--detect-password-prompt (&optional _render)
  "Watch the cursor row and open `read-passwd' on a password prompt.
Called after each render.  Debounced so short-lived matches don't flash."
  (when (and ebb-detect-password-prompts ebb--screen ebb--io
             (not (eq ebb--input-mode 'emacs))
             (not ebb--password-prompt-active))
    (let ((now (ebb--password-prompt-detected-p))
          (y (ebb-screen-cursor-y ebb--screen)))
      (cond
       ((not now)
        (ebb--cancel-password-confirm-timer)
        (when (or ebb--password-mode-p ebb--password-handled-y)
          (setq ebb--password-mode-p nil
                ebb--password-handled-y nil)
          (force-mode-line-update)))
       (ebb--password-mode-p nil)
       ((and ebb--password-handled-y
             (= y ebb--password-handled-y))
        nil)
       (t
        (setq ebb--password-mode-p t
              ebb--password-handled-y nil)
        (force-mode-line-update)
        (ebb--cancel-password-confirm-timer)
        (setq ebb--password-confirm-timer
              (run-at-time ebb-password-prompt-debounce nil
                           #'ebb--confirm-and-prompt
                           (current-buffer))))))))

(defun ebb-mouse-input (event)
  "Send mouse EVENT to the terminal when DEC mouse tracking is active."
  (interactive "e")
  (when-let* ((win (posn-window (event-start event))))
    (when (windowp win)
      (select-window win)))
  (when (and ebb--io ebb--screen ebb--render
             (ebb-screen-mouse-mode ebb--screen))
    (when (and (memq 'down (event-modifiers event))
               (not ebb--mouse-drag-transient-map-exit))
      (let ((old-track-mouse track-mouse)
            (buffer (current-buffer)))
        (setq track-mouse 'dragging)
        (setq ebb--mouse-drag-transient-map-exit
              (set-transient-map
               ebb-mouse-mode-map
               #'always
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq track-mouse old-track-mouse)
                     (setq ebb--mouse-drag-transient-map-exit nil))))))))
    (when-let* ((seq (ebb-input-encode-mouse
                     event ebb--screen
                     (marker-position
                      (ebb-render-state-display-begin ebb--render)))))
      (ebb-io-send ebb--io seq))
    (when (and ebb--mouse-drag-transient-map-exit
               (ebb-input--mouse-release-p event))
      (funcall ebb--mouse-drag-transient-map-exit)
      (setq ebb--mouse-drag-transient-map-exit nil))))

;;;; ---- Display Commands -----------------------------------------------

(defun ebb--scroll-rows (arg)
  "Return the row count represented by scroll command ARG."
  (if arg
      (prefix-numeric-value arg)
    (max 1 (- (window-body-height) next-screen-context-lines))))

(defun ebb-scroll-up (&optional arg)
  "Scroll forward through virtual terminal history by ARG rows."
  (interactive "P")
  (if ebb--render
      (ebb-render-scroll-history ebb--render (ebb--scroll-rows arg))
    (scroll-up-command arg)))

(defun ebb-scroll-down (&optional arg)
  "Scroll backward through virtual terminal history by ARG rows."
  (interactive "P")
  (if ebb--render
      (ebb-render-scroll-history ebb--render (- (ebb--scroll-rows arg)))
    (scroll-down-command arg)))

(defun ebb--wheel-rows (event)
  "Return the number of history rows wheel EVENT scrolls.
Follows `mouse-wheel-scroll-amount' semantics: an integer scrolls that
many rows, a float that fraction of the window, and nil a near-full
page.  Click-count acceleration applies only with
`mouse-wheel-progressive-speed'."
  (let* ((amount (and (boundp 'mouse-wheel-scroll-amount)
                      (car mouse-wheel-scroll-amount)))
         (base (cond
                ((integerp amount) amount)
                ((floatp amount)
                 (max 1 (round (* amount (window-body-height)))))
                ((null amount)
                 (max 1 (- (window-body-height)
                           next-screen-context-lines)))
                (t 5)))
         (lines (max 1 (or (ignore-errors (event-line-count event)) 1)))
         (clicks (if (bound-and-true-p mouse-wheel-progressive-speed)
                     (max 1 (or (ignore-errors (event-click-count event)) 1))
                   1)))
    (max 1 (* base lines clicks))))

(defun ebb-mouse-scroll-up (event)
  "Scroll the Ebb window under wheel EVENT forward through history.
Like `mwheel-scroll', act on the window the mouse is over, not the
selected window."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (with-selected-window window
        (ebb-scroll-up (ebb--wheel-rows event))))))

(defun ebb-mouse-scroll-down (event)
  "Scroll the Ebb window under wheel EVENT backward through history.
Like `mwheel-scroll', act on the window the mouse is over, not the
selected window."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (when (window-live-p window)
      (with-selected-window window
        (ebb-scroll-down (ebb--wheel-rows event))))))

(defun ebb--clear-screen (scrollback)
  "Clear the viewport, and history too when SCROLLBACK is non-nil."
  (ebb--require-running-terminal)
  (ebb-screen-cursor-goto ebb--screen 0 0)
  (ebb-screen-erase-in-display ebb--screen 2)
  (when scrollback
    (ebb-screen-erase-in-display ebb--screen 3))
  (ebb-shell-cleanup)
  (ebb-shell-setup-margins)
  (when ebb--render
    (ebb-render-refresh ebb--render))
  (ebb-send-key "l" "control"))

;;;###autoload
(defun ebb-clear ()
  "Clear the viewport, retaining scrollback, and request a prompt redraw."
  (interactive)
  (ebb--clear-screen nil))

;;;###autoload
(defun ebb-clear-scrollback ()
  "Clear the viewport and scrollback, then request a prompt redraw."
  (interactive)
  (ebb--clear-screen t))

;;;###autoload
(defun ebb-copy-all ()
  "Copy all terminal scrollback and viewport text to the kill ring."
  (interactive)
  (unless (and (eq major-mode 'ebb-mode) ebb--screen)
    (user-error "Not in a Ebb buffer"))
  (let ((text (ebb-screen-plain-text ebb--screen)))
    (kill-new text)
    text))

(defun ebb-copy-region (_begin _end)
  "Copy the active virtual terminal region to the kill ring."
  (interactive "r")
  (unless (and ebb--screen ebb--render mark-active (mark t))
    (user-error "No active terminal region"))
  (let* ((point-location (ebb-render-buffer-location ebb--render (point)))
         (saved-mark (ebb-render-state-virtual-mark ebb--render))
         (mark-location
          (if (and saved-mark
                   (= (mark t)
                      (marker-position
                       (ebb-render-state-region-begin ebb--render))))
              saved-mark
            (ebb-render-buffer-location ebb--render (mark t))))
         (text (ebb-screen-text-range
                ebb--screen point-location mark-location)))
    (kill-new text)
    (setq deactivate-mark t)
    text))

;;;; ---- Process Management Commands ------------------------------------

(defun ebb-kill-process ()
  "Kill the terminal process."
  (interactive)
  (when-let* ((proc (and ebb--io (ebb-io-process ebb--io))))
    (when (process-live-p proc)
      (kill-process proc))))

(defun ebb-reset ()
  "Reset the terminal to its initial state."
  (interactive)
  (when ebb--screen
    (ebb-screen-reset ebb--screen)
    (when ebb--render
      (ebb-render-full-reset ebb--render))))

(defun ebb-previous-prompt (&optional n)
  "Enter Emacs mode and move to the Nth previous shell prompt."
  (interactive "p")
  (unless (eq ebb--input-mode 'emacs)
    (ebb-emacs-mode))
  (ebb-shell-previous-prompt n))

(defun ebb-next-prompt (&optional n)
  "Enter Emacs mode and move to the Nth next shell prompt."
  (interactive "p")
  (unless (eq ebb--input-mode 'emacs)
    (ebb-emacs-mode))
  (ebb-shell-next-prompt n))

;;;; ---- Notifications and Progress -------------------------------------

(defun ebb--default-notification (title body)
  "Display a desktop notification with TITLE and BODY."
  (require 'notifications)
  (notifications-notify :title (or title "Terminal") :body body))

(defun ebb--default-progress (state percent)
  "Display normalized progress STATE and PERCENT in the mode line."
  (setq ebb--progress
        (pcase state
          ('remove nil)
          ('set (format "[%d%%]" percent))
          ('error (format "[Error %d%%]" percent))
          ('indeterminate "[…]")
          ('pause (format "[Paused %d%%]" percent))))
  (force-mode-line-update))

(defun ebb--run-callback (buffer function args)
  "Run FUNCTION with ARGS in BUFFER, isolating callback errors."
  (when (and (buffer-live-p buffer) function)
    (with-current-buffer buffer
      (condition-case error-data
          (apply function args)
        (error
         (message "[ebb] Callback error: %S" error-data)
         nil)))))

(defun ebb--defer-callback (function &rest args)
  "Run FUNCTION later in the current buffer with ARGS."
  (run-at-time 0 nil #'ebb--run-callback
               (current-buffer) function args))

;;;; ---- Event Handler --------------------------------------------------

(defun ebb--sync-model-size ()
  "Synchronize the PTY and renderer with `ebb--screen'."
  (when-let* ((proc (and ebb--io (ebb-io-process ebb--io))))
    (when (and (process-live-p proc)
               (ebb-io--pty-process-p proc))
      (set-process-window-size
       proc
       (ebb-screen-height ebb--screen)
       (ebb-screen-width ebb--screen))))
  (when (and ebb--io (ebb-io-render ebb--io))
    (ebb-render-full-reset (ebb-io-render ebb--io))))

(defun ebb--handle-event (type &rest args)
  "Handle events emitted by the parser."
  (pcase type
    ('bell (ding t))
    ('title
     (when ebb-show-title
       (ebb--set-title (car args))))
    ('cwd
     (when ebb-enable-directory-tracking
       (ebb--set-shell-cwd
        (ebb--cwd-to-path (car args) (cadr args)))))
    ('cursor-style
     ;; Could update cursor display here
     nil)
    ('mode-set
     (let ((mode (car args)))
       ;; Handle modes that need buffer-level action
       (pcase mode
         ((or 1000 1002 1003)
          (ebb--mouse-mode (if (ebb-screen-mouse-mode ebb--screen) 1 -1))
          (unless (ebb-screen-mouse-mode ebb--screen)
            (setf (ebb-screen-mouse-pressed ebb--screen) nil)
            (when ebb--mouse-drag-transient-map-exit
              (funcall ebb--mouse-drag-transient-map-exit)
              (setq ebb--mouse-drag-transient-map-exit nil))))
         (3
          ;; DECCOLM changes the model width independently of the Emacs
          ;; window.  Keep the PTY and renderer synchronized with that grid.
          (ebb--sync-model-size))
         (1004
          ;; Focus events -- could enable focus tracking here
          nil)
         (_ nil))))
    ('reset
     (ebb--sync-model-size))
    ('resize-request
     (ebb--sync-model-size))
    ('process-exit
     (let ((event (car args)))
       (when (buffer-live-p (current-buffer))
         (with-current-buffer (current-buffer)
           (let ((inhibit-read-only t))
             (goto-char (point-max))
             (insert (format "\n\n[Process %s]\n"
                             (string-trim event))))
           ;; Switch to emacs mode
           (ebb-emacs-mode)
           (when ebb-kill-buffer-on-exit
             (run-at-time 1 nil
                          (lambda (buf)
                            (when (buffer-live-p buf)
                              (kill-buffer buf)))
                          (current-buffer)))))))
    ('osc-51
     (let ((payload (car args)))
       (ebb-shell-handle-osc51 payload ebb--screen)))
    ('notification
     (when ebb-notification-function
       (apply #'ebb--defer-callback ebb-notification-function args)))
    ('progress
     (ebb--run-callback (current-buffer) ebb-progress-function args))
    (_ nil)))

;;;; ---- Focus Events ---------------------------------------------------

(defun ebb--focus-change ()
  "Handle focus changes for ebb buffers."
  (dolist (frame (frame-list))
    (let ((focused (frame-focus-state frame)))
      (dolist (win (window-list frame 'no-minibuf))
        (let ((buf (window-buffer win)))
          (when (buffer-local-value 'ebb--io buf)
            (with-current-buffer buf
              (when (and ebb--io ebb--screen
                         (ebb-screen-focus-events ebb--screen))
                (ebb-io-send ebb--io
                               (if focused
                                   (ebb-input-focus-in)
                                 (ebb-input-focus-out)))))))))))

(defun ebb--ensure-focus-change-hook ()
  "Install global focus tracking once."
  (unless ebb--focus-change-installed
    (add-function :after after-focus-change-function #'ebb--focus-change)
    (setq ebb--focus-change-installed t)))

(defun ebb--maybe-remove-focus-change-hook ()
  "Remove global focus tracking when no other ebb buffers remain."
  (when (and ebb--focus-change-installed
             (not (cl-some (lambda (buf)
                             (and (not (eq buf (current-buffer)))
                                  (buffer-live-p buf)
                                  (or (eq (buffer-local-value 'major-mode buf)
                                          'ebb-mode)
                                      ;; Inline Eshell terminals keep the
                                      ;; model in ordinary Eshell buffers.
                                      (buffer-local-value 'ebb--io buf))))
                           (buffer-list))))
    (remove-function after-focus-change-function #'ebb--focus-change)
    (setq ebb--focus-change-installed nil)))

;;;; ---- Resize Hook ----------------------------------------------------

(defun ebb--window-size-change (window)
  "Handle a size change for WINDOW showing a Ebb buffer."
  (let ((buffer (window-buffer window)))
    (when (buffer-local-value 'ebb--io buffer)
      (with-current-buffer buffer
        (let* ((process (ebb-io-process ebb--io))
               (windows (get-buffer-window-list buffer nil t))
               (size (and process windows
                          (funcall window-adjust-process-window-size-function
                                   process windows)))
               (new-width (or (car-safe size)
                              (window-max-chars-per-line window)))
               (new-height (or (cdr-safe size)
                               (window-body-height window))))
          (when (and (> new-width 0) (> new-height 0))
            (ebb-io-handle-resize ebb--io new-width new-height)))))))

;;;; ---- Major Mode -----------------------------------------------------

(defvar-keymap ebb-link-map
  :doc "Keymap for OSC 8 and automatically detected links."
  "<mouse-1>" #'ebb-open-link-at-click
  "<mouse-2>" #'ebb-open-link-at-click)

(defun ebb--link-at (position)
  "Return the link URI at POSITION, or nil."
  (let ((uri (get-text-property position 'help-echo)))
    (and (stringp uri) uri)))

(defun ebb--open-link (uri)
  "Open URI, including Ebb's internal fileref links."
  (when (stringp uri)
    (cond
     ((string-match
       "\\`fileref:\\(.*?\\)\\(?::\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\)?\\'"
       uri)
      (let ((file (match-string 1 uri))
            (line (and (match-string 2 uri)
                       (string-to-number (match-string 2 uri))))
            (column (and (match-string 3 uri)
                         (string-to-number (match-string 3 uri)))))
        (when (file-exists-p file)
          (find-file-other-window file)
          (when line
            (goto-char (point-min))
            (forward-line (1- (max 1 line)))
            (when column (move-to-column (1- (max 1 column))))))))
     ((string-match "\\`file://\\(?:localhost\\)?\\(/.*\\)" uri)
      (find-file (url-unhex-string (match-string 1 uri))))
     (t (browse-url uri)))))

(defun ebb-open-link-at-click (event)
  "Open the hyperlink at mouse EVENT."
  (interactive "e")
  (ebb--open-link (ebb--link-at (posn-point (event-start event)))))

(defun ebb-open-link-at-point ()
  "Open the hyperlink at point."
  (interactive)
  (ebb--open-link (ebb--link-at (point))))

(defun ebb--linkify (begin end uri)
  "Make BEGIN through END a clickable URI."
  (add-text-properties begin end
                       `(help-echo ,uri mouse-face highlight
                         keymap ,ebb-link-map)))

(defun ebb--detect-plain-links (_render)
  "Detect plain URLs and file references after a terminal refresh."
  (let* ((inhibit-read-only t)
         (inhibit-modification-hooks t)
         (cursor (and ebb--render ebb--screen
                      (save-excursion
                        (goto-char (ebb-render-state-display-begin ebb--render))
                        (forward-line (ebb-screen-cursor-y ebb--screen))
                        (cons (line-beginning-position) (line-end-position))))))
    (save-excursion
      (when ebb-enable-url-detection
        (goto-char (point-min))
        (while (re-search-forward
                "https?://[^ \t\n\r\"<>]*[^ \t\n\r\"<>.,;:!?)>]"
                nil t)
          (let ((begin (match-beginning 0)) (end (match-end 0)))
            (unless (or (get-text-property begin 'help-echo)
                        (and cursor (<= (car cursor) begin (cdr cursor))))
              (ebb--linkify begin end (match-string-no-properties 0))))))
      (when (and ebb-enable-file-detection
                 (not (file-remote-p default-directory)))
        (goto-char (point-min))
        (let ((regexp (concat "\\(?:^\\|[^[:alnum:]_./~-]\\)\\("
                              ebb-file-detection-path-regex
                              "\\)\\(\\(?::[0-9]+\\(?::[0-9]+\\)?\\)?\\)"))
              (seen (make-hash-table :test #'equal)))
          (while (re-search-forward regexp nil t)
            (let ((begin (match-beginning 1)) (end (match-end 2)))
              (unless (or (get-text-property begin 'help-echo)
                          (and cursor (<= (car cursor) begin (cdr cursor))))
                (let* ((path (match-string-no-properties 1))
                       (location (match-string-no-properties 2))
                       (file (expand-file-name path))
                       (cached (gethash file seen 'unknown))
                       (exists (if (eq cached 'unknown)
                                   (puthash file (file-exists-p file) seen)
                                 cached)))
                  (when exists
                    (ebb--linkify begin end
                                  (concat "fileref:" file location))))))))))))

(defun ebb--find-hyperlink (direction position)
  "Find a hyperlink from POSITION in DIRECTION."
  (let ((search (if (eq direction 'next)
                    #'text-property-search-forward
                  #'text-property-search-backward))
        (skip-id (get-text-property position 'ebb-link-id)))
    (save-excursion
      (goto-char position)
      (catch 'found
        (while-let ((match (funcall search 'help-echo nil
                                    (lambda (_ value) value) t)))
          (let ((at (prop-match-beginning match)))
            (unless (and skip-id
                         (equal skip-id (get-text-property at 'ebb-link-id)))
              (throw 'found at))))))))

(defun ebb--goto-hyperlink (direction)
  "Move to a hyperlink in DIRECTION, wrapping at buffer boundaries."
  (let* ((edge (if (eq direction 'next) (point-min) (point-max)))
         (target (or (ebb--find-hyperlink direction (point))
                     (ebb--find-hyperlink direction edge))))
    (if target (goto-char target) (user-error "No hyperlinks in buffer"))))

(defun ebb-next-hyperlink (&optional count)
  "Move to the COUNTth next hyperlink."
  (interactive "p")
  (ebb-emacs-mode)
  (dotimes (_ (or count 1)) (ebb--goto-hyperlink 'next)))

(defun ebb-previous-hyperlink (&optional count)
  "Move to the COUNTth previous hyperlink."
  (interactive "p")
  (ebb-emacs-mode)
  (dotimes (_ (or count 1)) (ebb--goto-hyperlink 'previous)))

(define-derived-mode ebb-mode fundamental-mode "Ebb"
  "Major mode for the ebb terminal emulator."
  (setq-local buffer-read-only t)
  ;; Terminal buffers are generated views.  Recording every streaming render
  ;; update would retain a large undo history for content users cannot edit.
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (setq-local scroll-margin 0)
  (setq-local scroll-conservatively 101)
  (setq-local scroll-step 1)
  (setq-local auto-hscroll-mode nil)
  (setq-local hscroll-margin 0)
  ;; Disable features that conflict with terminal display.
  ;; Ebb supplies its own text properties; deferred fontification makes
  ;; redisplay scan the entire scrollback when revisiting a terminal.
  (font-lock-mode -1)
  (when (fboundp 'emojify-mode)
    (emojify-mode -1))
  (setq-local bidi-paragraph-direction 'left-to-right)
  (setq-local show-trailing-whitespace nil)
  (setq-local display-line-numbers nil)
  (face-remap-add-relative 'default 'ebb-default)
  (setq-local bookmark-make-record-function #'ebb-bookmark-make-record)
  (setq-local imenu-create-index-function #'ebb-shell-imenu-create-index)
  (setq-local imenu-default-goto-function #'ebb-shell-imenu-goto)
  (setq-local mode-line-process
              '(" " (:eval (ebb--mode-line-input-mode))))
  (add-hook 'ebb-render-after-refresh-hook #'ebb--detect-plain-links nil t)
  ;; Set up shell integration margins
  (ebb-shell-setup-margins)
  ;; Buffer-local window-size-change hooks receive a window, not a frame.
  (add-hook 'window-size-change-functions #'ebb--window-size-change nil t)
  ;; Add focus tracking
  (ebb--ensure-focus-change-hook)
  ;; Clean up shell state on kill
  (add-hook 'kill-buffer-hook #'ebb-shell-cleanup nil t)
  ;; Kill the (possibly remote) shell process with the buffer; the
  ;; process has no buffer of its own, so Emacs won't reap it.
  (add-hook 'kill-buffer-hook
            (lambda () (when ebb--io (ebb-io-stop ebb--io)))
            nil t)
  (add-hook 'kill-buffer-hook #'ebb--maybe-remove-focus-change-hook nil t)
  ;; Query before kill
  (when ebb-query-before-kill
    (add-hook 'kill-buffer-query-functions #'ebb--kill-buffer-query nil t)))

(defun ebb--kill-buffer-query ()
  "Query before killing a ebb buffer with a live process."
  (or (not ebb--io)
      (not (ebb-io-process ebb--io))
      (not (process-live-p (ebb-io-process ebb--io)))
      (yes-or-no-p "Terminal process is running.  Kill buffer? ")))

;;;; ---- Entry Points ---------------------------------------------------

(defun ebb--set-shell-cwd (path)
  "Adopt the shell-reported working directory PATH.
Remote reports are trusted; a local path must exist (a synchronous
TRAMP `file-directory-p' would open a connection on every cd).
Also updates `list-buffers-directory' and renames the buffer when
`ebb-buffer-name-function' is set."
  (when (and path (if (file-remote-p path) t (file-directory-p path)))
    (setq default-directory (file-name-as-directory path)
          list-buffers-directory default-directory)
    (when ebb-buffer-name-function
      (ebb--rename-managed
       (funcall ebb-buffer-name-function ebb--title)))))

(defun ebb--buffers ()
  "Return live Ebb buffers sorted by name."
  (sort (seq-filter (lambda (buffer)
                      (eq (buffer-local-value 'major-mode buffer) 'ebb-mode))
                    (buffer-list))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

(defun ebb--find-session (identity)
  "Return the Ebb buffer with stable IDENTITY, if any."
  (seq-find (lambda (buffer)
              (equal identity
                     (buffer-local-value 'ebb--session-id buffer)))
            (ebb--buffers)))

(defun ebb--cycle (step)
  "Switch STEP places through the sorted Ebb buffer list, wrapping."
  (let ((buffers (ebb--buffers)))
    (unless buffers
      (user-error "No Ebb sessions"))
    (let* ((position (cl-position (current-buffer) buffers))
           (target (if position
                       (nth (mod (+ position step) (length buffers)) buffers)
                     (if (> step 0) (car buffers) (car (last buffers))))))
      (pop-to-buffer-same-window target)
      target)))

;;;###autoload
(defun ebb-next ()
  "Switch to the next Ebb session, wrapping at the end."
  (interactive)
  (ebb--cycle 1))

;;;###autoload
(defun ebb-previous ()
  "Switch to the previous Ebb session, wrapping at the beginning."
  (interactive)
  (ebb--cycle -1))

;;;###autoload
(defun ebb-list-buffers ()
  "Choose a Ebb session, defaulting to the next one."
  (interactive)
  (let* ((buffers (ebb--buffers))
         (next (and buffers
                    (nth (mod (1+ (or (cl-position (current-buffer) buffers) -1))
                              (length buffers))
                         buffers))))
    (unless buffers
      (user-error "No Ebb sessions"))
    (pop-to-buffer-same-window
     (get-buffer
      (completing-read "Ebb session: "
                       (mapcar #'buffer-name buffers) nil t nil nil
                       (buffer-name next))))))

;;;###autoload
(defun ebb-other (&optional program)
  "Switch to another Ebb session, or create one with PROGRAM."
  (interactive)
  (if-let* ((other (seq-find (lambda (buffer) (not (eq buffer (current-buffer))))
                            (ebb--buffers))))
      (pop-to-buffer-same-window other)
    (ebb program)))

(defun ebb--cwd-to-path (dir host)
  "Return OSC 7 report DIR as a usable path, given the reporting HOST.
Reports from a non-local HOST become TRAMP paths.  The buffer's remote
prefix is reused when it targets the reported host (preserves method,
user, multi-hop); a different host means the user ssh'd onward from
this buffer's host, so a fresh path is built via
`ebb-tramp-default-method'.  A local-looking HOST (or none) in a
remote buffer is the remote shell reporting on itself."
  (when (and dir (not (string-empty-p dir)))
    (let ((prefix (file-remote-p default-directory)))
      (cond
       ((not (ebb--local-host-p host))
        (if (and prefix
                 (equal (downcase host)
                        (downcase (or (file-remote-p default-directory 'host)
                                      ""))))
            (concat prefix dir)
          (progn
            (require 'tramp)
            (format "/%s:%s:%s"
                    (or ebb-tramp-default-method tramp-default-method)
                    host dir))))
       (prefix (concat prefix dir))
       (t dir)))))

(defun ebb--remote-login-shell ()
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

(defun ebb--remote-shell ()
  "Return the shell argv list to run for a remote `default-directory'.
Resolves per TRAMP method via `ebb-tramp-shells'; falls back to
/bin/sh when nothing resolves."
  (let* ((method (file-remote-p default-directory 'method))
         (spec (cdr (or (assoc method ebb-tramp-shells)
                        (assoc t ebb-tramp-shells))))
         (program (or (if (eq (car spec) 'login-shell)
                          (or (ebb--remote-login-shell) (cadr spec))
                        (car spec))
                      "/bin/sh"))
         (args (or (cddr spec)
                   (and (memq (ebb-io--detect-shell program)
                              '(bash zsh fish))
                        '("-l" "-i")))))
    (cons program args)))

(defun ebb--start (program display-function)
  "Start PROGRAM after DISPLAY-FUNCTION places the terminal buffer.
DISPLAY-FUNCTION receives the new buffer.  Displaying it before constructing
and starting the terminal ensures the screen and PTY use their final window
size from the outset."
  (let* ((shell (or program
                    (and (file-remote-p default-directory)
                         (ebb--remote-shell))
                    ebb-default-shell
                    (getenv "SHELL")
                    shell-file-name
                    "/bin/sh"))
         (buf (generate-new-buffer ebb-buffer-name)))
    (with-current-buffer buf
      (ebb-mode)
      (setq ebb--session-id (buffer-name buf)))
    (funcall display-function buf)
    (with-current-buffer buf
      ;; Construct the terminal only after BUF is in its destination window.
      ;; Starting it earlier causes an immediate resize and SIGWINCH; Readline
      ;; may then redraw the first prompt while its original output is pending.
      (setq ebb--io (ebb-io-create-terminal buf #'ebb--handle-event)
            ebb--screen (ebb-io-screen ebb--io)
            ebb--render (ebb-io-render ebb--io)
            ebb--parser (ebb-io-parser ebb--io))
      (ebb-io-start ebb--io shell buf (ebb-shell-env-vars))
      ;; Initial directory-based name (before any OSC title).
      ;; Leave custom/project buffer names alone until OSC updates.
      (when (and ebb-buffer-name-function
                 (or (equal (buffer-name) "*ebb*")
                     (string-match-p "\\`\\*ebb\\*<[0-9]+>\\'"
                                     (buffer-name))))
        (ebb--rename-managed
         (funcall ebb-buffer-name-function nil)))
      (pcase ebb-default-input-mode
        ('char (ebb-char-mode))
        ('emacs (ebb-emacs-mode))
        (_ (ebb-semi-char-mode))))
    buf))

;;;###autoload
(defun ebb (&optional program)
  "Start a terminal emulator.
With a numeric prefix argument N, switch to the Nth existing ebb
session (creating it if needed).  With \\[universal-argument] \\[universal-argument],
prompt for the program to run.  PROGRAM defaults to `ebb-default-shell'
or `$SHELL'.

When `default-directory' is remote, spawns the remote shell via TRAMP
(see `ebb-tramp-shells').  Shell integration is not deployed to the
remote host: cwd tracking there requires the remote rc files to emit
OSC 7/51 themselves (e.g. by sourcing the scripts in ebb's
`integration/' directory)."
  (interactive
   (list
    (cond
     ;; C-u C-u: prompt for program
     ((equal current-prefix-arg '(16))
      (let ((command
             (read-shell-command "Run program: "
                                 (or ebb-default-shell
                                     (getenv "SHELL")
                                     shell-file-name))))
        (let ((argv (split-string-shell-command command)))
          (when (or (null argv) (string-empty-p (car argv)))
            (user-error "Program cannot be empty"))
          argv)))
     (t nil))))
  ;; With numeric prefix: switch to Nth ebb buffer
  (let ((existing (and (numberp current-prefix-arg)
                       (not program)
                       (nth (1- current-prefix-arg) (ebb--buffers)))))
    (if existing
        (pop-to-buffer-same-window existing)
      (ebb--start program #'pop-to-buffer-same-window))))

;;;###autoload
(defun ebb-other-window (&optional program)
  "Start a terminal in another window."
  (interactive)
  (ebb--start program #'switch-to-buffer-other-window))

;;;###autoload
(defun ebb-project ()
  "Switch to or start a terminal in the current project root."
  (interactive)
  (let* ((project (project-current))
         (root (and project (file-truename (project-root project))))
         (existing (and root (ebb--find-session root))))
    (if existing
        (pop-to-buffer-same-window existing)
      (let ((default-directory (or root default-directory))
            (ebb-buffer-name (if project
                                   (project-prefixed-buffer-name "ebb")
                                 ebb-buffer-name)))
        (let ((buffer (ebb)))
          (when root
            (with-current-buffer buffer
              (setq ebb--session-id root)))
          buffer)))))

;;;###autoload
(defun ebb-project-other-window ()
  "Start a terminal in the current project root in another window."
  (interactive)
  (let ((default-directory
         (or (when-let* ((proj (project-current)))
               (project-root proj))
             default-directory)))
    (ebb-other-window)))

;;;; ---- Bookmarks ------------------------------------------------------

(defun ebb-bookmark-make-record ()
  "Return a bookmark record for the current local terminal session."
  (when (file-remote-p default-directory)
    (user-error "Remote Ebb bookmarks are unsupported"))
  (cons (buffer-name)
        `((handler . ebb-bookmark-jump)
          (ebb-directory . ,default-directory)
          (ebb-display-name . ,(buffer-name))
          (ebb-session-id . ,ebb--session-id))))

(defun ebb--bookmark-property (record property)
  "Return custom PROPERTY from bookmark RECORD."
  (alist-get property (if (stringp (car-safe record)) (cdr record) record)))

;;;###autoload
(defun ebb-bookmark-jump (record)
  "Jump to the terminal session described by bookmark RECORD."
  (let* ((directory (ebb--bookmark-property record 'ebb-directory))
         (display-name (ebb--bookmark-property record 'ebb-display-name))
         (identity (ebb--bookmark-property record 'ebb-session-id)))
    (when (or (null directory) (file-remote-p directory))
      (user-error "Remote Ebb bookmarks are unsupported"))
    (let ((buffer (ebb--find-session identity)))
      (if buffer
          (with-current-buffer buffer
            (unless (file-equal-p default-directory directory)
              (ebb-send-string
               (concat "cd "
                       (shell-quote-argument (directory-file-name directory))))
              (ebb-send-key "return")))
        (let ((default-directory directory)
              (ebb-buffer-name (or display-name ebb-buffer-name)))
          (setq buffer (ebb))
          (with-current-buffer buffer
            (setq ebb--session-id identity))))
      (pop-to-buffer-same-window buffer)
      buffer)))

;;;; ---- Buffer naming --------------------------------------------------

(defun ebb--local-host-p (host)
  "Return non-nil if HOST is this machine or empty/localhost."
  (or (null host)
      (string-empty-p host)
      (member (downcase host) '("localhost" "127.0.0.1" "::1"))
      (eq t (compare-strings host nil nil (system-name) nil nil t))
      (eq t (compare-strings host nil nil
                             (car (split-string (system-name) "\\."))
                             nil nil t))))

(defun ebb--format-title-for-buffer (title)
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

(defun ebb-buffer-name-by-title (title)
  "Return \"*PREFIX TITLE*\" from OSC TITLE per `ebb-buffer-name-title-prefix'.
Local user@host is stripped from TITLE; remote hosts are kept."
  (when-let* ((pretty (ebb--format-title-for-buffer title)))
    (concat "*" ebb-buffer-name-title-prefix pretty "*")))

(defun ebb-buffer-name-by-directory (&optional _title)
  "Return \"*ebb: DIR*\" from abbreviated `default-directory'.
Local paths omit the hostname; remote TRAMP paths keep the host."
  (format "*ebb: %s*"
          (abbreviate-file-name
           (directory-file-name default-directory))))

(defun ebb--rename-managed (new-name)
  "Rename buffer to NEW-NAME unless the user renamed it manually."
  (when (and new-name
             (or (null ebb--managed-buffer-name)
                 (equal (buffer-name) ebb--managed-buffer-name))
             (not (equal new-name (buffer-name))))
    (rename-buffer new-name t)
    (setq ebb--managed-buffer-name (buffer-name))))

(defun ebb--set-title (title)
  "Record OSC TITLE and rename via `ebb-buffer-name-function'."
  (setq ebb--title title)
  (when ebb-buffer-name-function
    (ebb--rename-managed
     (funcall ebb-buffer-name-function title))))

;;;; ---- Mode Line ------------------------------------------------------

(defun ebb--mode-line-input-mode ()
  "Return mode-line string for current input mode and terminal progress."
  (string-join
   (delq nil
         (list (pcase ebb--input-mode
                 ('char "[Char]")
                 ('semi-char "[Semi]")
                 ('emacs "[Emacs]")
                 (_ nil))
               (and ebb--password-mode-p
                    (propertize "🔒Password" 'face 'warning))
               ebb--progress))
   " "))

(add-hook 'ebb-io-after-render-functions #'ebb--detect-password-prompt)

(require 'ebb-eshell)

(provide 'ebb)
;;; ebb.el ends here
