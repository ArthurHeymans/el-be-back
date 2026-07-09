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
(require 'project)
(require 'chomp-term)
(require 'chomp-parse)
(require 'chomp-render)
(require 'chomp-input)
(require 'chomp-io)
(require 'chomp-shell)

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

(defcustom chomp-kill-buffer-on-exit nil
  "If non-nil, kill the buffer when the shell process exits."
  :type 'boolean
  :group 'chomp)

(defcustom chomp-query-before-kill 'auto
  "Whether to query before killing a buffer with a running process.
`auto' queries only if the process is running.  t always queries."
  :type '(choice (const :tag "Auto" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'chomp)

(defcustom chomp-show-title t
  "If non-nil, display the terminal title in the mode line."
  :type 'boolean
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

;;;; ---- Buffer-local Variables -----------------------------------------

(defvar-local chomp--io nil "The chomp-io instance for this buffer.")
(defvar-local chomp--screen nil "The chomp-screen for this buffer.")
(defvar-local chomp--parser nil "The chomp-parser for this buffer.")
(defvar-local chomp--render nil "The chomp-render-state for this buffer.")
(defvar-local chomp--input-mode nil "Current input mode symbol.")
(defvar-local chomp--mouse-drag-transient-map-exit nil
  "Function that exits the active mouse drag transient map.")

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
        (if (and chomp--screen (chomp-screen-bracketed-paste chomp--screen))
            (progn
              (chomp-io-send chomp--io (chomp-input-bracketed-paste-start))
              (chomp-io-send chomp--io text)
              (chomp-io-send chomp--io (chomp-input-bracketed-paste-end)))
          (chomp-io-send chomp--io text))))))

(defun chomp-yank-pop ()
  "Yank-pop: replace the last yank with the next kill ring entry."
  (interactive)
  (when chomp--io
    ;; Rotate kill ring
    (current-kill 1)
    (let ((text (current-kill 0)))
      (when text
        (if (and chomp--screen (chomp-screen-bracketed-paste chomp--screen))
            (progn
              (chomp-io-send chomp--io (chomp-input-bracketed-paste-start))
              (chomp-io-send chomp--io text)
              (chomp-io-send chomp--io (chomp-input-bracketed-paste-end)))
          (chomp-io-send chomp--io text))))))

(defun chomp-send-password (&optional password)
  "Read PASSWORD from the minibuffer and send it to the terminal.
Interactively, this uses `read-passwd' so password keystrokes do not go through
normal terminal input handling or appear in `view-lossage'."
  (interactive)
  (unless chomp--io
    (user-error "Process not running"))
  (let ((password (or password (read-passwd "Password: "))))
    (chomp-io-send chomp--io password)
    (chomp-io-send chomp--io "\r")))

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

;;;; ---- Event Handler --------------------------------------------------

(defun chomp--handle-event (type &rest args)
  "Handle events emitted by the parser."
  (pcase type
    ('bell (ding t))
    ('title
     (let ((title (car args)))
       (when chomp-show-title
         (rename-buffer (format "*chomp: %s*"
                                (if (> (length title) 40)
                                    (concat (substring title 0 37) "...")
                                  title))
                        t))))
    ('cwd
     (let ((dir (car args)))
       (when (and dir (file-directory-p dir))
         (setq default-directory (file-name-as-directory dir)))))
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

(defun chomp--window-size-change (frame)
  "Handle window size changes for chomp buffers in FRAME."
  (dolist (win (window-list frame 'no-minibuf))
    (let ((buf (window-buffer win)))
      (when (buffer-local-value 'chomp--io buf)
        (with-current-buffer buf
          (when chomp--io
            (let ((new-width (window-max-chars-per-line win))
                  (new-height (window-body-height win)))
              (when (and (> new-width 0) (> new-height 0))
                (chomp-io-handle-resize chomp--io new-width new-height)))))))))

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
  ;; Set up shell integration margins
  (chomp-shell-setup-margins)
  ;; Add resize hook
  (add-hook 'window-size-change-functions #'chomp--window-size-change nil t)
  ;; Add focus tracking
  (chomp--ensure-focus-change-hook)
  ;; Clean up shell state on kill
  (add-hook 'kill-buffer-hook #'chomp-shell-cleanup nil t)
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

;;;###autoload
(defun chomp (&optional program)
  "Start a terminal emulator.
With a numeric prefix argument N, switch to the Nth existing chomp
session (creating it if needed).  With \\[universal-argument] \\[universal-argument],
prompt for the program to run.  PROGRAM defaults to `chomp-default-shell'
or `$SHELL'."
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
           (chomp-bufs (seq-filter
                        (lambda (b)
                          (eq (buffer-local-value 'major-mode b) 'chomp-mode))
                        (buffer-list)))
           (existing (nth (1- n) chomp-bufs)))
      (when existing
        (pop-to-buffer-same-window existing)
        (cl-return-from chomp existing))))
  (let* ((shell (or program
                    chomp-default-shell
                    (getenv "SHELL")
                    shell-file-name
                    "/bin/sh"))
         (buf (generate-new-buffer chomp-buffer-name)))
    ;; Set up mode first
    (with-current-buffer buf
      (chomp-mode)
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
  "Start a terminal in the current project root."
  (interactive)
  (let ((default-directory
         (or (when-let ((proj (project-current)))
               (project-root proj))
             default-directory)))
    (chomp)))

;;;###autoload
(defun chomp-project-other-window ()
  "Start a terminal in the current project root in another window."
  (interactive)
  (let ((default-directory
         (or (when-let ((proj (project-current)))
               (project-root proj))
             default-directory)))
    (chomp-other-window)))

;;;; ---- Mode Line ------------------------------------------------------

(defun chomp--mode-line-input-mode ()
  "Return mode-line string for current input mode."
  (pcase chomp--input-mode
    ('char "[Char]")
    ('semi-char "[Semi]")
    ('emacs "[Emacs]")
    (_ "")))

;; Add to mode-line
(put 'chomp--input-mode 'risky-local-variable t)

(provide 'chomp)
;;; chomp.el ends here
