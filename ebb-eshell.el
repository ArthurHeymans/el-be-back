;;; ebb-eshell.el --- Eshell integration for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run Eshell's interactive subprocesses in an inline Ebb terminal, while
;; leaving Eshell responsible for creating, filtering, and reaping processes.

;;; Code:

(require 'cl-lib)
(require 'ebb-io)
(require 'ebb-input)
(require 'ebb-render)
(require 'ebb-shell)

(declare-function ebb "ebb" (&optional program))
(declare-function ebb-self-input "ebb")
(declare-function ebb-quoted-input "ebb")
(declare-function ebb-yank "ebb")
(declare-function ebb-yank-pop "ebb")
(declare-function ebb--mouse-mode "ebb")
(declare-function ebb--refresh-input-cursor "ebb")
(declare-function ebb--ensure-focus-change-hook "ebb")
(declare-function ebb--maybe-remove-focus-change-hook "ebb")
(declare-function ebb-shell-cleanup "ebb-shell")
(declare-function eshell-gather-process-output "esh-proc" (command args))
(declare-function eshell-interactive-output-p "esh-io" (&optional index handles))
(declare-function eshell-find-interpreter "esh-ext" (file args &optional no-examine-p))
(declare-function eshell-stringify-list "esh-util" (args))
(declare-function eshell-exec-visual "em-term" (&rest args))

(defgroup ebb-eshell nil
  "Ebb integration with Eshell."
  :group 'ebb)

(defconst ebb-eshell--inside-emacs (format "%s,ebb" emacs-version))
(defvar ebb--io)
(defvar ebb--input-mode)
(defvar ebb--screen)
(defvar ebb-scrollback-lines)
(defvar eshell-current-subjob-p)
(defvar eshell-destroy-buffer-when-process-dies)
(defvar eshell-interpreter-alist)
(defvar eshell-last-output-start)
(defvar eshell-last-output-end)
(defvar eshell-parent-buffer)
(defvar eshell-variable-aliases-list)
(defvar-local ebb-eshell--io nil)
(defvar-local ebb-eshell--input-mode nil)

;;;; Input

(defvar ebb-eshell-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'ebb-eshell-semi-char-mode)
    (define-key map [remap eshell-toggle-direct-send] #'ebb-eshell-char-mode)
    map))

(defun ebb-eshell--semi-char-map ()
  (ebb-input-make-semi-char-map '(:ascii :arrow :navigation)
                                  #'ebb-eshell-emacs-mode))

(defvar ebb-eshell-semi-char-mode-map
  (ignore-errors (ebb-eshell--semi-char-map)))

(defvar ebb-eshell-char-mode-map
  (let ((map (ebb-input-make-keymap
              #'ebb-self-input '(:ascii :arrow :navigation :function) nil)))
    (define-key map (kbd "C-M-RET") #'ebb-eshell-semi-char-mode)
    map))

(define-minor-mode ebb-eshell--running-mode
  "Enable Ebb's Eshell process keymap."
  :interactive nil
  :keymap ebb-eshell-emacs-mode-map)

(defun ebb-eshell--set-overriding-map (mode map enabled)
  "Give MODE's MAP precedence while ENABLED."
  (setq minor-mode-overriding-map-alist
        (delete (cons mode map) minor-mode-overriding-map-alist))
  (when enabled
    (push (cons mode map) minor-mode-overriding-map-alist)))

(define-minor-mode ebb-eshell--semi-char-mode
  "Enable inline terminal semi-char keybindings."
  :interactive nil
  :keymap ebb-eshell-semi-char-mode-map
  (ebb-eshell--set-overriding-map 'ebb-eshell--semi-char-mode
                                    ebb-eshell-semi-char-mode-map
                                    ebb-eshell--semi-char-mode))

(define-minor-mode ebb-eshell--char-mode
  "Enable inline terminal char keybindings."
  :interactive nil
  :keymap ebb-eshell-char-mode-map
  (ebb-eshell--set-overriding-map 'ebb-eshell--char-mode
                                    ebb-eshell-char-mode-map
                                    ebb-eshell--char-mode))

(defun ebb-eshell--switch-input-mode (mode &optional read-only)
  "Enable the inline terminal minor modes for MODE.
READ-ONLY also toggles the buffer's read-only state (emacs mode is
read-only; the terminal modes are writable)."
  (ebb-eshell--semi-char-mode (if (eq mode 'semi-char) 1 -1))
  (ebb-eshell--char-mode (if (eq mode 'char) 1 -1))
  (setq ebb-eshell--input-mode mode
        ebb--input-mode mode)
  (ebb--refresh-input-cursor)
  (when read-only
    (setq buffer-read-only (eq mode 'emacs)))
  (force-mode-line-update))

(defun ebb-eshell-emacs-mode ()
  "Switch the inline terminal to normal Eshell keybindings."
  (interactive)
  (ebb-eshell--switch-input-mode 'emacs t))

(defun ebb-eshell-semi-char-mode ()
  "Switch the inline terminal to semi-char keybindings."
  (interactive)
  (when ebb-eshell--io
    (ebb-eshell--switch-input-mode 'semi-char t)))

(defun ebb-eshell-char-mode ()
  "Switch the inline terminal to char keybindings."
  (interactive)
  (when ebb-eshell--io
    (ebb-eshell--switch-input-mode 'char t)))

;;;; Inline terminal lifecycle

(defun ebb-eshell--term-name (&rest _)
  "Return the terminal name exposed to Eshell subprocesses."
  ebb-term-name)

(defun ebb-eshell--event (type &rest args)
  "Handle terminal TYPE emitted from an inline Eshell terminal."
  (pcase type
    ('bell (ding t))
    ('osc-51 (ebb-shell-handle-osc51 (car args) ebb--screen))
    ('mode-set
     (when (memq (car args) '(1000 1002 1003))
       (ebb--mouse-mode (if (ebb-screen-mouse-mode ebb--screen) 1 -1))))))

(defun ebb-eshell--output-filter ()
  "Replace Eshell's just-inserted output with terminal rendering."
  (when ebb-eshell--io
    (let ((text (buffer-substring-no-properties eshell-last-output-start
                                                 eshell-last-output-end))
          (inhibit-read-only t)
          (render (ebb-io-render ebb-eshell--io)))
      (delete-region eshell-last-output-start eshell-last-output-end)
      (ebb-io--filter ebb-eshell--io (ebb-io-process ebb-eshell--io)
                         text)
      ;; Eshell must continue to own its process mark; render now so it points
      ;; after the new bounded region before its next output arrives.
      (ebb-io--process-pending ebb-eshell--io t)
      (let ((end (ebb-render-state-region-end render)))
        (set-marker eshell-last-output-start end)
        (set-marker eshell-last-output-end end)
        (set-marker (process-mark (ebb-io-process ebb-eshell--io)) end)))))

(defun ebb-eshell--resize (window)
  "Resize the inline terminal displayed in WINDOW."
  (let ((buffer (window-buffer window)))
    (when (buffer-local-value 'ebb-eshell--io buffer)
      (with-current-buffer buffer
        (ebb-io-handle-resize ebb-eshell--io
                                (window-max-chars-per-line window)
                                (window-body-height window))))))

(defun ebb-eshell--setup (process)
  "Attach a Ebb terminal to Eshell PROCESS at its output marker."
  (unless ebb-eshell--io
    (let* ((start (if (marker-buffer (process-mark process))
                      (process-mark process) (point-max)))
           (io (ebb-io-create-terminal (current-buffer)
                                         #'ebb-eshell--event start start)))
      (setq-local ebb--screen (ebb-io-screen io))
      (setq-local ebb--render (ebb-io-render io))
      (setq-local ebb--parser (ebb-io-parser io))
      (setq-local ebb--io io)
      (setq-local ebb-eshell--io io)
      ;; Avoid terminal OSC directory/title state changing the Eshell buffer.
      (setq-local ebb-enable-directory-tracking nil)
      (setq-local ebb-buffer-name-function nil)
      (ebb-io-attach ebb--io process (current-buffer))
      (set-marker (process-mark process)
                  (ebb-render-state-region-end ebb--render))
      (setq-local eshell-output-filter-functions '(ebb-eshell--output-filter))
      (add-hook 'window-size-change-functions #'ebb-eshell--resize nil t)
      ;; Inline terminals honor DEC mode 1004 focus reporting too.
      (ebb--ensure-focus-change-hook)
      (ebb-eshell--running-mode 1)
      (ebb-eshell-semi-char-mode))))

(defun ebb-eshell--cleanup (process)
  "Return to ordinary Eshell after attached PROCESS exits."
  (when (and ebb-eshell--io (eq process (ebb-io-process ebb-eshell--io)))
    (let ((end (copy-marker
                (ebb-render-state-region-end
                 (ebb-io-render ebb-eshell--io)))))
      (ebb-io-stop ebb-eshell--io)
      (let ((inhibit-read-only t))
        (goto-char end)
        (unless (or (= (point) (point-min)) (eq (char-before) ?\n))
          (insert "\n"))
        (set-marker eshell-last-output-start (point))
        (set-marker eshell-last-output-end (point))
        (set-marker (process-mark process) (point)))
      (ebb-shell-cleanup)
      (kill-local-variable 'eshell-output-filter-functions)
      (remove-hook 'window-size-change-functions #'ebb-eshell--resize t)
      (ebb-eshell--semi-char-mode -1)
      (ebb-eshell--char-mode -1)
      (ebb-eshell--running-mode -1)
      (ebb--mouse-mode -1)
      ;; Drop the global focus hook if no other terminal remains.
      (ebb--maybe-remove-focus-change-hook)
      (setq-local ebb-eshell--io nil)
      (setq-local ebb--io nil)
      (setq-local ebb--screen nil)
      (setq-local ebb--parser nil)
      (setq-local ebb--render nil)
      (setq-local ebb--input-mode nil)
      (setq-local cursor-type t)
      (setq buffer-read-only nil)
      (force-mode-line-update))))

(defun ebb-eshell--sentinel (original process event)
  "Clean up Ebb for PROCESS, then run Eshell's ORIGINAL sentinel."
  (when (memq (process-status process) '(exit signal))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (ebb-eshell--cleanup process))))
  (funcall original process event))

(defun ebb-eshell--around-gather (original command args)
  "Run interactive Eshell COMMAND ARGS in a PTY-backed Ebb region."
  (if (or eshell-current-subjob-p (not (eshell-interactive-output-p)))
      (funcall original command args)
    (let ((expected (cons (file-local-name (expand-file-name command)) args))
          (hook (lambda (process)
                  (let ((sentinel (process-sentinel process)))
                    (set-process-sentinel
                     process
                     (lambda (proc event)
                       (ebb-eshell--sentinel sentinel proc event)))
                    (ebb-eshell--setup process)))))
      (add-hook 'eshell-exec-hook hook 99)
      (unwind-protect
          (cl-letf (((symbol-function 'make-process)
                     (let ((make-process (symbol-function 'make-process)))
                       (lambda (&rest plist)
                         (if (equal (plist-get plist :command) expected)
                             (let ((width (window-max-chars-per-line))
                                   (height (window-body-height)))
                               (setf (plist-get plist :connection-type) 'pty
                                     (plist-get plist :command)
                                     ;; Remote visual commands need the
                                     ;; on-remote TERM probe; TRAMP strips
                                     ;; the TERM= env alias.
                                     (funcall (if (file-remote-p
                                                   default-directory)
                                                  #'ebb-io--remote-command
                                                #'ebb-io--wrap-command-with-stty)
                                              expected height width))
                               (apply make-process plist))
                           (apply make-process plist))))))
            (funcall original command args))
        (remove-hook 'eshell-exec-hook hook)))))

;;;; Modes

(defconst ebb-eshell--variable-aliases
  '(("TERM" ebb-eshell--term-name t)
    ("TERMINFO" ebb-terminfo-directory t)
    ("INSIDE_EMACS" ebb-eshell--inside-emacs t)
    ("EBB_SHELL_INTEGRATION_DIR" ebb-shell-integration-directory t)
    ("EAT_SHELL_INTEGRATION_DIR" ebb-shell-integration-directory t)))

(define-minor-mode ebb-eshell--local-mode
  "Configure one Eshell buffer for `ebb-eshell-mode'."
  :interactive nil
  (if ebb-eshell--local-mode
      (progn
        (setq-local eshell-variable-aliases-list
                    (append ebb-eshell--variable-aliases
                            eshell-variable-aliases-list)))
    (setq eshell-variable-aliases-list
          (cl-set-difference eshell-variable-aliases-list
                             ebb-eshell--variable-aliases :test #'equal))))

;;;###autoload
(define-minor-mode ebb-eshell-mode
  "Run interactive Eshell subprocesses in inline Ebb terminals."
  :global t
  :group 'ebb-eshell
  (require 'esh-mode)
  (require 'esh-proc)
  (require 'esh-var)
  (require 'esh-cmd)
  (if ebb-eshell-mode
      (progn
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (eq major-mode 'eshell-mode)
              (when ebb-eshell--io
                (user-error "Can't enable Ebb Eshell mode while a process is running"))
              (ebb-eshell--local-mode 1))))
        (add-hook 'eshell-mode-hook #'ebb-eshell--local-mode)
        (advice-add #'eshell-gather-process-output :around
                    #'ebb-eshell--around-gather))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode) ebb-eshell--io)
          (user-error "Can't disable Ebb Eshell mode while a process is running"))))
    (remove-hook 'eshell-mode-hook #'ebb-eshell--local-mode)
    (advice-remove #'eshell-gather-process-output #'ebb-eshell--around-gather)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode) ebb-eshell--local-mode)
          (ebb-eshell--local-mode -1))))))

;;;; Visual commands

(defun ebb-eshell--visual-sentinel (process _event)
  "Return from a visual Ebb command when PROCESS exits successfully."
  (when (and eshell-destroy-buffer-when-process-dies
             (not (eq (process-status process) 'run))
             (zerop (process-exit-status process)))
    (let ((buffer (process-buffer process)))
      (if (eq (current-buffer) buffer)
          (when (buffer-live-p eshell-parent-buffer)
            (switch-to-buffer eshell-parent-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun ebb-eshell--exec-visual (&rest args)
  "Run ARGS as a visual program in an ordinary Ebb buffer."
  (require 'esh-ext)
  (require 'esh-util)
  (let* ((eshell-interpreter-alist nil)
         (interpreter (eshell-find-interpreter (car args) (cdr args)))
         (program (car interpreter))
         (arguments (flatten-tree
                     (eshell-stringify-list (append (cdr interpreter) (cdr args)))))
         (parent (current-buffer))
         (buffer (ebb (cons program arguments))))
    (with-current-buffer buffer
      (setq-local eshell-parent-buffer parent)
      (setq-local ebb-kill-buffer-on-exit nil)
      (let* ((process (ebb-io-process ebb--io))
             (sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (funcall sentinel proc event)
           (ebb-eshell--visual-sentinel proc event)))))
    nil))

;;;###autoload
(define-minor-mode ebb-eshell-visual-command-mode
  "Run Eshell visual commands in ordinary Ebb buffers."
  :global t
  :group 'ebb-eshell
  (if ebb-eshell-visual-command-mode
      (advice-add #'eshell-exec-visual :override #'ebb-eshell--exec-visual)
    (advice-remove #'eshell-exec-visual #'ebb-eshell--exec-visual)))

(provide 'ebb-eshell)
;;; ebb-eshell.el ends here
