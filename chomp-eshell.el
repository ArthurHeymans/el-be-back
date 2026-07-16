;;; chomp-eshell.el --- Eshell integration for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run Eshell's interactive subprocesses in an inline Chomp terminal, while
;; leaving Eshell responsible for creating, filtering, and reaping processes.

;;; Code:

(require 'cl-lib)
(require 'chomp-io)
(require 'chomp-input)
(require 'chomp-render)
(require 'chomp-shell)

(declare-function chomp "chomp" (&optional program))
(declare-function chomp-self-input "chomp")
(declare-function chomp-quoted-input "chomp")
(declare-function chomp-yank "chomp")
(declare-function chomp-yank-pop "chomp")
(declare-function chomp--mouse-mode "chomp")
(declare-function chomp-shell-cleanup "chomp-shell")
(declare-function eshell-gather-process-output "esh-proc" (command args))
(declare-function eshell-interactive-output-p "esh-io" (&optional index handles))
(declare-function eshell-find-interpreter "esh-ext" (file args &optional no-examine-p))
(declare-function eshell-stringify-list "esh-util" (args))
(declare-function eshell-exec-visual "em-term" (&rest args))

(defgroup chomp-eshell nil
  "Chomp integration with Eshell."
  :group 'chomp)

(defconst chomp-eshell--inside-emacs (format "%s,chomp" emacs-version))
(defvar chomp--io)
(defvar chomp--screen)
(defvar chomp-scrollback-lines)
(defvar eshell-current-subjob-p)
(defvar eshell-destroy-buffer-when-process-dies)
(defvar eshell-interpreter-alist)
(defvar eshell-last-output-start)
(defvar eshell-last-output-end)
(defvar eshell-parent-buffer)
(defvar eshell-variable-aliases-list)
(defvar-local chomp-eshell--io nil)
(defvar-local chomp-eshell--input-mode nil)

;;;; Input

(defvar chomp-eshell-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'chomp-eshell-semi-char-mode)
    (define-key map [remap eshell-toggle-direct-send] #'chomp-eshell-char-mode)
    map))

(defun chomp-eshell--semi-char-map ()
  (let ((map (chomp-input-make-keymap
              #'chomp-self-input '(:ascii :arrow :navigation)
              `([?\C-c] [?\C-q] [?\C-y] [?\e ?y]
                ,@chomp-semi-char-non-bound-keys))))
    (define-key map [?\C-q] #'chomp-quoted-input)
    (define-key map [?\C-y] #'chomp-yank)
    (define-key map [?\M-y] #'chomp-yank-pop)
    (define-key map (kbd "C-c C-e") #'chomp-eshell-emacs-mode)
    map))

(defvar chomp-eshell-semi-char-mode-map
  (ignore-errors (chomp-eshell--semi-char-map)))

(defvar chomp-eshell-char-mode-map
  (let ((map (chomp-input-make-keymap
              #'chomp-self-input '(:ascii :arrow :navigation :function) nil)))
    (define-key map (kbd "C-M-RET") #'chomp-eshell-semi-char-mode)
    map))

(define-minor-mode chomp-eshell--running-mode
  "Enable Chomp's Eshell process keymap."
  :interactive nil
  :keymap chomp-eshell-emacs-mode-map)

(defun chomp-eshell--set-overriding-map (mode map enabled)
  "Give MODE's MAP precedence while ENABLED."
  (setq minor-mode-overriding-map-alist
        (delete (cons mode map) minor-mode-overriding-map-alist))
  (when enabled
    (push (cons mode map) minor-mode-overriding-map-alist)))

(define-minor-mode chomp-eshell--semi-char-mode
  "Enable inline terminal semi-char keybindings."
  :interactive nil
  :keymap chomp-eshell-semi-char-mode-map
  (chomp-eshell--set-overriding-map 'chomp-eshell--semi-char-mode
                                    chomp-eshell-semi-char-mode-map
                                    chomp-eshell--semi-char-mode))

(define-minor-mode chomp-eshell--char-mode
  "Enable inline terminal char keybindings."
  :interactive nil
  :keymap chomp-eshell-char-mode-map
  (chomp-eshell--set-overriding-map 'chomp-eshell--char-mode
                                    chomp-eshell-char-mode-map
                                    chomp-eshell--char-mode))

(defun chomp-eshell-emacs-mode ()
  "Switch the inline terminal to normal Eshell keybindings."
  (interactive)
  (chomp-eshell--semi-char-mode -1)
  (chomp-eshell--char-mode -1)
  (setq chomp-eshell--input-mode 'emacs
        chomp--input-mode 'emacs
        buffer-read-only t)
  (force-mode-line-update))

(defun chomp-eshell-semi-char-mode ()
  "Switch the inline terminal to semi-char keybindings."
  (interactive)
  (when chomp-eshell--io
    (setq buffer-read-only nil)
    (chomp-eshell--char-mode -1)
    (chomp-eshell--semi-char-mode 1)
    (setq chomp-eshell--input-mode 'semi-char
          chomp--input-mode 'semi-char)
    (force-mode-line-update)))

(defun chomp-eshell-char-mode ()
  "Switch the inline terminal to char keybindings."
  (interactive)
  (when chomp-eshell--io
    (setq buffer-read-only nil)
    (chomp-eshell--semi-char-mode -1)
    (chomp-eshell--char-mode 1)
    (setq chomp-eshell--input-mode 'char
          chomp--input-mode 'char)
    (force-mode-line-update)))

;;;; Inline terminal lifecycle

(defun chomp-eshell--term-name (&rest _)
  "Return the terminal name exposed to Eshell subprocesses."
  chomp-term-name)

(defun chomp-eshell--event (type &rest args)
  "Handle terminal TYPE emitted from an inline Eshell terminal."
  (pcase type
    ('bell (ding t))
    ('osc-51 (chomp-shell-handle-osc51 (car args) chomp--screen))
    ('mode-set
     (when (memq (car args) '(1000 1002 1003))
       (chomp--mouse-mode (if (chomp-screen-mouse-mode chomp--screen) 1 -1))))))

(defun chomp-eshell--output-filter ()
  "Replace Eshell's just-inserted output with terminal rendering."
  (when chomp-eshell--io
    (let ((text (buffer-substring-no-properties eshell-last-output-start
                                                 eshell-last-output-end))
          (inhibit-read-only t)
          (render (chomp-io-render chomp-eshell--io)))
      (delete-region eshell-last-output-start eshell-last-output-end)
      (chomp-io--filter chomp-eshell--io (chomp-io-process chomp-eshell--io)
                         text)
      ;; Eshell must continue to own its process mark; render now so it points
      ;; after the new bounded region before its next output arrives.
      (chomp-io--process-pending chomp-eshell--io t)
      (let ((end (chomp-render-state-region-end render)))
        (set-marker eshell-last-output-start end)
        (set-marker eshell-last-output-end end)
        (set-marker (process-mark (chomp-io-process chomp-eshell--io)) end)))))

(defun chomp-eshell--resize (window)
  "Resize the inline terminal displayed in WINDOW."
  (let ((buffer (window-buffer window)))
    (when (buffer-local-value 'chomp-eshell--io buffer)
      (with-current-buffer buffer
        (chomp-io-handle-resize chomp-eshell--io
                                (window-max-chars-per-line window)
                                (window-body-height window))))))

(defun chomp-eshell--setup (process)
  "Attach a Chomp terminal to Eshell PROCESS at its output marker."
  (unless chomp-eshell--io
    (let* ((window (or (get-buffer-window (current-buffer)) (selected-window)))
           (width (max 10 (window-max-chars-per-line window)))
           (height (max 3 (window-body-height window)))
           (start (if (marker-buffer (process-mark process))
                      (process-mark process) (point-max)))
           (screen (chomp-screen-create width height)))
      (setf (chomp-screen-scrollback-max screen) chomp-scrollback-lines)
      (setq-local chomp--screen screen)
      (setq-local chomp--render
                  (chomp-render-create screen (current-buffer) start start))
      (setq-local chomp--parser
                  (chomp-parse-create screen nil #'chomp-eshell--event))
      (setq-local chomp--io
                  (make-chomp-io :screen screen :parser chomp--parser
                                 :render chomp--render :buffer (current-buffer)
                                 :chunk-size chomp-chunk-size
                                 :min-latency chomp-minimum-latency
                                 :max-latency chomp-maximum-latency))
      (setq-local chomp-eshell--io chomp--io)
      ;; Avoid terminal OSC directory/title state changing the Eshell buffer.
      (setq-local chomp-enable-directory-tracking nil)
      (setq-local chomp-buffer-name-function nil)
      (chomp-io-attach chomp--io process (current-buffer))
      (set-marker (process-mark process)
                  (chomp-render-state-region-end chomp--render))
      (setq-local eshell-output-filter-functions '(chomp-eshell--output-filter))
      (add-hook 'window-size-change-functions #'chomp-eshell--resize nil t)
      (chomp-eshell--running-mode 1)
      (chomp-eshell-semi-char-mode))))

(defun chomp-eshell--cleanup (process)
  "Return to ordinary Eshell after attached PROCESS exits."
  (when (and chomp-eshell--io (eq process (chomp-io-process chomp-eshell--io)))
    (let ((end (copy-marker
                (chomp-render-state-region-end
                 (chomp-io-render chomp-eshell--io)))))
      (chomp-io-stop chomp-eshell--io)
      (let ((inhibit-read-only t))
        (goto-char end)
        (unless (or (= (point) (point-min)) (eq (char-before) ?\n))
          (insert "\n"))
        (set-marker eshell-last-output-start (point))
        (set-marker eshell-last-output-end (point))
        (set-marker (process-mark process) (point)))
      (chomp-shell-cleanup)
      (kill-local-variable 'eshell-output-filter-functions)
      (remove-hook 'window-size-change-functions #'chomp-eshell--resize t)
      (chomp-eshell--semi-char-mode -1)
      (chomp-eshell--char-mode -1)
      (chomp-eshell--running-mode -1)
      (chomp--mouse-mode -1)
      (setq-local chomp-eshell--io nil)
      (setq-local chomp--io nil)
      (setq-local chomp--screen nil)
      (setq-local chomp--parser nil)
      (setq-local chomp--render nil)
      (setq-local chomp--input-mode nil)
      (setq-local cursor-type t)
      (setq buffer-read-only nil)
      (force-mode-line-update))))

(defun chomp-eshell--sentinel (original process event)
  "Clean up Chomp for PROCESS, then run Eshell's ORIGINAL sentinel."
  (when (memq (process-status process) '(exit signal))
    (when (buffer-live-p (process-buffer process))
      (with-current-buffer (process-buffer process)
        (chomp-eshell--cleanup process))))
  (funcall original process event))

(defun chomp-eshell--around-gather (original command args)
  "Run interactive Eshell COMMAND ARGS in a PTY-backed Chomp region."
  (if (or eshell-current-subjob-p (not (eshell-interactive-output-p)))
      (funcall original command args)
    (let ((expected (cons (file-local-name (expand-file-name command)) args))
          (hook (lambda (process)
                  (let ((sentinel (process-sentinel process)))
                    (set-process-sentinel
                     process
                     (lambda (proc event)
                       (chomp-eshell--sentinel sentinel proc event)))
                    (chomp-eshell--setup process)))))
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
                                     (chomp-io--wrap-command-with-stty
                                      expected height width))
                               (apply make-process plist))
                           (apply make-process plist))))))
            (funcall original command args))
        (remove-hook 'eshell-exec-hook hook)))))

;;;; Modes

(defconst chomp-eshell--variable-aliases
  '(("TERM" chomp-eshell--term-name t)
    ("TERMINFO" chomp-terminfo-directory t)
    ("INSIDE_EMACS" chomp-eshell--inside-emacs t)
    ("CHOMP_SHELL_INTEGRATION_DIR" chomp-shell-integration-directory t)
    ("EAT_SHELL_INTEGRATION_DIR" chomp-shell-integration-directory t)))

(define-minor-mode chomp-eshell--local-mode
  "Configure one Eshell buffer for `chomp-eshell-mode'."
  :interactive nil
  (if chomp-eshell--local-mode
      (progn
        (setq-local eshell-variable-aliases-list
                    (append chomp-eshell--variable-aliases
                            eshell-variable-aliases-list)))
    (setq eshell-variable-aliases-list
          (cl-set-difference eshell-variable-aliases-list
                             chomp-eshell--variable-aliases :test #'equal))))

;;;###autoload
(define-minor-mode chomp-eshell-mode
  "Run interactive Eshell subprocesses in inline Chomp terminals."
  :global t
  :group 'chomp-eshell
  (require 'esh-mode)
  (require 'esh-proc)
  (require 'esh-var)
  (require 'esh-cmd)
  (if chomp-eshell-mode
      (progn
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (eq major-mode 'eshell-mode)
              (when chomp-eshell--io
                (user-error "Can't enable Chomp Eshell mode while a process is running"))
              (chomp-eshell--local-mode 1))))
        (add-hook 'eshell-mode-hook #'chomp-eshell--local-mode)
        (advice-add #'eshell-gather-process-output :around
                    #'chomp-eshell--around-gather))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode) chomp-eshell--io)
          (user-error "Can't disable Chomp Eshell mode while a process is running"))))
    (remove-hook 'eshell-mode-hook #'chomp-eshell--local-mode)
    (advice-remove #'eshell-gather-process-output #'chomp-eshell--around-gather)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'eshell-mode) chomp-eshell--local-mode)
          (chomp-eshell--local-mode -1))))))

;;;; Visual commands

(defun chomp-eshell--visual-sentinel (process _event)
  "Return from a visual Chomp command when PROCESS exits successfully."
  (when (and eshell-destroy-buffer-when-process-dies
             (not (eq (process-status process) 'run))
             (zerop (process-exit-status process)))
    (let ((buffer (process-buffer process)))
      (if (eq (current-buffer) buffer)
          (when (buffer-live-p eshell-parent-buffer)
            (switch-to-buffer eshell-parent-buffer))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun chomp-eshell--exec-visual (&rest args)
  "Run ARGS as a visual program in an ordinary Chomp buffer."
  (require 'esh-ext)
  (require 'esh-util)
  (let* ((eshell-interpreter-alist nil)
         (interpreter (eshell-find-interpreter (car args) (cdr args)))
         (program (car interpreter))
         (arguments (flatten-tree
                     (eshell-stringify-list (append (cdr interpreter) (cdr args)))))
         (parent (current-buffer))
         (buffer (chomp (cons program arguments))))
    (with-current-buffer buffer
      (setq-local eshell-parent-buffer parent)
      (setq-local chomp-kill-buffer-on-exit nil)
      (let* ((process (chomp-io-process chomp--io))
             (sentinel (process-sentinel process)))
        (set-process-sentinel
         process
         (lambda (proc event)
           (funcall sentinel proc event)
           (chomp-eshell--visual-sentinel proc event)))))
    nil))

;;;###autoload
(define-minor-mode chomp-eshell-visual-command-mode
  "Run Eshell visual commands in ordinary Chomp buffers."
  :global t
  :group 'chomp-eshell
  (if chomp-eshell-visual-command-mode
      (advice-add #'eshell-exec-visual :override #'chomp-eshell--exec-visual)
    (advice-remove #'eshell-exec-visual #'chomp-eshell--exec-visual)))

(provide 'chomp-eshell)
;;; chomp-eshell.el ends here
