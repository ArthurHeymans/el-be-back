;;; ebb-compile.el --- Run compilation commands in Ebb -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `ebb-compile' runs a command in a live terminal.  On exit the retained
;; output becomes an ordinary compilation buffer with `next-error' support.

;;; Code:

(require 'compile)
(require 'ebb)

(defvar ebb-compile-history nil
  "History of shell commands entered with `ebb-compile'.")

(defvar-local ebb-compile--command nil)
(defvar-local ebb-compile--environment nil)

(defun ebb-compile--diagnostic-directory (command directory)
  "Find the diagnostic directory for a simple initial cd in COMMAND.
Run the command from DIRECTORY regardless, so the shell executes cd only once."
  (let ((default-directory directory))
    (if (string-match
         (concat "\\`[ \t]*cd[ \t]+\\('[^']*'\\|\"[^\"`$]*\"\\|[^ \t;&\n|]+\\)"
                 "[ \t]*\\(?:&&\\|;\\|\n\\)") command)
        (let* ((word (match-string 1 command))
               (arg (car (split-string-shell-command word)))
               (target (and arg (expand-file-name (substitute-env-vars arg)))))
          (if (and target (file-directory-p target))
              (file-name-as-directory target)
            directory))
      directory)))

(define-derived-mode ebb-compilation-mode compilation-mode "Ebb Compilation"
  "Read a finished Ebb build log.")

(define-key ebb-compilation-mode-map [remap recompile] #'ebb-recompile)

(defun ebb-recompile (&optional edit-command)
  "Run the last Ebb build in a new PTY; with EDIT-COMMAND, edit it first."
  (interactive "P")
  (let ((default-directory (or compilation-directory default-directory)))
    (ebb-compile (if edit-command
                     (compilation-read-command compile-command)
                   compile-command))))

(defun ebb-compile--finish (event)
  "Turn the live terminal into a compilation log after EVENT."
  (let ((log (ebb-screen-plain-text ebb--screen t))
        (command ebb-compile--command)
        (directory default-directory)
        (environment ebb-compile--environment)
        (inhibit-read-only t))
    (ebb-io-stop ebb--io)
    (setq ebb--io nil ebb--render nil)
    (ebb-shell-cleanup)
    (ebb--maybe-remove-focus-change-hook)
    (erase-buffer)
    (ebb-compilation-mode)
    (pcase-let ((`(,env ,build-env ,path) environment))
      (setq-local process-environment env)
      (setq-local compilation-environment build-env)
      (setq-local exec-path path))
    (setq-local compile-command command)
    (setq-local compilation-arguments (list command))
    (setq-local compilation-directory directory)
    (setq default-directory (ebb-compile--diagnostic-directory command directory))
    ;; `compilation-insert-annotation' is not available on Emacs 29.
    (let ((start (point)))
      (insert (format "-*- mode: compilation; default-directory: %S -*-\n%s\n\n"
                      default-directory command))
      (put-text-property start (point) 'compilation-annotation t))
    (setq compilation-messages-start (copy-marker (1- (point))))
    (insert log)
    (unless (bolp) (insert "\n"))
    (let ((status (point)))
      (insert (format "\n[Process %s]\n" (string-trim event)))
      (put-text-property status (point) 'compilation-handle-exit t))
    (set-buffer-modified-p nil)
    (setq next-error-last-buffer (current-buffer))
    (run-hook-with-args 'compilation-finish-functions (current-buffer) event)))

;;;###autoload
(defun ebb-compile (command)
  "Run shell COMMAND in a fresh Ebb terminal and return its buffer.
While the command is running the buffer accepts terminal input.
After it exits, the retained terminal output is converted to a plain
compilation log for `next-error'.  The log includes retained virtual
scrollback, not just the rows currently shown in the terminal."
  (interactive (list (read-shell-command "Ebb compile command: "
                                         compile-command 'ebb-compile-history)))
  (when (string-blank-p command) (user-error "Empty compilation command"))
  (let ((ebb-buffer-name "*ebb-compilation*")
        (ebb-enable-shell-integration nil)
        (environment (list (copy-sequence process-environment)
                           (copy-sequence compilation-environment)
                           (copy-sequence exec-path)))
        (argv (with-connection-local-variables
                (list shell-file-name shell-command-switch command))))
    (ebb--start
     argv
     (lambda (buffer)
       (with-current-buffer buffer
         (setq-local ebb-compile--command command)
         (setq-local ebb-compile--environment environment)
         (setq-local process-environment (append (nth 1 environment) (car environment)))
         (setq-local exec-path (nth 2 environment))
         (setq-local ebb-kill-buffer-on-exit nil)
         (setq-local ebb-show-title nil)
         (setq-local ebb-enable-directory-tracking nil)
         (add-hook 'ebb-process-exit-hook #'ebb-compile--finish nil t))
       (pop-to-buffer-same-window buffer)))))

(provide 'ebb-compile)
;;; ebb-compile.el ends here
