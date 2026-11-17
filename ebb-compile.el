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

(defun ebb-compile--finish (event)
  "Turn the live terminal into a compilation log after EVENT."
  (let ((log (ebb-screen-plain-text ebb--screen))
        (command ebb-compile--command)
        (directory default-directory)
        (inhibit-read-only t))
    (ebb-io-stop ebb--io)
    (setq ebb--io nil ebb--render nil)
    (ebb-shell-cleanup)
    (ebb--maybe-remove-focus-change-hook)
    (erase-buffer)
    (compilation-mode)
    (setq default-directory directory)
    (compilation-insert-annotation
     (format "-*- mode: compilation; default-directory: %S -*-\n%s\n\n"
             directory command))
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
        (argv (with-connection-local-variables
                (list shell-file-name shell-command-switch command))))
    (ebb--start
     argv
     (lambda (buffer)
       (with-current-buffer buffer
         (setq-local ebb-compile--command command)
         (setq-local ebb-kill-buffer-on-exit nil)
         (setq-local ebb-show-title nil)
         (setq-local ebb-enable-directory-tracking nil)
         (add-hook 'ebb-process-exit-hook #'ebb-compile--finish nil t))
       (pop-to-buffer-same-window buffer)))))

(provide 'ebb-compile)
;;; ebb-compile.el ends here
