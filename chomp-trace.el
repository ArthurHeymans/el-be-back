;;; chomp-trace.el --- Terminal I/O tracing for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides a minor mode for recording all terminal I/O to a trace buffer.
;; Useful for debugging terminal display issues and reporting bugs.
;;
;; Usage:  M-x chomp-trace-mode  (in a chomp buffer)
;; The trace buffer contains timestamped lisp-data entries for every
;; output, resize, and redisplay event.

;;; Code:

(require 'cl-lib)

;;;; ---- Trace State ----------------------------------------------------

(defvar-local chomp-trace--buffer nil
  "Buffer where trace data is written.")

(defvar-local chomp-trace--original-filter nil
  "Original process filter before tracing started.")

;;;; ---- Trace Logging --------------------------------------------------

(defun chomp-trace--log (time operation &rest args)
  "Log TIME, OPERATION, and ARGS to the trace buffer.
TIME defaults to current time."
  (when (and chomp-trace--buffer (buffer-live-p chomp-trace--buffer))
    (with-current-buffer chomp-trace--buffer
      (goto-char (point-max))
      (insert
       (replace-regexp-in-string
        (rx (any (0 . 31)))
        (lambda (s) (format "\\\\x%02x" (aref s 0)))
        (format "%S" `(,(float-time (or time (current-time)))
                        ,operation ,@args)))
       "\n"))))

;;;; ---- Minor Mode -----------------------------------------------------

;;;###autoload
(define-minor-mode chomp-trace-mode
  "Toggle terminal I/O tracing for this chomp buffer.
When enabled, all terminal output, resize events, and redisplay
cycles are logged to a trace buffer."
  :lighter " Trace"
  :group 'chomp
  (if chomp-trace-mode
      (chomp-trace--start)
    (chomp-trace--stop)))

(defun chomp-trace--start ()
  "Start tracing the current chomp buffer."
  (unless (bound-and-true-p chomp--io)
    (chomp-trace-mode -1)
    (user-error "Not in a chomp terminal buffer"))
  (let ((trace-buf (generate-new-buffer
                    (format "*chomp-trace %s*" (buffer-name)))))
    (setq chomp-trace--buffer trace-buf)
    ;; Write header
    (with-current-buffer trace-buf
      (when (fboundp 'lisp-data-mode) (lisp-data-mode))
      (insert ";; -*- mode: lisp-data -*-\n")
      (insert (format ";; Trace of %s started at %s\n"
                      (buffer-name) (current-time-string))))
    ;; Log initial state
    (when (bound-and-true-p chomp--screen)
      (chomp-trace--log nil 'create 'chomp
                        (chomp-screen-width chomp--screen)
                        (chomp-screen-height chomp--screen)))
    ;; Advise the I/O filter to capture output
    (when (bound-and-true-p chomp--io)
      (advice-add 'chomp-io--filter :around #'chomp-trace--filter-advice)
      (advice-add 'chomp-io-handle-resize :around #'chomp-trace--resize-advice)
      (advice-add 'chomp-render-refresh :around #'chomp-trace--refresh-advice))
    ;; Stop on kill
    (add-hook 'kill-buffer-hook #'chomp-trace--stop nil t)
    (message "Trace started: %s" (buffer-name trace-buf))))

(defun chomp-trace--stop ()
  "Stop tracing."
  (when chomp-trace--buffer
    (chomp-trace--log nil 'finish)
    (advice-remove 'chomp-io--filter #'chomp-trace--filter-advice)
    (advice-remove 'chomp-io-handle-resize #'chomp-trace--resize-advice)
    (advice-remove 'chomp-render-refresh #'chomp-trace--refresh-advice)
    (remove-hook 'kill-buffer-hook #'chomp-trace--stop t)
    (setq chomp-trace--buffer nil)))

;;;; ---- Advice Functions -----------------------------------------------

(defun chomp-trace--filter-advice (orig-fn io process output)
  "Trace output arriving from the process."
  (when-let ((buffer (chomp-io-buffer io)))
    (with-current-buffer buffer
      (when chomp-trace--buffer
        (chomp-trace--log nil 'output output))))
  (funcall orig-fn io process output))

(defun chomp-trace--resize-advice (orig-fn io new-width new-height)
  "Trace resize events."
  (when-let ((buffer (chomp-io-buffer io)))
    (with-current-buffer buffer
      (when chomp-trace--buffer
        (chomp-trace--log nil 'resize new-width new-height))))
  (funcall orig-fn io new-width new-height))

(defun chomp-trace--refresh-advice (orig-fn render)
  "Trace redisplay events."
  (with-current-buffer (chomp-render-state-buffer render)
    (when chomp-trace--buffer
      (chomp-trace--log nil 'redisplay)))
  (funcall orig-fn render))

(provide 'chomp-trace)
;;; chomp-trace.el ends here
