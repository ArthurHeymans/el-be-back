;;; ebb-trace.el --- Terminal I/O tracing for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Provides a minor mode for recording all terminal I/O to a trace buffer.
;; Useful for debugging terminal display issues and reporting bugs.
;;
;; Usage:  M-x ebb-trace-mode  (in a ebb buffer)
;; The trace buffer contains timestamped lisp-data entries for every
;; output, resize, and redisplay event.

;;; Code:

(require 'cl-lib)

;;;; ---- Trace State ----------------------------------------------------

(defvar-local ebb-trace--buffer nil
  "Buffer where trace data is written.")

(defvar-local ebb-trace--original-filter nil
  "Original process filter before tracing started.")

;;;; ---- Trace Logging --------------------------------------------------

(defun ebb-trace--log (time operation &rest args)
  "Log TIME, OPERATION, and ARGS to the trace buffer.
TIME defaults to current time."
  (when (and ebb-trace--buffer (buffer-live-p ebb-trace--buffer))
    (with-current-buffer ebb-trace--buffer
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
(define-minor-mode ebb-trace-mode
  "Toggle terminal I/O tracing for this ebb buffer.
When enabled, all terminal output, resize events, and redisplay
cycles are logged to a trace buffer."
  :lighter " Trace"
  :group 'ebb
  (if ebb-trace-mode
      (ebb-trace--start)
    (ebb-trace--stop)))

(defun ebb-trace--start ()
  "Start tracing the current ebb buffer."
  (unless (bound-and-true-p ebb--io)
    (ebb-trace-mode -1)
    (user-error "Not in a ebb terminal buffer"))
  (let ((trace-buf (generate-new-buffer
                    (format "*ebb-trace %s*" (buffer-name)))))
    (setq ebb-trace--buffer trace-buf)
    ;; Write header
    (with-current-buffer trace-buf
      (when (fboundp 'lisp-data-mode) (lisp-data-mode))
      (insert ";; -*- mode: lisp-data -*-\n")
      (insert (format ";; Trace of %s started at %s\n"
                      (buffer-name) (current-time-string))))
    ;; Log initial state
    (when (bound-and-true-p ebb--screen)
      (ebb-trace--log nil 'create 'ebb
                        (ebb-screen-width ebb--screen)
                        (ebb-screen-height ebb--screen)))
    ;; Advise the I/O filter to capture output
    (when (bound-and-true-p ebb--io)
      (advice-add 'ebb-io--filter :around #'ebb-trace--filter-advice)
      (advice-add 'ebb-io-handle-resize :around #'ebb-trace--resize-advice)
      (advice-add 'ebb-render-refresh :around #'ebb-trace--refresh-advice))
    ;; Stop on kill
    (add-hook 'kill-buffer-hook #'ebb-trace--stop nil t)
    (message "Trace started: %s" (buffer-name trace-buf))))

(defun ebb-trace--stop ()
  "Stop tracing."
  (when ebb-trace--buffer
    (ebb-trace--log nil 'finish)
    (advice-remove 'ebb-io--filter #'ebb-trace--filter-advice)
    (advice-remove 'ebb-io-handle-resize #'ebb-trace--resize-advice)
    (advice-remove 'ebb-render-refresh #'ebb-trace--refresh-advice)
    (remove-hook 'kill-buffer-hook #'ebb-trace--stop t)
    (setq ebb-trace--buffer nil)))

;;;; ---- Advice Functions -----------------------------------------------

(defun ebb-trace--filter-advice (orig-fn io process output)
  "Trace output arriving from the process."
  (when-let* ((buffer (ebb-io-buffer io)))
    (with-current-buffer buffer
      (when ebb-trace--buffer
        (ebb-trace--log nil 'output output))))
  (funcall orig-fn io process output))

(defun ebb-trace--resize-advice (orig-fn io new-width new-height)
  "Trace resize events."
  (when-let* ((buffer (ebb-io-buffer io)))
    (with-current-buffer buffer
      (when ebb-trace--buffer
        (ebb-trace--log nil 'resize new-width new-height))))
  (funcall orig-fn io new-width new-height))

(defun ebb-trace--refresh-advice (orig-fn render)
  "Trace redisplay events."
  (with-current-buffer (ebb-render-state-buffer render)
    (when ebb-trace--buffer
      (ebb-trace--log nil 'redisplay)))
  (funcall orig-fn render))

(defun ebb-trace--processing-error (io error-data count)
  "Trace ERROR-DATA from IO after COUNT consecutive failures."
  (when-let* ((buffer (ebb-io-buffer io)))
    (with-current-buffer buffer
      (when ebb-trace--buffer
        (ebb-trace--log nil 'processing-error count error-data)))))

(add-hook 'ebb-io-processing-error-functions #'ebb-trace--processing-error)

(provide 'ebb-trace)
;;; ebb-trace.el ends here
