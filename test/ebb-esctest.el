;;; ebb-esctest.el --- Run focused esctest2 coverage -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Headless integration test that runs a focused esctest2 compatibility suite
;; through Ebb's real PTY, parser, and response path.

;;; Code:

(require 'ebb-io)

(defconst ebb-esctest--include
  (concat
   "(APCTests.test_APC_Basic|BSTests|CHATests|CHTTests|CNLTests|CPLTests|"
   "CRTests|CUBTests|CUDTests|CUFTests|CUPTests|CUUTests|DCHTests|DLTests|"
   "DECALNTests|DECCRATests|DECERATests|DECFRATests|DECRQMTests|"
   "DECRQSSTests|DECSEDTests|DECSELTests|DECSERATests|"
   "ECHTests|EDTests|ELTests|"
   "ICHTests|ILTests|INDTests|LFTests|NELTests|RITests|"
   "DECSTRTests.test_DECSTR_(CursorStaysPut|DECSC|IRM|STBM)|"
   "RISTests.test_RIS_(ClearsScreen|CursorToOrigin|ResetTabs|ResetDECCOLM)|"
   "XtermWinopsTests.test_XtermWinops_ResizeChars_BothParameters)"))

(defun ebb-esctest--run ()
  "Run the focused esctest2 suite and signal an error on failure."
  (let* ((root (or (getenv "EBB_ESCTEST_DIR")
                   (error "EBB_ESCTEST_DIR is not set")))
         (test-dir (expand-file-name "esctest" root))
         (script (expand-file-name "esctest.py" test-dir))
         (include (or (getenv "EBB_ESCTEST_INCLUDE") ebb-esctest--include))
         (expected (or (getenv "EBB_ESCTEST_EXPECTED")
                       "258 tests passed, 28 known bugs, 0 tests failed"))
         (logfile (make-temp-file "ebb-esctest-" nil ".log"))
         (default-directory (file-name-as-directory test-dir))
         (ebb-enable-shell-integration nil)
         (screen (ebb-screen-create 80 25))
         io parser render process terminal-buffer)
    (unless (file-readable-p script)
      (error "Cannot read esctest2 runner: %s" script))
    (unwind-protect
        (progn
          (setq terminal-buffer (generate-new-buffer " *ebb-esctest*"))
          (setq render (ebb-render-create screen terminal-buffer))
          (setq parser
                (ebb-parse-create
                 screen nil
                 (lambda (type &rest _args)
                   (when (eq type 'resize-request)
                     (when-let* ((proc (and io (ebb-io-process io))))
                       (when (and (process-live-p proc)
                                  (ebb-io--pty-process-p proc))
                         (set-process-window-size
                          proc
                          (ebb-screen-height screen)
                          (ebb-screen-width screen))))
                     (when io
                       (ebb-render-full-reset (ebb-io-render io)))))))
          (setq io (make-ebb-io :screen screen :parser parser :render render))
          (setq process
                (ebb-io-start
                 io
                 (list "python3" script
                       "--expected-terminal=xterm"
                       (concat "--include=" include)
                       (concat "--logfile=" logfile)
                       "--no-print-logs")
                 terminal-buffer))
          (while (process-live-p process)
            (accept-process-output process 0.1))
          (when (ebb-io--pending-p io)
            (ebb-io--process-pending io t))
          (when (ebb-io-last-processing-error io)
            (error "Ebb processing failed: %S"
                   (ebb-io-last-processing-error io)))
          (let ((log (with-temp-buffer
                       (insert-file-contents logfile)
                       (buffer-string))))
            (princ log)
            (unless (string-match-p
                     (concat "\\*\\*\\* " (regexp-quote expected) " \\*\\*\\*")
                     log)
              (error "Focused esctest2 suite failed"))))
      (when (and process (process-live-p process))
        (delete-process process))
      (when (buffer-live-p terminal-buffer)
        (kill-buffer terminal-buffer))
      (ignore-errors (delete-file logfile)))))

(ebb-esctest--run)

;;; ebb-esctest.el ends here
