;;; ebb-bench.el --- Compare Ebb, Eat, and Ghostel -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with bench/run-bench.sh.  This deliberately measures the same useful
;; boundary for each implementation: feed terminal output to its screen model
;; and materialize a buffer rendering.  It is not a PTY or GUI benchmark.
;; Native compilation is required by the launcher so the Elisp comparison is
;; not distorted by byte-code versus native-code execution.

;;; Code:

(require 'cl-lib)
(require 'ebb-term)
(require 'ebb-parse)
(require 'ebb-render)
(require 'eat)
(require 'ghostel)

(defvar ebb-bench-size (* 1024 1024))
(defvar ebb-bench-iterations 5)
(defvar ebb-bench-rows 24)
(defvar ebb-bench-cols 80)

(defun ebb-bench--plain (size)
  "Return about SIZE bytes of plain terminal output."
  (let ((line (concat (make-string 78 ?A) "\r\n"))
        (parts nil)
        (total 0))
    (while (< total size)
      (push line parts)
      (cl-incf total (length line)))
    (substring (apply #'concat (nreverse parts)) 0 size)))

(defun ebb-bench--sgr (size)
  "Return about SIZE bytes of colored terminal output."
  (let ((parts nil) (total 0) (color 0))
    (while (< total size)
      (let ((part (format "\e[38;5;%dmabcdefghij\e[0m\r\n" color)))
        (push part parts)
        (cl-incf total (length part))
        (setq color (mod (1+ color) 256))))
    (substring (apply #'concat (nreverse parts)) 0 size)))

(defun ebb-bench--tui (size)
  "Return a repeated TUI-like full-screen update stream."
  (let ((frame (concat "\e[2J\e[H"
                       (mapconcat
                        (lambda (row)
                          (format "\e[%d;1H\e[%dm%s"
                                  row (if (cl-evenp row) 44 42)
                                  (make-string ebb-bench-cols
                                               (if (cl-evenp row) ?- ?=))))
                        (number-sequence 1 ebb-bench-rows) "")
                       "\e[0m"))
        (parts nil) (total 0))
    (while (< total size)
      (push frame parts)
      (cl-incf total (length frame)))
    (substring (apply #'concat (nreverse parts)) 0 size)))

(defun ebb-bench--ebb (data)
  "Process DATA through Ebb's parser and renderer."
  (with-temp-buffer
    (let* ((screen (ebb-screen-create ebb-bench-cols ebb-bench-rows))
           (render (ebb-render-create screen (current-buffer)))
           (parser (ebb-parse-create screen)))
      (ebb-parse-bytes parser data)
      (ebb-render-refresh render))))

(defun ebb-bench--eat (data)
  "Process DATA through Eat's terminal and redisplay path."
  (with-temp-buffer
    (let ((term (eat-term-make (current-buffer) (point))))
      (eat-term-resize term ebb-bench-cols ebb-bench-rows)
      (eat-term-process-output term data)
      (eat-term-redisplay term)
      (eat-term-delete term))))

(defun ebb-bench--ghostel (data)
  "Process DATA through Ghostel's native VT and buffer renderer."
  (with-temp-buffer
    (ghostel-mode)
    (setq ghostel--term
          (ghostel--new ebb-bench-rows ebb-bench-cols (* 100 1024)))
    (if (fboundp 'ghostel--write-vt)
        (ghostel--write-vt ghostel--term data)
      ;; Compatibility with older released modules.
      (ghostel--debug-feed ghostel--term data))
    (let ((inhibit-read-only t)
          (inhibit-redisplay t)
          (inhibit-modification-hooks t)
          (gc-cons-threshold most-positive-fixnum))
      (ghostel--redraw ghostel--term nil))))

(defun ebb-bench--measure (name data fn)
  "Print and return NAME's timing for DATA and FN."
  (funcall fn data)
  (garbage-collect)
  (let ((start (float-time)))
    (dotimes (_ ebb-bench-iterations) (funcall fn data))
    (let* ((elapsed (- (float-time) start))
           (per (* 1000.0 (/ elapsed ebb-bench-iterations)))
           (mbs (/ (* (length data) ebb-bench-iterations)
                   elapsed 1048576.0)))
      (message "%-18s %8.2f ms/iter %8.2f MiB/s" name per mbs)
      (list name per mbs))))

(defun ebb-bench-run ()
  "Run all Ebb/Eat/Ghostel comparisons."
  (unless (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
    (error "Native compilation is required; use an Emacs with native-comp"))
  (let ((ghostel-available (or (fboundp 'ghostel--write-vt)
                               (fboundp 'ghostel--debug-feed))))
    (unless ghostel-available
      (message "Ghostel skipped: rebuild ../ghostel for a current native module"))
    (message "Ebb benchmark: %d bytes, %d iterations, %dx%d"
           ebb-bench-size ebb-bench-iterations ebb-bench-cols ebb-bench-rows)
  (dolist (case '(("plain" . ebb-bench--plain)
                  ("sgr" . ebb-bench--sgr)
                  ("tui" . ebb-bench--tui)))
    (let ((data (funcall (cdr case) ebb-bench-size)))
      (message "\n[%s]" (car case))
      (ebb-bench--measure (concat "ebb/" (car case)) data #'ebb-bench--ebb)
      (ebb-bench--measure (concat "eat/" (car case)) data #'ebb-bench--eat)
      (when ghostel-available
        (ebb-bench--measure (concat "ghostel/" (car case)) data
                            #'ebb-bench--ghostel))))
    (message "Done.")))

(provide 'ebb-bench)
;;; ebb-bench.el ends here
