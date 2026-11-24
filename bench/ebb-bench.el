;;; ebb-bench.el --- Compare Ebb, Eat, and Ghostel -*- lexical-binding: t; -*-

;;; Commentary:
;; Run the cross-terminal comparison with bench/run-bench.sh, or the
;; Ebb-only benchmark with bench/run-ebb-bench.sh (used by CI to gate
;; pull requests on performance regressions against their base).
;; Both measure the same useful boundary for each implementation:
;; feed terminal output to its screen model and materialize a buffer
;; rendering.  It is not a PTY or GUI benchmark.  The launchers
;; compile the Elisp first so the comparison is not distorted by
;; interpreted versus compiled execution.

;;; Code:

(require 'cl-lib)
(require 'ebb-term)
(require 'ebb-parse)
(require 'ebb-render)
;; Eat and Ghostel are only needed for the comparison runs; the
;; Ebb-only path (CI) must load without them.
(require 'eat nil t)
(require 'ghostel nil t)

(defvar ebb-bench-size (* 1024 1024))
(defvar ebb-bench-iterations 5)
(defvar ebb-bench-rows 24)
(defvar ebb-bench-cols 80)
(defvar ebb-bench-output-file nil
  "When non-nil, also write results as TSV to this file.
Each line is \"NAME<TAB>ms-per-iter<TAB>MiB/s\"; bench/compare.py
consumes this format.")

(defun ebb-bench--to-pty-string (s)
  "Return S decoded as `utf-8-unix', matching Emacs PTY filter output."
  (decode-coding-string (encode-coding-string s 'utf-8-unix) 'utf-8-unix))

(defun ebb-bench--repeat-to-size (chunk size)
  "Repeat CHUNK until at least SIZE bytes, then truncate cleanly."
  (let ((parts nil)
        (total 0)
        (chunk-bytes (string-bytes chunk)))
    (while (< total size)
      (push chunk parts)
      (cl-incf total chunk-bytes))
    (let* ((joined (apply #'concat (nreverse parts)))
           (limit (min (length joined) size)))
      (ebb-bench--to-pty-string
       (if (= (length joined) (string-bytes joined))
           (substring joined 0 limit)
         joined)))))

(defun ebb-bench--plain (size)
  "Return about SIZE bytes of plain terminal output."
  (ebb-bench--repeat-to-size (concat (make-string 78 ?A) "\r\n") size))

(defun ebb-bench--sgr (size)
  "Return about SIZE bytes of colored terminal output."
  (let ((parts nil) (total 0) (color 0))
    (while (< total size)
      (let ((part (format "\e[38;5;%dmabcdefghij\e[0m\r\n" color)))
        (push part parts)
        (cl-incf total (length part))
        (setq color (mod (1+ color) 256))))
    (ebb-bench--to-pty-string
     (substring (apply #'concat (nreverse parts)) 0 size))))

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
                       "\e[0m")))
    (ebb-bench--repeat-to-size frame size)))

(defun ebb-bench--checker (size)
  "Return a repeated full-screen stream with per-cell attributes.
Alternating colors per column maximize attribute runs per row, and
each frame begins with ED 2, so run-heavy paths (text-run writes,
ED history signatures) cannot hide behind the uniform-attribute
rows of the tui case."
  (let ((frame (concat "\e[2J\e[H"
                       (mapconcat
                        (lambda (row)
                          (concat
                           (mapconcat (lambda (col)
                                        (if (cl-evenp (+ row col))
                                            "\e[31ma" "\e[34ma"))
                                      (number-sequence 1 ebb-bench-cols) "")
                           "\e[0m\r\n"))
                        (number-sequence 1 ebb-bench-rows) ""))))
    (ebb-bench--repeat-to-size frame size)))

(defun ebb-bench--gcc-utf8 (size)
  "Return compiler-like diagnostic output with curly quotes on each line."
  (ebb-bench--repeat-to-size
   "src/foo.c:42:9: warning: unused variable \u2018counter\u2019 [-Wunused-variable]\r\n"
   size))

(defun ebb-bench--box-utf8 (size)
  "Return TUI-like rows framed with UTF-8 box-drawing glyphs."
  (let* ((inner (make-string (max 1 (- ebb-bench-cols 4)) ?-))
         (row (format "\u2502 %s \u2502\r\n" inner)))
    (ebb-bench--repeat-to-size row size)))

(defun ebb-bench--colorlog (size)
  "Return colored log output with timestamps, levels, paths, and URLs."
  (ebb-bench--repeat-to-size
   "\e[90m2026-04-12 10:00:01\e[0m \e[32mINFO\e[0m src/foo/bar.c:42 rate=12.4 MiB/s https://example.com/a\r\n"
   size))

(defun ebb-bench--alt-tui (size)
  "Return an alt-screen TUI stream using EL (\\e[K), SGR, and DECTCEM."
  (let ((frame (concat "\e[?25l\e[H"
                       (mapconcat
                        (lambda (row)
                          (format "\e[%d;1H\e[%dm row %02d status: ok\e[0m\e[K"
                                  row (if (cl-evenp row) 36 33) row))
                        (number-sequence 1 ebb-bench-rows) "")
                       "\e[?25h")))
    (concat (ebb-bench--to-pty-string "\e[?1049h")
            (ebb-bench--repeat-to-size frame size))))

(defun ebb-bench--ebb (data)
  "Process DATA through Ebb's parser and renderer."
  (with-temp-buffer
    (let* ((screen (ebb-screen-create ebb-bench-cols ebb-bench-rows))
           (render (ebb-render-create screen (current-buffer)))
           (parser (ebb-parse-create screen)))
      (ebb-parse-bytes parser data)
      (ebb-render-refresh render))))

(defun ebb-bench--ebb-chunked (data)
  "Process DATA in 4 KiB slices with a render every 16 KiB."
  (with-temp-buffer
    (let* ((screen (ebb-screen-create ebb-bench-cols ebb-bench-rows))
           (render (ebb-render-create screen (current-buffer)))
           (parser (ebb-parse-create screen))
           (len (length data))
           (pos 0)
           (chunks 0))
      (while (< pos len)
        (let ((end (min len (+ pos 4096))))
          (ebb-parse-bytes parser data pos end)
          (setq pos end)
          (cl-incf chunks)
          (when (= (mod chunks 4) 0)
            (ebb-render-refresh render))))
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
  "Print and return NAME's timing for DATA and FN.
Each iteration is timed separately (after a GC) and the median is
reported, so one GC pause or scheduling hiccup does not move the
result -- important when CI compares two runs on the same machine."
  (funcall fn data)                     ; warm up
  (let ((times nil))
    (dotimes (_ ebb-bench-iterations)
      (garbage-collect)
      (let ((start (float-time)))
        (funcall fn data)
        (push (- (float-time) start) times)))
    (setq times (sort times #'<))
    (let* ((elapsed (nth (/ (length times) 2) times))
           (per (* 1000.0 elapsed))
           (mbs (/ (string-bytes data) elapsed 1048576.0)))
      (message "%-22s %8.2f ms/iter %8.2f MiB/s" name per mbs)
      (when ebb-bench-output-file
        (write-region (format "%s\t%.3f\t%.3f\n" name per mbs) nil
                      ebb-bench-output-file t))
      (list name per mbs))))

(defun ebb-bench-run (&optional only)
  "Run the benchmark cases.
With ONLY `ebb', benchmark Ebb alone (no Eat/Ghostel needed, no
native-compilation requirement): this is what CI runs on both the
pull request and its base.  Otherwise run the full comparison."
  (unless (eq only 'ebb)
    (unless (and (fboundp 'native-comp-available-p)
                 (native-comp-available-p))
      (error "Native compilation is required; use an Emacs with native-comp")))
  (let ((ghostel-available (and (not (eq only 'ebb))
                                (or (fboundp 'ghostel--write-vt)
                                    (fboundp 'ghostel--debug-feed)))))
    (when (and (not (eq only 'ebb)) (not ghostel-available))
      (message "Ghostel skipped: rebuild ../ghostel for a current native module"))
    (message "Ebb benchmark: %d bytes, %d iterations, %dx%d"
           ebb-bench-size ebb-bench-iterations ebb-bench-cols ebb-bench-rows)
    (when ebb-bench-output-file
      (write-region "" nil ebb-bench-output-file nil 'silent))
    (dolist (case '(("plain" . ebb-bench--plain)
                    ("sgr" . ebb-bench--sgr)
                    ("tui" . ebb-bench--tui)
                    ("checker" . ebb-bench--checker)
                    ("gcc-utf8" . ebb-bench--gcc-utf8)
                    ("box-utf8" . ebb-bench--box-utf8)
                    ("colorlog" . ebb-bench--colorlog)
                    ("alt-tui" . ebb-bench--alt-tui)))
      (let ((data (funcall (cdr case) ebb-bench-size)))
        (message "\n[%s]" (car case))
        (ebb-bench--measure (concat "ebb/" (car case)) data #'ebb-bench--ebb)
        (unless (eq only 'ebb)
          (ebb-bench--measure (concat "eat/" (car case)) data #'ebb-bench--eat)
          (when ghostel-available
            (ebb-bench--measure (concat "ghostel/" (car case)) data
                                #'ebb-bench--ghostel)))))
    (let ((data (ebb-bench--plain ebb-bench-size)))
      (message "\n[plain-chunked]")
      (ebb-bench--measure "ebb/plain-chunked" data #'ebb-bench--ebb-chunked))
    (message "Done.")))

(provide 'ebb-bench)
;;; ebb-bench.el ends here
