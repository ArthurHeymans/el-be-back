;;; ebb-bench.el --- Performance benchmarks for el-be-back -*- lexical-binding: t; -*-

;;; Commentary:

;; Compare terminal emulator performance: ebb (el-be-back), ghostel,
;; vterm, eat, and Emacs built-in term.
;;
;; Run via:  bench/run-bench.sh          (recommended)
;;       or: emacs --batch -Q -L . \
;;             -l bench/ebb-bench.el \
;;             --eval '(ebb-bench-run-all)'

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defvar ebb-bench-data-size (* 1024 1024)
  "Size of test data in bytes (default 1 MB).")

(defvar ebb-bench-iterations 3
  "Number of iterations per benchmark.")

(defvar ebb-bench-terminal-sizes '((24 . 80) (40 . 120))
  "List of (ROWS . COLS) to benchmark.")

(defvar ebb-bench-scrollback 1000
  "Scrollback lines for terminal creation.")

(defvar ebb-bench-include-ghostel t
  "When non-nil, include ghostel in benchmarks.")

(defvar ebb-bench-include-vterm t
  "When non-nil, include vterm in benchmarks.")

(defvar ebb-bench-include-eat t
  "When non-nil, include eat in benchmarks.")

(defvar ebb-bench-include-term t
  "When non-nil, include Emacs built-in term in benchmarks.")

(defvar ebb-bench-chunk-size 4096
  "Chunk size for streaming benchmarks.")

;; ---------------------------------------------------------------------------
;; Results accumulator
;; ---------------------------------------------------------------------------

(defvar ebb-bench--results nil
  "List of result plists from benchmark runs.")

;; ---------------------------------------------------------------------------
;; Data generators
;; ---------------------------------------------------------------------------

(defun ebb-bench--gen-plain-ascii (size)
  "Generate SIZE bytes of printable ASCII with CRLF every 80 chars."
  (let* ((line (concat (make-string 78 ?A) "\r\n"))
         (line-len (length line))
         (repeats (/ size line-len))
         (parts (make-list repeats line)))
    (apply #'concat parts)))

(defun ebb-bench--gen-sgr-styled (size)
  "Generate ~SIZE bytes with SGR color escapes every ~10 chars."
  (let ((parts nil)
        (total 0))
    (while (< total size)
      (let* ((color (% (/ total 10) 256))
             (esc (format "\e[38;5;%dm" color))
             (text "abcdefghij")
             (chunk (concat esc text)))
        (push chunk parts)
        (setq total (+ total (length chunk)))))
    (let ((result (apply #'concat (nreverse parts))))
      (substring result 0 (min (length result) size)))))

(defun ebb-bench--gen-unicode (size)
  "Generate ~SIZE bytes of CJK UTF-8 text as a multibyte string."
  (let* ((chars-needed (/ size 3))
         (line-chars 26)
         (lines (/ chars-needed line-chars))
         (parts nil))
    (dotimes (l lines)
      (dotimes (c line-chars)
        (push (string (+ #x4e00 (% (+ (* l 7) c) 256))) parts))
      (push "\r\n" parts))
    (apply #'concat (nreverse parts))))

(defun ebb-bench--gen-urls-and-paths (size)
  "Generate ~SIZE bytes of output containing URLs and file:line refs."
  (let ((lines '("/usr/src/app/main.c:42: error: undeclared identifier\r\n"
                 "  at Object.<anonymous> (/home/user/project/index.js:17:5)\r\n"
                 "See https://example.com/docs/errors/E0042 for details\r\n"
                 "PASS ./tests/test_utils.py:88 test_parse_url\r\n"
                 "warning: unused variable at ./src/render.zig:156:13\r\n"
                 "Download: https://cdn.example.org/releases/v2.1.0/pkg.tar.gz\r\n"
                 "  File \"/opt/lib/python3/site.py\", line 73, in main\r\n"
                 "More info: https://github.com/user/repo/issues/42\r\n"))
        (parts nil)
        (total 0))
    (while (< total size)
      (let ((line (nth (% (/ total 60) (length lines)) lines)))
        (push line parts)
        (setq total (+ total (length line)))))
    (apply #'concat (nreverse parts))))

(defun ebb-bench--gen-tui-frame (rows cols)
  "Generate a single TUI-style frame: clear + fill ROWS x COLS."
  (let ((parts (list "\e[2J\e[H")))
    (dotimes (r rows)
      (push (format "\e[%d;1H" (1+ r)) parts)
      (push (format "\e[%sm" (if (cl-evenp r) "44" "42")) parts)
      (push (make-string cols (if (cl-evenp r) ?- ?=)) parts))
    (push "\e[0m" parts)
    (apply #'concat (nreverse parts))))

;; ---------------------------------------------------------------------------
;; Data encoding helper
;; ---------------------------------------------------------------------------

(defun ebb-bench--encode-for-backend (data backend)
  "Encode DATA for BACKEND."
  (if (eq backend 'eat)
      (if (multibyte-string-p data) data
        (decode-coding-string data 'utf-8))
    (if (multibyte-string-p data)
        (encode-coding-string data 'utf-8)
      data)))

;; ---------------------------------------------------------------------------
;; Timing harness
;; ---------------------------------------------------------------------------

(defun ebb-bench--measure (name data-size iterations body-fn)
  "Run BODY-FN ITERATIONS times, record results under NAME.
DATA-SIZE is the byte count processed per iteration (for MB/s)."
  (garbage-collect)
  (funcall body-fn)  ; warm up
  (garbage-collect)
  (let ((actual-iters iterations))
    ;; Auto-scale fast operations
    (let ((trial-start (float-time)))
      (dotimes (_ (min 3 iterations))
        (funcall body-fn))
      (let ((trial-time (- (float-time) trial-start)))
        (when (< trial-time 0.01)
          (setq actual-iters (max iterations
                                  (* 10 (ceiling (/ 0.5 (max trial-time 1e-6)))))))))
    (garbage-collect)
    (let ((start (float-time)))
      (dotimes (_ actual-iters)
        (funcall body-fn))
      (let* ((elapsed (- (float-time) start))
             (per-iter (/ elapsed actual-iters))
             (throughput (if (> elapsed 0)
                             (/ (* data-size actual-iters) elapsed (expt 1024.0 2))
                           0.0))
             (result (list :name name
                           :iterations actual-iters
                           :total-time elapsed
                           :per-iter-ms (* per-iter 1000.0)
                           :data-size data-size
                           :throughput-mbs throughput)))
        (push result ebb-bench--results)
        (message "  %-50s %5d  %8.3f  %10.2f  %8.1f"
                 name actual-iters elapsed (* per-iter 1000.0) throughput)
        result))))

;; ---------------------------------------------------------------------------
;; Terminal creation helpers
;; ---------------------------------------------------------------------------

(defun ebb-bench--make-ebb (rows cols)
  "Create an ebb terminal for benchmarking."
  (ebb--new rows cols ebb-bench-scrollback))

(defun ebb-bench--make-ghostel (rows cols)
  "Create a ghostel terminal for benchmarking."
  (ghostel--new rows cols ebb-bench-scrollback))

(defun ebb-bench--make-vterm (rows cols)
  "Create a vterm terminal for benchmarking."
  (vterm--new rows cols ebb-bench-scrollback nil nil nil nil nil))

(defun ebb-bench--make-eat (rows cols)
  "Create an eat terminal for benchmarking."
  (let ((term (eat-term-make (current-buffer) (point))))
    (eat-term-resize term cols rows)
    (eat-term-set-parameter term 'input-function (lambda (_term _str)))
    term))

(defun ebb-bench--make-term (rows cols)
  "Set up current buffer for term-mode benchmarking."
  (term-mode)
  (setq term-width cols)
  (setq term-height rows)
  (setq term-buffer-maximum-size ebb-bench-scrollback)
  (let ((proc (start-process "term-bench" (current-buffer) "cat")))
    (set-process-query-on-exit-flag proc nil)
    proc))

;; =========================================================================
;; SECTION 1: PTY benchmark
;; =========================================================================

(defun ebb-bench--write-data-file (gen-fn)
  "Write data from GEN-FN to a temp file, return path."
  (let ((file (make-temp-file "ebb-bench-" nil ".bin")))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert (funcall gen-fn ebb-bench-data-size)))
    file))

(defun ebb-bench--pty-ebb (data-file &optional no-detect)
  "Benchmark ebb processing `cat DATA-FILE' through a real PTY."
  (with-temp-buffer
    (let* ((rows 24) (cols 80)
           (term (ebb-bench--make-ebb rows cols))
           (ebb-enable-url-detection (not no-detect))
           (ebb-enable-file-detection (not no-detect))
           (inhibit-read-only t)
           (redraw-timer nil)
           (pending nil)
           (done nil)
           (proc (make-process
                  :name "ebb-bench"
                  :buffer (current-buffer)
                  :command (list "cat" (expand-file-name data-file))
                  :connection-type 'pipe
                  :coding 'binary
                  :noquery t
                  :filter (lambda (_proc output)
                            (push output pending)
                            (unless redraw-timer
                              (setq redraw-timer
                                    (run-with-timer
                                     0.033 nil
                                     (lambda ()
                                       (setq redraw-timer nil)
                                       (let ((inhibit-read-only t))
                                         (when pending
                                           (ebb--feed
                                            term
                                            (apply #'concat (nreverse pending)))
                                           (setq pending nil))
                                         (ebb--render term)))))))
                  :sentinel (lambda (_proc _event)
                              (setq done t)))))
      (set-process-window-size proc rows cols)
      (while (not done)
        (accept-process-output proc 30))
      (when redraw-timer (cancel-timer redraw-timer))
      (when pending
        (ebb--feed term (apply #'concat (nreverse pending)))
        (setq pending nil))
      (ebb--render term)
      (ebb--free term))))

(defun ebb-bench--pty-ghostel (data-file &optional no-detect)
  "Benchmark ghostel processing `cat DATA-FILE' through a real PTY."
  (with-temp-buffer
    (let* ((rows 24) (cols 80)
           (term (ebb-bench--make-ghostel rows cols))
           (ghostel-enable-url-detection (not no-detect))
           (ghostel-enable-file-detection (not no-detect))
           (inhibit-read-only t)
           (redraw-timer nil)
           (pending nil)
           (done nil)
           (proc (make-process
                  :name "ghostel-bench"
                  :buffer (current-buffer)
                  :command (list "cat" (expand-file-name data-file))
                  :connection-type 'pipe
                  :coding 'binary
                  :noquery t
                  :filter (lambda (_proc output)
                            (push output pending)
                            (unless redraw-timer
                              (setq redraw-timer
                                    (run-with-timer
                                     0.033 nil
                                     (lambda ()
                                       (setq redraw-timer nil)
                                       (let ((inhibit-read-only t))
                                         (when pending
                                           (ghostel--write-input
                                            term
                                            (apply #'concat (nreverse pending)))
                                           (setq pending nil))
                                         (ghostel--redraw term ghostel-full-redraw)))))))
                  :sentinel (lambda (_proc _event)
                              (setq done t)))))
      (set-process-window-size proc rows cols)
      (while (not done)
        (accept-process-output proc 30))
      (when redraw-timer (cancel-timer redraw-timer))
      (when pending
        (ghostel--write-input term (apply #'concat (nreverse pending)))
        (setq pending nil))
      (ghostel--redraw term ghostel-full-redraw))))

(defun ebb-bench--pty-vterm (data-file)
  "Benchmark vterm processing `cat DATA-FILE'."
  (with-temp-buffer
    (let* ((rows 24) (cols 80)
           (term (ebb-bench--make-vterm rows cols))
           (redraw-timer nil)
           (done nil)
           (proc (make-process
                  :name "vterm-bench"
                  :buffer (current-buffer)
                  :command (list "cat" (expand-file-name data-file))
                  :connection-type 'pipe
                  :coding 'binary
                  :noquery t
                  :filter (lambda (_proc output)
                            (vterm--write-input term output)
                            (unless redraw-timer
                              (setq redraw-timer
                                    (run-with-timer
                                     0.033 nil
                                     (lambda ()
                                       (setq redraw-timer nil)
                                       (vterm--redraw term))))))
                  :sentinel (lambda (_proc _event)
                              (setq done t)))))
      (set-process-window-size proc rows cols)
      (while (not done)
        (accept-process-output proc 30))
      (when redraw-timer (cancel-timer redraw-timer))
      (vterm--redraw term))))

(defun ebb-bench--pty-eat (data-file)
  "Benchmark eat processing `cat DATA-FILE'."
  (with-temp-buffer
    (let* ((rows 24) (cols 80)
           (term (ebb-bench--make-eat rows cols))
           (inhibit-read-only t)
           (redraw-timer nil)
           (done nil)
           (proc (make-process
                  :name "eat-bench"
                  :buffer (current-buffer)
                  :command (list "cat" (expand-file-name data-file))
                  :connection-type 'pipe
                  :coding 'binary
                  :noquery t
                  :filter (lambda (_proc output)
                            (let ((inhibit-read-only t))
                              (eat-term-process-output
                               term
                               (decode-coding-string output 'utf-8)))
                            (unless redraw-timer
                              (setq redraw-timer
                                    (run-with-timer
                                     0.033 nil
                                     (lambda ()
                                       (setq redraw-timer nil)
                                       (let ((inhibit-read-only t))
                                         (eat-term-redisplay term)))))))
                  :sentinel (lambda (_proc _event)
                              (setq done t)))))
      (set-process-window-size proc rows cols)
      (while (not done)
        (accept-process-output proc 30))
      (when redraw-timer (cancel-timer redraw-timer))
      (eat-term-redisplay term)
      (eat-term-delete term))))

(defun ebb-bench--pty-term (data-file)
  "Benchmark Emacs built-in term processing `cat DATA-FILE'."
  (with-temp-buffer
    (term-mode)
    (setq term-width 80 term-height 24)
    (setq term-buffer-maximum-size ebb-bench-scrollback)
    (let* ((inhibit-read-only t)
           (done nil)
           (proc (make-process
                  :name "term-bench"
                  :buffer (current-buffer)
                  :command (list "cat" (expand-file-name data-file))
                  :connection-type 'pipe
                  :coding 'binary
                  :noquery t
                  :filter #'term-emulate-terminal
                  :sentinel (lambda (_proc _event)
                              (setq done t)))))
      (set-process-window-size proc 24 80)
      (while (not done)
        (accept-process-output proc 30)))))

(defun ebb-bench--run-pty-scenarios ()
  "Run real PTY benchmarks."
  (message "\n--- Real-World PTY Benchmark (cat %s through process pipe) ---"
           (ebb-bench--human-size ebb-bench-data-size))
  (message "  %-50s %5s  %8s  %10s  %8s" "SCENARIO" "ITERS" "TOTAL(s)" "ITER(ms)" "MB/s")
  (message "  %s" (make-string 90 ?-))
  ;; Plain ASCII
  (let ((data-file (ebb-bench--write-data-file #'ebb-bench--gen-plain-ascii)))
    (unwind-protect
        (progn
          (message "  [plain ASCII data]")
          (ebb-bench--measure
           "pty/plain/ebb" ebb-bench-data-size ebb-bench-iterations
           (lambda () (ebb-bench--pty-ebb data-file)))
          (when ebb-bench-include-ghostel
            (ebb-bench--measure
             "pty/plain/ghostel" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-ghostel data-file))))
          (when ebb-bench-include-vterm
            (ebb-bench--measure
             "pty/plain/vterm" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-vterm data-file))))
          (when ebb-bench-include-eat
            (ebb-bench--measure
             "pty/plain/eat" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-eat data-file))))
          (when ebb-bench-include-term
            (ebb-bench--measure
             "pty/plain/term" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-term data-file)))))
      (delete-file data-file)))
  ;; URL/path-heavy
  (let ((data-file (ebb-bench--write-data-file #'ebb-bench--gen-urls-and-paths)))
    (unwind-protect
        (progn
          (message "  [URL & file-path heavy data]")
          (ebb-bench--measure
           "pty/urls/ebb" ebb-bench-data-size ebb-bench-iterations
           (lambda () (ebb-bench--pty-ebb data-file)))
          (ebb-bench--measure
           "pty/urls/ebb-nodetect" ebb-bench-data-size ebb-bench-iterations
           (lambda () (ebb-bench--pty-ebb data-file t)))
          (when ebb-bench-include-ghostel
            (ebb-bench--measure
             "pty/urls/ghostel" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-ghostel data-file))))
          (when ebb-bench-include-vterm
            (ebb-bench--measure
             "pty/urls/vterm" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-vterm data-file))))
          (when ebb-bench-include-eat
            (ebb-bench--measure
             "pty/urls/eat" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-eat data-file))))
          (when ebb-bench-include-term
            (ebb-bench--measure
             "pty/urls/term" ebb-bench-data-size ebb-bench-iterations
             (lambda () (ebb-bench--pty-term data-file)))))
      (delete-file data-file))))

;; =========================================================================
;; SECTION 2: Streaming benchmark
;; =========================================================================

(defun ebb-bench--run-stream-scenarios ()
  "Run streaming benchmarks (chunked input with periodic redraws)."
  (message "\n--- Streaming (chunked write + periodic redraw, no PTY) ---")
  (message "  4KB chunks, redraw every 16 chunks (~64KB)")
  (message "  %-50s %5s  %8s  %10s  %8s" "SCENARIO" "ITERS" "TOTAL(s)" "ITER(ms)" "MB/s")
  (message "  %s" (make-string 90 ?-))
  (let* ((raw-data (ebb-bench--gen-plain-ascii ebb-bench-data-size))
         (chunk-size ebb-bench-chunk-size)
         (redraw-every 16))
    ;; ebb (separate feed + render)
    (with-temp-buffer
      (let* ((data (ebb-bench--encode-for-backend raw-data 'ebb))
             (data-len (length data))
             (term (ebb-bench--make-ebb 24 80))
             (inhibit-read-only t))
        (ebb-bench--measure
         "stream/ebb-sep" (string-bytes data) ebb-bench-iterations
         (lambda ()
           (let ((offset 0) (chunk-count 0))
             (while (< offset data-len)
               (let ((end (min (+ offset chunk-size) data-len)))
                 (ebb--feed term (substring data offset end))
                 (setq offset end)
                 (cl-incf chunk-count)
                 (when (zerop (% chunk-count redraw-every))
                   (ebb--render term)))))))))
    ;; ebb (combined feed+render — O1 optimization, same redraw cadence)
    (with-temp-buffer
      (let* ((data (ebb-bench--encode-for-backend raw-data 'ebb))
             (data-len (length data))
             (term (ebb-bench--make-ebb 24 80))
             (inhibit-read-only t))
        (ebb-bench--measure
         "stream/ebb-combined" (string-bytes data) ebb-bench-iterations
         (lambda ()
           (let ((offset 0) (chunk-count 0))
             (while (< offset data-len)
               (let ((end (min (+ offset chunk-size) data-len)))
                 ;; Feed without render for most chunks
                 (cl-incf chunk-count)
                 (if (zerop (% chunk-count redraw-every))
                     ;; Combined feed+render every 16th chunk
                     (ebb--feed-and-render term (substring data offset end))
                   ;; Feed only for intermediate chunks
                   (ebb--feed term (substring data offset end)))
                 (setq offset end))))))))
    ;; ghostel
    (when ebb-bench-include-ghostel
      (with-temp-buffer
        (let* ((data (ebb-bench--encode-for-backend raw-data 'ghostel))
               (data-len (length data))
               (term (ebb-bench--make-ghostel 24 80))
               (inhibit-read-only t))
          (ebb-bench--measure
           "stream/ghostel" (string-bytes data) ebb-bench-iterations
           (lambda ()
             (let ((offset 0) (chunk-count 0))
               (while (< offset data-len)
                 (let ((end (min (+ offset chunk-size) data-len)))
                   (ghostel--write-input term (substring data offset end))
                   (setq offset end)
                   (cl-incf chunk-count)
                   (when (zerop (% chunk-count redraw-every))
                     (ghostel--redraw term ghostel-full-redraw))))))))))
    ;; vterm
    (when ebb-bench-include-vterm
      (with-temp-buffer
        (let* ((data (ebb-bench--encode-for-backend raw-data 'vterm))
               (data-len (length data))
               (term (ebb-bench--make-vterm 24 80)))
          (ebb-bench--measure
           "stream/vterm" (string-bytes data) ebb-bench-iterations
           (lambda ()
             (let ((offset 0) (chunk-count 0))
               (while (< offset data-len)
                 (let ((end (min (+ offset chunk-size) data-len)))
                   (vterm--write-input term (substring data offset end))
                   (setq offset end)
                   (cl-incf chunk-count)
                   (when (zerop (% chunk-count redraw-every))
                     (vterm--redraw term))))))))))
    ;; eat
    (when ebb-bench-include-eat
      (with-temp-buffer
        (let* ((data (ebb-bench--encode-for-backend raw-data 'eat))
               (data-len (length data))
               (term (ebb-bench--make-eat 24 80))
               (inhibit-read-only t))
          (ebb-bench--measure
           "stream/eat" (string-bytes data) ebb-bench-iterations
           (lambda ()
             (let ((offset 0) (chunk-count 0))
               (while (< offset data-len)
                 (let ((end (min (+ offset chunk-size) data-len)))
                   (eat-term-process-output term (substring data offset end))
                   (setq offset end)
                   (cl-incf chunk-count)
                   (when (zerop (% chunk-count redraw-every))
                     (eat-term-redisplay term)))))))
          (eat-term-delete term))))
    ;; term
    (when ebb-bench-include-term
      (with-temp-buffer
        (let* ((data (ebb-bench--encode-for-backend raw-data 'term))
               (data-len (length data))
               (proc (ebb-bench--make-term 24 80))
               (inhibit-read-only t))
          (ebb-bench--measure
           "stream/term" (string-bytes data) ebb-bench-iterations
           (lambda ()
             (let ((offset 0))
               (while (< offset data-len)
                 (let ((end (min (+ offset chunk-size) data-len)))
                   (term-emulate-terminal proc (substring data offset end))
                   (setq offset end))))))
          (delete-process proc))))))

;; =========================================================================
;; SECTION 3: TUI frame rendering
;; =========================================================================

(defun ebb-bench--run-tui-scenarios ()
  "Benchmark TUI-style full-screen rewrites."
  (message "\n--- TUI Frame Rendering (full-screen rewrites) ---")
  (message "  %-50s %5s  %8s  %10s  %8s" "SCENARIO" "ITERS" "TOTAL(s)" "ITER(ms)" "fps")
  (message "  %s" (make-string 90 ?-))
  (let ((tui-iterations (* ebb-bench-iterations 20)))
    (dolist (size ebb-bench-terminal-sizes)
      (let* ((rows (car size))
             (cols (cdr size))
             (raw-frame (ebb-bench--gen-tui-frame rows cols))
             (label (format "%dx%d" rows cols)))
        ;; ebb (combined feed+render)
        (with-temp-buffer
          (let ((frame (ebb-bench--encode-for-backend raw-frame 'ebb))
                (term (ebb-bench--make-ebb rows cols))
                (inhibit-read-only t))
            (let ((result
                   (ebb-bench--measure
                    (format "tui-frame/ebb/%s" label)
                    (string-bytes frame) tui-iterations
                    (lambda ()
                      (ebb--feed-and-render term frame)))))
              (message "    ^ %.0f fps" (/ 1000.0 (plist-get result :per-iter-ms))))))
        ;; ghostel
        (when ebb-bench-include-ghostel
          (with-temp-buffer
            (let ((frame (ebb-bench--encode-for-backend raw-frame 'ghostel))
                  (term (ebb-bench--make-ghostel rows cols))
                  (inhibit-read-only t))
              (let ((result
                     (ebb-bench--measure
                      (format "tui-frame/ghostel/%s" label)
                      (string-bytes frame) tui-iterations
                      (lambda ()
                        (ghostel--write-input term frame)
                        (ghostel--redraw term ghostel-full-redraw)))))
                (message "    ^ %.0f fps" (/ 1000.0 (plist-get result :per-iter-ms)))))))
        ;; vterm
        (when ebb-bench-include-vterm
          (with-temp-buffer
            (let ((frame (ebb-bench--encode-for-backend raw-frame 'vterm))
                  (term (ebb-bench--make-vterm rows cols)))
              (let ((result
                     (ebb-bench--measure
                      (format "tui-frame/vterm/%s" label)
                      (string-bytes frame) tui-iterations
                      (lambda ()
                        (vterm--write-input term frame)
                        (vterm--redraw term)))))
                (message "    ^ %.0f fps" (/ 1000.0 (plist-get result :per-iter-ms)))))))
        ;; eat
        (when ebb-bench-include-eat
          (with-temp-buffer
            (let ((frame (ebb-bench--encode-for-backend raw-frame 'eat))
                  (term (ebb-bench--make-eat rows cols))
                  (inhibit-read-only t))
              (let ((result
                     (ebb-bench--measure
                      (format "tui-frame/eat/%s" label)
                      (string-bytes frame) tui-iterations
                      (lambda ()
                        (eat-term-process-output term frame)
                        (eat-term-redisplay term)))))
                (message "    ^ %.0f fps" (/ 1000.0 (plist-get result :per-iter-ms))))
              (eat-term-delete term))))
        ;; term
        (when ebb-bench-include-term
          (with-temp-buffer
            (let* ((frame (ebb-bench--encode-for-backend raw-frame 'term))
                   (proc (ebb-bench--make-term rows cols))
                   (inhibit-read-only t))
              (let ((result
                     (ebb-bench--measure
                      (format "tui-frame/term/%s" label)
                      (string-bytes frame) tui-iterations
                      (lambda ()
                        (term-emulate-terminal proc frame)))))
                (message "    ^ %.0f fps" (/ 1000.0 (plist-get result :per-iter-ms))))
              (delete-process proc))))))))

;; ---------------------------------------------------------------------------
;; Header / summary
;; ---------------------------------------------------------------------------

(defun ebb-bench--human-size (bytes)
  "Format BYTES as a human-readable string."
  (cond
   ((>= bytes (* 1024 1024)) (format "%.1f MB" (/ bytes (expt 1024.0 2))))
   ((>= bytes 1024) (format "%.0f KB" (/ bytes 1024.0)))
   (t (format "%d B" bytes))))

(defun ebb-bench--print-header ()
  "Print benchmark header."
  (message "")
  (message "=== EBB Performance Benchmark Suite ===")
  (message "")
  (message "  Date:       %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
  (message "  Emacs:      %s" emacs-version)
  (message "  Data size:  %s" (ebb-bench--human-size ebb-bench-data-size))
  (message "  Iterations: %d" ebb-bench-iterations)
  (message "  Scrollback: %d" ebb-bench-scrollback)
  (message "  Backends:   ebb%s%s%s%s"
           (if ebb-bench-include-ghostel ", ghostel" "")
           (if ebb-bench-include-vterm ", vterm" "")
           (if ebb-bench-include-eat ", eat" "")
           (if ebb-bench-include-term ", term" ""))
  (message ""))

(defun ebb-bench--print-summary ()
  "Print summary with PTY results highlighted."
  (message "\n=== Summary ===")
  (let ((pty-results
         (cl-remove-if-not
          (lambda (r) (string-prefix-p "pty/" (plist-get r :name)))
          ebb-bench--results)))
    (when pty-results
      (message "\n  Real-world PTY throughput (cat %s):"
               (ebb-bench--human-size ebb-bench-data-size))
      (dolist (r (sort (copy-sequence pty-results)
                       (lambda (a b) (string< (plist-get a :name)
                                              (plist-get b :name)))))
        (message "    %-40s %8.0f ms  %6.1f MB/s"
                 (plist-get r :name)
                 (plist-get r :per-iter-ms)
                 (plist-get r :throughput-mbs)))))
  (message "\nDone."))

;; ---------------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------------

(defun ebb-bench--load-backends ()
  "Load available backends."
  (require 'el-be-back)
  (when ebb-bench-include-ghostel
    (condition-case err
        (require 'ghostel)
      (error
       (message "WARNING: ghostel not available, skipping (%s)" (error-message-string err))
       (setq ebb-bench-include-ghostel nil))))
  (when ebb-bench-include-vterm
    (condition-case err
        (require 'vterm)
      (error
       (message "WARNING: vterm not available, skipping (%s)" (error-message-string err))
       (setq ebb-bench-include-vterm nil))))
  (when ebb-bench-include-eat
    (condition-case err
        (require 'eat)
      (error
       (message "WARNING: eat not available, skipping (%s)" (error-message-string err))
       (setq ebb-bench-include-eat nil))))
  (when ebb-bench-include-term
    (condition-case err
        (require 'term)
      (error
       (message "WARNING: term not available, skipping (%s)" (error-message-string err))
       (setq ebb-bench-include-term nil)))))

(defun ebb-bench-run-all ()
  "Run all benchmarks and print results."
  (ebb-bench--load-backends)
  (setq ebb-bench--results nil)
  (ebb-bench--print-header)
  (ebb-bench--run-pty-scenarios)
  (ebb-bench--run-stream-scenarios)
  (ebb-bench--run-tui-scenarios)
  (ebb-bench--print-summary))

(defun ebb-bench-run-quick ()
  "Run a quick subset: smaller data, fewer iterations, single size."
  (setq ebb-bench-data-size (* 100 1024))  ; 100 KB
  (setq ebb-bench-iterations 2)
  (setq ebb-bench-terminal-sizes '((24 . 80)))
  (ebb-bench-run-all))

(provide 'ebb-bench)
;;; ebb-bench.el ends here
