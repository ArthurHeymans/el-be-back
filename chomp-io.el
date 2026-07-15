;;; chomp-io.el --- Async process I/O for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Manages the PTY process and async parse/render pipeline.
;; The process filter only appends to a queue.  Parsing happens in
;; bounded chunks driven by timers, ensuring the main thread never hangs.

;;; Code:

(require 'cl-lib)
(require 'chomp-term)
(require 'chomp-parse)
(require 'chomp-render)

;;;; ---- Data Structure -------------------------------------------------

(cl-defstruct (chomp-io (:copier nil))
  "Async I/O state for a chomp terminal."
  (process nil)           ; the PTY process
  (parser nil)            ; chomp-parser
  (screen nil)            ; chomp-screen
  (render nil)            ; chomp-render-state
  (buffer nil)            ; Emacs buffer
  ;; Output queue
  (pending-chunks nil)    ; unprocessed output chunks, oldest first
  (pending-tail nil)      ; tail cons of pending-chunks for O(1) append
  (pending-offset 0)      ; parse position in the head chunk
  ;; Processing state
  (processing nil)        ; non-nil when parse loop is active
  (chunk-size 4096)       ; bytes per parse chunk
  ;; Latency management
  (min-latency 0.008)     ; seconds: min delay before render
  (max-latency 0.033)     ; seconds: max delay (~30fps)
  (first-chunk-time nil)  ; time of first unrendered chunk
  (render-timer nil)      ; pending render timer
  ;; Throughput monitoring
  (throughput-time nil)
  (throughput-bytes 0)
  (binary-flood nil))

;;;; ---- Customization --------------------------------------------------

(defcustom chomp-chunk-size 4096
  "Number of bytes to parse per chunk before yielding to the event loop."
  :type 'integer
  :group 'chomp)

(defcustom chomp-minimum-latency 0.008
  "Minimum seconds to wait before rendering after output arrives.
Allows batching of rapid output."
  :type 'number
  :group 'chomp)

(defcustom chomp-maximum-latency 0.033
  "Maximum seconds to wait before rendering after output arrives.
Ensures responsiveness."
  :type 'number
  :group 'chomp)

;;;; ---- Process Filter (never blocks) ----------------------------------

(defun chomp-io--filter (io _process output)
  "Process filter.  Only appends to queue.  Never parses."
  (chomp-io--enqueue-output io output)
  ;; Record time of first unprocessed chunk
  (unless (chomp-io-first-chunk-time io)
    (setf (chomp-io-first-chunk-time io) (current-time)))
  ;; Schedule processing
  (unless (chomp-io-processing io)
    (chomp-io--schedule-processing io)))

(defun chomp-io--enqueue-output (io output)
  "Append OUTPUT to IO's pending chunk queue without copying old data."
  (unless (zerop (length output))
    (let ((cell (list output)))
      (if (chomp-io-pending-tail io)
          (setcdr (chomp-io-pending-tail io) cell)
        (setf (chomp-io-pending-chunks io) cell))
      (setf (chomp-io-pending-tail io) cell))))

(defun chomp-io--normalize-pending (io)
  "Drop fully consumed head chunks from IO's pending queue."
  (while (and (chomp-io-pending-chunks io)
              (>= (chomp-io-pending-offset io)
                  (length (car (chomp-io-pending-chunks io)))))
    (setf (chomp-io-pending-chunks io)
          (cdr (chomp-io-pending-chunks io)))
    (setf (chomp-io-pending-offset io) 0))
  (unless (chomp-io-pending-chunks io)
    (setf (chomp-io-pending-tail io) nil)))

(defun chomp-io--pending-p (io)
  "Return non-nil if IO has pending output."
  (chomp-io--normalize-pending io)
  (chomp-io-pending-chunks io))

;;;; ---- Chunked Processing ---------------------------------------------

(defun chomp-io--schedule-processing (io)
  "Schedule the next parse+render cycle."
  (let* ((now (current-time))
         (first (chomp-io-first-chunk-time io))
         (elapsed (if first (float-time (time-subtract now first)) 0))
         (time-left (- (chomp-io-max-latency io) elapsed))
         (delay (if (<= time-left 0) 0
                  (min time-left (chomp-io-min-latency io)))))
    ;; Cancel existing timer
    (when (chomp-io-render-timer io)
      (cancel-timer (chomp-io-render-timer io)))
    ;; Schedule via timer
    (setf (chomp-io-render-timer io)
          (run-at-time delay nil #'chomp-io--process-pending io))))

(defun chomp-io--process-pending (io &optional drain-all)
  "Process pending output: parse chunks, then render."
  (setf (chomp-io-processing io) t)
  (when (chomp-io-render-timer io)
    (cancel-timer (chomp-io-render-timer io)))
  (setf (chomp-io-render-timer io) nil)
  (condition-case err
      (when (buffer-live-p (chomp-io-buffer io))
        (with-current-buffer (chomp-io-buffer io)
          (let ((budget (chomp-io-chunk-size io))
                (total-parsed 0)
                (max-parsed (if drain-all most-positive-fixnum
                              (* (chomp-io-chunk-size io) 4))))
            ;; Parse in chunks up to budget
            (while (and (< total-parsed max-parsed)
                        (chomp-io--pending-p io))
              (let* ((bytes (car (chomp-io-pending-chunks io)))
                     (offset (chomp-io-pending-offset io))
                     (remaining (- (length bytes) offset))
                     (chunk-size (min remaining budget)))
                ;; Binary flood detection
                (chomp-io--update-throughput io chunk-size)
                ;; Even in flood mode, never silently drop terminal output.
                ;; The flag is diagnostic/throttling state; parsing remains
                ;; lossless unless a future user option explicitly suppresses it.
                (let ((consumed (chomp-parse-bytes
                                 (chomp-io-parser io)
                                 bytes offset (+ offset chunk-size))))
                  (cl-incf (chomp-io-pending-offset io) consumed))
                (cl-incf total-parsed chunk-size)))
            (chomp-io--normalize-pending io)

            ;; Render
            (chomp-render-refresh (chomp-io-render io))
            ;; Post-render shell integration (prompt annotations, etc.)
            (when (fboundp 'chomp-shell-post-render)
              (chomp-shell-post-render (chomp-io-render io)))
            (when (fboundp 'chomp--detect-password-prompt)
              (chomp--detect-password-prompt))))

        ;; Reset latency tracking
        (setf (chomp-io-first-chunk-time io) nil)

        ;; Check for more pending data
        (setf (chomp-io-processing io) nil)
        (when (chomp-io--pending-p io)
          (setf (chomp-io-first-chunk-time io) (current-time))
          (chomp-io--schedule-processing io)))
    (error
     (setf (chomp-io-processing io) nil)
     (message "[chomp-io] Processing error: %S" err))))

;;;; ---- Binary Flood Detection -----------------------------------------

(defun chomp-io--update-throughput (io chunk-bytes)
  "Track throughput and detect binary floods."
  (let ((now (float-time)))
    (when (or (null (chomp-io-throughput-time io))
              (> (- now (chomp-io-throughput-time io)) 1.0))
      ;; Reset window
      (setf (chomp-io-throughput-time io) now)
      (setf (chomp-io-throughput-bytes io) 0))
    (cl-incf (chomp-io-throughput-bytes io) chunk-bytes)
    ;; Flood if > 1MB/sec
    (setf (chomp-io-binary-flood io)
          (> (chomp-io-throughput-bytes io) 1048576))))

;;;; ---- Command Building -----------------------------------------------

(defcustom chomp-enable-shell-integration t
  "If non-nil, automatically source shell integration scripts.
When enabled, chomp wraps the shell command to source the appropriate
integration script for the detected shell."
  :type 'boolean
  :group 'chomp)

(defun chomp-io--detect-shell (shell-command)
  "Detect the shell type from SHELL-COMMAND.
Returns one of `bash', `zsh', `fish', or nil."
  (let ((cmd (if (listp shell-command)
                 (car shell-command)
               shell-command)))
    (cond
     ((string-match-p "\\(?:/\\|\\`\\)bash\\'" cmd) 'bash)
     ((string-match-p "\\(?:/\\|\\`\\)zsh\\'" cmd) 'zsh)
     ((string-match-p "\\(?:/\\|\\`\\)fish\\'" cmd) 'fish)
     (t nil))))

(defun chomp-io--integration-script (shell-type extra-env)
  "Find the integration script for SHELL-TYPE.
Returns the path to the script or nil.
EXTRA-ENV is the env var list (used to find integration dir)."
  (when-let* ((dir (cl-loop for var in (or extra-env nil)
                            when (string-prefix-p
                                  "CHOMP_SHELL_INTEGRATION_DIR=" var)
                            return (substring
                                    var
                                    (length "CHOMP_SHELL_INTEGRATION_DIR="))))
              (name (pcase shell-type
                      ('bash "bash") ('zsh "zsh") ('fish "fish") (_ nil)))
              (f (expand-file-name name dir)))
    (when (file-exists-p f) f)))

(defun chomp-io--environment-value (name environment)
  "Return NAME's value from ENVIRONMENT, a list of NAME=VALUE strings."
  (let ((prefix (concat name "=")))
    (cl-loop for entry in environment
             when (string-prefix-p prefix entry)
             return (substring entry (length prefix)))))

(defun chomp-io--prepare-environment (shell-command extra-env)
  "Prepare EXTRA-ENV for SHELL-COMMAND startup integration."
  (let* ((shell-type (chomp-io--detect-shell shell-command))
         (script (and chomp-enable-shell-integration
                      (chomp-io--integration-script shell-type extra-env))))
    (if (and (eq shell-type 'zsh) script)
        (let* ((environment (append extra-env process-environment))
               (old-entry (cl-find-if
                           (lambda (entry)
                             (string-prefix-p "ZDOTDIR=" entry))
                           environment))
               (old-zdotdir (and old-entry
                                  (substring old-entry (length "ZDOTDIR="))))
               (bootstrap (expand-file-name
                           "zsh-bootstrap"
                           (file-name-directory script))))
          (append (list (concat "ZDOTDIR=" bootstrap)
                        (concat "CHOMP_ZSH_ZDOTDIR=" (or old-zdotdir ""))
                        (concat "CHOMP_ZSH_ZDOTDIR_SET="
                                (if old-entry "1" "0")))
                  extra-env))
      extra-env)))

(defun chomp-io--build-command (shell-command extra-env)
  "Build a process command list for SHELL-COMMAND.
If `chomp-enable-shell-integration' is non-nil and an integration
script is available, wraps the command to source it.
EXTRA-ENV is the env var list (used to find integration dir)."
  (let* ((command (if (listp shell-command)
                      shell-command
                    (list shell-command)))
         (program (car command))
         (args (cdr command))
         (shell-type (chomp-io--detect-shell command))
         (script (when chomp-enable-shell-integration
                   (chomp-io--integration-script shell-type extra-env))))
    (pcase shell-type
      ;; Fish: set up XDG_CONFIG_HOME to auto-source integration.
      ('fish
       (when script
         (chomp-io--fish-setup-confd script))
       command)
      ;; Bash: --rcfile must precede user-provided short options.
      ('bash
       (if script
           (append (list program "--rcfile" (chomp-io--bash-rcfile script))
                   args)
         command))
      ;; Zsh integration is sourced by the user's .zshrc for now.
      ('zsh command)
      (_ command))))

(defun chomp-io--wrap-command-with-stty (command rows columns)
  "Wrap COMMAND to initialize PTY termios before exec.
Eat does this with `stty ... sane' before starting the client program; in
particular it makes the erase character match TERM's kbs=^?, which keeps
Backspace working in shells and line editors."
  (append
   (list "/usr/bin/env" "sh" "-c"
         (format "stty -nl echo rows %d columns %d sane 2>%s; if [ \"$1\" = .. ]; then shift; fi; exec \"$@\""
                 rows columns null-device)
         "..")
   command))

(defun chomp-io--bash-rcfile (integration-script)
  "Create a temporary rcfile that sources ~/.bashrc and INTEGRATION-SCRIPT.
Returns the path to the temporary file."
  (let ((tmpfile (make-temp-file "chomp-bashrc-")))
    (with-temp-file tmpfile
      (insert "# Chomp shell integration wrapper\n")
      (insert "if [ -f ~/.bashrc ]; then . ~/.bashrc; fi\n")
      (insert (format ". %s\n" (shell-quote-argument integration-script))))
    tmpfile))

(defun chomp-io--fish-setup-confd (integration-script)
  "Install a fish conf.d snippet that sources INTEGRATION-SCRIPT.
Fish automatically sources all .fish files in ~/.config/fish/conf.d/
on startup.  We create a symlink there pointing to our integration script."
  (ignore integration-script)
  (let* ((confd-dir (expand-file-name "fish/conf.d"
                                       (or (getenv "XDG_CONFIG_HOME")
                                           (expand-file-name ".config"
                                                             (getenv "HOME")))))
         (link-name (expand-file-name "chomp-integration.fish" confd-dir)))
    ;; Create conf.d directory if needed
    (make-directory confd-dir t)
    ;; Create a small sourcing script (not a symlink, to handle the
    ;; case where the integration dir moves between Emacs sessions)
    (unless (file-exists-p link-name)
      (with-temp-file link-name
        (insert "# Auto-generated by chomp terminal emulator.\n")
        (insert "# Sources chomp shell integration when running inside chomp.\n")
        (insert "if set -q CHOMP_SHELL_INTEGRATION_DIR\n")
        (insert "    source $CHOMP_SHELL_INTEGRATION_DIR/fish\n")
        (insert "end\n")
        (insert "# Also support EAT_SHELL_INTEGRATION_DIR for eat compatibility.\n")
        (insert "if set -q EAT_SHELL_INTEGRATION_DIR; and not set -q __chomp_integration_enabled\n")
        (insert "    set -l script $EAT_SHELL_INTEGRATION_DIR/fish\n")
        (insert "    if test -f $script\n")
        (insert "        source $script\n")
        (insert "    end\n")
        (insert "end\n")))))

;;;; ---- Process Lifecycle ----------------------------------------------

(defun chomp-io-start (io shell-command buffer &optional extra-env)
  "Start a terminal process running SHELL-COMMAND in BUFFER.
EXTRA-ENV is an optional list of \"VAR=VALUE\" strings to add to
the process environment."
  (let* ((extra-env (chomp-io--prepare-environment shell-command extra-env))
         (screen (chomp-io-screen io))
         (w (chomp-screen-width screen))
         (h (chomp-screen-height screen))
         ;; Environment
         (process-environment
          (append
           (list
            (concat "TERM=" (if (boundp 'chomp-term-name) chomp-term-name "eat-truecolor"))
            (format "COLUMNS=%d" w)
            (format "LINES=%d" h)
            "INSIDE_EMACS=chomp")
           (or extra-env nil)
           process-environment))
         ;; Construct the command with shell integration and initialize PTY
         ;; termios like Eat does before execing the client program.
         (cmd (chomp-io--wrap-command-with-stty
               (chomp-io--build-command shell-command extra-env) h w))
         ;; Create process
         (proc (make-process
                :name "chomp"
                :buffer nil  ; no associated buffer (we manage our own)
                :command cmd
                :connection-type 'pty
                :coding '(utf-8-unix . no-conversion)
                :noquery t
                :filter (lambda (proc output)
                          (chomp-io--filter io proc output))
                :sentinel (lambda (proc event)
                            (chomp-io--sentinel io proc event)))))
    ;; Set initial PTY size
    (set-process-window-size proc h w)
    ;; Wire up response writing
    (setf (chomp-parser-write-fn (chomp-io-parser io))
          (lambda (s) (chomp-io-send io s)))
    ;; Store
    (setf (chomp-io-process io) proc)
    (setf (chomp-io-buffer io) buffer)
    proc))

(defun chomp-io--sentinel (io _proc event)
  "Handle process state changes."
  (when (string-match-p "\\(finished\\|exited\\|killed\\|deleted\\)" event)
    (when (buffer-live-p (chomp-io-buffer io))
      (with-current-buffer (chomp-io-buffer io)
        (when (chomp-io-render-timer io)
          (cancel-timer (chomp-io-render-timer io))
          (setf (chomp-io-render-timer io) nil))
        ;; Flush remaining output.
        (when (chomp-io--pending-p io)
          (chomp-io--process-pending io t))
        ;; Notify in the terminal buffer because event handlers use
        ;; buffer-local chomp state and modify the current buffer.
        (chomp-parse--emit (chomp-io-parser io) 'process-exit event)))))

;;;; ---- Send to Process ------------------------------------------------

(defun chomp-io-send (io string)
  "Send STRING to the terminal process.
Multibyte text is encoded as UTF-8; unibyte strings are sent unchanged."
  (when-let ((proc (chomp-io-process io)))
    (when (process-live-p proc)
      (process-send-string
       proc
       (if (multibyte-string-p string)
           (encode-coding-string string 'utf-8 t)
         string)))))

;;;; ---- Resize ---------------------------------------------------------

(defun chomp-io-handle-resize (io new-width new-height)
  "Handle terminal resize to NEW-WIDTH x NEW-HEIGHT."
  (let ((screen (chomp-io-screen io)))
    (unless (and (= new-width (chomp-screen-width screen))
                 (= new-height (chomp-screen-height screen)))
      ;; Step 1: Resize screen model
      (chomp-screen-resize screen new-width new-height)
      ;; Step 2: Notify PTY
      (when-let ((proc (chomp-io-process io)))
        (when (process-live-p proc)
          (set-process-window-size proc new-height new-width)))
      ;; Step 3: Full re-render
      (when (chomp-io-render io)
        (chomp-render-full-reset (chomp-io-render io))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun chomp-io-stop (io)
  "Stop the terminal process and clean up."
  (when (chomp-io-render-timer io)
    (cancel-timer (chomp-io-render-timer io))
    (setf (chomp-io-render-timer io) nil))
  (when-let ((proc (chomp-io-process io)))
    (when (process-live-p proc)
      (delete-process proc))
    (setf (chomp-io-process io) nil))
  (when (chomp-io-render io)
    (chomp-render-destroy (chomp-io-render io))))

(provide 'chomp-io)
;;; chomp-io.el ends here
