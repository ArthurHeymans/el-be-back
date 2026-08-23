;;; ebb-io.el --- Async process I/O for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Manages the PTY process and async parse/render pipeline.
;; The process filter only appends to a queue.  Parsing happens in
;; bounded chunks driven by timers, ensuring the main thread never hangs.

;;; Code:

(require 'cl-lib)
(require 'ebb-term)
(require 'ebb-parse)
(require 'ebb-render)

;;;; ---- Data Structure -------------------------------------------------

(cl-defstruct (ebb-io (:copier nil))
  "Async I/O state for a ebb terminal."
  (process nil)           ; the PTY process
  (parser nil)            ; ebb-parser
  (screen nil)            ; ebb-screen
  (render nil)            ; ebb-render-state
  (buffer nil)            ; Emacs buffer
  (input-coding-system 'utf-8) ; encoding for multibyte input, or nil
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
  (first-chunk-time nil)  ; float time of first unrendered chunk
  (render-timer nil)      ; pending render timer
  ;; Error reporting
  (last-processing-error nil)
  (processing-error-count 0))

(defvar ebb-io-after-render-functions nil
  "Functions called after terminal output has been rendered.
Each function receives the current `ebb-render-state'.")

(defvar ebb-io-processing-error-functions nil
  "Functions called when asynchronous terminal processing fails.
Each function receives IO, the error value, and its consecutive count.")

;;;; ---- Customization --------------------------------------------------

(defcustom ebb-chunk-size 4096
  "Number of bytes to parse per chunk before yielding to the event loop."
  :type 'integer
  :group 'ebb)

(defcustom ebb-minimum-latency 0.008
  "Minimum seconds to wait before rendering after output arrives.
Allows batching of rapid output."
  :type 'number
  :group 'ebb)

(defcustom ebb-maximum-latency 0.033
  "Maximum seconds to wait before rendering after output arrives.
Ensures responsiveness."
  :type 'number
  :group 'ebb)

(defcustom ebb-enable-shell-integration t
  "If non-nil, automatically source shell integration scripts.
When enabled, ebb wraps the shell command to source the appropriate
integration script for the detected shell."
  :type 'boolean
  :group 'ebb)

;;;; ---- Process Filter (never blocks) ----------------------------------

(defun ebb-io-receive (io output)
  "Queue terminal OUTPUT for asynchronous parsing and rendering.
This is the supported entry point for non-PTY transports."
  (ebb-io--enqueue-output io output)
  ;; Record time of first unprocessed chunk
  (unless (ebb-io-first-chunk-time io)
    (setf (ebb-io-first-chunk-time io) (float-time)))
  ;; Schedule processing
  (unless (ebb-io-processing io)
    (ebb-io--schedule-processing io)))

(defun ebb-io--filter (io _process output)
  "Process filter for Ebb-owned processes."
  (ebb-io-receive io output))

(defun ebb-io--enqueue-output (io output)
  "Append OUTPUT to IO's pending chunk queue without copying old data."
  (unless (zerop (length output))
    (let ((cell (list output)))
      (if (ebb-io-pending-tail io)
          (setcdr (ebb-io-pending-tail io) cell)
        (setf (ebb-io-pending-chunks io) cell))
      (setf (ebb-io-pending-tail io) cell))))

(defun ebb-io--normalize-pending (io)
  "Drop fully consumed head chunks from IO's pending queue."
  (while (and (ebb-io-pending-chunks io)
              (>= (ebb-io-pending-offset io)
                  (length (car (ebb-io-pending-chunks io)))))
    (setf (ebb-io-pending-chunks io)
          (cdr (ebb-io-pending-chunks io)))
    (setf (ebb-io-pending-offset io) 0))
  (unless (ebb-io-pending-chunks io)
    (setf (ebb-io-pending-tail io) nil)))

(defun ebb-io--pending-p (io)
  "Return non-nil if IO has pending output."
  (ebb-io--normalize-pending io)
  (ebb-io-pending-chunks io))

;;;; ---- Chunked Processing ---------------------------------------------

(defun ebb-io--schedule-processing (io)
  "Schedule the next parse+render cycle."
  (let* ((now (float-time))
         (first (ebb-io-first-chunk-time io))
         (elapsed (if first (- now first) 0))
         (time-left (- (ebb-io-max-latency io) elapsed))
         (delay (if (<= time-left 0) 0
                  (min time-left (ebb-io-min-latency io)))))
    ;; Cancel existing timer
    (when (ebb-io-render-timer io)
      (cancel-timer (ebb-io-render-timer io)))
    ;; Schedule via timer
    (setf (ebb-io-render-timer io)
          (run-at-time delay nil #'ebb-io--process-pending io))))

(defun ebb-io--process-pending (io &optional drain-all)
  "Process pending output: parse chunks, then render."
  (setf (ebb-io-processing io) t)
  (when (ebb-io-render-timer io)
    (cancel-timer (ebb-io-render-timer io)))
  (setf (ebb-io-render-timer io) nil)
  (condition-case err
      (when (buffer-live-p (ebb-io-buffer io))
        (with-current-buffer (ebb-io-buffer io)
          (let ((budget (ebb-io-chunk-size io))
                (total-parsed 0)
                (max-parsed (if drain-all most-positive-fixnum
                              (* (ebb-io-chunk-size io) 4))))
            ;; Parse in chunks up to budget
            (while (and (< total-parsed max-parsed)
                        (ebb-io--pending-p io))
              (let* ((bytes (car (ebb-io-pending-chunks io)))
                     (offset (ebb-io-pending-offset io))
                     (remaining (- (length bytes) offset))
                     (chunk-size (min remaining budget)))
                (let ((consumed (ebb-parse-bytes
                                 (ebb-io-parser io)
                                 bytes offset (+ offset chunk-size))))
                  (cl-incf (ebb-io-pending-offset io) consumed))
                (cl-incf total-parsed chunk-size)))
            (ebb-io--normalize-pending io)

            ;; Render
            (ebb-render-refresh (ebb-io-render io))
            (run-hook-with-args 'ebb-io-after-render-functions
                                (ebb-io-render io))))

        ;; Reset latency tracking
        (setf (ebb-io-first-chunk-time io) nil
              (ebb-io-last-processing-error io) nil
              (ebb-io-processing-error-count io) 0)

        ;; Check for more pending data
        (setf (ebb-io-processing io) nil)
        (when (ebb-io--pending-p io)
          (setf (ebb-io-first-chunk-time io) (float-time))
          (ebb-io--schedule-processing io)))
    (error
     (setf (ebb-io-processing io) nil)
     (if (equal err (ebb-io-last-processing-error io))
         (cl-incf (ebb-io-processing-error-count io))
       (setf (ebb-io-last-processing-error io) err
             (ebb-io-processing-error-count io) 1))
     (run-hook-with-args 'ebb-io-processing-error-functions
                         io err (ebb-io-processing-error-count io)))))

(defun ebb-io--report-processing-error (_io error-data count)
  "Report asynchronous ERROR-DATA after COUNT consecutive failures.
Repeated identical failures are rate-limited to avoid flooding *Messages*."
  (when (or (<= count 3) (zerop (% count 100)))
    (message "[ebb-io] Processing error%s: %S"
             (if (> count 1) (format " (repeated %d times)" count) "")
             error-data)))

(add-hook 'ebb-io-processing-error-functions #'ebb-io--report-processing-error)

;;;; ---- Stack Construction ---------------------------------------------

(defun ebb-io-create-terminal (buffer event-handler &optional begin end)
  "Create a screen, renderer, parser, and I/O instance for BUFFER.
EVENT-HANDLER receives parser events.  When BEGIN and END are
non-nil, the render region is restricted to that buffer region
(used by the inline Eshell terminal).  The screen is sized from a
window currently displaying BUFFER, else the selected window."
  (let* ((window (or (get-buffer-window buffer) (selected-window)))
         (width (max (window-max-chars-per-line window) 10))
         (height (max (window-body-height window) 3))
         (screen (ebb-screen-create width height)))
    (setf (ebb-screen-scrollback-max screen) ebb-scrollback-lines)
    (make-ebb-io
     :screen screen
     :parser (ebb-parse-create screen nil event-handler)
     :render (ebb-render-create screen buffer begin end)
     :buffer buffer
     :chunk-size ebb-chunk-size
     :min-latency ebb-minimum-latency
     :max-latency ebb-maximum-latency)))

;;;; ---- Command Building -----------------------------------------------

(defun ebb-io--detect-shell (shell-command)
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

(defun ebb-io--integration-script (shell-type extra-env)
  "Find the integration script for SHELL-TYPE.
Returns the path to the script or nil.
EXTRA-ENV is the env var list (used to find integration dir)."
  (when-let* ((dir (cl-loop for var in (or extra-env nil)
                            when (string-prefix-p
                                  "EBB_SHELL_INTEGRATION_DIR=" var)
                            return (substring
                                    var
                                    (length "EBB_SHELL_INTEGRATION_DIR="))))
              (name (pcase shell-type
                      ('bash "bash") ('zsh "zsh") ('fish "fish") (_ nil)))
              (f (expand-file-name name dir)))
    (when (file-exists-p f) f)))

(defun ebb-io--environment-value (name environment)
  "Return NAME's value from ENVIRONMENT, a list of NAME=VALUE strings."
  (let ((prefix (concat name "=")))
    (cl-loop for entry in environment
             when (string-prefix-p prefix entry)
             return (substring entry (length prefix)))))

(defun ebb-io--prepare-environment (shell-command extra-env)
  "Prepare EXTRA-ENV for SHELL-COMMAND startup integration."
  (let* ((shell-type (ebb-io--detect-shell shell-command))
         (script (and ebb-enable-shell-integration
                      (ebb-io--integration-script shell-type extra-env))))
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
                        (concat "EBB_ZSH_ZDOTDIR=" (or old-zdotdir ""))
                        (concat "EBB_ZSH_ZDOTDIR_SET="
                                (if old-entry "1" "0")))
                  extra-env))
      extra-env)))

(defun ebb-io--build-command (shell-command extra-env)
  "Build a process command list for SHELL-COMMAND.
If `ebb-enable-shell-integration' is non-nil and an integration
script is available, wraps the command to source it.
EXTRA-ENV is the env var list (used to find integration dir)."
  (let* ((command (if (listp shell-command)
                      shell-command
                    (list shell-command)))
         (program (car command))
         (args (cdr command))
         (shell-type (ebb-io--detect-shell command))
         (script (when ebb-enable-shell-integration
                   (ebb-io--integration-script shell-type extra-env))))
    (pcase shell-type
      ;; Fish: set up XDG_CONFIG_HOME to auto-source integration.
      ('fish
       (when script
         (ebb-io--fish-setup-confd script))
       command)
      ;; Bash: --rcfile must precede user-provided short options.
      ('bash
       (if script
           (append (list program "--rcfile" (ebb-io--bash-rcfile script))
                   args)
         command))
      ;; Zsh integration is sourced by the user's .zshrc for now.
      ('zsh command)
      (_ command))))

(defun ebb-io--term-name ()
  "Return the TERM value ebb advertises."
  (if (boundp 'ebb-term-name) ebb-term-name "eat-truecolor"))

(defun ebb-io--remote-command (shell-command rows columns)
  "Wrap SHELL-COMMAND for a remote (TRAMP) spawn.
The wrapper runs on the remote host: TERM is chosen there because
TRAMP's `tramp-local-environment-variable-p' filter strips `TERM='
entries pushed via the environment, leaving the remote shell with
TERM=dumb.  It probes `infocmp' for the ebb terminfo entry and
falls back to xterm-256color, then initializes the PTY like the
local stty wrapper and execs SHELL-COMMAND."
  (let ((command (if (listp shell-command) shell-command (list shell-command)))
        (term (shell-quote-argument (ebb-io--term-name))))
    (list "/bin/sh" "-c"
          (concat
           (format "TERM=xterm-256color; if infocmp %s >/dev/null 2>&1; then TERM=%s; fi; "
                   term term)
           "COLORTERM=truecolor; export TERM COLORTERM; "
           (format "stty -nl echo rows %d columns %d sane 2>/dev/null; exec "
                   rows columns)
           (mapconcat #'shell-quote-argument command " ")))))

(defun ebb-io--wrap-command-with-stty (command rows columns)
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

(defun ebb-io--bash-rcfile (integration-script)
  "Create a temporary rcfile that sources ~/.bashrc and INTEGRATION-SCRIPT.
Returns the path to the temporary file."
  (let ((tmpfile (make-temp-file "ebb-bashrc-")))
    (with-temp-file tmpfile
      (insert "# Ebb shell integration wrapper\n")
      (insert "if [ -f ~/.bashrc ]; then . ~/.bashrc; fi\n")
      (insert (format ". %s\n" (shell-quote-argument integration-script))))
    tmpfile))

(defun ebb-io--fish-setup-confd (integration-script)
  "Install a fish conf.d snippet that sources INTEGRATION-SCRIPT.
Fish automatically sources all .fish files in ~/.config/fish/conf.d/
on startup.  We create a small sourcing script there (not a symlink,
to handle the case where the integration dir moves between Emacs
sessions).  The snippet is permanent and sources the integration
script only while EBB_SHELL_INTEGRATION_DIR is set."
  (ignore integration-script)
  (let* ((confd-dir (expand-file-name "fish/conf.d"
                                       (or (getenv "XDG_CONFIG_HOME")
                                           (expand-file-name ".config"
                                                             (getenv "HOME")))))
         (link-name (expand-file-name "ebb-integration.fish" confd-dir)))
    ;; Create conf.d directory if needed
    (make-directory confd-dir t)
    ;; Create a small sourcing script (not a symlink, to handle the
    ;; case where the integration dir moves between Emacs sessions)
    (unless (file-exists-p link-name)
      (with-temp-file link-name
        (insert "# Auto-generated by ebb terminal emulator.\n")
        (insert "# Sources ebb shell integration when running inside ebb.\n")
        (insert "if set -q EBB_SHELL_INTEGRATION_DIR\n")
        (insert "    source $EBB_SHELL_INTEGRATION_DIR/fish\n")
        (insert "end\n")
        (insert "# Also support EAT_SHELL_INTEGRATION_DIR for eat compatibility.\n")
        (insert "if set -q EAT_SHELL_INTEGRATION_DIR; and not set -q __ebb_integration_enabled\n")
        (insert "    set -l script $EAT_SHELL_INTEGRATION_DIR/fish\n")
        (insert "    if test -f $script\n")
        (insert "        source $script\n")
        (insert "    end\n")
        (insert "end\n")))))

;;;; ---- Process Lifecycle ----------------------------------------------

(defun ebb-io--pty-process-p (process)
  "Return non-nil when PROCESS talks to a PTY that can be resized.
`process-type' answers `real'/`network'/`serial'/`pipe' and never `pty',
so the connection is detected through the allocated terminal instead."
  (and (processp process)
       (eq (process-type process) 'real)
       (process-tty-name process)
       t))

(defun ebb-io-start (io shell-command buffer &optional extra-env)
  "Start a terminal process running SHELL-COMMAND in BUFFER.
EXTRA-ENV is an optional list of \"VAR=VALUE\" strings to add to
the process environment.

When `default-directory' is remote, the process is spawned on the
remote host through TRAMP (`:file-handler').  Shell integration,
EXTRA-ENV, and the local TERMINFO are skipped there (they carry
local paths), and TERM is chosen by an on-remote probe; see
`ebb-io--remote-command'."
  (let* ((remote (and (file-remote-p default-directory) t))
         (extra-env (if remote nil
                      (ebb-io--prepare-environment shell-command extra-env)))
         (screen (ebb-io-screen io))
         (w (ebb-screen-width screen))
         (h (ebb-screen-height screen))
         ;; Environment
         (process-environment
          (append
           ;; Never export COLUMNS/LINES: ncurses lets them override
           ;; TIOCGWINSZ, freezing full-screen apps (htop) at the startup
           ;; size and ignoring later SIGWINCH resizes.  The stty wrapper
           ;; sets the initial PTY size instead.
           (list "INSIDE_EMACS=ebb")
           ;; Remote TERM is exported by the on-remote wrapper; TRAMP's
           ;; env filter would strip a TERM= entry set here anyway.
           (unless remote
             (list (concat "TERM=" (ebb-io--term-name))))
           (or extra-env nil)
           process-environment))
         ;; Construct the command with shell integration and initialize PTY
         ;; termios like Eat does before execing the client program.
         (cmd (if remote
                  (ebb-io--remote-command shell-command h w)
                (ebb-io--wrap-command-with-stty
                 (ebb-io--build-command shell-command extra-env) h w)))
         ;; Create process
         (proc (make-process
                :name "ebb"
                :buffer nil  ; no associated buffer (we manage our own)
                :command cmd
                :connection-type 'pty
                :file-handler remote
                :coding '(utf-8-unix . no-conversion)
                :noquery t
                :filter (lambda (proc output)
                          (ebb-io--filter io proc output))
                :sentinel (lambda (proc event)
                            (ebb-io--sentinel io proc event)))))
    ;; Set initial PTY size.
    (when (ebb-io--pty-process-p proc)
      (set-process-window-size proc h w))
    ;; Wire up response writing
    (setf (ebb-parser-write-fn (ebb-io-parser io))
          (lambda (s) (ebb-io-send io s)))
    ;; Store
    (setf (ebb-io-process io) proc)
    (setf (ebb-io-buffer io) buffer)
    ;; Remember the temporary bash integration rcfile so the sentinel
    ;; can delete it once the shell exits.  Only files Ebb itself
    ;; created are cleaned up; a user-supplied --rcfile is left alone.
    (let ((pos (seq-position cmd "--rcfile")))
      (when pos
        (let ((rcfile (nth (1+ pos) cmd)))
          (when (and (stringp rcfile)
                     (string-prefix-p "ebb-bashrc-"
                                      (file-name-nondirectory rcfile))
                     (string-prefix-p
                      (file-name-as-directory temporary-file-directory)
                      rcfile))
            (process-put proc 'ebb-bashrc rcfile)))))
    proc))

(defun ebb-io-attach (io process buffer)
  "Attach IO to existing PROCESS in BUFFER without replacing its handlers.
Eshell owns PROCESS's filter, sentinel, and bookkeeping; its output hook feeds
Ebb through `ebb-io--filter'."
  (setf (ebb-io-process io) process
        (ebb-io-buffer io) buffer
        (ebb-parser-write-fn (ebb-io-parser io))
        (lambda (string) (ebb-io-send io string)))
  process)

(defun ebb-io--sentinel (io proc event)
  "Handle process state changes."
  (when (string-match-p "\\(finished\\|exited\\|killed\\|deleted\\)" event)
    ;; Delete the temporary bash integration rcfile, if any.  Read it
    ;; from PROC, not from IO, whose process slot may already be nil.
    (when-let* ((rcfile (and (processp proc)
                             (process-get proc 'ebb-bashrc))))
      (ignore-errors (delete-file rcfile)))
    (when (buffer-live-p (ebb-io-buffer io))
      (with-current-buffer (ebb-io-buffer io)
        (when (ebb-io-render-timer io)
          (cancel-timer (ebb-io-render-timer io))
          (setf (ebb-io-render-timer io) nil))
        ;; Flush remaining output.
        (when (ebb-io--pending-p io)
          (ebb-io--process-pending io t))
        ;; Notify in the terminal buffer because event handlers use
        ;; buffer-local ebb state and modify the current buffer.
        (ebb-parse--emit (ebb-io-parser io) 'process-exit event)))))

;;;; ---- Send to Process ------------------------------------------------

(defun ebb-io--prepare-interrupt (io)
  "Discard stale queued output and parser fragments before interrupting IO."
  (when (ebb-io-render-timer io)
    (cancel-timer (ebb-io-render-timer io)))
  (setf (ebb-io-render-timer io) nil
        (ebb-io-pending-chunks io) nil
        (ebb-io-pending-tail io) nil
        (ebb-io-pending-offset io) 0
        (ebb-io-first-chunk-time io) nil)
  (when (ebb-io-parser io)
    (ebb-parse-cancel-sequence (ebb-io-parser io))))

(defun ebb-io-send (io string)
  "Send STRING to the terminal process.
Multibyte text is encoded with `ebb-io-input-coding-system'.
Unibyte strings are always sent unchanged.  Sending an interrupt discards
queued output so the interrupted program's prompt is not stuck behind it."
  (when-let* ((proc (ebb-io-process io)))
    (when (process-live-p proc)
      ;; Only a bare interrupt discards queued output; pasted text that
      ;; merely contains ^C must pass through untouched.
      (when (string= string "\C-c")
        (ebb-io--prepare-interrupt io))
      (let ((coding (ebb-io-input-coding-system io)))
        (process-send-string
         proc
         (if (and coding (multibyte-string-p string))
             (encode-coding-string string coding t)
           string))))))

;;;; ---- Resize ---------------------------------------------------------

(defun ebb-io-handle-resize (io new-width new-height)
  "Handle terminal resize to NEW-WIDTH x NEW-HEIGHT."
  (let ((screen (ebb-io-screen io)))
    (unless (and (= new-width (ebb-screen-width screen))
                 (= new-height (ebb-screen-height screen)))
      (let ((width-changed (/= new-width (ebb-screen-width screen))))
      ;; Output already received was generated for the old dimensions.
      (when (ebb-io--pending-p io)
        (ebb-io--process-pending io t))
      ;; Step 1: Resize screen model
      (ebb-screen-resize screen new-width new-height)
      ;; Step 2: Notify PTY.  Without this the child keeps its startup size
      ;; and never gets SIGWINCH, so shells and TUIs format for the old
      ;; width and their output wraps in the middle of a row.
      (when-let* ((proc (ebb-io-process io)))
        (when (and (process-live-p proc)
                   (ebb-io--pty-process-p proc))
          (set-process-window-size proc new-height new-width)))
      ;; Step 3: A minibuffer only changes height.  Rebuild its viewport,
      ;; not thousands of unchanged scrollback rows.
      (when (ebb-io-render io)
        (if width-changed
            (ebb-render-full-reset (ebb-io-render io))
          (ebb-render-resize-height (ebb-io-render io))))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun ebb-io-stop (io)
  "Stop the terminal process and clean up."
  (when (ebb-io-render-timer io)
    (cancel-timer (ebb-io-render-timer io))
    (setf (ebb-io-render-timer io) nil))
  (when-let* ((proc (ebb-io-process io)))
    (when (process-live-p proc)
      (delete-process proc))
    (setf (ebb-io-process io) nil))
  (when (ebb-io-render io)
    (ebb-render-destroy (ebb-io-render io))))

(provide 'ebb-io)
;;; ebb-io.el ends here
