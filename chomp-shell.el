;;; chomp-shell.el --- Shell integration for chomp -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shell integration for chomp using the OSC 51 protocol (compatible
;; with eat's shell integration scripts).
;;
;; The shell emits escape sequences at key points in the command cycle:
;;   B = prompt start, C = prompt end, D/E = continuation prompt,
;;   F = command text, G = pre-exec, H = exit status,
;;   J = before new prompt, A = CWD, I = history, M = user message.
;;
;; Architecture note: In chomp's separated screen model, the buffer is
;; updated AFTER parsing.  Shell events that need buffer positions
;; (prompt annotations) are queued during parsing and applied after
;; rendering via `chomp-shell-post-render'.
;;
;; This module handles:
;; - Prompt annotation (margin overlays with status indicators)
;; - Directory tracking
;; - Prompt navigation (via text properties)
;; - Command status tracking

;;; Code:

(require 'cl-lib)
(require 'chomp-term)
(require 'chomp-render)

;;;; ---- Customization --------------------------------------------------

(defgroup chomp-shell nil
  "Shell integration settings for chomp."
  :group 'chomp
  :prefix "chomp-shell-")

(defcustom chomp-enable-shell-prompt-annotation nil
  "If non-nil, annotate shell prompts with status indicators."
  :type 'boolean
  :group 'chomp-shell)

(defcustom chomp-enable-directory-tracking t
  "If non-nil, track the shell's working directory."
  :type 'boolean
  :group 'chomp-shell)

(defcustom chomp-shell-prompt-annotation-position 'left-margin
  "Position for prompt annotation indicators.
Either `left-margin' or `right-margin'."
  :type '(choice (const left-margin) (const right-margin))
  :group 'chomp-shell)

(defcustom chomp-shell-prompt-annotation-running-indicator "+"
  "Margin indicator for a running command."
  :type 'string
  :group 'chomp-shell)

(defcustom chomp-shell-prompt-annotation-success-indicator "0"
  "Margin indicator for a command that exited with status 0."
  :type 'string
  :group 'chomp-shell)

(defcustom chomp-shell-prompt-annotation-failure-indicator "X"
  "Margin indicator for a command that exited with non-zero status."
  :type 'string
  :group 'chomp-shell)

(defcustom chomp-shell-message-handler-alist nil
  "Whitelist mapping shell message names to handler functions."
  :type '(alist :key-type string :value-type function)
  :group 'chomp-shell)

;;;; ---- Faces ----------------------------------------------------------

(defface chomp-shell-prompt-annotation-running
  '((t :inherit warning))
  "Face for running command prompt indicator."
  :group 'chomp-shell)

(defface chomp-shell-prompt-annotation-success
  '((t :inherit success))
  "Face for successful command prompt indicator."
  :group 'chomp-shell)

(defface chomp-shell-prompt-annotation-failure
  '((t :inherit error))
  "Face for failed command prompt indicator."
  :group 'chomp-shell)

;;;; ---- Shell State (buffer-local) -------------------------------------

;; Screen-model state (set during parsing, before render)
(defvar-local chomp-shell--pending-events nil
  "Queue of shell events waiting for post-render processing.
Each entry is (TYPE . DATA) where TYPE is a symbol.")

(defvar-local chomp-shell--prompt-start-line nil
  "Absolute line number (scrollback + display row) where the current
prompt started.  Set during parsing, consumed after render.")

;; Buffer state (set after render)
(defvar-local chomp-shell--prompt-mark nil
  "Display spec list for the current prompt's margin indicator.
Structure: ((margin POSITION) PROPERTIZED-STRING).
The second element is mutated in place to update the indicator.")

(defvar-local chomp-shell--prompt-overlays nil
  "List of all prompt mark overlay objects.")

(defvar-local chomp-shell--command-status 0
  "Exit status of the last command.")

(defvar-local chomp-shell--current-command nil
  "String of the current command being executed, or nil.")

(defvar-local chomp-shell--overlay-correction-timer nil
  "Timer for deferred overlay correction.")

;;;; ---- Helpers --------------------------------------------------------

(defun chomp-shell--base64-decode (str)
  "Decode base64 STR, returning nil on error."
  (condition-case nil
      (decode-coding-string (base64-decode-string str) 'utf-8)
    (error nil)))

(defun chomp-shell--split-payload (payload start)
  "Split PAYLOAD from position START on semicolons.
Returns a list of strings."
  (when (< start (length payload))
    (split-string (substring payload start) ";" t)))

(defun chomp-shell--absolute-line (screen)
  "Return the current absolute line number in SCREEN.
This is scrollback-count + cursor-y, giving a stable line index
that persists across renders."
  (+ (length (chomp-screen-scrollback screen))
     (chomp-screen-cursor-y screen)))

;;;; ---- Prompt Indicator Helpers ---------------------------------------

(defun chomp-shell--make-indicator (text face)
  "Create a propertized indicator string TEXT with FACE."
  (propertize text 'face (list face 'default)))

(defun chomp-shell--status-indicator (status)
  "Return the propertized indicator string for exit STATUS."
  (if (= status 0)
      (chomp-shell--make-indicator
       chomp-shell-prompt-annotation-success-indicator
       'chomp-shell-prompt-annotation-success)
    (chomp-shell--make-indicator
     chomp-shell-prompt-annotation-failure-indicator
     'chomp-shell-prompt-annotation-failure)))

(defun chomp-shell--running-indicator ()
  "Return the propertized indicator string for a running command."
  (chomp-shell--make-indicator
   chomp-shell-prompt-annotation-running-indicator
   'chomp-shell-prompt-annotation-running))

;;;; ---- OSC 51 Dispatch (called during parsing) ------------------------

(defun chomp-shell--mark-prompt (screen kind)
  "Record a prompt marker of KIND at SCREEN's cursor."
  (let* ((row (chomp-screen-cursor-y screen))
         (column (chomp-screen-cursor-x screen))
         (line (chomp--line-at screen row)))
    (pcase kind
      ('begin (cl-pushnew column (chomp-line-prompt-begins line)))
      ('end (cl-pushnew column (chomp-line-prompt-ends line))))
    (setf (chomp-line-dirty line) t)
    (chomp--mark-dirty screen row)))

(defun chomp-shell-handle-osc51 (payload screen)
  "Handle an OSC 51 shell integration sequence with PAYLOAD.
SCREEN is the chomp-screen model (for cursor position).
Should be called from the chomp event handler when an `osc-51' event
is received.  Events that need buffer positions are queued for
post-render processing."
  (when (and (>= (length payload) 3)
             (eq (aref payload 0) ?e)
             (eq (aref payload 1) ?\;))
    (let ((cmd (aref payload 2))
          (args (chomp-shell--split-payload payload 4)))
      (pcase cmd
        ;; CWD: immediate (no buffer position needed)
        (?A (chomp-shell--set-cwd args))
        ;; Prompt boundaries live on screen lines so rendering, scrolling, and
        ;; batched OSC events cannot lose them.
        (?B (chomp-shell--mark-prompt screen 'begin)
            (when chomp-enable-shell-prompt-annotation
              (push (list 'prompt-start
                          (chomp-shell--absolute-line screen)
                          (chomp-screen-cursor-x screen))
                    chomp-shell--pending-events)))
        (?C (chomp-shell--mark-prompt screen 'end)
            (when chomp-enable-shell-prompt-annotation
              (push (list 'prompt-end
                          (chomp-shell--absolute-line screen)
                          (chomp-screen-cursor-x screen))
                    chomp-shell--pending-events)))
        (?D nil)  ; continuation prompt start (unused)
        (?E nil)  ; continuation prompt end
        ;; Command text: immediate
        (?F (chomp-shell--set-command args))
        ;; Pre-exec: can mutate overlay display spec immediately
        (?G (push '(pre-exec) chomp-shell--pending-events))
        ;; Exit status: immediate
        (?H (chomp-shell--set-exit-status args))
        (?I nil)  ; history exchange (deferred to later)
        (?J nil)  ; before new prompt (no action needed)
        (?M (chomp-shell--handle-message args))
        (_  nil)))))

;;;; ---- Immediate Handlers (no buffer positions needed) ----------------

(defun chomp-shell--set-cwd (args)
  "Set the working directory from base64-encoded ARGS.
ARGS is (HOST-B64 PATH-B64)."
  (when (and chomp-enable-directory-tracking args (cdr args))
    (let ((host (chomp-shell--base64-decode (car args)))
          (path (chomp-shell--base64-decode (cadr args))))
      (when (and host path
                 (string= host (system-name))
                 (file-directory-p path))
        (setq default-directory (file-name-as-directory path))))))

(defun chomp-shell--set-command (args)
  "Handle command text (F sequence).
ARGS is (CMD-B64)."
  (when args
    (setq chomp-shell--current-command
          (chomp-shell--base64-decode (car args)))))

(defun chomp-shell--set-exit-status (args)
  "Handle exit status (H sequence).
ARGS is (STATUS-STRING)."
  (when args
    (setq chomp-shell--command-status
          (string-to-number (car args)))))

(defun chomp-shell--handle-message (args)
  "Decode ARGS and invoke an explicitly whitelisted message handler."
  (when args
    (let* ((decoded (mapcar #'chomp-shell--base64-decode args))
           (handler (and (not (memq nil decoded))
                         (assoc (car decoded)
                                chomp-shell-message-handler-alist))))
      (when handler
        (condition-case error-data
            (save-restriction
              (widen)
              (save-excursion
                (apply (cdr handler) (cdr decoded))))
          (error
           (message "[chomp-shell] Message handler error: %S" error-data)
           nil))))))

;;;; ---- Post-Render Processing -----------------------------------------

(defun chomp-shell-post-render (render)
  "Process pending shell events after rendering.
RENDER is the chomp-render-state.  Called from the I/O module
after `chomp-render-refresh'."
  (when chomp-shell--pending-events
    (let ((events (nreverse chomp-shell--pending-events))
          (inhibit-read-only t))
      (setq chomp-shell--pending-events nil)
      (dolist (event events)
        (pcase (car event)
          ('prompt-start
           (chomp-shell--apply-prompt-start render (nth 1 event) (nth 2 event)))
          ('prompt-end
           (chomp-shell--apply-prompt-end render (nth 1 event) (nth 2 event)))
          ('pre-exec
           (chomp-shell--apply-pre-exec)))))
    ;; Schedule overlay correction only when prompt overlays are enabled.
    (when chomp-enable-shell-prompt-annotation
      (chomp-shell--schedule-overlay-correction))))

(defun chomp-shell--line-to-buffer-pos (render abs-line &optional column)
  "Convert ABS-LINE and COLUMN to a buffer position in RENDER."
  (let* ((display-begin (chomp-render-state-display-begin render))
         (screen (chomp-render-state-screen render))
         (sb-count (length (chomp-screen-scrollback screen))))
    (with-current-buffer (chomp-render-state-buffer render)
      (save-excursion
        (cond
         ;; In scrollback region (before display-begin)
         ((< abs-line sb-count)
          (goto-char (point-min))
          (forward-line abs-line)
          (point))
         ;; In display region
         (t
          (let ((display-row (- abs-line sb-count)))
            (goto-char (marker-position display-begin))
            (forward-line display-row))))
        (move-to-column (or column 0))
        (point)))))

(defun chomp-shell--apply-prompt-start (render abs-line column)
  "Remember an annotation start at ABS-LINE and COLUMN in RENDER."
  (when-let ((pos (chomp-shell--line-to-buffer-pos render abs-line column)))
    (let ((m (copy-marker pos)))
      (set-marker-insertion-type m t)
      (setq chomp-shell--prompt-start-line m))))

(defun chomp-shell--apply-prompt-end (render abs-line column)
  "Apply a prompt annotation ending at ABS-LINE and COLUMN in RENDER."
  (let ((prompt-begin chomp-shell--prompt-start-line))
    (when (and prompt-begin (markerp prompt-begin)
               (marker-position prompt-begin))
      ;; Finalize previous prompt's indicator with last exit status
      (when chomp-shell--prompt-mark
        (setf (cadr chomp-shell--prompt-mark)
              (chomp-shell--status-indicator chomp-shell--command-status))
        (setq chomp-shell--prompt-mark nil))
      (when-let ((end (chomp-shell--line-to-buffer-pos
                       render abs-line column)))
        (let ((beg (marker-position prompt-begin)))
          (when (< beg end)
            (let* ((indicator (chomp-shell--running-indicator))
                   (display-spec
                    (list
                     (list 'margin chomp-shell-prompt-annotation-position)
                     indicator))
                   (before-str (propertize " " 'display display-spec))
                   (ov (make-overlay beg (min (1+ beg) end) nil t nil)))
              (overlay-put ov 'before-string before-str)
              (overlay-put ov 'chomp-shell-prompt t)
              (push ov chomp-shell--prompt-overlays)
              (setq chomp-shell--prompt-mark display-spec)))))))
  ;; Reset prompt-start-line for next cycle
  (setq chomp-shell--prompt-start-line nil))

(defun chomp-shell--apply-pre-exec ()
  "Apply pre-exec: update current prompt indicator to \"running\"."
  (when chomp-shell--prompt-mark
    (setf (cadr chomp-shell--prompt-mark) (chomp-shell--running-indicator))))

;;;; ---- Overlay Correction ---------------------------------------------

(defun chomp-shell--schedule-overlay-correction ()
  "Schedule deferred overlay correction.
When terminal output scrolls, overlays may need repositioning."
  (when chomp-shell--overlay-correction-timer
    (cancel-timer chomp-shell--overlay-correction-timer))
  (setq chomp-shell--overlay-correction-timer
        (run-at-time 0.1 nil #'chomp-shell--correct-overlays
                     (current-buffer))))

(defun chomp-shell--correct-overlays (buffer)
  "Correct prompt overlays in BUFFER.
Remove overlays that point to deleted text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq chomp-shell--overlay-correction-timer nil)
      (setq chomp-shell--prompt-overlays
            (cl-remove-if-not
             (lambda (ov)
               (and (overlay-buffer ov)
                    (< (overlay-start ov) (overlay-end ov))))
             chomp-shell--prompt-overlays)))))

;;;; ---- Prompt Navigation ----------------------------------------------

(defun chomp-shell-previous-prompt (&optional n)
  "Move to the Nth previous shell prompt.
N defaults to 1."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((prev (previous-single-property-change
                   (point) 'chomp-shell-prompt-end)))
        (if prev
            (progn
              (goto-char prev)
              ;; If we're on a prompt-end, jump past it to the previous one
              (when (get-text-property (point) 'chomp-shell-prompt-end)
                (let ((prev2 (previous-single-property-change
                              (point) 'chomp-shell-prompt-end)))
                  (when prev2 (goto-char prev2)))))
          (user-error "No previous prompt"))))))

(defun chomp-shell-next-prompt (&optional n)
  "Move to the Nth next shell prompt.
N defaults to 1."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (let ((next (next-single-property-change
                   (point) 'chomp-shell-prompt-end)))
        (if next
            (progn
              (goto-char next)
              ;; Skip past the prompt-end region
              (when (get-text-property (point) 'chomp-shell-prompt-end)
                (let ((next2 (next-single-property-change
                              (point) 'chomp-shell-prompt-end)))
                  (when next2 (goto-char next2)))))
          (user-error "No next prompt"))))))

;;;; ---- Margin Setup ---------------------------------------------------

(defun chomp-shell-setup-margins ()
  "Set up buffer margins for prompt annotation display.
Should be called during buffer initialization."
  (when chomp-enable-shell-prompt-annotation
    (let ((margin-width
           (max (string-width chomp-shell-prompt-annotation-running-indicator)
                (string-width chomp-shell-prompt-annotation-success-indicator)
                (string-width chomp-shell-prompt-annotation-failure-indicator))))
      (pcase chomp-shell-prompt-annotation-position
        ('left-margin (setq left-margin-width margin-width))
        ('right-margin (setq right-margin-width margin-width)))
      ;; Force windows to pick up new margin settings
      (dolist (win (get-buffer-window-list))
        (set-window-buffer win (current-buffer))))))

;;;; ---- Cleanup --------------------------------------------------------

(defun chomp-shell-cleanup ()
  "Clean up shell integration state.
Should be called when the buffer is killed."
  (when chomp-shell--overlay-correction-timer
    (cancel-timer chomp-shell--overlay-correction-timer)
    (setq chomp-shell--overlay-correction-timer nil))
  (dolist (ov chomp-shell--prompt-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq chomp-shell--prompt-overlays nil
        chomp-shell--prompt-mark nil
        chomp-shell--prompt-start-line nil
        chomp-shell--pending-events nil))

;;;; ---- Shell Integration Directory & Env Setup ------------------------

(defvar chomp-shell--install-path
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "chomp-shell")))
  "Directory where chomp-shell.el is installed.")

(defvar chomp-shell-integration-directory
  (expand-file-name "integration" chomp-shell--install-path)
  "Directory containing shell integration scripts.
This is passed to shells via the `CHOMP_SHELL_INTEGRATION_DIR'
and `EAT_SHELL_INTEGRATION_DIR' environment variables.")

(defcustom chomp-terminfo-directory
  (expand-file-name "terminfo" chomp-shell--install-path)
  "Directory containing terminfo databases.
Set `TERMINFO' env var so terminal programs find the eat-truecolor
terminfo entry."
  :type 'directory
  :group 'chomp-shell)

(defcustom chomp-term-name "eat-truecolor"
  "Value of `TERM' environment variable for chomp terminals.
Should match an available terminfo entry."
  :type 'string
  :group 'chomp-shell)

(defun chomp-shell-env-vars ()
  "Return a list of environment variable settings for shell integration.
These should be added to `process-environment' before starting
the shell process."
  (list (concat "CHOMP_SHELL_INTEGRATION_DIR="
                chomp-shell-integration-directory)
        ;; Also set EAT_SHELL_INTEGRATION_DIR for compatibility with
        ;; eat's existing shell scripts
        (concat "EAT_SHELL_INTEGRATION_DIR="
                chomp-shell-integration-directory)
        (concat "TERMINFO=" chomp-terminfo-directory)))

(provide 'chomp-shell)
;;; chomp-shell.el ends here
