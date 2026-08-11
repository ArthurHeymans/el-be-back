;;; ebb-shell.el --- Shell integration for ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Shell integration for ebb using the OSC 51 protocol (compatible
;; with eat's shell integration scripts).
;;
;; The shell emits escape sequences at key points in the command cycle:
;;   B = prompt start, C = prompt end, D/E = continuation prompt,
;;   F = command text, G = pre-exec, H = exit status,
;;   J = before new prompt, A = CWD, I = history, M = user message.
;;
;; Architecture note: In ebb's separated screen model, the buffer is
;; updated AFTER parsing.  Shell events that need buffer positions
;; (prompt annotations) are queued during parsing and applied after
;; rendering via `ebb-shell-post-render'.
;;
;; This module handles:
;; - Prompt annotation (margin overlays with status indicators)
;; - Directory tracking
;; - Prompt navigation (via text properties)
;; - Command status tracking

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ebb-term)
(require 'ebb-render)

(declare-function ebb-emacs-mode "ebb")
(declare-function ebb--cwd-to-path "ebb")
(defvar ebb-buffer-name-function)
(defvar ebb--render)
(defvar ebb--screen)

;;;; ---- Customization --------------------------------------------------

(defgroup ebb-shell nil
  "Shell integration settings for ebb."
  :group 'ebb
  :prefix "ebb-shell-")

(defcustom ebb-enable-shell-prompt-annotation nil
  "If non-nil, annotate shell prompts with status indicators."
  :type 'boolean
  :group 'ebb-shell)

(defcustom ebb-enable-directory-tracking t
  "If non-nil, track the shell's working directory."
  :type 'boolean
  :group 'ebb-shell)

(defcustom ebb-shell-prompt-annotation-position 'left-margin
  "Position for prompt annotation indicators.
Either `left-margin' or `right-margin'."
  :type '(choice (const left-margin) (const right-margin))
  :group 'ebb-shell)

(defcustom ebb-shell-prompt-annotation-running-indicator "+"
  "Margin indicator for a running command."
  :type 'string
  :group 'ebb-shell)

(defcustom ebb-shell-prompt-annotation-success-indicator "0"
  "Margin indicator for a command that exited with status 0."
  :type 'string
  :group 'ebb-shell)

(defcustom ebb-shell-prompt-annotation-failure-indicator "X"
  "Margin indicator for a command that exited with non-zero status."
  :type 'string
  :group 'ebb-shell)

(defcustom ebb-shell-message-handler-alist nil
  "Whitelist mapping shell message names to handler functions."
  :type '(alist :key-type string :value-type function)
  :group 'ebb-shell)

;;;; ---- Faces ----------------------------------------------------------

(defface ebb-shell-prompt-annotation-running
  '((t :inherit warning))
  "Face for running command prompt indicator."
  :group 'ebb-shell)

(defface ebb-shell-prompt-annotation-success
  '((t :inherit success))
  "Face for successful command prompt indicator."
  :group 'ebb-shell)

(defface ebb-shell-prompt-annotation-failure
  '((t :inherit error))
  "Face for failed command prompt indicator."
  :group 'ebb-shell)

;;;; ---- Shell State (buffer-local) -------------------------------------

;; Screen-model state (set during parsing, before render)
(defvar-local ebb-shell--pending-events nil
  "Queue of shell events waiting for post-render processing.
Each entry is (TYPE . DATA) where TYPE is a symbol.")

(defvar-local ebb-shell--prompt-start-line nil
  "Absolute line number (scrollback + display row) where the current
prompt started.  Set during parsing, consumed after render.")

;; Buffer state (set after render)
(defvar-local ebb-shell--prompt-mark nil
  "Display spec list for the current prompt's margin indicator.
Structure: ((margin POSITION) PROPERTIZED-STRING).
The second element is mutated in place to update the indicator.")

(defvar-local ebb-shell--prompt-overlays nil
  "List of all prompt mark overlay objects.")

(defvar-local ebb-shell--command-status 0
  "Exit status of the last command.")

(defvar-local ebb-shell--current-command nil
  "String of the current command being executed, or nil.")

(defvar-local ebb-shell--overlay-correction-timer nil
  "Timer for deferred overlay correction.")

;;;; ---- Helpers --------------------------------------------------------

(defun ebb-shell--base64-decode (str)
  "Decode base64 STR as valid UTF-8, returning nil on error."
  (condition-case nil
      (let ((decoded (decode-coding-string (base64-decode-string str) 'utf-8)))
        (and (cl-loop for char across decoded
                      never (eq (char-charset char) 'eight-bit))
             decoded))
    (error nil)))

(defun ebb-shell--split-payload (payload start)
  "Split PAYLOAD from position START on semicolons.
Returns a list of strings."
  (when (< start (length payload))
    (split-string (substring payload start) ";" nil)))

(defun ebb-shell--absolute-line (screen)
  "Return the current physical row number in SCREEN.
This is the current-width history row count plus cursor-y."
  (+ (ebb-screen-history-row-count screen)
     (ebb-screen-cursor-y screen)))

;;;; ---- Prompt Indicator Helpers ---------------------------------------

(defun ebb-shell--make-indicator (text face)
  "Create a propertized indicator string TEXT with FACE."
  (propertize text 'face (list face 'default)))

(defun ebb-shell--status-indicator (status)
  "Return the propertized indicator string for exit STATUS."
  (if (= status 0)
      (ebb-shell--make-indicator
       ebb-shell-prompt-annotation-success-indicator
       'ebb-shell-prompt-annotation-success)
    (ebb-shell--make-indicator
     ebb-shell-prompt-annotation-failure-indicator
     'ebb-shell-prompt-annotation-failure)))

(defun ebb-shell--running-indicator ()
  "Return the propertized indicator string for a running command."
  (ebb-shell--make-indicator
   ebb-shell-prompt-annotation-running-indicator
   'ebb-shell-prompt-annotation-running))

;;;; ---- OSC 51 Dispatch (called during parsing) ------------------------

(defun ebb-shell--mark-prompt (screen kind)
  "Record a prompt marker of KIND at SCREEN's cursor."
  (let* ((row (ebb-screen-cursor-y screen))
         (column (ebb-screen-cursor-x screen))
         (line (ebb--line-at screen row)))
    (pcase kind
      ('begin (cl-pushnew column (ebb-line-prompt-begins line)))
      ('end (cl-pushnew column (ebb-line-prompt-ends line))))
    (setf (ebb-line-dirty line) t)
    (ebb--mark-dirty screen row)))

(defun ebb-shell-handle-osc51 (payload screen)
  "Handle an OSC 51 shell integration sequence with PAYLOAD.
SCREEN is the ebb-screen model (for cursor position).
Should be called from the ebb event handler when an `osc-51' event
is received.  Events that need buffer positions are queued for
post-render processing."
  (when (and (>= (length payload) 3)
             (eq (aref payload 0) ?e)
             (eq (aref payload 1) ?\;))
    (let ((cmd (aref payload 2))
          (args (ebb-shell--split-payload payload 4)))
      (pcase cmd
        ;; CWD: immediate (no buffer position needed)
        (?A (ebb-shell--set-cwd args))
        ;; Prompt boundaries live on screen lines so rendering, scrolling, and
        ;; batched OSC events cannot lose them.
        (?B (ebb-shell--mark-prompt screen 'begin)
            (when ebb-enable-shell-prompt-annotation
              (push (list 'prompt-start
                          (ebb-shell--absolute-line screen)
                          (ebb-screen-cursor-x screen))
                    ebb-shell--pending-events)))
        (?C (ebb-shell--mark-prompt screen 'end)
            (when ebb-enable-shell-prompt-annotation
              (push (list 'prompt-end
                          (ebb-shell--absolute-line screen)
                          (ebb-screen-cursor-x screen))
                    ebb-shell--pending-events)))
        (?D nil)  ; continuation prompt start (unused)
        (?E nil)  ; continuation prompt end
        ;; Command text: immediate
        (?F (ebb-shell--set-command args))
        ;; Pre-exec: can mutate overlay display spec immediately
        (?G (push '(pre-exec) ebb-shell--pending-events))
        ;; Exit status: immediate
        (?H (ebb-shell--set-exit-status args))
        (?I nil)  ; history exchange (deferred to later)
        (?J nil)  ; before new prompt (no action needed)
        (?M (ebb-shell--handle-message args))
        (_  nil)))))

;;;; ---- Immediate Handlers (no buffer positions needed) ----------------

(defun ebb-shell--set-cwd (args)
  "Set the working directory from base64-encoded ARGS.
ARGS is (HOST-B64 PATH-B64).  Local hosts update a plain path;
remote hosts keep/build a TRAMP path.  Renames the buffer when
`ebb-buffer-name-function' is set."
  (when (and ebb-enable-directory-tracking args (cdr args)
             (fboundp 'ebb--cwd-to-path))
    (let ((path (ebb--cwd-to-path
                 (ebb-shell--base64-decode (cadr args))
                 (ebb-shell--base64-decode (car args)))))
      ;; Remote: trust the shell's report; `file-directory-p' would open
      ;; a synchronous TRAMP connection on every cd.
      (when (and path (if (file-remote-p path) t (file-directory-p path)))
        (setq default-directory (file-name-as-directory path))
        (when (and (fboundp 'ebb-buffer-name-function)
                   ebb-buffer-name-function
                   (fboundp 'ebb--rename-managed))
          (ebb--rename-managed
           (funcall ebb-buffer-name-function
                    (bound-and-true-p ebb--title))))))))

(defun ebb-shell--set-command (args)
  "Handle command text (F sequence).
ARGS is (CMD-B64)."
  (when args
    (setq ebb-shell--current-command
          (ebb-shell--base64-decode (car args)))))

(defun ebb-shell--set-exit-status (args)
  "Handle exit status (H sequence).
ARGS is (STATUS-STRING)."
  (when args
    (setq ebb-shell--command-status
          (string-to-number (car args)))))

(defun ebb-shell--handle-message (args)
  "Decode ARGS and invoke an explicitly whitelisted message handler."
  (when args
    (let* ((decoded (mapcar #'ebb-shell--base64-decode args))
           (handler (and (not (memq nil decoded))
                         (assoc (car decoded)
                                ebb-shell-message-handler-alist))))
      (when handler
        (condition-case error-data
            (save-restriction
              (widen)
              (save-excursion
                (apply (cdr handler) (cdr decoded))))
          (error
           (message "[ebb-shell] Message handler error: %S" error-data)
           nil))))))

;;;; ---- Post-Render Processing -----------------------------------------

(defun ebb-shell-post-render (render)
  "Process pending shell events after rendering.
RENDER is the ebb-render-state.  Called from the I/O module
after `ebb-render-refresh'."
  (when ebb-shell--pending-events
    (let ((events (nreverse ebb-shell--pending-events))
          (inhibit-read-only t))
      (setq ebb-shell--pending-events nil)
      (dolist (event events)
        (pcase (car event)
          ('prompt-start
           (ebb-shell--apply-prompt-start render (nth 1 event) (nth 2 event)))
          ('prompt-end
           (ebb-shell--apply-prompt-end render (nth 1 event) (nth 2 event)))
          ('pre-exec
           (ebb-shell--apply-pre-exec)))))
    ;; Schedule overlay correction only when prompt overlays are enabled.
    (when ebb-enable-shell-prompt-annotation
      (ebb-shell--schedule-overlay-correction))))

(add-hook 'ebb-io-after-render-functions #'ebb-shell-post-render)

(defun ebb-shell--line-to-buffer-pos (render abs-line &optional column)
  "Convert ABS-LINE and COLUMN to a buffer position in RENDER."
  (let* ((display-begin (ebb-render-state-display-begin render))
         (screen (ebb-render-state-screen render))
         (sb-count (ebb-screen-history-row-count screen)))
    (with-current-buffer (ebb-render-state-buffer render)
      (save-excursion
        (cond
         ;; In scrollback region (before display-begin)
         ((< abs-line sb-count)
          (goto-char (ebb-render-state-region-begin render))
          (forward-line abs-line)
          (point))
         ;; In display region
         (t
          (let ((display-row (- abs-line sb-count)))
            (goto-char (marker-position display-begin))
            (forward-line display-row))))
        (move-to-column (or column 0))
        (point)))))

(defun ebb-shell--apply-prompt-start (render abs-line column)
  "Remember an annotation start at ABS-LINE and COLUMN in RENDER."
  (when-let* ((pos (ebb-shell--line-to-buffer-pos render abs-line column)))
    (let ((m (copy-marker pos)))
      (set-marker-insertion-type m t)
      (setq ebb-shell--prompt-start-line m))))

(defun ebb-shell--apply-prompt-end (render abs-line column)
  "Apply a prompt annotation ending at ABS-LINE and COLUMN in RENDER."
  (let ((prompt-begin ebb-shell--prompt-start-line))
    (when (and prompt-begin (markerp prompt-begin)
               (marker-position prompt-begin))
      ;; Finalize previous prompt's indicator with last exit status
      (when ebb-shell--prompt-mark
        (setf (cadr ebb-shell--prompt-mark)
              (ebb-shell--status-indicator ebb-shell--command-status))
        (setq ebb-shell--prompt-mark nil))
      (when-let* ((end (ebb-shell--line-to-buffer-pos
                       render abs-line column)))
        (let ((beg (marker-position prompt-begin)))
          (when (< beg end)
            (let* ((indicator (ebb-shell--running-indicator))
                   (display-spec
                    (list
                     (list 'margin ebb-shell-prompt-annotation-position)
                     indicator))
                   (before-str (propertize " " 'display display-spec))
                   (ov (make-overlay beg (min (1+ beg) end) nil t nil)))
              (overlay-put ov 'before-string before-str)
              (overlay-put ov 'ebb-shell-prompt t)
              (push ov ebb-shell--prompt-overlays)
              (setq ebb-shell--prompt-mark display-spec)))))))
  ;; Reset prompt-start-line for next cycle
  (setq ebb-shell--prompt-start-line nil))

(defun ebb-shell--apply-pre-exec ()
  "Apply pre-exec: update current prompt indicator to \"running\"."
  (when ebb-shell--prompt-mark
    (setf (cadr ebb-shell--prompt-mark) (ebb-shell--running-indicator))))

;;;; ---- Overlay Correction ---------------------------------------------

(defun ebb-shell--schedule-overlay-correction ()
  "Schedule deferred overlay correction.
When terminal output scrolls, overlays may need repositioning."
  (when ebb-shell--overlay-correction-timer
    (cancel-timer ebb-shell--overlay-correction-timer))
  (setq ebb-shell--overlay-correction-timer
        (run-at-time 0.1 nil #'ebb-shell--correct-overlays
                     (current-buffer))))

(defun ebb-shell--correct-overlays (buffer)
  "Correct prompt overlays in BUFFER.
Remove overlays that point to deleted text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ebb-shell--overlay-correction-timer nil)
      (setq ebb-shell--prompt-overlays
            (cl-remove-if-not
             (lambda (ov)
               (and (overlay-buffer ov)
                    (< (overlay-start ov) (overlay-end ov))))
             ebb-shell--prompt-overlays)))))

;;;; ---- Prompt Imenu ---------------------------------------------------

(defun ebb-shell-imenu-create-index ()
  "Return ordered command entries for complete prompts in terminal history."
  (if (bound-and-true-p ebb--screen)
      (let ((width (ebb-screen-width ebb--screen))
            entries)
        (dolist (location (ebb-screen-prompt-end-locations ebb--screen))
          (let* ((row (car location))
                 (command (string-trim
                           (ebb-screen-text-range
                            ebb--screen location (cons row width)))))
            (unless (string-empty-p command)
              ;; A virtual location remains valid even when its row is not in
              ;; the renderer's currently materialized history slab.
              (push (cons command location) entries))))
        (nreverse entries))
    ;; Keep the renderer-only fallback useful to embedders and tests that do
    ;; not install the buffer-local terminal model.
    (let ((position (point-min))
          entries)
      (while-let ((prompt-end
                   (text-property-any position (point-max)
                                      'ebb-shell-prompt-end t)))
        (let* ((command-start (1+ prompt-end))
               (line-start (save-excursion
                             (goto-char prompt-end) (line-beginning-position)))
               (complete (text-property-any line-start command-start
                                            'ebb-shell-prompt-begin t))
               (command (and complete
                             (string-trim
                              (buffer-substring-no-properties
                               command-start
                               (save-excursion
                                 (goto-char command-start)
                                 (line-end-position)))))))
          (when (and command (not (string-empty-p command)))
            (push (cons command (copy-marker command-start)) entries))
          (setq position command-start)))
      (nreverse entries))))

(defun ebb-shell-imenu-goto (_name position &rest _rest)
  "Enter Emacs mode and move to virtual prompt POSITION."
  (ebb-emacs-mode)
  (if (and (consp position) (integerp (car position)))
      (ebb-render-goto-location
       ebb--render (car position) (cdr position))
    (goto-char position)))

;;;; ---- Prompt Navigation ----------------------------------------------

(defun ebb-shell-previous-prompt (&optional n)
  "Move to the Nth previous shell prompt.
N defaults to 1."
  (interactive "p")
  (ebb-shell--goto-prompt (- (or n 1))))

(defun ebb-shell-next-prompt (&optional n)
  "Move to the Nth next shell prompt.
N defaults to 1."
  (interactive "p")
  (ebb-shell--goto-prompt (or n 1)))

(defun ebb-shell--goto-prompt (step)
  "Move STEP prompt boundaries through model-backed history."
  (if (not (and (bound-and-true-p ebb--render)
                (bound-and-true-p ebb--screen)))
      (dotimes (_ (abs step))
        (let ((position
               (if (> step 0)
                   (next-single-property-change
                    (point) 'ebb-shell-prompt-end)
                 (previous-single-property-change
                  (point) 'ebb-shell-prompt-end))))
          (unless position
            (user-error "No %s prompt" (if (> step 0) "next" "previous")))
          (goto-char position)
          (when (get-text-property (point) 'ebb-shell-prompt-end)
            (when-let* ((next
                        (if (> step 0)
                            (next-single-property-change
                             (point) 'ebb-shell-prompt-end)
                          (previous-single-property-change
                           (point) 'ebb-shell-prompt-end))))
              (goto-char next)))))
    (let ((current (ebb-render-buffer-location ebb--render))
          (locations (ebb-screen-prompt-end-locations ebb--screen))
          target)
      (dotimes (_ (abs step))
        (setq target
              (if (> step 0)
                  (cl-find-if
                   (lambda (location)
                     (or (> (car location) (car current))
                         (and (= (car location) (car current))
                              (> (cdr location) (cdr current)))))
                   locations)
                (cl-find-if
                 (lambda (location)
                   (or (< (car location) (car current))
                       (and (= (car location) (car current))
                            (< (cdr location) (cdr current)))))
                 (reverse locations))))
        (unless target
          (user-error "No %s prompt" (if (> step 0) "next" "previous")))
        (setq current target))
      (ebb-render-goto-location
       ebb--render (car current) (cdr current)))))

;;;; ---- Margin Setup ---------------------------------------------------

(defun ebb-shell-setup-margins ()
  "Apply the configured prompt annotation margin to this buffer."
  (setq left-margin-width nil
        right-margin-width nil)
  (when ebb-enable-shell-prompt-annotation
    (let ((margin-width
           (max (string-width ebb-shell-prompt-annotation-running-indicator)
                (string-width ebb-shell-prompt-annotation-success-indicator)
                (string-width ebb-shell-prompt-annotation-failure-indicator))))
      (pcase ebb-shell-prompt-annotation-position
        ('left-margin (setq left-margin-width margin-width))
        ('right-margin (setq right-margin-width margin-width)))))
  ;; Existing windows cache margin widths until their buffer is reset.
  (dolist (win (get-buffer-window-list))
    (set-window-buffer win (current-buffer))))

;;;; ---- Cleanup --------------------------------------------------------

(defun ebb-shell-cleanup ()
  "Clean up shell integration state.
Should be called when the buffer is killed."
  (when ebb-shell--overlay-correction-timer
    (cancel-timer ebb-shell--overlay-correction-timer)
    (setq ebb-shell--overlay-correction-timer nil))
  (dolist (ov ebb-shell--prompt-overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq ebb-shell--prompt-overlays nil
        ebb-shell--prompt-mark nil
        ebb-shell--prompt-start-line nil
        ebb-shell--pending-events nil))

;;;; ---- Shell Integration Directory & Env Setup ------------------------

(defvar ebb-shell--install-path
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "ebb-shell")))
  "Directory where ebb-shell.el is installed.")

(defvar ebb-shell-integration-directory
  (expand-file-name "integration" ebb-shell--install-path)
  "Directory containing shell integration scripts.
This is passed to shells via the `EBB_SHELL_INTEGRATION_DIR'
and `EAT_SHELL_INTEGRATION_DIR' environment variables.")

(defcustom ebb-terminfo-directory
  (expand-file-name "terminfo" ebb-shell--install-path)
  "Directory containing terminfo databases.
Set `TERMINFO' env var so terminal programs find the eat-truecolor
terminfo entry."
  :type 'directory
  :group 'ebb-shell)

(defcustom ebb-term-name "eat-truecolor"
  "Value of `TERM' environment variable for ebb terminals.
Should match an available terminfo entry."
  :type 'string
  :group 'ebb-shell)

(defun ebb-shell-env-vars ()
  "Return a list of environment variable settings for shell integration.
These should be added to `process-environment' before starting
the shell process."
  (list (concat "EBB_SHELL_INTEGRATION_DIR="
                ebb-shell-integration-directory)
        ;; Also set EAT_SHELL_INTEGRATION_DIR for compatibility with
        ;; eat's existing shell scripts
        (concat "EAT_SHELL_INTEGRATION_DIR="
                ebb-shell-integration-directory)
        (concat "TERMINFO=" ebb-terminfo-directory)))

(provide 'ebb-shell)
;;; ebb-shell.el ends here
