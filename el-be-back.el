;;; el-be-back.el --- Terminal emulator for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Arthur
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: terminals
;; URL: https://github.com/ArthurHeymans/el-be-back

;;; Commentary:

;; el-be-back is a terminal emulator for Emacs built on wezterm's terminal
;; emulation engine, exposed as a Rust dynamic module.

;;; Code:

(require 'cl-lib)

;;; --- Customization ---

(defgroup el-be-back nil
  "Terminal emulator built on wezterm."
  :group 'terminals
  :prefix "ebb-")

(defcustom ebb-shell-name (or (bound-and-true-p explicit-shell-file-name)
                              (getenv "SHELL")
                              "/bin/sh")
  "Shell to run in the terminal."
  :type 'string
  :group 'el-be-back)

(defcustom ebb-term-environment-variable "xterm-256color"
  "Value of TERM environment variable for the shell process."
  :type 'string
  :group 'el-be-back)

(defcustom ebb-max-scrollback 10000
  "Maximum number of scrollback lines."
  :type 'integer
  :group 'el-be-back)

(defcustom ebb-minimum-latency 0.008
  "Minimum time in seconds between terminal redraws."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-maximum-latency 0.033
  "Maximum time in seconds before a forced redraw."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-always-compile-module nil
  "If non-nil, compile the module automatically if not found.
When nil, an error is raised with instructions to run
\\[ebb-compile-module] manually."
  :type 'boolean
  :group 'el-be-back)

(defcustom ebb-tramp-method "sshx"
  "TRAMP method to use when constructing paths for remote hosts.
Used when the shell reports a CWD on an unknown remote host
\(e.g. after typing `ssh host' inside ebb)."
  :type 'string
  :group 'el-be-back)

;; --- Phase A defcustoms ---

(defcustom ebb-immediate-redraw-threshold 256
  "Maximum output size (bytes) that triggers an immediate redraw.
When output arrives within `ebb-immediate-redraw-interval' of the
last keystroke and is smaller than this threshold, the redraw
bypasses the timer for lower typing latency.  Set to 0 to disable."
  :type 'integer
  :group 'el-be-back)

(defcustom ebb-immediate-redraw-interval 0.05
  "Maximum time in seconds since last keystroke for immediate redraw."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-input-coalesce-delay 0.003
  "Delay in seconds to coalesce rapid keystrokes.
Single-character keystrokes are buffered for this delay and sent
as a single PTY write.  Set to 0 to disable coalescing."
  :type 'number
  :group 'el-be-back)

(defcustom ebb-scroll-on-input t
  "Automatically scroll to bottom when typing in scrollback."
  :type 'boolean
  :group 'el-be-back)

;; --- Phase B defcustoms ---

(defcustom ebb-shell-integration t
  "If non-nil, auto-inject shell integration scripts.
Provides OSC 7 (CWD), OSC 133 (prompts), and OSC 51 (Elisp eval)
without requiring user RC file edits."
  :type 'boolean
  :group 'el-be-back)

(defcustom ebb-eval-cmds
  '(("find-file" find-file)
    ("find-file-other-window" find-file-other-window)
    ("dired" dired)
    ("dired-other-window" dired-other-window)
    ("message" message))
  "Alist of whitelisted commands for OSC 51 evaluation.
Each entry is (NAME FUNCTION).  Shell scripts call
`ebb_cmd NAME ARGS...' to invoke FUNCTION with ARGS."
  :type '(alist :key-type string :value-type (list function))
  :group 'el-be-back)

(defcustom ebb-enable-osc52 nil
  "If non-nil, allow terminal programs to set the clipboard via OSC 52."
  :type 'boolean
  :group 'el-be-back)

;; --- Phase D defcustoms ---

(defcustom ebb-enable-url-detection t
  "If non-nil, auto-linkify plain-text URLs in terminal output."
  :type 'boolean
  :group 'el-be-back)

(defcustom ebb-enable-file-detection t
  "If non-nil, make file:line references clickable."
  :type 'boolean
  :group 'el-be-back)

;; --- Phase F defcustoms ---

(defcustom ebb-keymap-exceptions
  '("C-c" "C-x" "C-u" "C-h" "C-g" "M-x" "M-o" "M-:" "C-\\")
  "Key sequences that should not be sent to the terminal.
These keys pass through to Emacs instead."
  :type '(repeat string)
  :group 'el-be-back)

;; --- Phase G defcustoms ---

(defcustom ebb-adaptive-fps t
  "If non-nil, stop the redraw timer when idle to save CPU."
  :type 'boolean
  :group 'el-be-back)

(defcustom ebb-module-auto-install nil
  "How to handle a missing native module.
`ask' — prompt the user.
`download' — download a prebuilt binary.
`compile' — compile from source.
nil — raise an error."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Download prebuilt" download)
                 (const :tag "Compile from source" compile)
                 (const :tag "Error" nil))
  :group 'el-be-back)

;;; --- ANSI color faces (Phase E1) ---

(defface ebb-color-black
  '((t :inherit term-color-black))
  "Face used to render ANSI black."
  :group 'el-be-back)

(defface ebb-color-red
  '((t :inherit term-color-red))
  "Face used to render ANSI red."
  :group 'el-be-back)

(defface ebb-color-green
  '((t :inherit term-color-green))
  "Face used to render ANSI green."
  :group 'el-be-back)

(defface ebb-color-yellow
  '((t :inherit term-color-yellow))
  "Face used to render ANSI yellow."
  :group 'el-be-back)

(defface ebb-color-blue
  '((t :inherit term-color-blue))
  "Face used to render ANSI blue."
  :group 'el-be-back)

(defface ebb-color-magenta
  '((t :inherit term-color-magenta))
  "Face used to render ANSI magenta."
  :group 'el-be-back)

(defface ebb-color-cyan
  '((t :inherit term-color-cyan))
  "Face used to render ANSI cyan."
  :group 'el-be-back)

(defface ebb-color-white
  '((t :inherit term-color-white))
  "Face used to render ANSI white."
  :group 'el-be-back)

(defface ebb-color-bright-black
  `((t :inherit ,(if (facep 'term-color-bright-black)
                     'term-color-bright-black
                   'term-color-black)))
  "Face used to render ANSI bright black."
  :group 'el-be-back)

(defface ebb-color-bright-red
  `((t :inherit ,(if (facep 'term-color-bright-red)
                     'term-color-bright-red
                   'term-color-red)))
  "Face used to render ANSI bright red."
  :group 'el-be-back)

(defface ebb-color-bright-green
  `((t :inherit ,(if (facep 'term-color-bright-green)
                     'term-color-bright-green
                   'term-color-green)))
  "Face used to render ANSI bright green."
  :group 'el-be-back)

(defface ebb-color-bright-yellow
  `((t :inherit ,(if (facep 'term-color-bright-yellow)
                     'term-color-bright-yellow
                   'term-color-yellow)))
  "Face used to render ANSI bright yellow."
  :group 'el-be-back)

(defface ebb-color-bright-blue
  `((t :inherit ,(if (facep 'term-color-bright-blue)
                     'term-color-bright-blue
                   'term-color-blue)))
  "Face used to render ANSI bright blue."
  :group 'el-be-back)

(defface ebb-color-bright-magenta
  `((t :inherit ,(if (facep 'term-color-bright-magenta)
                     'term-color-bright-magenta
                   'term-color-magenta)))
  "Face used to render ANSI bright magenta."
  :group 'el-be-back)

(defface ebb-color-bright-cyan
  `((t :inherit ,(if (facep 'term-color-bright-cyan)
                     'term-color-bright-cyan
                   'term-color-cyan)))
  "Face used to render ANSI bright cyan."
  :group 'el-be-back)

(defface ebb-color-bright-white
  `((t :inherit ,(if (facep 'term-color-bright-white)
                     'term-color-bright-white
                   'term-color-white)))
  "Face used to render ANSI bright white."
  :group 'el-be-back)

(defvar ebb-color-palette
  [ebb-color-black
   ebb-color-red
   ebb-color-green
   ebb-color-yellow
   ebb-color-blue
   ebb-color-magenta
   ebb-color-cyan
   ebb-color-white
   ebb-color-bright-black
   ebb-color-bright-red
   ebb-color-bright-green
   ebb-color-bright-yellow
   ebb-color-bright-blue
   ebb-color-bright-magenta
   ebb-color-bright-cyan
   ebb-color-bright-white]
  "Color palette for the terminal (vector of 16 face names).")

;;; --- Module loading ---

(defconst ebb--module-name "ebb-module"
  "Name of the dynamic module.")

(defconst ebb--minimum-module-version "0.1.0"
  "Minimum module version required by this Elisp.")

(defconst ebb--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing el-be-back.")

(defun ebb--module-file ()
  "Return the expected path of the compiled module."
  (expand-file-name (concat ebb--module-name ".so") ebb--directory))

(defun ebb-compile-module ()
  "Compile the Rust dynamic module.
On NixOS, uses `nix shell' to provide a working Rust toolchain
\(rustup's linker paths are broken on NixOS).  Otherwise uses
`cargo build --release' directly."
  (interactive)
  (let ((default-directory ebb--directory))
    (cond
     ;; NixOS: always use nix shell (rustup's linker is broken)
     ((file-exists-p "/etc/NIXOS")
      (ebb--compile-module-nix))
     ;; Has cargo: use it directly
     ((executable-find "cargo")
      (ebb--compile-module-cargo))
     ;; No cargo but has nix: use nix shell
     ((executable-find "nix")
      (ebb--compile-module-nix))
     (t
      (error "[ebb] Neither cargo nor nix found.  Install Rust or Nix to compile the module")))))

(defun ebb--compile-module-nix ()
  "Compile the module using a nix-provided Rust toolchain."
  (let ((default-directory ebb--directory))
    (message "[ebb] Compiling module via nix shell (this may take a few minutes)...")
    (let ((status (call-process
                   "nix" nil "*ebb-compile*" t
                   "shell" "nixpkgs#cargo" "nixpkgs#rustc" "nixpkgs#gcc"
                   "-c" "cargo" "build" "--release")))
      (unless (= status 0)
        (pop-to-buffer "*ebb-compile*")
        (error "[ebb] nix cargo build failed (exit code %d)" status)))
    (ebb--install-built-module)))

(defun ebb--compile-module-cargo ()
  "Compile the module using `cargo build --release'."
  (let ((default-directory ebb--directory))
    (message "[ebb] Compiling module via cargo (this may take a minute)...")
    (let ((status (call-process
                   "cargo" nil "*ebb-compile*" t
                   "build" "--release")))
      (unless (= status 0)
        (pop-to-buffer "*ebb-compile*")
        (error "[ebb] cargo build failed (exit code %d)" status)))
    (ebb--install-built-module)))

(defun ebb--install-built-module ()
  "Copy the built module from target/release/ to the package directory."
  (let* ((target-dir (expand-file-name "target/release/" ebb--directory))
         ;; Cargo converts hyphens to underscores in library filenames
         (crate-name (replace-regexp-in-string "-" "_" ebb--module-name))
         (lib-name (cond
                    ((eq system-type 'darwin)
                     (concat "lib" crate-name ".dylib"))
                    (t
                     (concat "lib" crate-name ".so"))))
         (src (expand-file-name lib-name target-dir))
         (dst (ebb--module-file)))
    (unless (file-exists-p src)
      (error "[ebb] Built library not found at %s" src))
    (copy-file src dst t)
    (message "[ebb] Module compiled successfully.")))

;; --- Module auto-download (Phase G3) ---

(defun ebb--module-platform-tag ()
  "Detect platform tag for prebuilt binary downloads."
  (let ((arch (car (split-string system-configuration "-")))
        (os (cond ((eq system-type 'darwin) "macos")
                  ((eq system-type 'gnu/linux) "linux")
                  (t nil))))
    (when os
      (format "%s-%s" arch os))))

(defun ebb--module-download-url ()
  "Construct GitHub release URL for prebuilt module."
  (let ((tag (ebb--module-platform-tag)))
    (when tag
      (format "https://github.com/ArthurHeymans/el-be-back/releases/latest/download/ebb-module-%s.so"
              tag))))

(defun ebb-download-module ()
  "Download a prebuilt module binary."
  (interactive)
  (let ((url (ebb--module-download-url))
        (dst (ebb--module-file)))
    (unless url
      (error "[ebb] Unsupported platform: %s" system-configuration))
    (message "[ebb] Downloading module from %s..." url)
    (url-copy-file url dst t)
    (message "[ebb] Module downloaded to %s" dst)))

(add-to-list 'load-path ebb--directory)
(unless (require 'ebb-module nil t)
  (pcase ebb-module-auto-install
    ('compile (ebb-compile-module) (require 'ebb-module))
    ('download (ebb-download-module) (require 'ebb-module))
    ('ask
     (if (y-or-n-p "[ebb] Module not found. Compile from source? ")
         (progn (ebb-compile-module) (require 'ebb-module))
       (error "[ebb] Module not found. Run M-x ebb-compile-module")))
    (_ (if ebb-always-compile-module
           (progn (ebb-compile-module) (require 'ebb-module))
         (error "[ebb] Module not found. Run M-x ebb-compile-module")))))

;; --- Module version check (Phase G4) ---

(when (and (fboundp 'ebb--version)
           (version< (ebb--version) ebb--minimum-module-version))
  (warn "[ebb] Module version %s is older than required %s. \
Run M-x ebb-compile-module to update."
        (ebb--version) ebb--minimum-module-version))

;;; --- Buffer-local variables ---

(defvar-local ebb--terminal nil
  "The terminal instance (Rust user-ptr).")

(defvar-local ebb--process nil
  "The shell process.")

(defvar-local ebb--pending-chunks nil
  "List of output chunks waiting to be fed to the terminal.")

(defvar-local ebb--render-timer nil
  "Timer for latency-bounded rendering.")

(defvar-local ebb--first-chunk-time nil
  "Timestamp of the first pending output chunk.")

(defvar-local ebb--copy-mode nil
  "Non-nil when copy mode is active (terminal output paused).")

(defvar-local ebb--remote-prefix nil
  "TRAMP remote prefix for SSH sessions.")

;; Phase A5: immediate redraw
(defvar-local ebb--last-send-time nil
  "Timestamp of the last keystroke sent to the terminal.")

;; Phase A6: input coalescing
(defvar-local ebb--input-buffer nil
  "Accumulated keystrokes waiting to be flushed to the PTY.")

(defvar-local ebb--input-timer nil
  "Timer for flushing coalesced input.")

;; Phase A3: synchronized output
(defvar-local ebb--force-next-redraw nil
  "If non-nil, force a redraw on the next timer cycle.")

;; Phase B2: OSC 133 prompt tracking
(defvar-local ebb--prompt-positions nil
  "Alist of prompt positions: ((LINE . EXIT-CODE) ...).")

;; Phase C2: yank-pop tracking
(defvar-local ebb--yank-index 0
  "Index into kill ring for yank-pop cycling.")

;; Copy mode state
(defvar-local ebb--saved-cursor-type nil
  "Saved cursor-type before entering copy mode.")

;; Phase E4: face remapping cookie
(defvar-local ebb--face-remap-cookie nil
  "Cookie from `face-remap-add-relative' for buffer face.")

;;; --- Theme integration (Phase E) ---

(defun ebb--face-hex-color (face attr)
  "Extract hex color string from FACE's ATTR (:foreground or :background).
Falls back to \"#000000\" if the color cannot be resolved."
  (or (let ((color (face-attribute face attr nil 'default)))
        (when (and (stringp color) (not (string= color "unspecified")))
          (let ((rgb (color-values color)))
            (if rgb
                (apply #'format "#%02x%02x%02x"
                       (mapcar (lambda (c) (ash c -8)) rgb))
              ;; Batch mode: color-values returns nil without a display.
              (and (string-prefix-p "#" color) (= (length color) 7)
                   color)))))
      "#000000"))

(defun ebb--apply-palette (term)
  "Apply colors from `ebb-color-palette' faces and default fg/bg to TERM."
  (when term
    (ebb--set-default-colors
     term
     (ebb--face-hex-color 'default :foreground)
     (ebb--face-hex-color 'default :background))
    (when ebb-color-palette
      (let ((colors
             (mapconcat
              (lambda (face)
                (ebb--face-hex-color face :foreground))
              ebb-color-palette
              "")))
        (ebb--set-palette term colors)))))

(defun ebb--update-buffer-face ()
  "Set the buffer's default face to match the terminal background."
  (when ebb--face-remap-cookie
    (face-remap-remove-relative ebb--face-remap-cookie)
    (setq ebb--face-remap-cookie nil))
  (let ((fg (ebb--face-hex-color 'default :foreground))
        (bg (ebb--face-hex-color 'default :background)))
    (setq ebb--face-remap-cookie
          (face-remap-add-relative 'default
                                   :foreground fg
                                   :background bg))))

(defun ebb-sync-theme ()
  "Re-sync the terminal palette after an Emacs theme change.
Iterates all live ebb buffers and updates their palette, default
colors, buffer face, and forces a full redraw."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'ebb-mode) ebb--terminal)
        (ebb--apply-palette ebb--terminal)
        (ebb--update-buffer-face)
        ;; Force full redraw
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (ebb--render-screen))))))

;; Hook into theme changes (Emacs 29+)
(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions (lambda (_) (ebb-sync-theme))))

;;; --- OSC 7 directory tracking injection ---

(defun ebb--osc7-injection ()
  "Return shell commands to set up OSC 7 directory tracking.
Detects the local shell type from `ebb-shell-name' and returns the
appropriate prompt hook."
  (let ((shell (file-name-nondirectory ebb-shell-name)))
    (cond
     ((string-match-p "fish" shell)
      (concat
       " function __ebb_osc7 --on-event fish_prompt\n"
       "     printf '\\e]7;file://%s%s\\e\\\\' (hostname)"
       " (string escape --style=url -- $PWD)\n"
       " end\n"))
     ((string-match-p "zsh" shell)
      (concat
       " __ebb_osc7() { printf '\\033]7;file://%s%s\\033\\\\'"
       " \"$(hostname)\" \"$(pwd)\"; }\n"
       " precmd_functions+=(__ebb_osc7)\n"))
     (t
      " PROMPT_COMMAND='printf \"\\033]7;file://%s%s\\033\\\\\\\\\" \"$(hostname)\" \"$(pwd)\"'\n"))))

;;; --- CWD resolution ---

(defun ebb--resolve-cwd (cwd)
  "Convert an OSC 7 CWD URL to a local or TRAMP path."
  (let* ((hostname (and (string-match "^file://\\([^/]*\\)" cwd)
                        (match-string 1 cwd)))
         (path (if (string-prefix-p "file://" cwd)
                   (replace-regexp-in-string "^file://[^/]*" "" cwd)
                 cwd))
         (local-host-p (or (null hostname)
                           (string= hostname "")
                           (string-equal hostname "localhost")
                           (string-equal hostname (system-name))
                           (string-equal hostname
                                         (car (split-string (system-name) "\\."))))))
    (cond
     (local-host-p path)
     (ebb--remote-prefix (concat ebb--remote-prefix path))
     (t (format "/%s:%s:%s" ebb-tramp-method hostname path)))))

;;; --- Link infrastructure (Phase D3) ---

(defvar ebb-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'ebb-open-link-at-click)
    (define-key map [mouse-2] #'ebb-open-link-at-click)
    (define-key map (kbd "RET") #'ebb-open-link-at-point)
    map)
  "Keymap active on links (OSC 8, URL, file:line) in the terminal buffer.")

;; Keep the old name as an alias for compatibility
(defvar ebb-hyperlink-map ebb-link-map
  "Keymap active on OSC 8 hyperlinks in the terminal buffer.")

(defun ebb--open-link (url)
  "Open URL, dispatching by scheme.
`fileref:' URIs open the file at the given line in another window.
`file://' URIs open in Emacs.  Other schemes use `browse-url'."
  (when (and url (stringp url))
    (cond
     ((string-match "\\`fileref:\\(.*\\):\\([0-9]+\\)\\'" url)
      (let ((file (match-string 1 url))
            (line (string-to-number (match-string 2 url))))
        (when (file-exists-p file)
          (find-file-other-window file)
          (goto-char (point-min))
          (forward-line (1- line)))))
     ((string-match "\\`file://\\(?:localhost\\)?\\(/.*\\)" url)
      (find-file (url-unhex-string (match-string 1 url))))
     ((string-match-p "\\`[a-z]+://" url)
      (browse-url url)))))

(defun ebb-open-link-at-point ()
  "Open the link at point."
  (interactive)
  (let ((url (or (get-text-property (point) 'help-echo)
                 (get-text-property (point) 'ebb-url))))
    (if url
        (ebb--open-link url)
      (message "No link at point"))))

(defun ebb-open-link-at-click (event)
  "Open the link at the mouse click EVENT."
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (url (and pos (or (get-text-property pos 'help-echo)
                           (get-text-property pos 'ebb-url)))))
    (if url
        (ebb--open-link url)
      (message "No link at click position"))))

;; Keep old names as aliases
(defalias 'ebb-follow-link-at-point 'ebb-open-link-at-point)
(defalias 'ebb-follow-link-at-mouse 'ebb-open-link-at-click)

;;; --- Link detection (Phase D1, D2) ---

(defun ebb--detect-urls ()
  "Scan the buffer for plain-text URLs and file:line references.
Skips regions that already have a `help-echo' property."
  (save-excursion
    ;; Pass 1: http(s) URLs
    (when ebb-enable-url-detection
      (goto-char (point-min))
      (while (re-search-forward
              "https?://[^ \t\n\r\"<>]*[^ \t\n\r\"<>.,;:!?)>]"
              nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (unless (get-text-property beg 'help-echo)
            (let ((url (match-string-no-properties 0)))
              (put-text-property beg end 'help-echo url)
              (put-text-property beg end 'mouse-face 'highlight)
              (put-text-property beg end 'keymap ebb-link-map))))))
    ;; Pass 2: file:line references
    (when ebb-enable-file-detection
      (goto-char (point-min))
      (while (re-search-forward
              "\\(?:\\./\\|/\\)[^ \t\n\r:\"<>]+:[0-9]+"
              nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (unless (get-text-property beg 'help-echo)
            (let* ((text (match-string-no-properties 0))
                   (sep (string-match ":[0-9]+\\'" text))
                   (path (substring text 0 sep))
                   (line (substring text (1+ sep)))
                   (abs-path (expand-file-name path)))
              (when (file-exists-p abs-path)
                (put-text-property beg end 'help-echo
                                   (concat "fileref:" abs-path ":" line))
                (put-text-property beg end 'mouse-face 'highlight)
                (put-text-property beg end 'keymap ebb-link-map)))))))))

;;; --- OSC scanning (Phase B2, B4, B5) ---

(defun ebb--scan-osc-sequences (data)
  "Scan raw PTY output DATA for OSC sequences and dispatch handlers.
Handles OSC 133 (prompts), OSC 51 (Elisp eval), and OSC 52 (clipboard).
These are scanned before feeding to the terminal parser so we can
act on them at the Elisp level."
  (let ((pos 0)
        (len (length data)))
    (while (< pos len)
      (let ((osc-start (string-match "\e\\]" data pos)))
        (if (not osc-start)
            (setq pos len)
          (let ((st-pos (string-match "[\a\e]" data (+ osc-start 2))))
            (if (not st-pos)
                (setq pos len)
              ;; Handle ST = ESC \ (two chars) or BEL (one char)
              (let* ((terminator-len (if (and (< (1+ st-pos) len)
                                              (= (aref data st-pos) ?\e)
                                              (= (aref data (1+ st-pos)) ?\\))
                                         2
                                       1))
                     (payload (substring data (+ osc-start 2) st-pos)))
                (cond
                 ;; OSC 133 — semantic prompt markers
                 ((string-prefix-p "133;" payload)
                  (let* ((rest (substring payload 4))
                         (type (and (> (length rest) 0)
                                    (substring rest 0 1)))
                         (param (and (> (length rest) 2)
                                     (= (aref rest 1) ?\;)
                                     (substring rest 2))))
                    (ebb--osc133-marker type param)))
                 ;; OSC 51 — Elisp eval
                 ((string-prefix-p "51;" payload)
                  (let ((rest (substring payload 3)))
                    (when (and (> (length rest) 0)
                               (= (aref rest 0) ?E))
                      (ebb--osc51-eval (substring rest 1)))))
                 ;; OSC 52 — clipboard
                 ((string-prefix-p "52;" payload)
                  (let* ((rest (substring payload 3))
                         (semi (string-match ";" rest)))
                    (when semi
                      (let ((b64 (substring rest (1+ semi))))
                        (unless (string= b64 "?")
                          (ebb--osc52-handle b64)))))))
                (setq pos (+ st-pos terminator-len))))))))))

;;; --- OSC 133 handler (Phase B2) ---

(defun ebb--osc133-marker (type param)
  "Handle an OSC 133 semantic prompt marker.
TYPE is a single-char string (A/B/C/D).  PARAM is the optional
parameter string (e.g. exit status for D markers)."
  (pcase type
    ("A"
     ;; Prompt start: record buffer line
     (push (cons (line-number-at-pos (point-max)) nil)
           ebb--prompt-positions))
    ("D"
     ;; Command finished: record exit status on most recent prompt
     (when ebb--prompt-positions
       (setcdr (car ebb--prompt-positions)
               (and param (string-to-number param)))))))

;;; --- OSC 51 handler (Phase B4) ---

(defun ebb--osc51-eval (str)
  "Handle an OSC 51 Elisp eval payload STR.
Parses the command and arguments, looks up in `ebb-eval-cmds'."
  (condition-case err
      (let* ((parts (split-string-and-unquote str))
             (cmd (car parts))
             (args (cdr parts))
             (entry (assoc cmd ebb-eval-cmds)))
        (if entry
            (apply (cadr entry) args)
          (message "[ebb] Unknown OSC 51 command: %s" cmd)))
    (error (message "[ebb] OSC 51 eval error: %s" (error-message-string err)))))

;;; --- OSC 52 handler (Phase B5) ---

(defun ebb--osc52-handle (base64-data)
  "Handle an OSC 52 clipboard payload BASE64-DATA."
  (when ebb-enable-osc52
    (condition-case err
        (let ((text (base64-decode-string base64-data)))
          (kill-new text)
          (when (fboundp 'gui-set-selection)
            (gui-set-selection 'CLIPBOARD text)))
      (error (message "[ebb] OSC 52 error: %s" (error-message-string err))))))

;;; --- Prompt navigation (Phase B3) ---

(defun ebb-next-prompt (&optional n)
  "Move to the next shell prompt.  With prefix arg N, move N prompts."
  (interactive "p")
  (unless ebb-copy-mode
    (ebb-copy-mode 1))
  (let ((found (text-property-search-forward 'ebb-prompt t nil n)))
    (when found
      (goto-char (prop-match-beginning found)))))

(defun ebb-previous-prompt (&optional n)
  "Move to the previous shell prompt.  With prefix arg N, move N prompts."
  (interactive "p")
  ;; Skip past current prompt region first
  (when (get-text-property (point) 'ebb-prompt)
    (goto-char (or (previous-single-property-change (point) 'ebb-prompt)
                   (point-min))))
  (unless ebb-copy-mode
    (ebb-copy-mode 1))
  (let ((found (text-property-search-backward 'ebb-prompt t nil n)))
    (when found
      (goto-char (prop-match-beginning found)))))

;;; --- Copy mode (Phase C) ---

(defun ebb--filter-soft-wraps (text)
  "Remove newlines from TEXT that were inserted by soft line wrapping.
These are newlines with the `ebb-wrap' text property."
  (let ((result "")
        (pos 0)
        (len (length text)))
    (while (< pos len)
      (if (and (eq (aref text pos) ?\n)
               (get-text-property pos 'ebb-wrap text))
          (setq pos (1+ pos))
        (setq result (concat result (substring text pos (1+ pos)))
              pos (1+ pos))))
    result))

(defun ebb--clean-copy-text (text)
  "Clean TEXT for copying: remove soft-wrap newlines, strip trailing whitespace."
  (let* ((unwrapped (ebb--filter-soft-wraps text))
         (lines (split-string unwrapped "\n"))
         (trimmed (mapcar (lambda (line) (string-trim-right line)) lines)))
    (mapconcat #'identity trimmed "\n")))

(defvar ebb-copy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Exit
    (define-key map (kbd "C-c C-c") #'ebb-copy-mode-exit)
    (define-key map (kbd "C-c C-t") #'ebb-copy-mode-exit)
    (define-key map (kbd "q") #'ebb-copy-mode-exit)
    ;; Copy
    (define-key map (kbd "M-w") #'ebb-copy-mode-copy)
    (define-key map (kbd "C-w") #'ebb-copy-mode-copy)
    ;; Page scrolling
    (define-key map (kbd "M-v") #'scroll-down-command)
    (define-key map (kbd "C-v") #'scroll-up-command)
    ;; Beginning/end of scrollback
    (define-key map (kbd "M-<") #'beginning-of-buffer)
    (define-key map (kbd "M->") #'end-of-buffer)
    ;; Line movement with viewport scroll at edges
    (define-key map (kbd "C-n") #'ebb-copy-mode-next-line)
    (define-key map (kbd "C-p") #'ebb-copy-mode-previous-line)
    ;; End of line (non-whitespace)
    (define-key map (kbd "C-e") #'ebb-copy-mode-end-of-line)
    ;; Prompt navigation
    (define-key map (kbd "C-c C-n") #'ebb-next-prompt)
    (define-key map (kbd "C-c C-p") #'ebb-previous-prompt)
    ;; Self-insert exits copy mode and sends the key
    (let ((i 32))
      (while (<= i 126)
        (define-key map (string i) #'ebb-copy-mode-exit-and-send)
        (setq i (1+ i))))
    map)
  "Keymap for copy mode (terminal paused, Emacs navigation).")

(define-minor-mode ebb-copy-mode
  "Minor mode for navigating terminal output.
Terminal output is paused; standard Emacs keys work for navigation,
search, and copying."
  :lighter " Copy"
  :keymap ebb-copy-mode-map
  (if ebb-copy-mode
      (progn
        (setq ebb--copy-mode t
              ebb--saved-cursor-type cursor-type)
        ;; Disable semi-char mode while in copy mode
        (when (bound-and-true-p ebb-semi-char-mode)
          (ebb-semi-char-mode -1))
        ;; Restore visible cursor
        (setq cursor-type (default-value 'cursor-type))
        (setq buffer-read-only t)
        (message "Copy mode: navigate with Emacs keys, q or C-c C-c to exit"))
    (setq ebb--copy-mode nil)
    ;; Restore cursor type
    (when ebb--saved-cursor-type
      (setq cursor-type ebb--saved-cursor-type))
    (setq buffer-read-only t)
    ;; Re-enable semi-char mode
    (ebb-semi-char-mode 1)
    ;; Scroll to bottom and flush pending output
    (when ebb--terminal
      (setq ebb--force-next-redraw t))
    (ebb--flush-output)))

(defun ebb-copy-mode-exit ()
  "Exit copy mode and resume terminal."
  (interactive)
  (ebb-copy-mode -1))

(defun ebb-copy-mode-copy ()
  "Copy the selected region and exit copy mode.
Soft-wrapped newlines are removed and trailing whitespace is stripped."
  (interactive)
  (when (use-region-p)
    (let ((text (ebb--clean-copy-text
                 (buffer-substring (region-beginning) (region-end)))))
      (kill-new text)
      (message "Copied to kill ring")))
  (ebb-copy-mode-exit))

(defun ebb-copy-mode-exit-and-send ()
  "Exit copy mode and send the typed character to the terminal."
  (interactive)
  (let ((key (this-command-keys)))
    (ebb-copy-mode-exit)
    (when (and ebb--process (process-live-p ebb--process))
      (ebb--send-key (string (aref key (1- (length key))))))))

(defun ebb-copy-mode-next-line ()
  "Move to next line, scrolling viewport at bottom edge."
  (interactive)
  (forward-line 1))

(defun ebb-copy-mode-previous-line ()
  "Move to previous line, scrolling viewport at top edge."
  (interactive)
  (forward-line -1))

(defun ebb-copy-mode-end-of-line ()
  "Move to last non-whitespace character on the line."
  (interactive)
  (end-of-line)
  (skip-chars-backward " \t"))

;;; --- Major mode ---

(define-derived-mode ebb-mode fundamental-mode "EBB"
  "Major mode for el-be-back terminal emulator."
  (buffer-disable-undo)
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  ;; Larger PTY read buffer
  (setq-local read-process-output-max (* 64 1024))
  (setq-local scroll-conservatively 101)
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0)
  (setq-local left-margin-width 0)
  (setq-local right-margin-width 0)
  ;; Disable font-lock completely
  (setq-local font-lock-defaults nil)
  (setq-local font-lock-function #'ignore)
  (setq-local font-lock-keywords nil)
  (font-lock-mode -1)
  (when (bound-and-true-p jit-lock-mode)
    (jit-lock-mode nil))
  (when (bound-and-true-p indent-bars-mode)
    (indent-bars-mode -1))
  ;; Header line
  (setq-local header-line-format
              '(:eval (ebb--header-line)))
  (add-hook 'kill-buffer-hook #'ebb--kill-buffer-hook nil t)
  (add-hook 'window-state-change-functions #'ebb--window-state-change nil t))

(defun ebb--header-line ()
  "Generate the header line for the terminal buffer."
  (let ((title (and ebb--terminal (ebb--get-title ebb--terminal)))
        (cwd (and ebb--terminal (ebb--get-cwd ebb--terminal)))
        (size (format "%dx%d"
                      (if ebb--terminal (ebb--get-rows ebb--terminal) 0)
                      (if ebb--terminal (ebb--get-cols ebb--terminal) 0))))
    (concat " EBB"
            (when ebb--copy-mode " [COPY]")
            (when title (format " | %s" title))
            (when cwd
              (format " | %s" (abbreviate-file-name
                                (ebb--resolve-cwd cwd))))
            (format " | %s" size))))

;;; --- Process management ---

(defun ebb--process-filter (process output)
  "Process filter: queue OUTPUT from PROCESS and schedule rendering."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (and ebb--terminal (not (string-empty-p output)))
        ;; Scan for OSC sequences before queuing
        (ebb--scan-osc-sequences output)
        ;; Always queue the output (even in copy mode)
        (push output ebb--pending-chunks)
        (unless ebb--first-chunk-time
          (setq ebb--first-chunk-time (current-time)))
        ;; Cancel existing timer
        (when ebb--render-timer
          (cancel-timer ebb--render-timer)
          (setq ebb--render-timer nil))
        ;; Don't schedule render in copy mode -- just queue
        (unless ebb--copy-mode
          ;; Phase A5: immediate redraw for typing echo
          (if (and (> ebb-immediate-redraw-threshold 0)
                   ebb--last-send-time
                   (<= (length output) ebb-immediate-redraw-threshold)
                   (< (float-time (time-subtract (current-time)
                                                 ebb--last-send-time))
                      ebb-immediate-redraw-interval))
              ;; Immediate redraw: small output shortly after keystroke
              (ebb--flush-output)
            ;; Standard latency-bounded scheduling
            (let ((elapsed (float-time
                            (time-subtract nil ebb--first-chunk-time))))
              (if (>= elapsed ebb-maximum-latency)
                  (ebb--flush-output)
                (setq ebb--render-timer
                      (run-with-timer
                       (min (- ebb-maximum-latency elapsed)
                            ebb-minimum-latency)
                       nil #'ebb--flush-output-in-buffer
                       (current-buffer)))))))))))

(defun ebb--flush-output-in-buffer (buffer)
  "Flush output in BUFFER if it is still alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (ebb--flush-output))))

(defun ebb--flush-output ()
  "Process all pending output chunks and render."
  ;; Cancel pending timer
  (when ebb--render-timer
    (cancel-timer ebb--render-timer)
    (setq ebb--render-timer nil))
  (when (and ebb--terminal ebb--pending-chunks)
    ;; Feed current pending chunks.
    (let ((chunks (nreverse ebb--pending-chunks)))
      (setq ebb--pending-chunks nil
            ebb--first-chunk-time nil)
      (ebb--feed ebb--terminal (apply #'concat chunks)))
    ;; Drain any immediately available additional data before rendering.
    (let ((deadline (+ (float-time) ebb-maximum-latency)))
      (while (and ebb--process
                  (process-live-p ebb--process)
                  (< (float-time) deadline)
                  (accept-process-output ebb--process 0 nil t)
                  ebb--pending-chunks)
        (when ebb--render-timer
          (cancel-timer ebb--render-timer)
          (setq ebb--render-timer nil))
        (let ((chunks (nreverse ebb--pending-chunks)))
          (setq ebb--pending-chunks nil
                ebb--first-chunk-time nil)
          (ebb--feed ebb--terminal (apply #'concat chunks)))))
    ;; Render the final screen state once.
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (inhibit-quit t)
          (buffer-undo-list t))
      (ebb--render-screen)
      ;; Detect links after rendering
      (ebb--detect-urls))
    ;; Drain terminal responses and send to PTY
    (ebb--drain-and-send)
    ;; Process title/CWD/bell alerts
    (ebb--process-alerts)))

(defun ebb--render-screen (&optional window)
  "Render the terminal screen into the current buffer."
  (when ebb--terminal
    (ebb--render ebb--terminal)
    ;; Phase A4: update cursor style
    (ebb--update-cursor-style)
    ;; Pin the window to show the display region
    (unless ebb-copy-mode
      (let ((win (or window (get-buffer-window (current-buffer)))))
        (when win
          (let ((rows (ebb--get-rows ebb--terminal)))
            (save-excursion
              (goto-char (point-max))
              (forward-line (- (1- rows)))
              (set-window-start win (point) t))))))))

(defun ebb--update-cursor-style ()
  "Set cursor-type based on DECSCUSR and cursor visibility."
  (when ebb--terminal
    (let ((style (ebb--cursor-style ebb--terminal))
          (visible (ebb--cursor-visible ebb--terminal)))
      (setq cursor-type
            (if visible
                (pcase style
                  (0 '(bar . 2))       ; bar
                  (1 'box)             ; block
                  (2 '(hbar . 2))      ; underline
                  (_ 'box))
              nil)))))

(defun ebb--drain-and-send ()
  "Drain captured terminal output and send to the PTY."
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((response (ebb--drain-output ebb--terminal)))
      (when response
        (process-send-string ebb--process response)))))

(defun ebb--process-alerts ()
  "Process pending alerts (title, CWD, bell) from the terminal."
  (when ebb--terminal
    ;; Title change
    (let ((title (ebb--poll-title ebb--terminal)))
      (when title
        (rename-buffer (format "*ebb: %s*" title) t)))
    ;; CWD change
    (let ((cwd (ebb--poll-cwd ebb--terminal)))
      (when cwd
        (let ((dir (ebb--resolve-cwd cwd)))
          (when (or (file-remote-p dir) (file-directory-p dir))
            (setq-local default-directory
                        (file-name-as-directory dir))))))
    ;; Bell
    (when (ebb--poll-bell ebb--terminal)
      (ding t))))

(defun ebb--process-sentinel (process event)
  "Handle PROCESS state change EVENT."
  (when (buffer-live-p (process-buffer process))
    (let ((buf (process-buffer process)))
      (if (memq (process-status process) '(exit signal))
          (kill-buffer buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\n[Process %s]\n" (string-trim event)))))))))

(defun ebb--kill-buffer-hook ()
  "Clean up when the terminal buffer is killed."
  (when ebb--render-timer
    (cancel-timer ebb--render-timer))
  (when ebb--input-timer
    (cancel-timer ebb--input-timer))
  (when (and ebb--process (process-live-p ebb--process))
    (delete-process ebb--process))
  ;; Remove focus change hook
  (remove-function after-focus-change-function #'ebb--focus-change)
  (when ebb--terminal
    (ebb--free ebb--terminal)
    (setq ebb--terminal nil)))

;;; --- Focus events (Phase A2) ---

(defun ebb--focus-change ()
  "Notify ebb terminals about focus change."
  (let ((focused (frame-focus-state)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'ebb-mode)
                   ebb--terminal
                   ebb--process
                   (process-live-p ebb--process))
          (let ((encoded (ebb--focus-event ebb--terminal (if focused 1 nil))))
            (when encoded
              (process-send-string ebb--process encoded))))))))

;;; --- Resize handling ---

(defun ebb--window-state-change (window)
  "Handle state change (including resize) for WINDOW."
  (when (and ebb--terminal (window-live-p window))
    (let ((rows (window-body-height window))
          (cols (window-body-width window))
          (cur-rows (ebb--get-rows ebb--terminal))
          (cur-cols (ebb--get-cols ebb--terminal)))
      (when (or (/= rows cur-rows)
                (/= cols cur-cols))
        (ebb--resize ebb--terminal rows cols)
        (when (and ebb--process (process-live-p ebb--process))
          (set-process-window-size ebb--process rows cols))
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              (buffer-undo-list t))
          (ebb--render-screen window))))))

;;; --- Input handling ---

(defun ebb-send-raw (string)
  "Send STRING directly to the terminal's PTY."
  (interactive "sSend: ")
  (when (and ebb--process (process-live-p ebb--process))
    (process-send-string ebb--process string)))

(defconst ebb--simple-key-bytes
  '(("return"    . "\r")
    ("backspace" . "\x7f")
    ("tab"       . "\t")
    ("escape"    . "\x1b")
    ("DEL"       . "\x7f")
    ("delete"    . "\x1b[3~")
    ("deletechar" . "\x1b[3~")
    ("RET"       . "\r")
    ("TAB"       . "\t")
    ("ESC"       . "\x1b"))
  "Direct byte mappings for keys that KeyCode::encode() returns empty for.")

;; --- Phase A6: Input coalescing ---

(defun ebb--flush-input (buffer)
  "Flush coalesced input in BUFFER to the PTY."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ebb--input-timer nil)
      (when (and ebb--input-buffer ebb--process
                 (process-live-p ebb--process))
        (process-send-string ebb--process
                             (apply #'concat (nreverse ebb--input-buffer)))
        (setq ebb--input-buffer nil)))))

(defun ebb--send-key (key-name &optional shift ctrl meta)
  "Send KEY-NAME to the terminal via wezterm key encoding.
Records send time for immediate-redraw detection and optionally
coalesces rapid keystrokes."
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    ;; Phase C3: scroll-on-input
    (when ebb-scroll-on-input
      (setq ebb--force-next-redraw t))
    ;; Phase A5: record send time
    (setq ebb--last-send-time (current-time))
    (let ((encoded (or
                    ;; Primary: terminal.key_down()
                    (let ((result (ebb--key-down ebb--terminal key-name
                                                (if shift 1 nil)
                                                (if ctrl 1 nil)
                                                (if meta 1 nil))))
                      (and result (not (string-empty-p result)) result))
                    ;; Phase F5: raw key fallback
                    (ebb--raw-key-sequence key-name
                                          (+ (if shift 1 0)
                                             (if meta 2 0)
                                             (if ctrl 4 0)))
                    ;; Fallback: simple key byte table
                    (let ((entry (assoc key-name ebb--simple-key-bytes)))
                      (when entry
                        (let ((bytes (cdr entry)))
                          (if meta (concat "\x1b" bytes) bytes))))
                    ;; Fallback: ctrl+letter
                    (when (and ctrl (= (length key-name) 1))
                      (let* ((ch (aref key-name 0))
                             (code (- (downcase ch) ?a -1)))
                        (when (and (>= code 1) (<= code 26))
                          (let ((bytes (string code)))
                            (if meta (concat "\x1b" bytes) bytes)))))
                    ;; Fallback: plain character
                    (when (and (= (length key-name) 1) (not ctrl) (not meta))
                      key-name))))
      (when encoded
        ;; Phase A6: input coalescing
        (if (and (> ebb-input-coalesce-delay 0)
                 (= (length encoded) 1))
            ;; Coalesce single-char keystrokes
            (progn
              (push encoded ebb--input-buffer)
              (unless ebb--input-timer
                (setq ebb--input-timer
                      (run-with-timer ebb-input-coalesce-delay nil
                                      #'ebb--flush-input (current-buffer)))))
          ;; Multi-byte or coalescing disabled: send immediately
          (when ebb--input-timer
            (cancel-timer ebb--input-timer)
            (setq ebb--input-timer nil)
            (when ebb--input-buffer
              (process-send-string ebb--process
                                   (apply #'concat (nreverse ebb--input-buffer)))
              (setq ebb--input-buffer nil)))
          (process-send-string ebb--process encoded))))))

;;; --- Raw key fallback (Phase F5) ---

(defun ebb--csi-letter (letter mod-num)
  "Build a CSI cursor key sequence with optional modifier."
  (if (> mod-num 0)
      (format "\e[1;%d%s" (1+ mod-num) letter)
    (format "\e[%s" letter)))

(defun ebb--csi-tilde (code mod-num)
  "Build a CSI tilde key sequence with optional modifier."
  (if (> mod-num 0)
      (format "\e[%d;%d~" code (1+ mod-num))
    (format "\e[%d~" code)))

(defun ebb--raw-key-sequence (key-name mods)
  "Build a raw escape sequence for KEY-NAME with MODS bitmask.
Returns the sequence string, or nil for unknown keys.
MODS: shift=1, meta=2, ctrl=4."
  (let ((mod-num mods))
    (cond
     ;; Simple special keys (CSI u encoding for modified variants)
     ((string= key-name "backspace") (if (> mod-num 0) (format "\e[127;%du" (1+ mod-num)) nil))
     ((string= key-name "return")    (if (> mod-num 0) (format "\e[13;%du" (1+ mod-num)) nil))
     ((string= key-name "tab")       (if (> mod-num 0) (format "\e[9;%du" (1+ mod-num)) nil))
     ((string= key-name "escape")    (if (> mod-num 0) (format "\e[27;%du" (1+ mod-num)) nil))
     ((string= key-name "space")     (if (> mod-num 0) (format "\e[32;%du" (1+ mod-num)) nil))
     ;; Cursor keys
     ((string= key-name "up")    (ebb--csi-letter "A" mod-num))
     ((string= key-name "down")  (ebb--csi-letter "B" mod-num))
     ((string= key-name "right") (ebb--csi-letter "C" mod-num))
     ((string= key-name "left")  (ebb--csi-letter "D" mod-num))
     ((string= key-name "home")  (ebb--csi-letter "H" mod-num))
     ((string= key-name "end")   (ebb--csi-letter "F" mod-num))
     ;; Tilde keys
     ((string= key-name "insert") (ebb--csi-tilde 2 mod-num))
     ((string= key-name "delete") (ebb--csi-tilde 3 mod-num))
     ((string= key-name "prior")  (ebb--csi-tilde 5 mod-num))
     ((string= key-name "next")   (ebb--csi-tilde 6 mod-num))
     ;; Function keys (F1-F4 use SS3, F5-F12 use tilde)
     ((string= key-name "f1")  (if (> mod-num 0) (format "\e[1;%dP" (1+ mod-num)) "\eOP"))
     ((string= key-name "f2")  (if (> mod-num 0) (format "\e[1;%dQ" (1+ mod-num)) "\eOQ"))
     ((string= key-name "f3")  (if (> mod-num 0) (format "\e[1;%dR" (1+ mod-num)) "\eOR"))
     ((string= key-name "f4")  (if (> mod-num 0) (format "\e[1;%dS" (1+ mod-num)) "\eOS"))
     ((string= key-name "f5")  (ebb--csi-tilde 15 mod-num))
     ((string= key-name "f6")  (ebb--csi-tilde 17 mod-num))
     ((string= key-name "f7")  (ebb--csi-tilde 18 mod-num))
     ((string= key-name "f8")  (ebb--csi-tilde 19 mod-num))
     ((string= key-name "f9")  (ebb--csi-tilde 20 mod-num))
     ((string= key-name "f10") (ebb--csi-tilde 21 mod-num))
     ((string= key-name "f11") (ebb--csi-tilde 23 mod-num))
     ((string= key-name "f12") (ebb--csi-tilde 24 mod-num))
     (t nil))))

;;; --- Self-insert and event handlers ---

(defun ebb-self-input ()
  "Send the last input event to the terminal."
  (interactive)
  (let* ((event last-input-event)
         (mods (event-modifiers event))
         (basic (event-basic-type event))
         (shift (and (memq 'shift mods) t))
         (ctrl (and (memq 'control mods) t))
         (meta (and (memq 'meta mods) t))
         (key-name (cond
                    ;; Character event (printable, no ctrl/meta)
                    ((and (characterp basic) (not ctrl) (not meta))
                     (string (if shift (upcase basic) basic)))
                    ;; Character with ctrl/meta
                    ((characterp basic)
                     (string basic))
                    ;; Named key (symbol)
                    ((symbolp basic)
                     (symbol-name basic))
                    (t nil))))
    (when key-name
      (ebb--send-key key-name shift ctrl meta))))

(defun ebb--send-event ()
  "Send the current event (special key with modifiers) to the terminal."
  (interactive)
  (let* ((event last-command-event)
         (mods (event-modifiers event))
         (basic (event-basic-type event))
         (shift (and (memq 'shift mods) t))
         (ctrl (and (memq 'control mods) t))
         (meta (and (memq 'meta mods) t))
         (key-name (cond
                    ((symbolp basic) (symbol-name basic))
                    ((characterp basic) (string basic))
                    (t nil))))
    (when key-name
      (ebb--send-key key-name shift ctrl meta))))

;;; --- Yank and yank-pop (Phase C2) ---

(defun ebb-yank ()
  "Paste the kill ring content into the terminal.
Uses bracketed paste if the terminal has it enabled."
  (interactive)
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((text (current-kill 0)))
      (when text
        (setq ebb--yank-index 0)
        (ebb--send-paste ebb--terminal text)
        (ebb--drain-and-send)
        (setq this-command 'ebb-yank)))))

(defun ebb-yank-pop ()
  "Replace the just-yanked text with the next kill ring entry.
Sends backspaces to erase the previous yank, then pastes the next entry."
  (interactive)
  (unless (memq last-command '(ebb-yank ebb-yank-pop))
    (user-error "Previous command was not a yank"))
  (let* ((prev-text (current-kill ebb--yank-index t))
         (prev-len (length prev-text)))
    (setq ebb--yank-index (1+ ebb--yank-index))
    ;; Erase previous paste: send backspaces
    (when (and ebb--process (process-live-p ebb--process))
      (process-send-string ebb--process
                           (make-string prev-len ?\x7f)))
    ;; Paste the next entry
    (let ((text (current-kill ebb--yank-index t)))
      (when text
        (ebb--send-paste ebb--terminal text)
        (ebb--drain-and-send)))
    (setq this-command 'ebb-yank-pop)))

;;; --- Mouse tracking (Phase A1) ---

(defun ebb--mouse-button-number (event)
  "Return the button number for mouse EVENT.
Maps Emacs mouse-1/2/3 to terminal button 1/3/2."
  (pcase (event-basic-type event)
    ('mouse-1 1)
    ('mouse-2 3)
    ('mouse-3 2)
    (_ 0)))

(defun ebb--mouse-mods (event)
  "Return modifier bitmask for mouse EVENT.
shift=1, meta=2, ctrl=4."
  (let ((mods (event-modifiers event))
        (result 0))
    (when (memq 'shift mods) (setq result (logior result 1)))
    (when (memq 'control mods) (setq result (logior result 4)))
    (when (memq 'meta mods) (setq result (logior result 2)))
    result))

(defun ebb--mouse-press (event)
  "Handle mouse button press EVENT for terminal mouse tracking."
  (interactive "e")
  (select-window (posn-window (event-start event)))
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let* ((posn (event-start event))
           (col-row (posn-col-row posn))
           (col (car col-row))
           (row (cdr col-row)))
      (let ((encoded (ebb--mouse-event ebb--terminal
                                       0  ; press
                                       (ebb--mouse-button-number event)
                                       row col
                                       (ebb--mouse-mods event))))
        (when encoded
          (process-send-string ebb--process encoded))))))

(defun ebb--mouse-release (event)
  "Handle mouse button release EVENT."
  (interactive "e")
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let* ((posn (event-end event))
           (col-row (posn-col-row posn))
           (col (car col-row))
           (row (cdr col-row)))
      (let ((encoded (ebb--mouse-event ebb--terminal
                                       1  ; release
                                       (ebb--mouse-button-number event)
                                       row col
                                       (ebb--mouse-mods event))))
        (when encoded
          (process-send-string ebb--process encoded))))))

(defun ebb--mouse-drag (event)
  "Handle mouse drag EVENT as motion."
  (interactive "e")
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let* ((posn (event-end event))
           (col-row (posn-col-row posn))
           (col (car col-row))
           (row (cdr col-row)))
      (let ((encoded (ebb--mouse-event ebb--terminal
                                       2  ; motion
                                       (ebb--mouse-button-number event)
                                       row col
                                       (ebb--mouse-mods event))))
        (when encoded
          (process-send-string ebb--process encoded))))))

(defun ebb--scroll-up (&optional _event)
  "Scroll the terminal viewport up (mouse wheel up)."
  (interactive "e")
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    ;; Send scroll-up (prior/page-up) key events
    (let ((encoded (ebb--mouse-event ebb--terminal
                                     0 ; press
                                     4 ; wheel up (button 4)
                                     0 0 0)))
      ;; Wheel events go through the mouse event handler if mouse is grabbed,
      ;; otherwise scroll the buffer in copy mode style.
      (if (and (ebb--is-mouse-grabbed ebb--terminal) encoded)
          (process-send-string ebb--process encoded)
        ;; Scroll viewport
        (scroll-down 3)))))

(defun ebb--scroll-down (&optional _event)
  "Scroll the terminal viewport down (mouse wheel down)."
  (interactive "e")
  (when (and ebb--terminal ebb--process (process-live-p ebb--process))
    (let ((encoded (ebb--mouse-event ebb--terminal
                                     0 ; press
                                     5 ; wheel down (button 5)
                                     0 0 0)))
      (if (and (ebb--is-mouse-grabbed ebb--terminal) encoded)
          (process-send-string ebb--process encoded)
        (scroll-up 3)))))

;;; --- Send-next-key (Phase F1) ---

(defun ebb-send-next-key ()
  "Read the next key event and send it to the terminal.
Escape hatch for sending keys normally intercepted by Emacs."
  (interactive)
  (let* ((key (read-key-sequence "Send key: "))
         (char (aref key 0)))
    (cond
     ;; Control character
     ((and (integerp char) (<= char 31))
      (ebb--send-key (string char)))
     ;; Regular character
     ((and (integerp char) (< char 128))
      (ebb--send-key (string char)))
     ;; Multi-byte character
     ((integerp char)
      (ebb--send-key (encode-coding-string (string char) 'utf-8)))
     ;; Function key / special key
     (t
      (let ((binding (key-binding key)))
        (if (and binding (commandp binding))
            (call-interactively binding)
          (message "[ebb] Unrecognized key %S" key)))))))

;;; --- Drag-and-drop (Phase F3) ---

(defun ebb--drop (event)
  "Handle a drag-and-drop EVENT into the terminal.
Dropped files insert their path (shell-quoted); dropped text is
pasted using bracketed paste."
  (interactive "e")
  (when (and ebb--process (process-live-p ebb--process))
    (let ((arg (nth 2 event)))
      (when (and arg (not (eq arg 'lambda)))
        (let ((type (car arg))
              (objects (cddr arg)))
          (if (eq type 'file)
              (ebb--send-key
               (mapconcat #'shell-quote-argument objects " "))
            (when ebb--terminal
              (ebb--send-paste ebb--terminal
                               (mapconcat #'identity objects "\n"))
              (ebb--drain-and-send))))))))

;;; --- Semi-char mode keymap (Phase F2: dynamic from exceptions) ---

(defun ebb--build-semi-char-map ()
  "Build the semi-char mode keymap dynamically from `ebb-keymap-exceptions'."
  (let ((map (make-sparse-keymap)))
    ;; Printable ASCII -> forward to terminal
    (let ((i 32))
      (while (<= i 126)
        (define-key map (string i) #'ebb-self-input)
        (setq i (1+ i))))
    ;; Named keys
    (define-key map (kbd "RET")        (lambda () (interactive) (ebb--send-key "return")))
    (define-key map (kbd "<return>")   (lambda () (interactive) (ebb--send-key "return")))
    (define-key map (kbd "TAB")        (lambda () (interactive) (ebb--send-key "tab")))
    (define-key map (kbd "<tab>")      (lambda () (interactive) (ebb--send-key "tab")))
    (define-key map (kbd "DEL")        (lambda () (interactive) (ebb--send-key "backspace")))
    (define-key map (kbd "<backspace>")(lambda () (interactive) (ebb--send-key "backspace")))
    (define-key map (kbd "<delete>")   (lambda () (interactive) (ebb--send-key "delete")))
    (define-key map (kbd "<deletechar>")(lambda () (interactive) (ebb--send-key "delete")))
    (define-key map (kbd "<escape>")   (lambda () (interactive) (ebb--send-key "escape")))
    ;; Arrow keys and modifiers
    (dolist (key '("up" "down" "left" "right" "home" "end" "prior" "next" "insert"))
      (define-key map (kbd (format "<%s>" key))
        (let ((k key)) (lambda () (interactive) (ebb--send-key k)))))
    ;; Shifted arrow keys etc.
    (dolist (key '("up" "down" "left" "right" "home" "end"))
      (define-key map (kbd (format "<S-%s>" key))
        (let ((k key)) (lambda () (interactive) (ebb--send-key k t nil nil)))))
    ;; Function keys
    (dotimes (i 12)
      (let ((fn-name (format "f%d" (1+ i))))
        (define-key map (kbd (format "<f%d>" (1+ i)))
          (let ((name fn-name))
            (lambda () (interactive) (ebb--send-key name))))))
    ;; Control keys — bind all C-<letter> except exceptions and special cases
    (let ((skip '(?i ?m ?y)))   ; i=TAB, m=RET, y=yank
      (dolist (c (number-sequence ?a ?z))
        (let ((key-str (format "C-%c" c)))
          (unless (or (member key-str ebb-keymap-exceptions)
                      (memq c skip))
            (define-key map (kbd key-str)
                        (let ((code (- c 96)))
                          (lambda () (interactive)
                            (ebb--send-key (string (+ c 0)) nil t nil))))))))
    ;; Meta keys
    (dolist (c (number-sequence ?a ?z))
      (let ((key-str (format "M-%c" c)))
        (unless (member key-str ebb-keymap-exceptions)
          (define-key map (kbd key-str) #'ebb--send-event))))
    ;; Special bindings
    (define-key map (kbd "C-y") #'ebb-yank)
    (define-key map (kbd "M-y") #'ebb-yank-pop)
    (define-key map (kbd "C-\\") (lambda () (interactive) (ebb--send-key "\\" nil t nil)))
    (define-key map (kbd "C-_") (lambda () (interactive) (ebb--send-key "_" nil t nil)))
    ;; C-c C-c sends interrupt
    (define-key map (kbd "C-c C-c") (lambda () (interactive) (ebb--send-key "c" nil t nil)))
    ;; C-c C-k enters copy mode
    (define-key map (kbd "C-c C-k") #'ebb-copy-mode)
    ;; C-c C-q sends next key literally
    (define-key map (kbd "C-c C-q") #'ebb-send-next-key)
    ;; Prompt navigation
    (define-key map (kbd "C-c C-n") #'ebb-next-prompt)
    (define-key map (kbd "C-c C-p") #'ebb-previous-prompt)
    ;; Mouse events (when mouse is grabbed by terminal)
    (define-key map [down-mouse-1] #'ebb--mouse-press)
    (define-key map [mouse-1] #'ebb--mouse-release)
    (define-key map [drag-mouse-1] #'ebb--mouse-drag)
    (define-key map [down-mouse-2] #'ebb--mouse-press)
    (define-key map [mouse-2] #'ebb--mouse-release)
    (define-key map [drag-mouse-2] #'ebb--mouse-drag)
    (define-key map [down-mouse-3] #'ebb--mouse-press)
    (define-key map [mouse-3] #'ebb--mouse-release)
    (define-key map [drag-mouse-3] #'ebb--mouse-drag)
    ;; Scroll wheel
    (define-key map [mouse-4] #'ebb--scroll-up)
    (define-key map [mouse-5] #'ebb--scroll-down)
    (define-key map [wheel-up] #'ebb--scroll-up)
    (define-key map [wheel-down] #'ebb--scroll-down)
    ;; Drag-and-drop
    (define-key map [drag-n-drop] #'ebb--drop)
    map))

(defvar ebb-semi-char-mode-map (ebb--build-semi-char-map)
  "Keymap for semi-char mode: most keys forwarded, Emacs prefixes preserved.")

(define-minor-mode ebb-semi-char-mode
  "Minor mode that forwards most keys to the terminal."
  :lighter " Semi"
  :keymap ebb-semi-char-mode-map)

;; --- ebb-mode-map: base keymap ---
(define-key ebb-mode-map (kbd "C-c C-j") #'ebb-semi-char-mode)
(define-key ebb-mode-map (kbd "C-c C-k") #'ebb-copy-mode)

;;; --- Shell integration injection (Phase B1) ---

(defun ebb--detect-shell (shell)
  "Detect the shell type from SHELL path.
Returns a symbol: bash, zsh, fish, or nil."
  (let ((name (file-name-nondirectory shell)))
    (cond
     ((string-match-p "bash" name) 'bash)
     ((string-match-p "zsh" name) 'zsh)
     ((string-match-p "fish" name) 'fish)
     (t nil))))

(defun ebb--shell-integration-env ()
  "Return extra environment variables for shell integration.
Returns a list of \"VAR=VALUE\" strings to prepend to process-environment."
  (when ebb-shell-integration
    (let* ((shell-type (ebb--detect-shell ebb-shell-name))
           (ebb-dir ebb--directory))
      (pcase shell-type
        ('bash
         (let ((inject-script (expand-file-name
                               "etc/shell-integration/bash/ebb-inject.bash"
                               ebb-dir))
               (env (list "EBB_BASH_INJECT=1")))
           (when (file-readable-p inject-script)
             (let ((old-env (getenv "ENV")))
               (when old-env
                 (push (format "EBB_BASH_ENV=%s" old-env) env)))
             (push (format "ENV=%s" inject-script) env)
             (unless (getenv "HISTFILE")
               (push (format "HISTFILE=%s/.bash_history"
                              (expand-file-name "~"))
                     env)
               (push "EBB_BASH_UNEXPORT_HISTFILE=1" env))
             env)))
        ('zsh
         (let ((zsh-dir (expand-file-name
                          "etc/shell-integration/zsh" ebb-dir)))
           (when (file-directory-p zsh-dir)
             (let ((env nil)
                   (old-zdotdir (getenv "ZDOTDIR")))
               (when old-zdotdir
                 (push (format "EBB_ZSH_ZDOTDIR=%s" old-zdotdir) env))
               (push (format "ZDOTDIR=%s" zsh-dir) env)
               env))))
        ('fish
         (let ((integ-dir (expand-file-name
                            "etc/shell-integration" ebb-dir)))
           (when (file-directory-p integ-dir)
             (let ((xdg (or (getenv "XDG_DATA_DIRS")
                            "/usr/local/share:/usr/share")))
               (list
                (format "XDG_DATA_DIRS=%s:%s" integ-dir xdg)
                (format "EBB_SHELL_INTEGRATION_XDG_DIR=%s"
                        integ-dir))))))
        (_ nil)))))

;;; --- Project integration (Phase F4) ---

;;;###autoload
(defun ebb-project (&optional _arg)
  "Start a terminal in the current project's root directory."
  (interactive "P")
  (let ((default-directory (project-root (project-current t))))
    (ebb)))

;;;###autoload
(defun ebb-other ()
  "Switch to next ebb buffer, or create one."
  (interactive)
  (let ((ebb-buffers (cl-remove-if-not
                      (lambda (buf)
                        (with-current-buffer buf
                          (derived-mode-p 'ebb-mode)))
                      (buffer-list))))
    (if ebb-buffers
        (let ((next (car (cdr (memq (current-buffer) ebb-buffers)))))
          (pop-to-buffer-same-window (or next (car ebb-buffers))))
      (ebb))))

;;; --- Entry point ---

;;;###autoload
(defun ebb ()
  "Start a terminal.
When `default-directory' is a remote TRAMP path, opens an SSH
session to the remote host instead of a local shell."
  (interactive)
  (let* ((buf (generate-new-buffer "*ebb*")))
    (with-current-buffer buf
      (ebb-mode)
      ;; Create terminal with initial window dimensions
      (let ((rows (max 1 (window-body-height)))
            (cols (max 1 (window-body-width))))
        (setq ebb--terminal (ebb--new rows cols ebb-max-scrollback))
        ;; Remember the TRAMP prefix for remote CWD tracking.
        (setq ebb--remote-prefix (file-remote-p default-directory))
        ;; Apply theme palette
        (ebb--apply-palette ebb--terminal)
        (ebb--update-buffer-face)
        ;; Shell integration env vars
        (let* ((integration-env (ebb--shell-integration-env))
               (shell-type (and ebb-shell-integration
                                (ebb--detect-shell ebb-shell-name)))
               (shell-args (if (and (eq shell-type 'bash) integration-env)
                               (list "--posix")
                             nil))
               (stty-flags (if (and (eq shell-type 'bash)
                                    (not integration-env))
                               "erase '^?' iutf8 echo"
                             "erase '^?' iutf8"))
               (process-environment
                (append
                 (list (concat "TERM=" ebb-term-environment-variable)
                       "COLORTERM=truecolor"
                       (format "INSIDE_EMACS=%s,ebb" emacs-version)
                       (format "EMACS_EBB_PATH=%s" ebb--directory))
                 integration-env
                 process-environment))
               (inhibit-eol-conversion t))
          (setq ebb--process
                (make-process
                 :name "ebb"
                 :buffer buf
                 :command (ebb--build-shell-command rows cols
                                                    stty-flags shell-args)
                 :filter #'ebb--process-filter
                 :sentinel #'ebb--process-sentinel
                 :connection-type 'pty))
          ;; Set binary coding for raw I/O
          (set-process-coding-system ebb--process 'binary 'binary)
          (set-process-window-size ebb--process rows cols)
          (set-process-query-on-exit-flag ebb--process nil))
        ;; Inject OSC 7 if no shell integration (fallback)
        (unless (and ebb-shell-integration
                     (ebb--detect-shell ebb-shell-name))
          (let ((proc ebb--process)
                (injection
                 (concat
                  (if ebb--remote-prefix
                      " PROMPT_COMMAND='printf \"\\033]7;file://%s%s\\033\\\\\\\\\" \"$(hostname)\" \"$(pwd)\"'\n"
                    (ebb--osc7-injection))
                  " clear\n")))
            (run-at-time (if ebb--remote-prefix 1 0.2) nil
              (lambda ()
                (when (process-live-p proc)
                  (process-send-string proc injection))))))
        ;; Install focus change hook
        (add-function :after after-focus-change-function #'ebb--focus-change)
        ;; Enter semi-char mode by default
        (ebb-semi-char-mode 1)))
    (pop-to-buffer-same-window buf)))

(defun ebb--build-shell-command (rows cols &optional stty-flags shell-args)
  "Build the command list to start a shell.
STTY-FLAGS are passed to stty for PTY configuration.
SHELL-ARGS are additional arguments to the shell."
  (let ((remote (file-remote-p default-directory))
        (flags (or stty-flags "erase '^?' iutf8")))
    (if (not remote)
        ;; Local shell
        `("/usr/bin/env" "sh" "-c"
          ,(concat "stty " flags " 2>/dev/null; "
                   "printf '\\033[H\\033[2J'; exec "
                   (shell-quote-argument ebb-shell-name)
                   (and shell-args
                        (concat " "
                                (mapconcat #'shell-quote-argument
                                           shell-args " "))))
          "--" ,ebb-shell-name ,@(or shell-args '("-l")))
      ;; Remote: start a local ssh command to the remote host.
      (require 'tramp)
      (let* ((dissected (tramp-dissect-file-name default-directory))
             (method (tramp-file-name-method dissected))
             (user (tramp-file-name-user dissected))
             (host (tramp-file-name-host dissected))
             (port (tramp-file-name-port dissected))
             (localname (tramp-file-name-localname dissected)))
        (if (member method '("sudo" "su" "doas"))
            ;; Local privilege escalation
            `("/usr/bin/env" "sh" "-c"
              ,(format "stty -nl echo rows %d columns %d sane 2>/dev/null; exec \"$@\""
                       rows cols)
              "--" ,ebb-shell-name "-l")
          ;; SSH to remote host
          (let ((ssh-args (list "-t")))
            (when port
              (push "-p" ssh-args)
              (push (if (numberp port) (number-to-string port) port)
                    ssh-args))
            (push (if user (format "%s@%s" user host) host) ssh-args)
            (let ((cd-cmd (if (and localname
                                   (not (string= localname "/"))
                                   (not (string= localname "")))
                              (format "cd %s; " (shell-quote-argument localname))
                            "")))
              (setq ssh-args
                    (append ssh-args
                            (list (format "%sexec $SHELL -l" cd-cmd)))))
            `("/usr/bin/env" "sh" "-c"
              ,(format "stty rows %d columns %d sane 2>/dev/null; exec \"$@\""
                       rows cols)
              "--" "ssh" ,@ssh-args)))))))

;;; --- Public API ---

(defun ebb-version ()
  "Return the el-be-back version."
  (ebb--version))

;; Register with project-switch-commands if available
(with-eval-after-load 'project
  (when (boundp 'project-switch-commands)
    (add-to-list 'project-switch-commands '(ebb-project "EBB") t)))

(provide 'el-be-back)
;;; el-be-back.el ends here
