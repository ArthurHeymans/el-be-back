;;; ebb-serial.el --- Ebb-backed serial terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Maintainer: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.1.1
;; Keywords: terminals, serial, processes
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/ArthurHeymans/ebb-serial
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ebb-serial is a Ebb-backed alternative to `serial-term'.  It uses
;; Ebb's terminal renderer and input modes with an Emacs serial backend.
;; Serial input is opened with `no-conversion' and decoded by
;; `ebb-serial-codec' so split UTF-8 and malformed bytes do not corrupt the
;; terminal parser.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'format-spec)
(require 'ebb)
(require 'ebb-serial-codec)

(defgroup ebb-serial nil
  "Ebb-backed serial terminal."
  :group 'terminals
  :prefix "ebb-serial-")

(defcustom ebb-serial-default-speed 115200
  "Default serial port speed used by `ebb-serial'."
  :type 'integer
  :group 'ebb-serial)

(defcustom ebb-serial-default-coding-system 'utf-8-unix
  "Coding system used to encode text sent to the serial port."
  :type 'coding-system
  :group 'ebb-serial)

(defcustom ebb-serial-default-input-mode 'semi-char
  "Ebb input mode selected when a new serial terminal is created."
  :type '(choice (const semi-char)
                 (const char)
                 (const emacs))
  :group 'ebb-serial)

(defcustom ebb-serial-speed-history
  '(9600 19200 38400 57600 115200 230400 460800 921600)
  "Serial port speeds offered by the mode-line speed menu."
  :type '(repeat integer)
  :group 'ebb-serial)

(defcustom ebb-serial-buffer-name-format "*ebb-serial %p*"
  "Format used to create serial terminal buffer names.

The format specifier %p expands to the serial port path."
  :type 'string
  :group 'ebb-serial)

(defcustom ebb-serial-break-duration 0
  "Default duration argument passed to `ebb-serial-send-break-function'.

Helpers commonly treat 0 as a request to use the operating system's
default break length."
  :type 'integer
  :group 'ebb-serial)

(defcustom ebb-serial-send-break-function nil
  "Optional function used by `ebb-serial-send-break'.

The function is called with PROCESS and DURATION.  When nil,
`ebb-serial-send-break' reports that serial break is unsupported.
Emacs does not currently expose a portable serial-break primitive."
  :type '(choice (const nil) function)
  :group 'ebb-serial)

(defcustom ebb-serial-remote-socat-program "socat"
  "Program used on remote hosts to bridge standard I/O to a serial port."
  :type 'string
  :group 'ebb-serial)

(defvar-local ebb-serial--port nil)
(defvar-local ebb-serial--speed nil)
(defvar-local ebb-serial--bytesize 8)
(defvar-local ebb-serial--parity nil)
(defvar-local ebb-serial--stopbits 1)
(defvar-local ebb-serial--flowcontrol nil)
(defvar-local ebb-serial--process nil)
(defvar-local ebb-serial--codec-state nil)
(defvar-local ebb-serial--connection-state 'disconnected)
(defvar-local ebb-serial--mode-line-process nil)

(defvar ebb-serial-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'ebb-serial-disconnect)
    (define-key map (kbd "C-c C-s r") #'ebb-serial-reconnect)
    (define-key map (kbd "C-c C-s d") #'ebb-serial-disconnect)
    (define-key map (kbd "C-c C-s c") #'ebb-serial-configure)
    (define-key map (kbd "C-c C-s b") #'ebb-serial-send-break)
    (define-key map (kbd "C-c C-s x") #'ebb-serial-send-byte)
    map)
  "Keymap for serial-specific commands in Ebb serial buffers.")

(define-minor-mode ebb-serial-mode
  "Minor mode for serial-specific Ebb terminal commands."
  :lighter " EbbSerial"
  :keymap ebb-serial-mode-map)

(defun ebb-serial--buffer-name (port)
  "Return the buffer name for PORT."
  (format-spec ebb-serial-buffer-name-format `((?p . ,port))))

(defun ebb-serial--read-port ()
  "Read a serial port path from the minibuffer."
  (read-file-name "Serial port: " "/dev/" nil t))

(defun ebb-serial--live-process-p (&optional process)
  "Return non-nil if PROCESS, or the current serial process, is live."
  (let ((proc (or process ebb-serial--process)))
    (and (processp proc)
         (memq (process-status proc)
               '(run stop open listen connect)))))

(defun ebb-serial--require-process ()
  "Return the current live serial process or signal a user error."
  (unless (ebb-serial--live-process-p)
    (user-error "No live ebb-serial process in this buffer"))
  ebb-serial--process)

(defun ebb-serial--mode-line-string ()
  "Return mode-line text for the current serial connection."
  (when ebb-serial--port
    (concat
     " ["
     (ebb-serial--mode-line-item
      ebb-serial--port
      "mouse-1: serial actions"
      #'ebb-serial-mode-line-connection-menu)
     " "
     (ebb-serial--mode-line-item
      (ebb-serial--speed-string)
      "mouse-1: change serial speed"
      #'ebb-serial-mode-line-speed-menu)
     " "
     (ebb-serial--mode-line-item
      (ebb-serial--configuration-summary)
      "mouse-1: change serial framing/flow control"
      #'ebb-serial-mode-line-config-menu)
     " "
     (ebb-serial--mode-line-item
      (symbol-name ebb-serial--connection-state)
      "mouse-1: serial actions"
      #'ebb-serial-mode-line-connection-menu)
     "]")))

(defun ebb-serial--mode-line-item (text help-echo command)
  "Return mode-line TEXT with HELP-ECHO and mouse COMMAND."
  (propertize text
              'help-echo help-echo
              'mouse-face 'mode-line-highlight
              'local-map `(keymap (mode-line keymap
                                              (down-mouse-1 . ,command)))))

(defun ebb-serial--speed-string ()
  "Return human-readable speed text for the mode line."
  (if ebb-serial--speed
      (format "%s" ebb-serial--speed)
    "port-default"))

(defun ebb-serial--configuration-summary ()
  "Return compact serial framing and flow-control summary."
  (concat
   (format "%s%s%s"
           (or ebb-serial--bytesize 8)
           (pcase ebb-serial--parity
             ('odd "O")
             ('even "E")
             (_ "N"))
           (or ebb-serial--stopbits 1))
   (pcase ebb-serial--flowcontrol
     ('hw "+RTS/CTS")
     ('sw "+XON/XOFF")
     (_ ""))))

(defun ebb-serial--popup-mode-line-menu (event keymap)
  "Popup KEYMAP for mode-line EVENT and run the selected command."
  (save-selected-window
    (when-let ((window (and event (posn-window (event-start event)))))
      (when (windowp window)
        (select-window window)))
    (let* ((selection (x-popup-menu event keymap))
           (binding (and selection
                         (lookup-key keymap (vconcat selection)))))
      (when binding
        (call-interactively binding)))))

(defun ebb-serial--install-mode-line ()
  "Append serial status to Ebb's mode line in the current buffer."
  (unless ebb-serial--mode-line-process
    (setq ebb-serial--mode-line-process mode-line-process)
    (setq mode-line-process
          (append mode-line-process
                  '((:eval (ebb-serial--mode-line-string)))))))

(defun ebb-serial--display-window ()
  "Return a window that should determine the current terminal size."
  (or (and (eq (window-buffer) (current-buffer))
           (selected-window))
      (car (sort (get-buffer-window-list (current-buffer) nil t)
                 (lambda (left right)
                   (> (* (window-total-width left)
                         (window-total-height left))
                      (* (window-total-width right)
                         (window-total-height right))))))))

(defun ebb-serial--resize-terminal-to-window (&rest _)
  "Resize the Ebb model to the window displaying this buffer."
  (when (and ebb--io (get-buffer-window (current-buffer) t))
    (when-let ((window (ebb-serial--display-window)))
      (with-selected-window window
        (ebb-io-handle-resize
         ebb--io
         (max (window-max-chars-per-line window) 1)
         (max (window-body-height window) 1))))))

(defun ebb-serial--select-default-input-mode ()
  "Switch to `ebb-serial-default-input-mode'."
  (pcase ebb-serial-default-input-mode
    ('emacs (ebb-emacs-mode))
    ('char (ebb-char-mode))
    (_ (ebb-semi-char-mode))))

(defun ebb-serial--ensure-terminal ()
  "Ensure the current buffer has a Ebb terminal stack."
  (unless ebb--io
    (let* ((window (or (get-buffer-window (current-buffer))
                       (selected-window)))
           (width (max (window-max-chars-per-line window) 10))
           (height (max (window-body-height window) 3)))
      (setq ebb--screen (ebb-screen-create width height))
      (setf (ebb-screen-scrollback-max ebb--screen) ebb-scrollback-lines)
      (setq ebb--render (ebb-render-create ebb--screen (current-buffer)))
      (setq ebb--parser
            (ebb-parse-create ebb--screen nil #'ebb--handle-event))
      (setq ebb--io
            (make-ebb-io
             :screen ebb--screen
             :parser ebb--parser
             :render ebb--render
             :buffer (current-buffer)
             :input-coding-system ebb-serial-default-coding-system
             :chunk-size ebb-chunk-size
             :min-latency ebb-minimum-latency
             :max-latency ebb-maximum-latency))
      (ebb-serial--select-default-input-mode)
      (ebb-render-refresh ebb--render))))

(defun ebb-serial--setup-buffer (port speed)
  "Set up the current buffer for serial PORT at SPEED."
  (unless (eq major-mode 'ebb-mode)
    (ebb-mode))
  (ebb-serial-mode 1)
  (add-hook 'window-size-change-functions
            #'ebb-serial--resize-terminal-to-window nil t)
  (ebb-serial--install-mode-line)
  (setq ebb-serial--port port)
  (unless (ebb-serial--live-process-p)
    (setq ebb-serial--speed speed))
  (setq ebb-serial--codec-state
        (ebb-serial-codec-make-state ebb-serial-invalid-byte-policy))
  (ebb-serial--ensure-terminal)
  (setf (ebb-io-input-coding-system ebb--io)
        ebb-serial-default-coding-system))

(defun ebb-serial--set-configuration (speed bytesize parity stopbits flowcontrol)
  "Store serial configuration values in the current buffer."
  (setq ebb-serial--speed speed)
  (setq ebb-serial--bytesize bytesize)
  (setq ebb-serial--parity parity)
  (setq ebb-serial--stopbits stopbits)
  (setq ebb-serial--flowcontrol flowcontrol))

(defun ebb-serial--remote-port-p (&optional port)
  "Return non-nil if PORT, or the current port, is a remote Tramp file name."
  (when-let ((port (or port ebb-serial--port)))
    (file-remote-p port)))

(defun ebb-serial--remote-default-directory (port)
  "Return a remote `default-directory' for remote serial PORT."
  (when-let ((remote (file-remote-p port)))
    (concat remote "/")))

(defun ebb-serial--remote-port-localname (port)
  "Return the device path component of remote serial PORT."
  (or (file-remote-p port 'localname) port))

(defun ebb-serial--socat-bool (name enabled)
  "Return a socat boolean option NAME set according to ENABLED."
  (format "%s=%d" name (if enabled 1 0)))

(defun ebb-serial--socat-open-address (port)
  "Return a socat OPEN address for serial PORT using current settings."
  (let ((options (list "echo=0" "raw")))
    (when ebb-serial--speed
      (push (format "b%s" ebb-serial--speed) options))
    (when ebb-serial--bytesize
      (push (format "cs%s" ebb-serial--bytesize) options))
    (pcase ebb-serial--parity
      ('odd
       (push (ebb-serial--socat-bool "parenb" t) options)
       (push (ebb-serial--socat-bool "parodd" t) options))
      ('even
       (push (ebb-serial--socat-bool "parenb" t) options)
       (push (ebb-serial--socat-bool "parodd" nil) options))
      (_
       (push (ebb-serial--socat-bool "parenb" nil) options)))
    (when ebb-serial--stopbits
      (push (ebb-serial--socat-bool "cstopb" (= ebb-serial--stopbits 2))
            options))
    (pcase ebb-serial--flowcontrol
      ('hw
       (push (ebb-serial--socat-bool "crtscts" t) options)
       (push (ebb-serial--socat-bool "ixon" nil) options)
       (push (ebb-serial--socat-bool "ixoff" nil) options))
      ('sw
       (push (ebb-serial--socat-bool "crtscts" nil) options)
       (push (ebb-serial--socat-bool "ixon" t) options)
       (push (ebb-serial--socat-bool "ixoff" t) options))
      (_
       (push (ebb-serial--socat-bool "crtscts" nil) options)
       (push (ebb-serial--socat-bool "ixon" nil) options)
       (push (ebb-serial--socat-bool "ixoff" nil) options)))
    (concat "OPEN:" port "," (string-join (nreverse options) ","))))

(defun ebb-serial--socat-command ()
  "Return the remote socat command for the current serial settings."
  (list ebb-serial-remote-socat-program
        "-"
        (ebb-serial--socat-open-address
         (ebb-serial--remote-port-localname ebb-serial--port))))

(defun ebb-serial--configure-process
    (process speed bytesize parity stopbits flowcontrol)
  "Apply serial configuration values to PROCESS and store them."
  (if (process-get process 'ebb-serial-remote)
      (progn
        (ebb-serial--set-configuration speed bytesize parity stopbits
                                       flowcontrol)
        (ebb-serial--open-process))
    (serial-process-configure :process process
                              :speed speed
                              :bytesize bytesize
                              :parity parity
                              :stopbits stopbits
                              :flowcontrol flowcontrol)
    (ebb-serial--set-configuration speed bytesize parity stopbits flowcontrol)))

(defun ebb-serial--apply-configuration
    (speed bytesize parity stopbits flowcontrol)
  "Apply serial settings, or store them until reconnect.

SPEED, BYTESIZE, PARITY, STOPBITS, and FLOWCONTROL are the same
values accepted by `serial-process-configure'."
  (unless ebb-serial--port
    (user-error "This buffer is not an ebb-serial buffer"))
  (let ((process (and (ebb-serial--live-process-p) ebb-serial--process)))
    (if process
        (ebb-serial--configure-process process speed bytesize parity
                                       stopbits flowcontrol)
      (ebb-serial--set-configuration speed bytesize parity
                                     stopbits flowcontrol))
    (force-mode-line-update)
    (message "Configured %s as %s %s%s"
             ebb-serial--port
             (ebb-serial--speed-string)
             (ebb-serial--configuration-summary)
             (if process "" " (pending reconnect)"))))

;;;###autoload
(defun ebb-serial-set-speed (speed)
  "Configure the current Ebb serial buffer to use SPEED."
  (interactive
   (list (read-number "Speed: " (or ebb-serial--speed
                                      ebb-serial-default-speed))))
  (ebb-serial--apply-configuration speed
                                   ebb-serial--bytesize
                                   ebb-serial--parity
                                   ebb-serial--stopbits
                                   ebb-serial--flowcontrol))

(defun ebb-serial-set-framing (bytesize parity stopbits)
  "Configure serial BYTESIZE, PARITY, and STOPBITS."
  (interactive
   (list (string-to-number
          (ebb-serial--read-choice
           "Byte size" '("8" "7")
           (number-to-string (or ebb-serial--bytesize 8))))
         (pcase (ebb-serial--read-choice
                 "Parity" '("none" "odd" "even")
                 (pcase ebb-serial--parity
                   ('odd "odd")
                   ('even "even")
                   (_ "none")))
           ("odd" 'odd)
           ("even" 'even)
           (_ nil))
         (string-to-number
          (ebb-serial--read-choice
           "Stop bits" '("1" "2")
           (number-to-string (or ebb-serial--stopbits 1))))))
  (ebb-serial--apply-configuration ebb-serial--speed
                                   bytesize
                                   parity
                                   stopbits
                                   ebb-serial--flowcontrol))

(defun ebb-serial-set-flowcontrol (flowcontrol)
  "Configure serial FLOWCONTROL."
  (interactive
   (list (pcase (ebb-serial--read-choice
                 "Flow control" '("none" "hw" "sw")
                 (pcase ebb-serial--flowcontrol
                   ('hw "hw")
                   ('sw "sw")
                   (_ "none")))
           ("hw" 'hw)
           ("sw" 'sw)
           (_ nil))))
  (ebb-serial--apply-configuration ebb-serial--speed
                                   ebb-serial--bytesize
                                   ebb-serial--parity
                                   ebb-serial--stopbits
                                   flowcontrol))

(defun ebb-serial--clear-process-state (&optional state)
  "Forget the current process and set connection STATE."
  (setq ebb-serial--process nil)
  (setq ebb-serial--connection-state (or state 'disconnected))
  (when ebb--io
    (setf (ebb-io-process ebb--io) nil))
  (force-mode-line-update))

(defun ebb-serial--process-arguments ()
  "Return keyword arguments for `make-serial-process'."
  (append (list :name (format "ebb-serial-%s" ebb-serial--port)
                :buffer (current-buffer)
                :port ebb-serial--port
                :speed ebb-serial--speed
                :coding 'no-conversion
                :noquery t
                :filter #'ebb-serial--filter
                :sentinel #'ebb-serial--sentinel)
          (when ebb-serial--bytesize
            (list :bytesize ebb-serial--bytesize))
          (list :parity ebb-serial--parity)
          (when ebb-serial--stopbits
            (list :stopbits ebb-serial--stopbits))
          (list :flowcontrol ebb-serial--flowcontrol)))

(defun ebb-serial--remote-process-arguments ()
  "Return keyword arguments for a remote socat serial process."
  (list :name (format "ebb-serial-%s" ebb-serial--port)
        :buffer (current-buffer)
        :command (ebb-serial--socat-command)
        :connection-type 'pipe
        :coding 'no-conversion
        :noquery t
        :file-handler t
        :filter #'ebb-serial--filter
        :sentinel #'ebb-serial--sentinel))

(defun ebb-serial--make-process ()
  "Create the process for the current serial backend."
  (if (ebb-serial--remote-port-p)
      (let ((default-directory
             (or (ebb-serial--remote-default-directory ebb-serial--port)
                 default-directory)))
        (apply #'make-process (ebb-serial--remote-process-arguments)))
    (apply #'make-serial-process (ebb-serial--process-arguments))))

(defun ebb-serial--open-process ()
  "Open the serial process for the current buffer."
  (ebb-serial--ensure-terminal)
  (when (ebb-serial--live-process-p)
    (let ((old-process ebb-serial--process))
      (ebb-serial--clear-process-state 'disconnected)
      (delete-process old-process)))
  (setq ebb-serial--codec-state
        (ebb-serial-codec-make-state ebb-serial-invalid-byte-policy))
  (setq ebb-serial--connection-state 'connecting)
  (condition-case err
      (let ((process (ebb-serial--make-process)))
        (process-put process 'ebb-serial-buffer (current-buffer))
        (process-put process 'ebb-serial-port ebb-serial--port)
        (process-put process 'ebb-serial-remote (ebb-serial--remote-port-p))
        (process-put process 'ebb-serial-backend
                     (if (ebb-serial--remote-port-p) 'socat 'local))
        (setq ebb-serial--process process)
        (setf (ebb-io-process ebb--io) process
              (ebb-io-buffer ebb--io) (current-buffer)
              (ebb-parser-write-fn ebb--parser)
              (lambda (string) (ebb-io-send ebb--io string)))
        (setq ebb-serial--connection-state 'connected)
        (force-mode-line-update)
        process)
    (error
     (ebb-serial--clear-process-state 'disconnected)
     (signal (car err) (cdr err)))))

(defun ebb-serial--queue-output (text)
  "Queue decoded serial TEXT for Ebb to render."
  (when (and ebb--io (> (length text) 0))
    (ebb-io-receive ebb--io text)))

(defun ebb-serial--filter (process chunk)
  "Decode raw serial CHUNK from PROCESS and feed it to Ebb."
  (when-let ((buffer (process-get process 'ebb-serial-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process ebb-serial--process)
          (unless ebb-serial--codec-state
            (setq ebb-serial--codec-state
                  (ebb-serial-codec-make-state
                   ebb-serial-invalid-byte-policy)))
          (ebb-serial--queue-output
           (ebb-serial-codec-decode ebb-serial--codec-state chunk)))))))

(defun ebb-serial--sentinel (process message)
  "Handle serial PROCESS state changes described by MESSAGE."
  (when-let ((buffer (process-get process 'ebb-serial-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (eq process ebb-serial--process)
                   (not (ebb-serial--live-process-p process)))
          (when ebb-serial--codec-state
            (ebb-serial--queue-output
             (ebb-serial-codec-flush ebb-serial--codec-state)))
          (when (and ebb--io (ebb-io--pending-p ebb--io))
            (ebb-io--process-pending ebb--io t))
          (ebb-serial--clear-process-state 'disconnected)
          (message "ebb-serial %s: %s"
                   (or ebb-serial--port process)
                   (string-trim message)))))))

(defun ebb-serial--send-raw-string (process bytes)
  "Send raw BYTES to PROCESS, chunking large writes."
  (let ((index 0)
        (chunk-size 1024))
    (while (< index (length bytes))
      (process-send-string
       process
       (substring bytes index (min (+ index chunk-size) (length bytes))))
      (accept-process-output process 0)
      (setq index (+ index chunk-size)))))

(defun ebb-serial--send-input (input)
  "Encode terminal INPUT and send it to the current serial process."
  (let ((process (ebb-serial--require-process)))
    (ebb-serial--send-raw-string
     process
     (if (multibyte-string-p input)
         (encode-coding-string input ebb-serial-default-coding-system t)
       input))))

;;;###autoload
(defun ebb-serial (port &optional speed)
  "Open a Ebb-backed serial terminal for PORT at SPEED."
  (interactive
   (list (ebb-serial--read-port)
         (read-number "Speed: " ebb-serial-default-speed)))
  (let ((buffer (get-buffer-create (ebb-serial--buffer-name port)))
        (requested-speed (or speed ebb-serial-default-speed)))
    (with-current-buffer buffer
      (ebb-serial--setup-buffer port requested-speed)
      (if (ebb-serial--live-process-p)
          (unless (equal ebb-serial--speed requested-speed)
            (ebb-serial--configure-process ebb-serial--process
                                             requested-speed
                                             ebb-serial--bytesize
                                             ebb-serial--parity
                                             ebb-serial--stopbits
                                             ebb-serial--flowcontrol)
            (force-mode-line-update)
            (message "Configured %s at %s"
                     ebb-serial--port ebb-serial--speed))
        (setq ebb-serial--speed requested-speed)
        (ebb-serial--open-process)))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (ebb-serial--resize-terminal-to-window))))

;;;###autoload
(defun ebb-serial-reconnect ()
  "Close and reopen the serial process for the current Ebb serial buffer."
  (interactive)
  (unless ebb-serial--port
    (user-error "Not a ebb-serial buffer"))
  (ebb-serial--open-process))

;;;###autoload
(defun ebb-serial-disconnect ()
  "Disconnect the current serial process without killing its buffer."
  (interactive)
  (when (ebb-serial--live-process-p)
    (let ((process ebb-serial--process))
      (ebb-serial--clear-process-state 'disconnected)
      (delete-process process)))
  (force-mode-line-update))

(defun ebb-serial--read-choice (prompt choices current)
  "Read PROMPT as one of CHOICES, defaulting to CURRENT."
  (let* ((default (or current (car choices)))
         (answer (completing-read
                  (format-prompt prompt default)
                  choices nil t nil nil default)))
    answer))

;;;###autoload
(defun ebb-serial-configure (speed bytesize parity stopbits flowcontrol)
  "Configure the current serial process.

When a serial process is live, SPEED, BYTESIZE, PARITY, STOPBITS,
and FLOWCONTROL are passed to `serial-process-configure'.  When the
buffer is disconnected, store the settings for the next reconnect."
  (interactive
   (let ((speed (read-number "Speed: " ebb-serial--speed))
         (bytesize (string-to-number
                    (ebb-serial--read-choice
                     "Byte size" '("8" "7")
                     (number-to-string (or ebb-serial--bytesize 8)))))
         (parity (ebb-serial--read-choice
                  "Parity" '("none" "odd" "even")
                  (pcase ebb-serial--parity
                    ('odd "odd")
                    ('even "even")
                    (_ "none"))))
         (stopbits (string-to-number
                    (ebb-serial--read-choice
                     "Stop bits" '("1" "2")
                     (number-to-string (or ebb-serial--stopbits 1)))))
         (flowcontrol (ebb-serial--read-choice
                       "Flow control" '("none" "hw" "sw")
                       (pcase ebb-serial--flowcontrol
                         ('hw "hw")
                         ('sw "sw")
                         (_ "none")))))
     (list speed bytesize
           (pcase parity
             ("odd" 'odd)
             ("even" 'even)
             (_ nil))
           stopbits
           (pcase flowcontrol
             ("hw" 'hw)
             ("sw" 'sw)
             (_ nil)))))
  (unless ebb-serial--port
    (user-error "This buffer is not an ebb-serial buffer"))
  (ebb-serial--apply-configuration speed bytesize parity stopbits flowcontrol))

(defun ebb-serial--parse-byte (string)
  "Parse STRING as a byte value."
  (let ((trimmed (string-trim string)))
    (cond
     ((string-match-p "\\`0[xX][0-9a-fA-F]+\\'" trimmed)
      (string-to-number (substring trimmed 2) 16))
     ((string-match-p "\\`[0-9]+\\'" trimmed)
      (string-to-number trimmed 10))
     ((= (length trimmed) 1)
      (aref trimmed 0))
     (t
      (user-error "Enter a byte as decimal, hex (0x1b), or one character")))))

(defun ebb-serial--read-byte ()
  "Read a byte value from the minibuffer."
  (let ((byte (ebb-serial--parse-byte
               (read-string "Send byte (decimal, 0xNN, or char): "))))
    (unless (and (integerp byte) (<= 0 byte #xff))
      (user-error "Byte must be in range 0..255"))
    byte))

;;;###autoload
(defun ebb-serial-send-byte (byte)
  "Send BYTE, an integer 0..255, to the serial port exactly."
  (interactive (list (ebb-serial--read-byte)))
  (process-send-string (ebb-serial--require-process)
                       (unibyte-string byte)))

;;;###autoload
(defun ebb-serial-send-break (&optional duration)
  "Send a serial break to the current port.

DURATION defaults to `ebb-serial-break-duration'.  Emacs does not
currently expose a portable serial-break primitive, so callers must
customize `ebb-serial-send-break-function' to enable this command."
  (interactive)
  (let ((process (ebb-serial--require-process))
        (duration (or duration ebb-serial-break-duration)))
    (if ebb-serial-send-break-function
        (funcall ebb-serial-send-break-function process duration)
      (user-error "Serial break is unsupported; customize `ebb-serial-send-break-function'"))
    (message "Sent serial break on %s" ebb-serial--port)))

;;;###autoload
(defun ebb-serial-copy-port-name ()
  "Copy the current serial port name to the kill ring."
  (interactive)
  (unless ebb-serial--port
    (user-error "This buffer is not an ebb-serial buffer"))
  (kill-new ebb-serial--port)
  (message "Copied serial port %s" ebb-serial--port))

(defun ebb-serial--connection-menu ()
  "Return the mode-line connection menu."
  (let ((map (make-sparse-keymap "ebb-serial")))
    (define-key map [copy-port]
      '(menu-item "Copy port name" ebb-serial-copy-port-name
                  :enable ebb-serial--port))
    (define-key map [send-byte]
      '(menu-item "Send raw byte..." ebb-serial-send-byte
                  :enable (ebb-serial--live-process-p)))
    (define-key map [send-break]
      '(menu-item "Send break" ebb-serial-send-break
                  :enable (ebb-serial--live-process-p)))
    (define-key map [separator-1] '(menu-item "--"))
    (define-key map [configure]
      '(menu-item "Configure..." ebb-serial-configure
                  :enable ebb-serial--port))
    (define-key map [disconnect]
      '(menu-item "Disconnect" ebb-serial-disconnect
                  :enable (ebb-serial--live-process-p)))
    (define-key map [reconnect]
      '(menu-item "Reconnect" ebb-serial-reconnect
                  :enable ebb-serial--port))
    map))

(defun ebb-serial--speed-menu ()
  "Return the mode-line speed menu."
  (let* ((speeds (cl-delete-duplicates
                  (delq nil (copy-sequence
                             (cons ebb-serial--speed
                                   ebb-serial-speed-history)))
                  :test #'equal))
         (speeds (sort speeds #'>))
         (map (make-sparse-keymap "Speed (b/s)")))
    (define-key map [other]
      '(menu-item "Other..." ebb-serial-set-speed
                  :enable ebb-serial--port))
    (define-key map [separator-1] '(menu-item "--"))
    (dolist (speed speeds)
      (define-key
       map
       (vector (make-symbol (format "speed-%s" speed)))
       `(menu-item
         ,(format "%s" speed)
         (lambda ()
           (interactive)
           (ebb-serial-set-speed ,speed))
         :button (:radio . (equal ebb-serial--speed ,speed)))))
    map))

(defun ebb-serial--config-menu ()
  "Return the mode-line serial configuration menu."
  (let ((map (make-sparse-keymap "Serial configuration")))
    (define-key map [configure]
      '(menu-item "Configure all..." ebb-serial-configure
                  :enable ebb-serial--port))
    (define-key map [separator-1] '(menu-item "--"))
    (dolist (preset '(("8N1" 8 nil 1)
                      ("7E1" 7 even 1)
                      ("7O1" 7 odd 1)))
      (pcase-let ((`(,label ,bytesize ,parity ,stopbits) preset))
        (define-key
         map
         (vector (make-symbol (format "preset-%s" label)))
         `(menu-item
           ,(format "Preset %s" label)
           (lambda ()
             (interactive)
             (ebb-serial-set-framing ,bytesize ',parity ,stopbits))
           :button (:radio . (and (equal ebb-serial--bytesize ,bytesize)
                                  (eq ebb-serial--parity ',parity)
                                  (equal ebb-serial--stopbits ,stopbits)))))))
    (define-key map [separator-2] '(menu-item "--"))
    (dolist (bytesize '(8 7))
      (define-key
       map
       (vector (make-symbol (format "bytesize-%s" bytesize)))
       `(menu-item
         ,(format "%s data bits" bytesize)
         (lambda ()
           (interactive)
           (ebb-serial-set-framing ,bytesize
                                   ebb-serial--parity
                                   ebb-serial--stopbits))
         :button (:radio . (equal ebb-serial--bytesize ,bytesize)))))
    (define-key map [separator-3] '(menu-item "--"))
    (dolist (parity '((nil "No parity")
                      (even "Even parity")
                      (odd "Odd parity")))
      (pcase-let ((`(,value ,label) parity))
        (define-key
         map
         (vector (make-symbol (format "parity-%s" value)))
         `(menu-item
           ,label
           (lambda ()
             (interactive)
             (ebb-serial-set-framing ebb-serial--bytesize
                                     ',value
                                     ebb-serial--stopbits))
           :button (:radio . (eq ebb-serial--parity ',value))))))
    (define-key map [separator-4] '(menu-item "--"))
    (dolist (stopbits '(1 2))
      (define-key
       map
       (vector (make-symbol (format "stopbits-%s" stopbits)))
       `(menu-item
         ,(format "%s stop bit%s" stopbits (if (= stopbits 1) "" "s"))
         (lambda ()
           (interactive)
           (ebb-serial-set-framing ebb-serial--bytesize
                                   ebb-serial--parity
                                   ,stopbits))
         :button (:radio . (equal ebb-serial--stopbits ,stopbits)))))
    (define-key map [separator-5] '(menu-item "--"))
    (dolist (flow '((nil "No flow control")
                    (hw "Hardware flow control (RTS/CTS)")
                    (sw "Software flow control (XON/XOFF)")))
      (pcase-let ((`(,value ,label) flow))
        (define-key
         map
         (vector (make-symbol (format "flow-%s" value)))
         `(menu-item
           ,label
           (lambda ()
             (interactive)
             (ebb-serial-set-flowcontrol ',value))
           :button (:radio . (eq ebb-serial--flowcontrol ',value))))))
    map))

(defun ebb-serial-mode-line-connection-menu (event)
  "Show the mode-line connection menu for EVENT."
  (interactive "e")
  (ebb-serial--popup-mode-line-menu event (ebb-serial--connection-menu)))

(defun ebb-serial-mode-line-speed-menu (event)
  "Show the mode-line speed menu for EVENT."
  (interactive "e")
  (ebb-serial--popup-mode-line-menu event (ebb-serial--speed-menu)))

(defun ebb-serial-mode-line-config-menu (event)
  "Show the mode-line configuration menu for EVENT."
  (interactive "e")
  (ebb-serial--popup-mode-line-menu event (ebb-serial--config-menu)))

(provide 'ebb-serial)

;;; ebb-serial.el ends here
