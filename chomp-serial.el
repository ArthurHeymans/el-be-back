;;; chomp-serial.el --- Chomp-backed serial terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Maintainer: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.1.1
;; Keywords: terminals, serial, processes
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/ArthurHeymans/chomp-serial
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; chomp-serial is a Chomp-backed alternative to `serial-term'.  It uses
;; Chomp's terminal renderer and input modes with an Emacs serial backend.
;; Serial input is opened with `no-conversion' and decoded by
;; `chomp-serial-codec' so split UTF-8 and malformed bytes do not corrupt the
;; terminal parser.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'format-spec)
(require 'chomp)
(require 'chomp-serial-codec)

(defgroup chomp-serial nil
  "Chomp-backed serial terminal."
  :group 'terminals
  :prefix "chomp-serial-")

(defcustom chomp-serial-default-speed 115200
  "Default serial port speed used by `chomp-serial'."
  :type 'integer
  :group 'chomp-serial)

(defcustom chomp-serial-default-coding-system 'utf-8-unix
  "Coding system used to encode text sent to the serial port."
  :type 'coding-system
  :group 'chomp-serial)

(defcustom chomp-serial-default-input-mode 'semi-char
  "Chomp input mode selected when a new serial terminal is created."
  :type '(choice (const semi-char)
                 (const char)
                 (const emacs))
  :group 'chomp-serial)

(defcustom chomp-serial-speed-history
  '(9600 19200 38400 57600 115200 230400 460800 921600)
  "Serial port speeds offered by the mode-line speed menu."
  :type '(repeat integer)
  :group 'chomp-serial)

(defcustom chomp-serial-buffer-name-format "*chomp-serial %p*"
  "Format used to create serial terminal buffer names.

The format specifier %p expands to the serial port path."
  :type 'string
  :group 'chomp-serial)

(defcustom chomp-serial-break-duration 0
  "Default duration argument passed to `chomp-serial-send-break-function'.

Helpers commonly treat 0 as a request to use the operating system's
default break length."
  :type 'integer
  :group 'chomp-serial)

(defcustom chomp-serial-send-break-function nil
  "Optional function used by `chomp-serial-send-break'.

The function is called with PROCESS and DURATION.  When nil,
`chomp-serial-send-break' reports that serial break is unsupported.
Emacs does not currently expose a portable serial-break primitive."
  :type '(choice (const nil) function)
  :group 'chomp-serial)

(defcustom chomp-serial-remote-socat-program "socat"
  "Program used on remote hosts to bridge standard I/O to a serial port."
  :type 'string
  :group 'chomp-serial)

(defvar-local chomp-serial--port nil)
(defvar-local chomp-serial--speed nil)
(defvar-local chomp-serial--bytesize 8)
(defvar-local chomp-serial--parity nil)
(defvar-local chomp-serial--stopbits 1)
(defvar-local chomp-serial--flowcontrol nil)
(defvar-local chomp-serial--process nil)
(defvar-local chomp-serial--codec-state nil)
(defvar-local chomp-serial--connection-state 'disconnected)
(defvar-local chomp-serial--mode-line-process nil)

(defvar chomp-serial-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'chomp-serial-disconnect)
    (define-key map (kbd "C-c C-s r") #'chomp-serial-reconnect)
    (define-key map (kbd "C-c C-s d") #'chomp-serial-disconnect)
    (define-key map (kbd "C-c C-s c") #'chomp-serial-configure)
    (define-key map (kbd "C-c C-s b") #'chomp-serial-send-break)
    (define-key map (kbd "C-c C-s x") #'chomp-serial-send-byte)
    map)
  "Keymap for serial-specific commands in Chomp serial buffers.")

(define-minor-mode chomp-serial-mode
  "Minor mode for serial-specific Chomp terminal commands."
  :lighter " ChompSerial"
  :keymap chomp-serial-mode-map)

(defun chomp-serial--buffer-name (port)
  "Return the buffer name for PORT."
  (format-spec chomp-serial-buffer-name-format `((?p . ,port))))

(defun chomp-serial--read-port ()
  "Read a serial port path from the minibuffer."
  (read-file-name "Serial port: " "/dev/" nil t))

(defun chomp-serial--live-process-p (&optional process)
  "Return non-nil if PROCESS, or the current serial process, is live."
  (let ((proc (or process chomp-serial--process)))
    (and (processp proc)
         (memq (process-status proc)
               '(run stop open listen connect)))))

(defun chomp-serial--require-process ()
  "Return the current live serial process or signal a user error."
  (unless (chomp-serial--live-process-p)
    (user-error "No live chomp-serial process in this buffer"))
  chomp-serial--process)

(defun chomp-serial--mode-line-string ()
  "Return mode-line text for the current serial connection."
  (when chomp-serial--port
    (concat
     " ["
     (chomp-serial--mode-line-item
      chomp-serial--port
      "mouse-1: serial actions"
      #'chomp-serial-mode-line-connection-menu)
     " "
     (chomp-serial--mode-line-item
      (chomp-serial--speed-string)
      "mouse-1: change serial speed"
      #'chomp-serial-mode-line-speed-menu)
     " "
     (chomp-serial--mode-line-item
      (chomp-serial--configuration-summary)
      "mouse-1: change serial framing/flow control"
      #'chomp-serial-mode-line-config-menu)
     " "
     (chomp-serial--mode-line-item
      (symbol-name chomp-serial--connection-state)
      "mouse-1: serial actions"
      #'chomp-serial-mode-line-connection-menu)
     "]")))

(defun chomp-serial--mode-line-item (text help-echo command)
  "Return mode-line TEXT with HELP-ECHO and mouse COMMAND."
  (propertize text
              'help-echo help-echo
              'mouse-face 'mode-line-highlight
              'local-map `(keymap (mode-line keymap
                                              (down-mouse-1 . ,command)))))

(defun chomp-serial--speed-string ()
  "Return human-readable speed text for the mode line."
  (if chomp-serial--speed
      (format "%s" chomp-serial--speed)
    "port-default"))

(defun chomp-serial--configuration-summary ()
  "Return compact serial framing and flow-control summary."
  (concat
   (format "%s%s%s"
           (or chomp-serial--bytesize 8)
           (pcase chomp-serial--parity
             ('odd "O")
             ('even "E")
             (_ "N"))
           (or chomp-serial--stopbits 1))
   (pcase chomp-serial--flowcontrol
     ('hw "+RTS/CTS")
     ('sw "+XON/XOFF")
     (_ ""))))

(defun chomp-serial--popup-mode-line-menu (event keymap)
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

(defun chomp-serial--install-mode-line ()
  "Append serial status to Chomp's mode line in the current buffer."
  (unless chomp-serial--mode-line-process
    (setq chomp-serial--mode-line-process mode-line-process)
    (setq mode-line-process
          (append mode-line-process
                  '((:eval (chomp-serial--mode-line-string)))))))

(defun chomp-serial--display-window ()
  "Return a window that should determine the current terminal size."
  (or (and (eq (window-buffer) (current-buffer))
           (selected-window))
      (car (sort (get-buffer-window-list (current-buffer) nil t)
                 (lambda (left right)
                   (> (* (window-total-width left)
                         (window-total-height left))
                      (* (window-total-width right)
                         (window-total-height right))))))))

(defun chomp-serial--resize-terminal-to-window (&rest _)
  "Resize the Chomp model to the window displaying this buffer."
  (when (and chomp--io (get-buffer-window (current-buffer) t))
    (when-let ((window (chomp-serial--display-window)))
      (with-selected-window window
        (chomp-io-handle-resize
         chomp--io
         (max (window-max-chars-per-line window) 1)
         (max (window-body-height window) 1))))))

(defun chomp-serial--select-default-input-mode ()
  "Switch to `chomp-serial-default-input-mode'."
  (pcase chomp-serial-default-input-mode
    ('emacs (chomp-emacs-mode))
    ('char (chomp-char-mode))
    (_ (chomp-semi-char-mode))))

(defun chomp-serial--ensure-terminal ()
  "Ensure the current buffer has a Chomp terminal stack."
  (unless chomp--io
    (let* ((window (or (get-buffer-window (current-buffer))
                       (selected-window)))
           (width (max (window-max-chars-per-line window) 10))
           (height (max (window-body-height window) 3)))
      (setq chomp--screen (chomp-screen-create width height))
      (setf (chomp-screen-scrollback-max chomp--screen) chomp-scrollback-lines)
      (setq chomp--render (chomp-render-create chomp--screen (current-buffer)))
      (setq chomp--parser
            (chomp-parse-create chomp--screen nil #'chomp--handle-event))
      (setq chomp--io
            (make-chomp-io
             :screen chomp--screen
             :parser chomp--parser
             :render chomp--render
             :buffer (current-buffer)
             :input-coding-system chomp-serial-default-coding-system
             :chunk-size chomp-chunk-size
             :min-latency chomp-minimum-latency
             :max-latency chomp-maximum-latency))
      (chomp-serial--select-default-input-mode)
      (chomp-render-refresh chomp--render))))

(defun chomp-serial--setup-buffer (port speed)
  "Set up the current buffer for serial PORT at SPEED."
  (unless (eq major-mode 'chomp-mode)
    (chomp-mode))
  (chomp-serial-mode 1)
  (add-hook 'window-size-change-functions
            #'chomp-serial--resize-terminal-to-window nil t)
  (chomp-serial--install-mode-line)
  (setq chomp-serial--port port)
  (unless (chomp-serial--live-process-p)
    (setq chomp-serial--speed speed))
  (setq chomp-serial--codec-state
        (chomp-serial-codec-make-state chomp-serial-invalid-byte-policy))
  (chomp-serial--ensure-terminal)
  (setf (chomp-io-input-coding-system chomp--io)
        chomp-serial-default-coding-system))

(defun chomp-serial--set-configuration (speed bytesize parity stopbits flowcontrol)
  "Store serial configuration values in the current buffer."
  (setq chomp-serial--speed speed)
  (setq chomp-serial--bytesize bytesize)
  (setq chomp-serial--parity parity)
  (setq chomp-serial--stopbits stopbits)
  (setq chomp-serial--flowcontrol flowcontrol))

(defun chomp-serial--remote-port-p (&optional port)
  "Return non-nil if PORT, or the current port, is a remote Tramp file name."
  (when-let ((port (or port chomp-serial--port)))
    (file-remote-p port)))

(defun chomp-serial--remote-default-directory (port)
  "Return a remote `default-directory' for remote serial PORT."
  (when-let ((remote (file-remote-p port)))
    (concat remote "/")))

(defun chomp-serial--remote-port-localname (port)
  "Return the device path component of remote serial PORT."
  (or (file-remote-p port 'localname) port))

(defun chomp-serial--socat-bool (name enabled)
  "Return a socat boolean option NAME set according to ENABLED."
  (format "%s=%d" name (if enabled 1 0)))

(defun chomp-serial--socat-open-address (port)
  "Return a socat OPEN address for serial PORT using current settings."
  (let ((options (list "echo=0" "raw")))
    (when chomp-serial--speed
      (push (format "b%s" chomp-serial--speed) options))
    (when chomp-serial--bytesize
      (push (format "cs%s" chomp-serial--bytesize) options))
    (pcase chomp-serial--parity
      ('odd
       (push (chomp-serial--socat-bool "parenb" t) options)
       (push (chomp-serial--socat-bool "parodd" t) options))
      ('even
       (push (chomp-serial--socat-bool "parenb" t) options)
       (push (chomp-serial--socat-bool "parodd" nil) options))
      (_
       (push (chomp-serial--socat-bool "parenb" nil) options)))
    (when chomp-serial--stopbits
      (push (chomp-serial--socat-bool "cstopb" (= chomp-serial--stopbits 2))
            options))
    (pcase chomp-serial--flowcontrol
      ('hw
       (push (chomp-serial--socat-bool "crtscts" t) options)
       (push (chomp-serial--socat-bool "ixon" nil) options)
       (push (chomp-serial--socat-bool "ixoff" nil) options))
      ('sw
       (push (chomp-serial--socat-bool "crtscts" nil) options)
       (push (chomp-serial--socat-bool "ixon" t) options)
       (push (chomp-serial--socat-bool "ixoff" t) options))
      (_
       (push (chomp-serial--socat-bool "crtscts" nil) options)
       (push (chomp-serial--socat-bool "ixon" nil) options)
       (push (chomp-serial--socat-bool "ixoff" nil) options)))
    (concat "OPEN:" port "," (string-join (nreverse options) ","))))

(defun chomp-serial--socat-command ()
  "Return the remote socat command for the current serial settings."
  (list chomp-serial-remote-socat-program
        "-"
        (chomp-serial--socat-open-address
         (chomp-serial--remote-port-localname chomp-serial--port))))

(defun chomp-serial--configure-process
    (process speed bytesize parity stopbits flowcontrol)
  "Apply serial configuration values to PROCESS and store them."
  (if (process-get process 'chomp-serial-remote)
      (progn
        (chomp-serial--set-configuration speed bytesize parity stopbits
                                       flowcontrol)
        (chomp-serial--open-process))
    (serial-process-configure :process process
                              :speed speed
                              :bytesize bytesize
                              :parity parity
                              :stopbits stopbits
                              :flowcontrol flowcontrol)
    (chomp-serial--set-configuration speed bytesize parity stopbits flowcontrol)))

(defun chomp-serial--apply-configuration
    (speed bytesize parity stopbits flowcontrol)
  "Apply serial settings, or store them until reconnect.

SPEED, BYTESIZE, PARITY, STOPBITS, and FLOWCONTROL are the same
values accepted by `serial-process-configure'."
  (unless chomp-serial--port
    (user-error "This buffer is not an chomp-serial buffer"))
  (let ((process (and (chomp-serial--live-process-p) chomp-serial--process)))
    (if process
        (chomp-serial--configure-process process speed bytesize parity
                                       stopbits flowcontrol)
      (chomp-serial--set-configuration speed bytesize parity
                                     stopbits flowcontrol))
    (force-mode-line-update)
    (message "Configured %s as %s %s%s"
             chomp-serial--port
             (chomp-serial--speed-string)
             (chomp-serial--configuration-summary)
             (if process "" " (pending reconnect)"))))

;;;###autoload
(defun chomp-serial-set-speed (speed)
  "Configure the current Chomp serial buffer to use SPEED."
  (interactive
   (list (read-number "Speed: " (or chomp-serial--speed
                                      chomp-serial-default-speed))))
  (chomp-serial--apply-configuration speed
                                   chomp-serial--bytesize
                                   chomp-serial--parity
                                   chomp-serial--stopbits
                                   chomp-serial--flowcontrol))

(defun chomp-serial-set-framing (bytesize parity stopbits)
  "Configure serial BYTESIZE, PARITY, and STOPBITS."
  (interactive
   (list (string-to-number
          (chomp-serial--read-choice
           "Byte size" '("8" "7")
           (number-to-string (or chomp-serial--bytesize 8))))
         (pcase (chomp-serial--read-choice
                 "Parity" '("none" "odd" "even")
                 (pcase chomp-serial--parity
                   ('odd "odd")
                   ('even "even")
                   (_ "none")))
           ("odd" 'odd)
           ("even" 'even)
           (_ nil))
         (string-to-number
          (chomp-serial--read-choice
           "Stop bits" '("1" "2")
           (number-to-string (or chomp-serial--stopbits 1))))))
  (chomp-serial--apply-configuration chomp-serial--speed
                                   bytesize
                                   parity
                                   stopbits
                                   chomp-serial--flowcontrol))

(defun chomp-serial-set-flowcontrol (flowcontrol)
  "Configure serial FLOWCONTROL."
  (interactive
   (list (pcase (chomp-serial--read-choice
                 "Flow control" '("none" "hw" "sw")
                 (pcase chomp-serial--flowcontrol
                   ('hw "hw")
                   ('sw "sw")
                   (_ "none")))
           ("hw" 'hw)
           ("sw" 'sw)
           (_ nil))))
  (chomp-serial--apply-configuration chomp-serial--speed
                                   chomp-serial--bytesize
                                   chomp-serial--parity
                                   chomp-serial--stopbits
                                   flowcontrol))

(defun chomp-serial--clear-process-state (&optional state)
  "Forget the current process and set connection STATE."
  (setq chomp-serial--process nil)
  (setq chomp-serial--connection-state (or state 'disconnected))
  (when chomp--io
    (setf (chomp-io-process chomp--io) nil))
  (force-mode-line-update))

(defun chomp-serial--process-arguments ()
  "Return keyword arguments for `make-serial-process'."
  (append (list :name (format "chomp-serial-%s" chomp-serial--port)
                :buffer nil
                :port chomp-serial--port
                :speed chomp-serial--speed
                :coding 'no-conversion
                :noquery t
                :filter #'chomp-serial--filter
                :sentinel #'chomp-serial--sentinel)
          (when chomp-serial--bytesize
            (list :bytesize chomp-serial--bytesize))
          (list :parity chomp-serial--parity)
          (when chomp-serial--stopbits
            (list :stopbits chomp-serial--stopbits))
          (list :flowcontrol chomp-serial--flowcontrol)))

(defun chomp-serial--remote-process-arguments ()
  "Return keyword arguments for a remote socat serial process."
  (list :name (format "chomp-serial-%s" chomp-serial--port)
        :buffer nil
        :command (chomp-serial--socat-command)
        :connection-type 'pipe
        :coding 'no-conversion
        :noquery t
        :file-handler t
        :filter #'chomp-serial--filter
        :sentinel #'chomp-serial--sentinel))

(defun chomp-serial--make-process ()
  "Create the process for the current serial backend."
  (if (chomp-serial--remote-port-p)
      (let ((default-directory
             (or (chomp-serial--remote-default-directory chomp-serial--port)
                 default-directory)))
        (apply #'make-process (chomp-serial--remote-process-arguments)))
    (apply #'make-serial-process (chomp-serial--process-arguments))))

(defun chomp-serial--open-process ()
  "Open the serial process for the current buffer."
  (chomp-serial--ensure-terminal)
  (when (chomp-serial--live-process-p)
    (let ((old-process chomp-serial--process))
      (chomp-serial--clear-process-state 'disconnected)
      (delete-process old-process)))
  (setq chomp-serial--codec-state
        (chomp-serial-codec-make-state chomp-serial-invalid-byte-policy))
  (setq chomp-serial--connection-state 'connecting)
  (condition-case err
      (let ((process (chomp-serial--make-process)))
        (process-put process 'chomp-serial-buffer (current-buffer))
        (process-put process 'chomp-serial-port chomp-serial--port)
        (process-put process 'chomp-serial-remote (chomp-serial--remote-port-p))
        (process-put process 'chomp-serial-backend
                     (if (chomp-serial--remote-port-p) 'socat 'local))
        (setq chomp-serial--process process)
        (setf (chomp-io-process chomp--io) process
              (chomp-io-buffer chomp--io) (current-buffer)
              (chomp-parser-write-fn chomp--parser)
              (lambda (string) (chomp-io-send chomp--io string)))
        (setq chomp-serial--connection-state 'connected)
        (force-mode-line-update)
        process)
    (error
     (chomp-serial--clear-process-state 'disconnected)
     (signal (car err) (cdr err)))))

(defun chomp-serial--queue-output (text)
  "Queue decoded serial TEXT for Chomp to render."
  (when (and chomp--io (> (length text) 0))
    (chomp-io-receive chomp--io text)))

(defun chomp-serial--filter (process chunk)
  "Decode raw serial CHUNK from PROCESS and feed it to Chomp."
  (when-let ((buffer (process-get process 'chomp-serial-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (eq process chomp-serial--process)
          (unless chomp-serial--codec-state
            (setq chomp-serial--codec-state
                  (chomp-serial-codec-make-state
                   chomp-serial-invalid-byte-policy)))
          (chomp-serial--queue-output
           (chomp-serial-codec-decode chomp-serial--codec-state chunk)))))))

(defun chomp-serial--sentinel (process message)
  "Handle serial PROCESS state changes described by MESSAGE."
  (when-let ((buffer (process-get process 'chomp-serial-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (eq process chomp-serial--process)
                   (not (chomp-serial--live-process-p process)))
          (when chomp-serial--codec-state
            (chomp-serial--queue-output
             (chomp-serial-codec-flush chomp-serial--codec-state)))
          (when (and chomp--io (chomp-io--pending-p chomp--io))
            (chomp-io--process-pending chomp--io t))
          (chomp-serial--clear-process-state 'disconnected)
          (message "chomp-serial %s: %s"
                   (or chomp-serial--port process)
                   (string-trim message)))))))

(defun chomp-serial--send-raw-string (process bytes)
  "Send raw BYTES to PROCESS, chunking large writes."
  (let ((index 0)
        (chunk-size 1024))
    (while (< index (length bytes))
      (process-send-string
       process
       (substring bytes index (min (+ index chunk-size) (length bytes))))
      (accept-process-output process 0)
      (setq index (+ index chunk-size)))))

(defun chomp-serial--send-input (input)
  "Encode terminal INPUT and send it to the current serial process."
  (let ((process (chomp-serial--require-process)))
    (chomp-serial--send-raw-string
     process
     (if (multibyte-string-p input)
         (encode-coding-string input chomp-serial-default-coding-system t)
       input))))

;;;###autoload
(defun chomp-serial (port &optional speed)
  "Open a Chomp-backed serial terminal for PORT at SPEED."
  (interactive
   (list (chomp-serial--read-port)
         (read-number "Speed: " chomp-serial-default-speed)))
  (let ((buffer (get-buffer-create (chomp-serial--buffer-name port)))
        (requested-speed (or speed chomp-serial-default-speed)))
    (with-current-buffer buffer
      (chomp-serial--setup-buffer port requested-speed)
      (if (chomp-serial--live-process-p)
          (unless (equal chomp-serial--speed requested-speed)
            (chomp-serial--configure-process chomp-serial--process
                                             requested-speed
                                             chomp-serial--bytesize
                                             chomp-serial--parity
                                             chomp-serial--stopbits
                                             chomp-serial--flowcontrol)
            (force-mode-line-update)
            (message "Configured %s at %s"
                     chomp-serial--port chomp-serial--speed))
        (setq chomp-serial--speed requested-speed)
        (chomp-serial--open-process)))
    (pop-to-buffer-same-window buffer)
    (with-current-buffer buffer
      (chomp-serial--resize-terminal-to-window))))

;;;###autoload
(defun chomp-serial-reconnect ()
  "Close and reopen the serial process for the current Chomp serial buffer."
  (interactive)
  (unless chomp-serial--port
    (user-error "Not a chomp-serial buffer"))
  (chomp-serial--open-process))

;;;###autoload
(defun chomp-serial-disconnect ()
  "Disconnect the current serial process without killing its buffer."
  (interactive)
  (when (chomp-serial--live-process-p)
    (let ((process chomp-serial--process))
      (chomp-serial--clear-process-state 'disconnected)
      (delete-process process)))
  (force-mode-line-update))

(defun chomp-serial--read-choice (prompt choices current)
  "Read PROMPT as one of CHOICES, defaulting to CURRENT."
  (let* ((default (or current (car choices)))
         (answer (completing-read
                  (format-prompt prompt default)
                  choices nil t nil nil default)))
    answer))

;;;###autoload
(defun chomp-serial-configure (speed bytesize parity stopbits flowcontrol)
  "Configure the current serial process.

When a serial process is live, SPEED, BYTESIZE, PARITY, STOPBITS,
and FLOWCONTROL are passed to `serial-process-configure'.  When the
buffer is disconnected, store the settings for the next reconnect."
  (interactive
   (let ((speed (read-number "Speed: " chomp-serial--speed))
         (bytesize (string-to-number
                    (chomp-serial--read-choice
                     "Byte size" '("8" "7")
                     (number-to-string (or chomp-serial--bytesize 8)))))
         (parity (chomp-serial--read-choice
                  "Parity" '("none" "odd" "even")
                  (pcase chomp-serial--parity
                    ('odd "odd")
                    ('even "even")
                    (_ "none"))))
         (stopbits (string-to-number
                    (chomp-serial--read-choice
                     "Stop bits" '("1" "2")
                     (number-to-string (or chomp-serial--stopbits 1)))))
         (flowcontrol (chomp-serial--read-choice
                       "Flow control" '("none" "hw" "sw")
                       (pcase chomp-serial--flowcontrol
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
  (unless chomp-serial--port
    (user-error "This buffer is not an chomp-serial buffer"))
  (chomp-serial--apply-configuration speed bytesize parity stopbits flowcontrol))

(defun chomp-serial--parse-byte (string)
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

(defun chomp-serial--read-byte ()
  "Read a byte value from the minibuffer."
  (let ((byte (chomp-serial--parse-byte
               (read-string "Send byte (decimal, 0xNN, or char): "))))
    (unless (and (integerp byte) (<= 0 byte #xff))
      (user-error "Byte must be in range 0..255"))
    byte))

;;;###autoload
(defun chomp-serial-send-byte (byte)
  "Send BYTE, an integer 0..255, to the serial port exactly."
  (interactive (list (chomp-serial--read-byte)))
  (process-send-string (chomp-serial--require-process)
                       (unibyte-string byte)))

;;;###autoload
(defun chomp-serial-send-break (&optional duration)
  "Send a serial break to the current port.

DURATION defaults to `chomp-serial-break-duration'.  Emacs does not
currently expose a portable serial-break primitive, so callers must
customize `chomp-serial-send-break-function' to enable this command."
  (interactive)
  (let ((process (chomp-serial--require-process))
        (duration (or duration chomp-serial-break-duration)))
    (if chomp-serial-send-break-function
        (funcall chomp-serial-send-break-function process duration)
      (user-error "Serial break is unsupported; customize `chomp-serial-send-break-function'"))
    (message "Sent serial break on %s" chomp-serial--port)))

;;;###autoload
(defun chomp-serial-copy-port-name ()
  "Copy the current serial port name to the kill ring."
  (interactive)
  (unless chomp-serial--port
    (user-error "This buffer is not an chomp-serial buffer"))
  (kill-new chomp-serial--port)
  (message "Copied serial port %s" chomp-serial--port))

(defun chomp-serial--connection-menu ()
  "Return the mode-line connection menu."
  (let ((map (make-sparse-keymap "chomp-serial")))
    (define-key map [copy-port]
      '(menu-item "Copy port name" chomp-serial-copy-port-name
                  :enable chomp-serial--port))
    (define-key map [send-byte]
      '(menu-item "Send raw byte..." chomp-serial-send-byte
                  :enable (chomp-serial--live-process-p)))
    (define-key map [send-break]
      '(menu-item "Send break" chomp-serial-send-break
                  :enable (chomp-serial--live-process-p)))
    (define-key map [separator-1] '(menu-item "--"))
    (define-key map [configure]
      '(menu-item "Configure..." chomp-serial-configure
                  :enable chomp-serial--port))
    (define-key map [disconnect]
      '(menu-item "Disconnect" chomp-serial-disconnect
                  :enable (chomp-serial--live-process-p)))
    (define-key map [reconnect]
      '(menu-item "Reconnect" chomp-serial-reconnect
                  :enable chomp-serial--port))
    map))

(defun chomp-serial--speed-menu ()
  "Return the mode-line speed menu."
  (let* ((speeds (cl-delete-duplicates
                  (delq nil (copy-sequence
                             (cons chomp-serial--speed
                                   chomp-serial-speed-history)))
                  :test #'equal))
         (speeds (sort speeds #'>))
         (map (make-sparse-keymap "Speed (b/s)")))
    (define-key map [other]
      '(menu-item "Other..." chomp-serial-set-speed
                  :enable chomp-serial--port))
    (define-key map [separator-1] '(menu-item "--"))
    (dolist (speed speeds)
      (define-key
       map
       (vector (make-symbol (format "speed-%s" speed)))
       `(menu-item
         ,(format "%s" speed)
         (lambda ()
           (interactive)
           (chomp-serial-set-speed ,speed))
         :button (:radio . (equal chomp-serial--speed ,speed)))))
    map))

(defun chomp-serial--config-menu ()
  "Return the mode-line serial configuration menu."
  (let ((map (make-sparse-keymap "Serial configuration")))
    (define-key map [configure]
      '(menu-item "Configure all..." chomp-serial-configure
                  :enable chomp-serial--port))
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
             (chomp-serial-set-framing ,bytesize ',parity ,stopbits))
           :button (:radio . (and (equal chomp-serial--bytesize ,bytesize)
                                  (eq chomp-serial--parity ',parity)
                                  (equal chomp-serial--stopbits ,stopbits)))))))
    (define-key map [separator-2] '(menu-item "--"))
    (dolist (bytesize '(8 7))
      (define-key
       map
       (vector (make-symbol (format "bytesize-%s" bytesize)))
       `(menu-item
         ,(format "%s data bits" bytesize)
         (lambda ()
           (interactive)
           (chomp-serial-set-framing ,bytesize
                                   chomp-serial--parity
                                   chomp-serial--stopbits))
         :button (:radio . (equal chomp-serial--bytesize ,bytesize)))))
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
             (chomp-serial-set-framing chomp-serial--bytesize
                                     ',value
                                     chomp-serial--stopbits))
           :button (:radio . (eq chomp-serial--parity ',value))))))
    (define-key map [separator-4] '(menu-item "--"))
    (dolist (stopbits '(1 2))
      (define-key
       map
       (vector (make-symbol (format "stopbits-%s" stopbits)))
       `(menu-item
         ,(format "%s stop bit%s" stopbits (if (= stopbits 1) "" "s"))
         (lambda ()
           (interactive)
           (chomp-serial-set-framing chomp-serial--bytesize
                                   chomp-serial--parity
                                   ,stopbits))
         :button (:radio . (equal chomp-serial--stopbits ,stopbits)))))
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
             (chomp-serial-set-flowcontrol ',value))
           :button (:radio . (eq chomp-serial--flowcontrol ',value))))))
    map))

(defun chomp-serial-mode-line-connection-menu (event)
  "Show the mode-line connection menu for EVENT."
  (interactive "e")
  (chomp-serial--popup-mode-line-menu event (chomp-serial--connection-menu)))

(defun chomp-serial-mode-line-speed-menu (event)
  "Show the mode-line speed menu for EVENT."
  (interactive "e")
  (chomp-serial--popup-mode-line-menu event (chomp-serial--speed-menu)))

(defun chomp-serial-mode-line-config-menu (event)
  "Show the mode-line configuration menu for EVENT."
  (interactive "e")
  (chomp-serial--popup-mode-line-menu event (chomp-serial--config-menu)))

(provide 'chomp-serial)

;;; chomp-serial.el ends here
