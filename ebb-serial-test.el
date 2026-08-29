;;; ebb-serial-tests.el --- Tests for ebb-serial -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ebb-serial-codec)

(declare-function ebb-serial "ebb-serial")
(declare-function ebb-serial--buffer-name "ebb-serial")
(declare-function ebb-serial--create-terminal "ebb-serial")
(declare-function ebb-serial--delete-foreign-buffer-processes "ebb-serial")
(declare-function ebb-serial--live-process-p "ebb-serial")
(declare-function ebb-serial--open-process "ebb-serial")
(declare-function ebb-serial--process-arguments "ebb-serial")
(declare-function ebb-serial--filter "ebb-serial")
(declare-function ebb-serial--remote-default-directory "ebb-serial")
(declare-function ebb-serial--remote-port-localname "ebb-serial")
(declare-function ebb-serial--socat-command "ebb-serial")
(declare-function ebb-serial--socat-open-address "ebb-serial")
(declare-function ebb-serial-configure "ebb-serial")
(declare-function ebb-serial-send-break "ebb-serial")
(declare-function ebb-serial-mode "ebb-serial")

(defvar ebb-serial--bytesize)
(defvar ebb-serial--codec-state)
(defvar ebb-serial--connection-state)
(defvar ebb-serial--flowcontrol)
(defvar ebb-serial--parity)
(defvar ebb-serial--port)
(defvar ebb-serial--process)
(defvar ebb-serial--speed)
(defvar ebb-serial--stopbits)
(defvar ebb-serial-send-break-function)

(defvar ebb-serial-tests--ebb-serial-available
  (condition-case nil
      (progn
        (require 'ebb-serial)
        t)
    (error nil))
  "Non-nil when Ebb is available for ebb-serial integration tests.")

(defun ebb-serial-tests--require-ebb-serial ()
  "Skip the current test unless `ebb-serial' can be loaded."
  (unless ebb-serial-tests--ebb-serial-available
    (ert-skip "Ebb is not available")))

(defun ebb-serial-tests--sleep-process (buffer)
  "Return a long-lived process attached to BUFFER."
  (make-process :name (generate-new-buffer-name "ebb-serial-test-process")
                :buffer buffer
                :command '("sh" "-c" "sleep 30")
                :noquery t))

(defun ebb-serial-tests--decode-chunks (chunks &optional policy)
  "Decode CHUNKS with optional invalid-byte POLICY."
  (let ((state (ebb-serial-codec-make-state policy))
        (pieces nil))
    (dolist (chunk chunks)
      (push (ebb-serial-codec-decode state chunk) pieces))
    (push (ebb-serial-codec-flush state) pieces)
    (apply #'concat (nreverse pieces))))

(ert-deftest ebb-serial-codec-valid-utf-8 ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (concat "hello " (unibyte-string #xe2 #x98 #x83) "\n")))
           "hello ☃\n")))

(ert-deftest ebb-serial-codec-split-utf-8 ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xe2)
                  (unibyte-string #x98)
                  (unibyte-string #x83)))
           "☃")))

(ert-deftest ebb-serial-codec-malformed-sequence-continues ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xe2 ?x)))
           "�x")))

(ert-deftest ebb-serial-codec-hex-policy ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xff ?A #x80))
            'hex)
           "<FF>A<80>")))

(ert-deftest ebb-serial-codec-latin-1-policy ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xe9))
            'latin-1)
           "é")))

(ert-deftest ebb-serial-codec-nul-is-preserved ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string ?a 0 ?b)))
           (string ?a 0 ?b))))

(ert-deftest ebb-serial-codec-escape-sequences-can-split ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #x1b ?\[) "31m"))
           "\e[31m")))

(ert-deftest ebb-serial-codec-overlong-is-malformed ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xc0 #xaf)))
           "��"))
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xe0 #x80 #x80)))
           "���"))
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xf0 #x80 #x80 #x80)))
           "����")))

(ert-deftest ebb-serial-codec-invalid-prefix-is-not-buffered ()
  (dolist (chunk (list (unibyte-string #xe0 #x80)
                       (unibyte-string #xed #xa0)
                       (unibyte-string #xf0 #x80)
                       (unibyte-string #xf4 #x90)))
    (let ((state (ebb-serial-codec-make-state)))
      (should (string= (ebb-serial-codec-decode state chunk) "��"))
      (should (string= (ebb-serial-codec-state-pending state) "")))))

(ert-deftest ebb-serial-codec-unicode-boundaries ()
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xf0 #x90 #x80 #x80)))
           (char-to-string #x10000)))
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xf4 #x8f #xbf #xbf)))
           (char-to-string #x10ffff)))
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xf4 #x90 #x80 #x80)))
           "����"))
  (should (string=
           (ebb-serial-tests--decode-chunks
            (list (unibyte-string #xed #xa0 #x80)))
           "���")))

(ert-deftest ebb-serial-codec-random-bytes-do-not-signal ()
  (let ((state (ebb-serial-codec-make-state)))
    (dotimes (_ 64)
      (let ((chunk (make-string 32 0)))
        (dotimes (index (length chunk))
          (aset chunk index (random 256)))
        (should (stringp (ebb-serial-codec-decode state chunk)))))
    (should (stringp (ebb-serial-codec-flush state)))))

(ert-deftest ebb-serial-creates-terminal-with-ebb-0.1.0 ()
  (ebb-serial-tests--require-ebb-serial)
  (cl-letf (((symbol-function 'ebb-io-create-terminal) nil))
    (with-temp-buffer
      (let ((ebb-scrollback-lines 37))
        (let ((io (ebb-serial--create-terminal)))
          (should (eq (ebb-io-buffer io) (current-buffer)))
          (should (ebb-io-screen io))
          (should (ebb-io-render io))
          (should (ebb-io-parser io))
          (should (= 37 (ebb-screen-scrollback-max
                         (ebb-io-screen io))))))
      (let ((ebb-scrollback-lines -1))
        (should-error (ebb-serial--create-terminal))))))

(ert-deftest ebb-serial-setup-uses-shared-resize-hook ()
  (ebb-serial-tests--require-ebb-serial)
  (with-temp-buffer
    (ebb-serial--setup-buffer "/tmp/ebb-serial-test-port" 115200)
    (should (memq #'ebb--window-size-change
                  window-size-change-functions))
    (should-not (memq #'ebb-serial--resize-terminal-to-window
                      window-size-change-functions))))

(ert-deftest ebb-serial-reuses-live-process-and-reconfigures-speed ()
  (ebb-serial-tests--require-ebb-serial)
  (let* ((port "/tmp/ebb-serial-test-port")
         (buffer (get-buffer-create (ebb-serial--buffer-name port)))
         (process (ebb-serial-tests--sleep-process buffer))
         configured opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq ebb-serial--port port)
            (setq ebb-serial--speed 9600)
            (setq ebb-serial--bytesize 8)
            (setq ebb-serial--parity nil)
            (setq ebb-serial--stopbits 1)
            (setq ebb-serial--flowcontrol nil)
            (setq ebb-serial--process process))
          (cl-letf (((symbol-function 'ebb-serial--setup-buffer)
                     (lambda (setup-port setup-speed)
                       (setq ebb-serial--port setup-port)
                       (unless (ebb-serial--live-process-p)
                         (setq ebb-serial--speed setup-speed))))
                    ((symbol-function 'serial-process-configure)
                     (lambda (&rest args)
                       (setq configured args)))
                    ((symbol-function 'ebb-serial--open-process)
                     (lambda ()
                       (setq opened t)))
                    ((symbol-function 'pop-to-buffer-same-window)
                     (lambda (&rest _) nil))
                    ((symbol-function 'ebb-serial--resize-terminal-to-window)
                     (lambda (&rest _) nil)))
            (should (eq (ebb-serial port 115200) buffer)))
          (with-current-buffer buffer
            (should (eq ebb-serial--process process))
            (should (equal ebb-serial--speed 115200)))
          (should-not opened)
          (should (equal (plist-get configured :process) process))
          (should (equal (plist-get configured :speed) 115200)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest ebb-serial-configure-stores-settings-while-disconnected ()
  (ebb-serial-tests--require-ebb-serial)
  (let (called)
    (with-temp-buffer
      (setq ebb-serial--port "/tmp/ebb-serial-test-port")
      (setq ebb-serial--process nil)
      (cl-letf (((symbol-function 'serial-process-configure)
                 (lambda (&rest _)
                   (setq called t))))
        (ebb-serial-configure 57600 7 'even 2 'hw))
      (should-not called)
      (should (equal ebb-serial--speed 57600))
      (should (equal ebb-serial--bytesize 7))
      (should (eq ebb-serial--parity 'even))
      (should (equal ebb-serial--stopbits 2))
      (should (eq ebb-serial--flowcontrol 'hw)))))

(ert-deftest ebb-serial-socat-address-uses-current-configuration ()
  (ebb-serial-tests--require-ebb-serial)
  (with-temp-buffer
    (setq ebb-serial--speed 115200)
    (setq ebb-serial--bytesize 7)
    (setq ebb-serial--parity 'odd)
    (setq ebb-serial--stopbits 2)
    (setq ebb-serial--flowcontrol 'hw)
    (should
     (string=
      (ebb-serial--socat-open-address "/dev/ttyUSB0")
      "OPEN:/dev/ttyUSB0,raw,echo=0,b115200,cs7,parenb=1,parodd=1,cstopb=1,crtscts=1,ixon=0,ixoff=0"))))

(ert-deftest ebb-serial-remote-port-components-use-tramp-localname ()
  (ebb-serial-tests--require-ebb-serial)
  (should (string= (ebb-serial--remote-port-localname
                    "/ssh:host:/dev/ttyUSB0")
                   "/dev/ttyUSB0"))
  (should (string= (ebb-serial--remote-default-directory
                    "/ssh:host:/dev/ttyUSB0")
                   "/ssh:host:/")))

(ert-deftest ebb-serial-process-arguments-use-terminal-buffer ()
  (ebb-serial-tests--require-ebb-serial)
  (with-temp-buffer
    (setq ebb-serial--port "/dev/ttyUSB0")
    (should (eq (plist-get (ebb-serial--process-arguments) :buffer)
                (current-buffer)))))

(ert-deftest ebb-serial-char-mode-sends-control-keys-directly ()
  "Serial command bindings must not shadow char-mode control keys."
  (ebb-serial-tests--require-ebb-serial)
  (with-temp-buffer
    (ebb-mode)
    (ebb-serial-mode 1)
    (ebb-char-mode)
    (should (eq (key-binding (kbd "C-c")) #'ebb-self-input))
    (ebb-semi-char-mode)
    (should (eq (key-binding (kbd "C-c C-c")) #'ebb-self-input))
    (should (eq (key-binding (kbd "C-c C-s b"))
                #'ebb-serial-send-break))))

(ert-deftest ebb-serial-remote-open-uses-socat-process ()
  (ebb-serial-tests--require-ebb-serial)
  (let* ((buffer (generate-new-buffer " *ebb-serial-remote-open*"))
         (real-make-process (symbol-function 'make-process))
         captured-args captured-default process)
    (unwind-protect
        (with-current-buffer buffer
          (setq ebb-serial--port "/ssh:host:/dev/ttyUSB0")
          (setq ebb-serial--speed 57600)
          (setq ebb-serial--bytesize 8)
          (setq ebb-serial--parity nil)
          (setq ebb-serial--stopbits 1)
          (setq ebb-serial--flowcontrol nil)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-args args)
                       (setq captured-default default-directory)
                       (let ((default-directory temporary-file-directory))
                         (funcall real-make-process
                                  :name "ebb-serial-remote-test-process"
                                  :buffer (plist-get args :buffer)
                                  :command (list "sh" "-c" "sleep 30")
                                  :noquery t))))
                    ((symbol-function 'ebb-serial--install-terminal-functions)
                     (lambda (&rest _) nil)))
            (setq process (ebb-serial--open-process)))
          (should (process-live-p process))
          (should (process-get process 'ebb-serial-remote))
          (should (eq (process-get process 'ebb-serial-backend) 'socat))
          (should (equal captured-default "/ssh:host:/"))
          (should (eq (plist-get captured-args :file-handler) t))
          (should (eq (plist-get captured-args :buffer) buffer))
          (should (equal (plist-get captured-args :command)
                         '("socat" "-"
                           "OPEN:/dev/ttyUSB0,raw,echo=0,b57600,cs8,parenb=0,cstopb=0,crtscts=0,ixon=0,ixoff=0"))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest ebb-serial-send-break-without-helper-errors ()
  (ebb-serial-tests--require-ebb-serial)
  (let* ((buffer (generate-new-buffer " *ebb-serial-break*"))
         (process (ebb-serial-tests--sleep-process buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (setq ebb-serial--port "/tmp/ebb-serial-test-port")
          (setq ebb-serial--process process)
          (let ((ebb-serial-send-break-function nil))
            (should-error (ebb-serial-send-break) :type 'user-error)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest ebb-serial-open-failure-clears-stale-process-state ()
  (ebb-serial-tests--require-ebb-serial)
  (let* ((buffer (generate-new-buffer " *ebb-serial-open-failure*"))
         (process (ebb-serial-tests--sleep-process buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (setq ebb-serial--port "/tmp/ebb-serial-test-port")
          (setq ebb-serial--speed 9600)
          (setq ebb-serial--process process)
          (setq ebb-serial--connection-state 'connected)
          (cl-letf (((symbol-function 'make-serial-process)
                     (lambda (&rest _)
                       (error "open failed"))))
            (should-error (ebb-serial--open-process)))
          (should-not ebb-serial--process)
          (should (eq ebb-serial--connection-state 'disconnected))
          (should-not (process-live-p process)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest ebb-serial-filter-decodes-and-queues-output ()
  (ebb-serial-tests--require-ebb-serial)
  (let* ((buffer (generate-new-buffer " *ebb-serial-filter*"))
         (process (ebb-serial-tests--sleep-process buffer))
         queued)
    (unwind-protect
        (with-current-buffer buffer
          (setq ebb-serial--process process)
          (setq ebb-serial--codec-state (ebb-serial-codec-make-state))
          (process-put process 'ebb-serial-buffer buffer)
          (cl-letf (((symbol-function 'ebb-serial--queue-output)
                     (lambda (text) (push text queued))))
            (ebb-serial--filter process (unibyte-string #xe2))
            (ebb-serial--filter process (unibyte-string #x98 #x83)))
          (should (equal '("☃" "") queued)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(provide 'ebb-serial-tests)

;;; ebb-serial-tests.el ends here
