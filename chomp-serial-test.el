;;; chomp-serial-tests.el --- Tests for chomp-serial -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'chomp-serial-codec)

(declare-function chomp-serial "chomp-serial")
(declare-function chomp-serial--buffer-name "chomp-serial")
(declare-function chomp-serial--delete-foreign-buffer-processes "chomp-serial")
(declare-function chomp-serial--live-process-p "chomp-serial")
(declare-function chomp-serial--open-process "chomp-serial")
(declare-function chomp-serial--filter "chomp-serial")
(declare-function chomp-serial--remote-default-directory "chomp-serial")
(declare-function chomp-serial--remote-port-localname "chomp-serial")
(declare-function chomp-serial--socat-command "chomp-serial")
(declare-function chomp-serial--socat-open-address "chomp-serial")
(declare-function chomp-serial-configure "chomp-serial")
(declare-function chomp-serial-send-break "chomp-serial")

(defvar chomp-serial--bytesize)
(defvar chomp-serial--codec-state)
(defvar chomp-serial--connection-state)
(defvar chomp-serial--flowcontrol)
(defvar chomp-serial--parity)
(defvar chomp-serial--port)
(defvar chomp-serial--process)
(defvar chomp-serial--speed)
(defvar chomp-serial--stopbits)
(defvar chomp-serial-send-break-function)

(defvar chomp-serial-tests--chomp-serial-available
  (condition-case nil
      (progn
        (require 'chomp-serial)
        t)
    (error nil))
  "Non-nil when Chomp is available for chomp-serial integration tests.")

(defun chomp-serial-tests--require-chomp-serial ()
  "Skip the current test unless `chomp-serial' can be loaded."
  (unless chomp-serial-tests--chomp-serial-available
    (ert-skip "Chomp is not available")))

(defun chomp-serial-tests--sleep-process (buffer)
  "Return a long-lived process attached to BUFFER."
  (make-process :name (generate-new-buffer-name "chomp-serial-test-process")
                :buffer buffer
                :command '("sh" "-c" "sleep 30")
                :noquery t))

(defun chomp-serial-tests--decode-chunks (chunks &optional policy)
  "Decode CHUNKS with optional invalid-byte POLICY."
  (let ((state (chomp-serial-codec-make-state policy))
        (pieces nil))
    (dolist (chunk chunks)
      (push (chomp-serial-codec-decode state chunk) pieces))
    (push (chomp-serial-codec-flush state) pieces)
    (apply #'concat (nreverse pieces))))

(ert-deftest chomp-serial-codec-valid-utf-8 ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (concat "hello " (unibyte-string #xe2 #x98 #x83) "\n")))
           "hello ☃\n")))

(ert-deftest chomp-serial-codec-split-utf-8 ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xe2)
                  (unibyte-string #x98)
                  (unibyte-string #x83)))
           "☃")))

(ert-deftest chomp-serial-codec-malformed-sequence-continues ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xe2 ?x)))
           "�x")))

(ert-deftest chomp-serial-codec-hex-policy ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xff ?A #x80))
            'hex)
           "<FF>A<80>")))

(ert-deftest chomp-serial-codec-latin-1-policy ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xe9))
            'latin-1)
           "é")))

(ert-deftest chomp-serial-codec-nul-is-preserved ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string ?a 0 ?b)))
           (string ?a 0 ?b))))

(ert-deftest chomp-serial-codec-escape-sequences-can-split ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #x1b ?\[) "31m"))
           "\e[31m")))

(ert-deftest chomp-serial-codec-overlong-is-malformed ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xc0 #xaf)))
           "��"))
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xe0 #x80 #x80)))
           "���"))
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xf0 #x80 #x80 #x80)))
           "����")))

(ert-deftest chomp-serial-codec-invalid-prefix-is-not-buffered ()
  (dolist (chunk (list (unibyte-string #xe0 #x80)
                       (unibyte-string #xed #xa0)
                       (unibyte-string #xf0 #x80)
                       (unibyte-string #xf4 #x90)))
    (let ((state (chomp-serial-codec-make-state)))
      (should (string= (chomp-serial-codec-decode state chunk) "��"))
      (should (string= (chomp-serial-codec-state-pending state) "")))))

(ert-deftest chomp-serial-codec-unicode-boundaries ()
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xf0 #x90 #x80 #x80)))
           (char-to-string #x10000)))
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xf4 #x8f #xbf #xbf)))
           (char-to-string #x10ffff)))
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xf4 #x90 #x80 #x80)))
           "����"))
  (should (string=
           (chomp-serial-tests--decode-chunks
            (list (unibyte-string #xed #xa0 #x80)))
           "���")))

(ert-deftest chomp-serial-codec-random-bytes-do-not-signal ()
  (let ((state (chomp-serial-codec-make-state)))
    (dotimes (_ 64)
      (let ((chunk (make-string 32 0)))
        (dotimes (index (length chunk))
          (aset chunk index (random 256)))
        (should (stringp (chomp-serial-codec-decode state chunk)))))
    (should (stringp (chomp-serial-codec-flush state)))))

(ert-deftest chomp-serial-reuses-live-process-and-reconfigures-speed ()
  (chomp-serial-tests--require-chomp-serial)
  (let* ((port "/tmp/chomp-serial-test-port")
         (buffer (get-buffer-create (chomp-serial--buffer-name port)))
         (process (chomp-serial-tests--sleep-process buffer))
         configured opened)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq chomp-serial--port port)
            (setq chomp-serial--speed 9600)
            (setq chomp-serial--bytesize 8)
            (setq chomp-serial--parity nil)
            (setq chomp-serial--stopbits 1)
            (setq chomp-serial--flowcontrol nil)
            (setq chomp-serial--process process))
          (cl-letf (((symbol-function 'chomp-serial--setup-buffer)
                     (lambda (setup-port setup-speed)
                       (setq chomp-serial--port setup-port)
                       (unless (chomp-serial--live-process-p)
                         (setq chomp-serial--speed setup-speed))))
                    ((symbol-function 'serial-process-configure)
                     (lambda (&rest args)
                       (setq configured args)))
                    ((symbol-function 'chomp-serial--open-process)
                     (lambda ()
                       (setq opened t)))
                    ((symbol-function 'pop-to-buffer-same-window)
                     (lambda (&rest _) nil))
                    ((symbol-function 'chomp-serial--resize-terminal-to-window)
                     (lambda (&rest _) nil)))
            (chomp-serial port 115200))
          (with-current-buffer buffer
            (should (eq chomp-serial--process process))
            (should (equal chomp-serial--speed 115200)))
          (should-not opened)
          (should (equal (plist-get configured :process) process))
          (should (equal (plist-get configured :speed) 115200)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest chomp-serial-configure-stores-settings-while-disconnected ()
  (chomp-serial-tests--require-chomp-serial)
  (let (called)
    (with-temp-buffer
      (setq chomp-serial--port "/tmp/chomp-serial-test-port")
      (setq chomp-serial--process nil)
      (cl-letf (((symbol-function 'serial-process-configure)
                 (lambda (&rest _)
                   (setq called t))))
        (chomp-serial-configure 57600 7 'even 2 'hw))
      (should-not called)
      (should (equal chomp-serial--speed 57600))
      (should (equal chomp-serial--bytesize 7))
      (should (eq chomp-serial--parity 'even))
      (should (equal chomp-serial--stopbits 2))
      (should (eq chomp-serial--flowcontrol 'hw)))))

(ert-deftest chomp-serial-socat-address-uses-current-configuration ()
  (chomp-serial-tests--require-chomp-serial)
  (with-temp-buffer
    (setq chomp-serial--speed 115200)
    (setq chomp-serial--bytesize 7)
    (setq chomp-serial--parity 'odd)
    (setq chomp-serial--stopbits 2)
    (setq chomp-serial--flowcontrol 'hw)
    (should
     (string=
      (chomp-serial--socat-open-address "/dev/ttyUSB0")
      "OPEN:/dev/ttyUSB0,raw,echo=0,b115200,cs7,parenb=1,parodd=1,cstopb=1,crtscts=1,ixon=0,ixoff=0"))))

(ert-deftest chomp-serial-remote-port-components-use-tramp-localname ()
  (chomp-serial-tests--require-chomp-serial)
  (should (string= (chomp-serial--remote-port-localname
                    "/ssh:host:/dev/ttyUSB0")
                   "/dev/ttyUSB0"))
  (should (string= (chomp-serial--remote-default-directory
                    "/ssh:host:/dev/ttyUSB0")
                   "/ssh:host:/")))

(ert-deftest chomp-serial-remote-open-uses-socat-process ()
  (chomp-serial-tests--require-chomp-serial)
  (let* ((buffer (generate-new-buffer " *chomp-serial-remote-open*"))
         (real-make-process (symbol-function 'make-process))
         captured-args captured-default process)
    (unwind-protect
        (with-current-buffer buffer
          (setq chomp-serial--port "/ssh:host:/dev/ttyUSB0")
          (setq chomp-serial--speed 57600)
          (setq chomp-serial--bytesize 8)
          (setq chomp-serial--parity nil)
          (setq chomp-serial--stopbits 1)
          (setq chomp-serial--flowcontrol nil)
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (setq captured-args args)
                       (setq captured-default default-directory)
                       (let ((default-directory temporary-file-directory))
                         (funcall real-make-process
                                  :name "chomp-serial-remote-test-process"
                                  :buffer (plist-get args :buffer)
                                  :command (list "sh" "-c" "sleep 30")
                                  :noquery t))))
                    ((symbol-function 'chomp-serial--install-terminal-functions)
                     (lambda (&rest _) nil)))
            (setq process (chomp-serial--open-process)))
          (should (process-live-p process))
          (should (process-get process 'chomp-serial-remote))
          (should (eq (process-get process 'chomp-serial-backend) 'socat))
          (should (equal captured-default "/ssh:host:/"))
          (should (eq (plist-get captured-args :file-handler) t))
          (should (equal (plist-get captured-args :command)
                         '("socat" "-"
                           "OPEN:/dev/ttyUSB0,raw,echo=0,b57600,cs8,parenb=0,cstopb=0,crtscts=0,ixon=0,ixoff=0"))))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest chomp-serial-send-break-without-helper-errors ()
  (chomp-serial-tests--require-chomp-serial)
  (let* ((buffer (generate-new-buffer " *chomp-serial-break*"))
         (process (chomp-serial-tests--sleep-process buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (setq chomp-serial--port "/tmp/chomp-serial-test-port")
          (setq chomp-serial--process process)
          (let ((chomp-serial-send-break-function nil))
            (should-error (chomp-serial-send-break) :type 'user-error)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest chomp-serial-open-failure-clears-stale-process-state ()
  (chomp-serial-tests--require-chomp-serial)
  (let* ((buffer (generate-new-buffer " *chomp-serial-open-failure*"))
         (process (chomp-serial-tests--sleep-process buffer)))
    (unwind-protect
        (with-current-buffer buffer
          (setq chomp-serial--port "/tmp/chomp-serial-test-port")
          (setq chomp-serial--speed 9600)
          (setq chomp-serial--process process)
          (setq chomp-serial--connection-state 'connected)
          (cl-letf (((symbol-function 'make-serial-process)
                     (lambda (&rest _)
                       (error "open failed"))))
            (should-error (chomp-serial--open-process)))
          (should-not chomp-serial--process)
          (should (eq chomp-serial--connection-state 'disconnected))
          (should-not (process-live-p process)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(ert-deftest chomp-serial-filter-decodes-and-queues-output ()
  (chomp-serial-tests--require-chomp-serial)
  (let* ((buffer (generate-new-buffer " *chomp-serial-filter*"))
         (process (chomp-serial-tests--sleep-process buffer))
         queued)
    (unwind-protect
        (with-current-buffer buffer
          (setq chomp-serial--process process)
          (setq chomp-serial--codec-state (chomp-serial-codec-make-state))
          (process-put process 'chomp-serial-buffer buffer)
          (cl-letf (((symbol-function 'chomp-serial--queue-output)
                     (lambda (text) (push text queued))))
            (chomp-serial--filter process (unibyte-string #xe2))
            (chomp-serial--filter process (unibyte-string #x98 #x83)))
          (should (equal '("☃" "") queued)))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer buffer))))

(provide 'chomp-serial-tests)

;;; chomp-serial-tests.el ends here
