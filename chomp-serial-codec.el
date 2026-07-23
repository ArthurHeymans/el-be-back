;;; chomp-serial-codec.el --- Streaming byte codec for chomp-serial -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Maintainer: Arthur Heymans <arthur@aheymans.xyz>
;; Version: 0.1.1
;; Keywords: terminals, serial, processes
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/ArthurHeymans/chomp-serial
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small streaming UTF-8 decoder for serial terminals.  Serial process
;; filters receive arbitrary byte chunks: UTF-8 sequences can be split across
;; chunks, and malformed bytes must not be allowed to signal errors in the
;; terminal parser.

;;; Code:

(require 'cl-lib)

(defgroup chomp-serial nil
  "Chomp-backed serial terminal."
  :group 'terminals
  :prefix "chomp-serial-")

(defcustom chomp-serial-invalid-byte-policy 'replacement
  "How malformed UTF-8 bytes from the serial port are displayed.

`replacement' emits U+FFFD, `hex' emits markers like <E2>, and
`latin-1' maps bytes #x80..#xff directly to the corresponding
Unicode code point."
  :type '(choice (const :tag "Replacement character" replacement)
                 (const :tag "Hex marker" hex)
                 (const :tag "Latin-1" latin-1))
  :group 'chomp-serial)

(cl-defstruct (chomp-serial-codec-state
               (:constructor chomp-serial-codec--make-state))
  "State for streaming UTF-8 decoding."
  (pending "" :type string)
  (invalid-byte-policy chomp-serial-invalid-byte-policy :type symbol))

(defun chomp-serial-codec-make-state (&optional invalid-byte-policy)
  "Return a new serial decoder state.

INVALID-BYTE-POLICY defaults to `chomp-serial-invalid-byte-policy'."
  (chomp-serial-codec--make-state
   :pending ""
   :invalid-byte-policy (or invalid-byte-policy
                            chomp-serial-invalid-byte-policy)))

(defun chomp-serial-codec-reset (state)
  "Clear pending bytes in STATE."
  (setf (chomp-serial-codec-state-pending state) ""))

(defun chomp-serial-codec--unibyte (string)
  "Return STRING as a unibyte string.

Process filters configured with `no-conversion' normally pass unibyte
strings already.  This fallback handles accidental multibyte strings whose
characters are byte-sized."
  (if (not (multibyte-string-p string))
      string
    (let ((index 0)
          (pieces nil))
      (while (< index (length string))
        (let ((end (min (length string) (+ index 512)))
              (bytes nil))
          (while (< index end)
            (push (logand (aref string index) #xff) bytes)
            (setq index (1+ index)))
          (push (apply #'unibyte-string (nreverse bytes)) pieces)))
      (apply #'concat (nreverse pieces)))))

(defun chomp-serial-codec--continuation-byte-p (byte)
  "Return non-nil when BYTE is a UTF-8 continuation byte."
  (and (<= #x80 byte) (<= byte #xbf)))

(defun chomp-serial-codec--valid-sequence-byte-p (lead offset byte)
  "Return non-nil if BYTE can appear at OFFSET after UTF-8 LEAD."
  (and (chomp-serial-codec--continuation-byte-p byte)
       (or (/= offset 1)
           (cond
            ((= lead #xe0) (<= #xa0 byte #xbf))
            ((= lead #xed) (<= #x80 byte #x9f))
            ((= lead #xf0) (<= #x90 byte #xbf))
            ((= lead #xf4) (<= #x80 byte #x8f))
            (t t)))))

(defun chomp-serial-codec--valid-prefix-p (bytes index end)
  "Return non-nil if BYTES from INDEX to END can begin valid UTF-8."
  (let ((lead (aref bytes index))
        (offset 1)
        (valid t))
    (while (< (+ index offset) end)
      (unless (chomp-serial-codec--valid-sequence-byte-p
               lead offset (aref bytes (+ index offset)))
        (setq valid nil))
      (setq offset (1+ offset)))
    valid))

(defun chomp-serial-codec--invalid-byte-string (byte policy)
  "Return display text for malformed BYTE according to POLICY."
  (pcase policy
    ('latin-1 (char-to-string byte))
    ('hex (format "<%02X>" byte))
    (_ (char-to-string #xfffd))))

(defun chomp-serial-codec--valid-codepoint-p (codepoint min-codepoint)
  "Return non-nil if CODEPOINT is valid UTF-8 with MIN-CODEPOINT."
  (and (<= min-codepoint codepoint)
       (<= codepoint #x10ffff)
       (not (<= #xd800 codepoint #xdfff))))

(defun chomp-serial-codec--decode-codepoint (bytes index length)
  "Decode a UTF-8 sequence of LENGTH in BYTES at INDEX.

The caller must ensure all continuation bytes are available and valid."
  (let ((b0 (aref bytes index)))
    (pcase length
      (2 (logior (ash (logand b0 #x1f) 6)
                 (logand (aref bytes (1+ index)) #x3f)))
      (3 (logior (ash (logand b0 #x0f) 12)
                 (ash (logand (aref bytes (+ index 1)) #x3f) 6)
                 (logand (aref bytes (+ index 2)) #x3f)))
      (4 (logior (ash (logand b0 #x07) 18)
                 (ash (logand (aref bytes (+ index 1)) #x3f) 12)
                 (ash (logand (aref bytes (+ index 2)) #x3f) 6)
                 (logand (aref bytes (+ index 3)) #x3f))))))

(defun chomp-serial-codec--sequence-shape (byte)
  "Return (LENGTH . MIN-CODEPOINT) for leading BYTE, or nil if invalid."
  (cond
   ((<= #xc2 byte #xdf) '(2 . #x80))
   ((<= #xe0 byte #xef) '(3 . #x800))
   ((<= #xf0 byte #xf4) '(4 . #x10000))
   (t nil)))

;;;###autoload
(defun chomp-serial-codec-decode (state chunk)
  "Decode raw serial byte CHUNK using streaming decoder STATE.

The return value is a multibyte Emacs string.  Incomplete UTF-8
prefixes are retained in STATE and used by the next call."
  (let* ((policy (chomp-serial-codec-state-invalid-byte-policy state))
         (bytes (concat (chomp-serial-codec-state-pending state)
                        (chomp-serial-codec--unibyte chunk)))
         (length (length bytes))
         (index 0)
         (pieces nil))
    (setf (chomp-serial-codec-state-pending state) "")
    (while (< index length)
      (let ((byte (aref bytes index)))
        (cond
         ((< byte #x80)
          (push (char-to-string byte) pieces)
          (setq index (1+ index)))
         ((chomp-serial-codec--sequence-shape byte)
          (let* ((shape (chomp-serial-codec--sequence-shape byte))
                 (sequence-length (car shape))
                 (min-codepoint (cdr shape)))
            (if (< (- length index) sequence-length)
                (if (chomp-serial-codec--valid-prefix-p bytes index length)
                    (progn
                      (setf (chomp-serial-codec-state-pending state)
                            (substring bytes index))
                      (setq index length))
                  (push (chomp-serial-codec--invalid-byte-string
                         byte policy)
                        pieces)
                  (setq index (1+ index)))
              (let ((valid-continuations t)
                    (offset 1))
                (while (< offset sequence-length)
                  (unless (chomp-serial-codec--valid-sequence-byte-p
                           byte offset (aref bytes (+ index offset)))
                    (setq valid-continuations nil))
                  (setq offset (1+ offset)))
                (if (not valid-continuations)
                    (progn
                      (push (chomp-serial-codec--invalid-byte-string
                             byte policy)
                            pieces)
                      (setq index (1+ index)))
                  (let ((codepoint
                         (chomp-serial-codec--decode-codepoint
                          bytes index sequence-length)))
                    (if (chomp-serial-codec--valid-codepoint-p
                         codepoint min-codepoint)
                        (progn
                          (push (char-to-string codepoint) pieces)
                          (setq index (+ index sequence-length)))
                      (push (chomp-serial-codec--invalid-byte-string
                             byte policy)
                            pieces)
                      (setq index (1+ index)))))))))
         (t
          (push (chomp-serial-codec--invalid-byte-string byte policy) pieces)
          (setq index (1+ index))))))
    (apply #'concat (nreverse pieces))))

(defun chomp-serial-codec-flush (state)
  "Flush pending incomplete bytes from STATE as malformed-byte text."
  (let* ((policy (chomp-serial-codec-state-invalid-byte-policy state))
         (pending (chomp-serial-codec--unibyte
                   (chomp-serial-codec-state-pending state)))
         (pieces nil))
    (setf (chomp-serial-codec-state-pending state) "")
    (dotimes (index (length pending))
      (push (chomp-serial-codec--invalid-byte-string
             (aref pending index) policy)
            pieces))
    (apply #'concat (nreverse pieces))))

(provide 'chomp-serial-codec)

;;; chomp-serial-codec.el ends here
