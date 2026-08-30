;;; ebb-graphics.el --- Kitty graphics model for Ebb -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Model-owned state and protocol command handling for terminal graphics.
;; This implements bounded Kitty APC parsing, image transmission over the
;; direct, file, temporary-file, and shared-memory mediums, zlib payload
;; compression, image identity, placements with kitty's sizing rules,
;; deletion, and replies.  Buffer rendering is intentionally kept out of this
;; module.
;;
;; Not implemented: animation (a=f, a=a, a=c), relative placements (P/Q/H/V),
;; source rectangles (x/y/w/h) and in-cell offsets (X/Y).

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'subr-x)

(defcustom ebb-kitty-graphics-storage-limit (* 64 1024 1024)
  "Maximum estimated Kitty image memory retained by one Ebb terminal.
PNG images are charged by at least their decoded four-byte surface size.
The quota is shared by the main and alternate screens, but eviction is
per-screen: one screen never deletes images backing another screen's
placements, so a full main screen can leave an alternate-screen upload
with ENOSPC.  A value of zero disables Kitty graphics support entirely,
which is also the kill-switch if untrusted programs abuse the synchronous
decode helpers (see README)."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-image-limit (* 32 1024 1024)
  "Maximum decoded bytes accepted for one Kitty image."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-allow-files nil
  "If non-nil, allow Kitty `t=f' transmissions to read local files.
Temporary-file (`t=t') and shared-memory (`t=s') transmissions remain
available for local terminals.  File media are always refused for remote
terminals, since their paths would otherwise be resolved on the Emacs host."
  :type 'boolean
  :group 'ebb)

(defcustom ebb-kitty-graphics-placement-limit 1024
  "Maximum number of placements retained by one terminal screen."
  :type 'integer
  :group 'ebb)

(defcustom ebb-kitty-graphics-surface-limit (* 64 1024 1024)
  "Maximum estimated bytes in one rendered placement surface.
The estimate uses four bytes per pixel for the complete occupied cell box."
  :type 'integer
  :group 'ebb)

(defcustom ebb-graphics-fallback-cell-size '(8 . 16)
  "Cell pixel size used only when the display cannot report its geometry.
Set this to the actual font cell size when using a custom renderer or a frame
whose pixel geometry is unavailable."
  :type '(cons integer integer)
  :group 'ebb)

(defun ebb-graphics-normalize-cell-size (size &optional fallback)
  "Return SIZE as a positive integer pixel pair, or FALLBACK.
Components are bounded to the unsigned-short range used by PTY winsize."
  (if (and (consp size)
           (integerp (car size)) (> (car size) 0)
           (integerp (cdr size)) (> (cdr size) 0))
      (cons (min #xffff (car size)) (min #xffff (cdr size)))
    fallback))

(cl-defstruct (ebb-graphics-image (:copier nil))
  "One stored Kitty graphics image.
FORMAT is 24, 32, or 100 (PNG).  WIDTH and HEIGHT are pixel dimensions."
  id number format width height data storage-bytes
  (cache-token (make-symbol "ebb-graphics-image"))
  render-source-key render-source)

(cl-defstruct (ebb-graphics-placement (:copier nil))
  "One placement of a stored Kitty image.
COLUMNS and ROWS are the currently visible cell rectangle.  BOX-COLUMNS and
BOX-ROWS preserve the original backing canvas when scrolling clips that
rectangle.  PIXEL-WIDTH and PIXEL-HEIGHT are the displayed image size in
pixels, which fits inside the original rectangle preserving aspect ratio."
  image-id placement-id row column columns rows box-columns box-rows
  pixel-width pixel-height z-index cursor-policy virtual
  (row-offset 0))

(cl-defstruct (ebb-graphics-upload (:copier nil))
  "An in-progress multi-APC Kitty image upload."
  params chunks byte-count row column cell-size)

(cl-defstruct (ebb-graphics-budget (:copier nil))
  "Storage accounting shared by a terminal's main and alternate screens."
  (byte-count 0))

(cl-defstruct (ebb-graphics-state (:copier nil))
  "Graphics resources owned by one terminal screen."
  (budget (make-ebb-graphics-budget))
  (images (make-hash-table :test #'eql))
  (placements nil)
  (image-order nil)
  (next-image-id 1)
  (generation 0)
  (upload nil))

(defun ebb-graphics-state-byte-count (state)
  "Return terminal-wide stored image bytes visible to STATE."
  (ebb-graphics-budget-byte-count (ebb-graphics-state-budget state)))

(gv-define-setter ebb-graphics-state-byte-count (value state)
  `(setf (ebb-graphics-budget-byte-count (ebb-graphics-state-budget ,state))
         ,value))

(defun ebb-graphics-create (&optional budget)
  "Create empty screen graphics state, optionally sharing BUDGET."
  (make-ebb-graphics-state :budget (or budget (make-ebb-graphics-budget))))

(defun ebb-graphics-reset (state)
  "Discard every image, placement, and partial upload in STATE."
  (setf (ebb-graphics-state-byte-count state)
        (max 0
             (- (ebb-graphics-state-byte-count state)
                (cl-loop for image being the hash-values
                         of (ebb-graphics-state-images state)
                         sum (ebb-graphics--image-storage-bytes image)))))
  (clrhash (ebb-graphics-state-images state))
  (cl-incf (ebb-graphics-state-generation state))
  (setf (ebb-graphics-state-placements state) nil
        (ebb-graphics-state-image-order state) nil
        (ebb-graphics-state-next-image-id state) 1
        (ebb-graphics-state-upload state) nil))

;;;; ---- Control data ---------------------------------------------------

(defun ebb-graphics--parse-integer (string &optional signed)
  "Parse decimal STRING, or return nil when invalid.
When SIGNED is non-nil, accept a leading minus sign."
  (when (and (stringp string)
             (string-match-p (if signed "\\`-?[0-9]+\\'" "\\`[0-9]+\\'")
                             string))
    (let ((value (string-to-number string)))
      (when (if signed
                (<= (- (expt 2 31)) value (1- (expt 2 31)))
              (<= 0 value (1- (expt 2 32))))
        value))))

(defun ebb-graphics--parse-params (header)
  "Return a hash table for Kitty control HEADER, or nil if malformed.
Empty headers are valid for payload-only packets.  Empty entries, empty
values, multi-character keys, and duplicate keys are rejected."
  (let ((params (make-hash-table :test #'eql))
        (valid t))
    (unless (string-empty-p header)
      (dolist (entry (split-string header "," nil))
        (if (not (string-match "\\`\\(.\\)=\\(.+\\)\\'" entry))
            (setq valid nil)
          (let ((key (aref (match-string 1 entry) 0))
                (value (match-string 2 entry)))
            (if (gethash key params)
                (setq valid nil)
              (puthash key value params))))))
    (and valid params)))

(defun ebb-graphics--param-string (params key &optional default)
  "Return PARAMS value for KEY, or DEFAULT."
  (or (gethash key params) default))

(defun ebb-graphics--param-char (params key default)
  "Return the single-character PARAMS value for KEY, or DEFAULT.
Return nil when the value is present but not a single character."
  (let ((value (gethash key params)))
    (cond ((null value) default)
          ((= (length value) 1) (aref value 0))
          (t nil))))

(defun ebb-graphics--param-integer (params key &optional default signed)
  "Return integer PARAMS value for KEY, or DEFAULT when absent.
Return the symbol `invalid' when the value is malformed."
  (if-let* ((raw (gethash key params)))
      (or (ebb-graphics--parse-integer raw signed) 'invalid)
    default))

(defun ebb-graphics--quiet-p (params kind)
  "Return non-nil when PARAMS suppress response KIND.
KIND is either `ok' or `error'.  As in kitty, q=1 suppresses OK responses
and q=2 suppresses all responses."
  (let ((quiet (ebb-graphics--param-integer params ?q 0)))
    (and (integerp quiet)
         (pcase kind
           ('ok (>= quiet 1))
           ('error (>= quiet 2))
           (_ nil)))))

(defun ebb-graphics--response-prefix (params &optional actual-id)
  "Build a Kitty response control prefix from PARAMS and ACTUAL-ID."
  (let ((id (or actual-id (ebb-graphics--param-integer params ?i nil)))
        (number (ebb-graphics--param-integer params ?I nil))
        (placement (ebb-graphics--param-integer params ?p nil))
        fields)
    (when (integerp id) (push (format "i=%d" id) fields))
    (when (integerp number) (push (format "I=%d" number) fields))
    (when (integerp placement) (push (format "p=%d" placement) fields))
    (concat "\e_G" (mapconcat #'identity (nreverse fields) ",") ";")))

(defun ebb-graphics--respond (respond-fn params kind message &optional actual-id)
  "Call RESPOND-FN with a Kitty response unless PARAMS suppress KIND."
  (unless (ebb-graphics--quiet-p params kind)
    (funcall respond-fn
             (concat (ebb-graphics--response-prefix params actual-id)
                     message "\e\\"))))

;;;; ---- Payload loading ------------------------------------------------

;; Loading helpers return a unibyte string on success or a cons
;; (error . "ECODE:message") whose text becomes the protocol reply.

(defun ebb-graphics--error (message)
  "Return the failed-load marker carrying MESSAGE."
  (cons 'error message))

(defun ebb-graphics--error-p (value)
  "Return non-nil when VALUE is a failed-load marker."
  (and (consp value) (eq (car value) 'error)))

(defun ebb-graphics--decode (payload)
  "Decode base64 PAYLOAD, returning a unibyte string or nil."
  (condition-case nil
      (let ((decoded (base64-decode-string payload)))
        (and (<= (length decoded) ebb-kitty-graphics-image-limit)
             (encode-coding-string decoded 'binary)))
    (error nil)))

(defun ebb-graphics--inflate (data)
  "Return zlib-compressed DATA inflated within the image limit, or an error.
Use a bounded helper so a small compressed input cannot make Emacs allocate
an unbounded output buffer."
  (or
   (when-let* ((python (executable-find "python3")))
     (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert data)
      (let ((status
             (call-process-region
              (point-min) (point-max) python t t nil "-c"
              (concat
               "import sys,zlib\n"
               "limit=int(sys.argv[1])\n"
               "try:\n"
               " d=zlib.decompressobj(); out=d.decompress(sys.stdin.buffer.read(),limit+1)\n"
               " if len(out)>limit or (not d.eof and (d.unconsumed_tail or len(out)>=limit)): sys.exit(3)\n"
               " if not d.eof: sys.exit(2)\n"
               " sys.stdout.buffer.write(out)\n"
               "except Exception: sys.exit(2)\n")
              (number-to-string ebb-kitty-graphics-image-limit))))
        (pcase status
          (0 (buffer-string))
          (3 (ebb-graphics--error "EFBIG:inflated image too large"))
          (_ (ebb-graphics--error "EINVAL:failed to inflate image data"))))))
   (ebb-graphics--error "ENOTSUP:bounded zlib helper unavailable")))

(defconst ebb-graphics--temporary-directories
  '("/tmp" "/dev/shm" "/var/tmp")
  "Directories besides `temporary-file-directory' holding deletable files.")

(defun ebb-graphics--temporary-file-p (path)
  "Return non-nil when PATH may be deleted after a `t=t' transmission."
  (and (string-search "tty-graphics-protocol" (file-name-nondirectory path))
       (let ((directory (file-name-directory (expand-file-name path)))
             (allowed (cons temporary-file-directory
                            (append (when (getenv "TMPDIR")
                                      (list (getenv "TMPDIR")))
                                    ebb-graphics--temporary-directories))))
         (cl-some (lambda (root)
                    (file-in-directory-p directory root))
                  allowed))))

(defun ebb-graphics--sensitive-path-p (path)
  "Return non-nil when PATH lies in a filesystem area that must not be read.
/dev/shm is exempt: clients such as `kitten icat' put temporary files there."
  (let ((truename (or (ignore-errors (file-truename path))
                      (expand-file-name path))))
    (and (not (string-prefix-p "/dev/shm/" truename))
         (cl-some (lambda (root) (string-prefix-p root truename))
                  '("/proc/" "/sys/" "/dev/")))))

(defun ebb-graphics--read-file-bytes (path offset size &optional delete-after)
  "Safely read up to SIZE bytes of regular file PATH from OFFSET.
SIZE zero reads to EOF.  When DELETE-AFTER is non-nil, unlink only if PATH
still names the opened inode.  Return bytes or an error marker."
  (cond
   ((ebb-graphics--sensitive-path-p path)
    (ebb-graphics--error "EPERM:refusing to read from a sensitive path"))
   ((not (executable-find "python3"))
    (ebb-graphics--error "ENOTSUP:safe file helper unavailable"))
   (t
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (let ((status
             (call-process
              "python3" nil t nil "-c"
              (concat
               "import os,stat,sys\n"
               "p=sys.argv[1]; off=int(sys.argv[2]); size=int(sys.argv[3]); limit=int(sys.argv[4]); delete=int(sys.argv[5])\n"
               "fd=None\n"
               "try:\n"
               " fd=os.open(p,os.O_RDONLY|os.O_NONBLOCK|getattr(os,'O_NOFOLLOW',0))\n"
               " st=os.fstat(fd)\n"
               " if not stat.S_ISREG(st.st_mode): sys.exit(3)\n"
               " available=max(0,st.st_size-off); count=available if size==0 else min(size,available)\n"
               " if count>limit: sys.exit(4)\n"
               " os.lseek(fd,off,os.SEEK_SET); parts=[]; remaining=count\n"
               " while remaining:\n"
               "  chunk=os.read(fd,min(remaining,1048576))\n"
               "  if not chunk: break\n"
               "  parts.append(chunk); remaining-=len(chunk)\n"
               " if delete:\n"
               "  current=os.lstat(p)\n"
               "  if (current.st_dev,current.st_ino)!=(st.st_dev,st.st_ino): sys.exit(5)\n"
               "  os.unlink(p)\n"
               " sys.stdout.buffer.write(b''.join(parts))\n"
               "except PermissionError: sys.exit(2)\n"
               "except OSError: sys.exit(5)\n"
               "finally:\n"
               " if fd is not None: os.close(fd)\n")
              path (number-to-string offset) (number-to-string size)
              (number-to-string ebb-kitty-graphics-image-limit)
              (if delete-after "1" "0"))))
        (pcase status
          (0 (buffer-string))
          (2 (ebb-graphics--error "EPERM:cannot open image file"))
          (3 (ebb-graphics--error "EBADF:not a regular file"))
          (4 (ebb-graphics--error "EFBIG:image file too large"))
          (_ (ebb-graphics--error "EIO:failed to read image file"))))))))

(defun ebb-graphics--shm-path (name)
  "Return the filesystem path of POSIX shared memory NAME, or nil.
The leading slash the protocol requires is optional: `kitten icat' omits it."
  (and (string-match "\\`/?\\([^/]+\\)\\'" name)
       (file-directory-p "/dev/shm")
       (concat "/dev/shm/" (match-string 1 name))))

(defun ebb-graphics--load-file-medium
    (params medium payload file-media-enabled)
  "Load image bytes for file MEDIUM (f, t, or s) named by base64 PAYLOAD."
  (let* ((name (and (<= (length payload) 4096)
                    (ebb-graphics--decode payload)))
         (decoded-path (and name (not (string-search "\0" name))
                            (decode-coding-string name 'utf-8)))
         (valid-path (and decoded-path
                          (equal name
                                 (encode-coding-string decoded-path 'utf-8))))
         (offset (ebb-graphics--param-integer params ?O 0))
         (size (ebb-graphics--param-integer params ?S 0))
         (path (pcase medium
                 (?s (and valid-path
                          (ebb-graphics--shm-path decoded-path)))
                 (_ (and valid-path decoded-path)))))
    (cond
     ((not file-media-enabled)
      (ebb-graphics--error
       "EPERM:file media disabled for remote terminals"))
     ((and (eq medium ?f) (not ebb-kitty-graphics-allow-files))
      (ebb-graphics--error "EPERM:file medium disabled"))
     ((or (null valid-path) (> (length name) 2048))
      (ebb-graphics--error "EINVAL:invalid file name"))
     ((not (and (integerp offset) (integerp size)))
      (ebb-graphics--error "EINVAL:invalid file offset or size"))
     ((null path)
      (ebb-graphics--error "EBADF:unsupported shared memory name"))
     ((and (eq medium ?t) (not (ebb-graphics--temporary-file-p path)))
      (ebb-graphics--error "EPERM:invalid temporary-file path"))
     (t
      (ebb-graphics--read-file-bytes
       path offset size (or (eq medium ?s) (eq medium ?t)))))))

(defun ebb-graphics--load-direct
    (state params payload row column cell-size)
  "Decode direct PAYLOAD, joining chunked uploads recorded in STATE.
Return (PARAMS DATA ROW COLUMN CELL-SIZE), nil while pending, or an error."
  (let ((decoded (ebb-graphics--decode payload))
        (more (ebb-graphics--param-integer params ?m 0))
        (continuation
         (let ((valid t))
           (maphash (lambda (key _value)
                      (unless (memq key '(?m ?q))
                        (setq valid nil)))
                    params)
           valid)))
    (cond
     ((or (null decoded) (not (memq more '(0 1))))
      (setf (ebb-graphics-state-upload state) nil)
      (ebb-graphics--error "EINVAL:invalid image data"))
     ((= more 1)
      (if-let* ((upload (and continuation
                             (ebb-graphics-state-upload state))))
          (if (> (+ (ebb-graphics-upload-byte-count upload) (length decoded))
                 ebb-kitty-graphics-image-limit)
              (progn
                (setf (ebb-graphics-state-upload state) nil)
                (ebb-graphics--error "EFBIG:image data too large"))
            (push decoded (ebb-graphics-upload-chunks upload))
            (cl-incf (ebb-graphics-upload-byte-count upload) (length decoded))
            nil)
        (setf (ebb-graphics-state-upload state)
              (make-ebb-graphics-upload
               :params params :chunks (list decoded)
               :byte-count (length decoded)
               :row row :column column :cell-size cell-size))
        nil))
     (t
      (if-let* ((upload (and continuation
                             (ebb-graphics-state-upload state))))
          (let ((initial (ebb-graphics-upload-params upload))
                (chunks (nreverse (cons decoded
                                         (ebb-graphics-upload-chunks upload)))))
            (setf (ebb-graphics-state-upload state) nil)
            ;; Kitty defines the final chunk's cursor and cell geometry as the
            ;; placement anchor for a chunked transmit-and-display command.
            (list initial (apply #'concat chunks) row column cell-size))
        (setf (ebb-graphics-state-upload state) nil)
        (list params decoded row column cell-size))))))

(defun ebb-graphics--png-size (data)
  "Return (WIDTH . HEIGHT) from the IHDR chunk of PNG DATA, or nil."
  (when (and (>= (length data) 24)
             (= (aref data 0) #x89)
             (equal (substring data 1 4) "PNG")
             (equal (substring data 12 16) "IHDR"))
    (cl-flet ((be32 (offset)
                (logior (ash (aref data offset) 24)
                        (ash (aref data (1+ offset)) 16)
                        (ash (aref data (+ offset 2)) 8)
                        (aref data (+ offset 3)))))
      (cons (be32 16) (be32 20)))))

(defun ebb-graphics--prepare-image (params data)
  "Inflate and validate DATA against PARAMS.
Return (FORMAT WIDTH HEIGHT DATA) or an error marker."
  (let* ((format (ebb-graphics--param-integer params ?f 32))
         (width (ebb-graphics--param-integer params ?s 0))
         (height (ebb-graphics--param-integer params ?v 0))
         (compression (ebb-graphics--param-char params ?o nil))
         (data (pcase compression
                 ('nil data)
                 (?z (or (ebb-graphics--inflate data)
                         (ebb-graphics--error "EINVAL:failed to inflate image data")))
                 (_ (ebb-graphics--error "EINVAL:unknown image compression")))))
    (cond
     ((ebb-graphics--error-p data) data)
     ((not (and (integerp format) (integerp width) (integerp height)))
      (ebb-graphics--error "EINVAL:invalid image parameters"))
     ((= format 100)
      (if-let* ((size (ebb-graphics--png-size data))
                ((> (car size) 0))
                ((> (cdr size) 0)))
          (if (> (* (car size) (cdr size) 4)
                 ebb-kitty-graphics-image-limit)
              (ebb-graphics--error "EFBIG:decoded PNG too large")
            (list format (car size) (cdr size) data))
        (ebb-graphics--error "EINVAL:invalid PNG data")))
     ((memq format '(24 32))
      (let ((required (* width height (/ format 8))))
        (cond
       ((not (and (> width 0) (> height 0)))
        (ebb-graphics--error "EINVAL:missing image dimensions"))
       ((> required ebb-kitty-graphics-image-limit)
        (ebb-graphics--error "EFBIG:decoded image too large"))
       ((< (length data) required)
        (ebb-graphics--error "ENODATA:insufficient image data"))
       (t (list format width height (substring data 0 required))))))
     (t (ebb-graphics--error "EINVAL:unknown image format")))))

(defun ebb-graphics--load-image
    (state params medium payload file-media-enabled row column cell-size)
  "Load an image for PARAMS over MEDIUM from PAYLOAD.
Return (PARAMS FORMAT WIDTH HEIGHT DATA), nil while a direct upload is still
pending, or an error marker."
  (pcase medium
    (?d
     (let ((loaded (ebb-graphics--load-direct
                    state params payload row column cell-size)))
       (cond
        ((null loaded) nil)
        ((ebb-graphics--error-p loaded) loaded)
        (t (let ((prepared (ebb-graphics--prepare-image
                            (nth 0 loaded) (nth 1 loaded))))
             (if (ebb-graphics--error-p prepared)
                 prepared
               (append (list (nth 0 loaded)) prepared (nthcdr 2 loaded))))))))
    ((or ?f ?t ?s)
     (setf (ebb-graphics-state-upload state) nil)
     (let ((data (ebb-graphics--load-file-medium
                  params medium payload file-media-enabled)))
       (if (ebb-graphics--error-p data)
           data
         (let ((prepared (ebb-graphics--prepare-image params data)))
           (if (ebb-graphics--error-p prepared)
               prepared
             (append (list params) prepared (list row column cell-size)))))))
    (_
     (setf (ebb-graphics-state-upload state) nil)
     (ebb-graphics--error "EINVAL:unknown transmission medium"))))

;;;; ---- Image storage --------------------------------------------------

(defun ebb-graphics--image-storage-size (format width height data)
  "Return the retained-memory charge for image DATA and decoded dimensions.
PNG storage is charged at no less than its estimated four-byte decoded
surface so compressed images cannot bypass the terminal storage quota."
  (max (length data)
       (if (and (eql format 100) (integerp width) (integerp height))
           (* width height 4)
         (length data))))

(defun ebb-graphics--image-storage-bytes (image)
  "Return IMAGE's retained-memory quota charge."
  (or (ebb-graphics-image-storage-bytes image)
      (ebb-graphics--image-storage-size
       (ebb-graphics-image-format image)
       (ebb-graphics-image-width image)
       (ebb-graphics-image-height image)
       (ebb-graphics-image-data image))))

(defun ebb-graphics--next-unused-id (state)
  "Return the next generated image ID without mutating STATE."
  (let ((images (ebb-graphics-state-images state))
        (candidate (ebb-graphics-state-next-image-id state)))
    (while (gethash candidate images)
      (setq candidate (1+ candidate)))
    candidate))

(defun ebb-graphics--allocate-id (state)
  "Reserve and return a currently unused generated image ID in STATE."
  (let ((candidate (ebb-graphics--next-unused-id state)))
    (setf (ebb-graphics-state-next-image-id state) (1+ candidate))
    candidate))

(defun ebb-graphics--prospective-image-id (state params)
  "Return the image ID a successful store for PARAMS would use."
  (let ((requested (ebb-graphics--param-integer params ?i nil)))
    (if (and (integerp requested) (> requested 0))
        requested
      (ebb-graphics--next-unused-id state))))

(defun ebb-graphics--delete-image (state image-id)
  "Delete IMAGE-ID and its placements from STATE."
  (when-let* ((image (gethash image-id (ebb-graphics-state-images state))))
    (remhash image-id (ebb-graphics-state-images state))
    (setf (ebb-graphics-state-byte-count state)
          (max 0 (- (ebb-graphics-state-byte-count state)
                    (ebb-graphics--image-storage-bytes image))))
    (cl-incf (ebb-graphics-state-generation state))
    (setf (ebb-graphics-state-image-order state)
          (delq image-id (ebb-graphics-state-image-order state))
          (ebb-graphics-state-placements state)
          (cl-delete image-id (ebb-graphics-state-placements state)
                     :key #'ebb-graphics-placement-image-id :test #'eql))
    t))

(defun ebb-graphics--eviction-plan (state required-bytes excluded-id)
  "Return oldest unreferenced image IDs freeing REQUIRED-BYTES, or nil.
EXCLUDED-ID is a replacement target and is never considered a victim."
  (if (<= required-bytes 0)
      'fit
    (let ((oldest (reverse (ebb-graphics-state-image-order state)))
          (freed 0)
          victims)
      (while (and oldest (< freed required-bytes))
        (let ((image-id (pop oldest)))
          (when (and (not (eql image-id excluded-id))
                     (not (cl-find
                           image-id (ebb-graphics-state-placements state)
                           :key #'ebb-graphics-placement-image-id :test #'eql)))
            (push image-id victims)
            (cl-incf freed
                     (ebb-graphics--image-storage-bytes
                      (gethash image-id
                               (ebb-graphics-state-images state)))))))
      (and (>= freed required-bytes) (nreverse victims)))))

(defun ebb-graphics--store-image (state params format width height data)
  "Store DATA of FORMAT and WIDTH by HEIGHT under PARAMS in STATE.
Return the image ID, or nil when the storage quota cannot admit it."
  (let* ((requested-id (ebb-graphics--param-integer params ?i nil))
         (image-id (if (and (integerp requested-id) (> requested-id 0))
                       requested-id
                     (ebb-graphics--allocate-id state)))
         (number (ebb-graphics--param-integer params ?I nil)))
    (let* ((old (gethash image-id (ebb-graphics-state-images state)))
           (old-bytes (if old (ebb-graphics--image-storage-bytes old) 0))
           (new-bytes (ebb-graphics--image-storage-size
                       format width height data))
           (projected (+ (- (ebb-graphics-state-byte-count state) old-bytes)
                         new-bytes))
           (required (- projected ebb-kitty-graphics-storage-limit))
           (victims (and (<= new-bytes ebb-kitty-graphics-storage-limit)
                         (ebb-graphics--eviction-plan
                          state required image-id))))
      ;; Do not mutate the old image, its placements, or accounting until a
      ;; complete admission plan has been proved to fit.
      (when victims
        (unless (eq victims 'fit)
          (dolist (victim victims)
            (ebb-graphics--delete-image state victim)))
        (when old
          (ebb-graphics--delete-image state image-id))
        (puthash image-id
                 (make-ebb-graphics-image
                  :id image-id :number (and (integerp number) number)
                  :format format :width width :height height :data data
                  :storage-bytes new-bytes)
                 (ebb-graphics-state-images state))
        (push image-id (ebb-graphics-state-image-order state))
        (cl-incf (ebb-graphics-state-byte-count state) new-bytes)
        (cl-incf (ebb-graphics-state-generation state))
        image-id))))

;;;; ---- Placements -----------------------------------------------------

(defun ebb-graphics--placement-geometry (image columns rows cell-size)
  "Return (COLUMNS ROWS PIXEL-WIDTH PIXEL-HEIGHT) for IMAGE.
COLUMNS and ROWS are the requested cell counts, zero meaning unspecified.
CELL-SIZE is (WIDTH . HEIGHT) in pixels.  An unspecified dimension preserves
the image aspect ratio.  When both dimensions are specified, the image is
letterboxed inside the requested cell box.  The renderer supplies centering
and partial-cell padding in a box-sized backing image, so row slices always
match Emacs glyph geometry."
  (let* ((cell-width (car cell-size))
         (cell-height (cdr cell-size))
         (image-width (max 1 (ebb-graphics-image-width image)))
         (image-height (max 1 (ebb-graphics-image-height image)))
         (both-specified (and (> columns 0) (> rows 0)))
         (scale (and both-specified
                     (min (/ (* columns cell-width) (float image-width))
                          (/ (* rows cell-height) (float image-height)))))
         (pixel-width
          (cond
           (both-specified (max 1 (round (* image-width scale))))
           ((> columns 0) (* columns cell-width))
           ((> rows 0)
            (max 1 (round (* image-width
                             (/ (* rows cell-height)
                                (float image-height))))))
           (t image-width)))
         (pixel-height
          (cond
           (both-specified (max 1 (round (* image-height scale))))
           ((> rows 0) (* rows cell-height))
           ((> columns 0)
            (max 1 (round (* image-height
                             (/ (* columns cell-width)
                                (float image-width))))))
           (t image-height))))
    (list (if (zerop columns) (ceiling pixel-width cell-width) columns)
          (if (zerop rows) (ceiling pixel-height cell-height) rows)
          pixel-width pixel-height)))

(defun ebb-graphics--placement-candidate
    (state params image row column cell-size &optional replacing-image)
  "Build a validated placement without mutating STATE.
When REPLACING-IMAGE is non-nil, existing placements for IMAGE are assumed to
be removed by an atomic retransmission before this candidate is committed."
  (let ((placement-id (ebb-graphics--param-integer params ?p nil))
        (columns (ebb-graphics--param-integer params ?c 0))
        (rows (ebb-graphics--param-integer params ?r 0))
        (z-index (ebb-graphics--param-integer params ?z 0 t))
        (cursor-policy (ebb-graphics--param-integer params ?C 0))
        (virtual (ebb-graphics--param-integer params ?U 0))
        (image-id (ebb-graphics-image-id image)))
    (when (and (integerp columns) (>= columns 0)
               (integerp rows) (>= rows 0)
               (integerp z-index) (>= z-index 0)
               (integerp cursor-policy) (memq cursor-policy '(0 1))
               (integerp virtual) (memq virtual '(0 1)))
      (pcase-let* ((`(,columns ,rows ,pixel-width ,pixel-height)
                    (ebb-graphics--placement-geometry
                     image columns rows cell-size))
                   (remaining
                    (cl-remove-if
                     (lambda (placement)
                       (or (and replacing-image
                                (= image-id
                                   (ebb-graphics-placement-image-id placement)))
                           (and (integerp placement-id) (> placement-id 0)
                                (= image-id
                                   (ebb-graphics-placement-image-id placement))
                                (eql placement-id
                                     (ebb-graphics-placement-placement-id
                                      placement)))))
                     (ebb-graphics-state-placements state)))
                   (surface-bytes
                    (* columns (car cell-size) rows (cdr cell-size) 4)))
        (when (and (> columns 0) (> rows 0)
                   (<= surface-bytes ebb-kitty-graphics-surface-limit)
                   (< (length remaining) ebb-kitty-graphics-placement-limit))
          (make-ebb-graphics-placement
           :image-id image-id
           :placement-id (and (integerp placement-id) placement-id)
           :row row :column column :columns columns :rows rows
           :box-columns columns :box-rows rows
           :pixel-width pixel-width :pixel-height pixel-height
           :z-index z-index :cursor-policy cursor-policy
           :virtual (= virtual 1)))))))

(defun ebb-graphics--commit-placement (state placement)
  "Commit validated PLACEMENT to STATE and return it."
  (when-let* ((placement-id (ebb-graphics-placement-placement-id placement)))
    (setf (ebb-graphics-state-placements state)
          (cl-delete-if
           (lambda (old)
             (and (= (ebb-graphics-placement-image-id placement)
                     (ebb-graphics-placement-image-id old))
                  (eql placement-id
                       (ebb-graphics-placement-placement-id old))))
           (ebb-graphics-state-placements state))))
  (push placement (ebb-graphics-state-placements state))
  (cl-incf (ebb-graphics-state-generation state))
  placement)

(defun ebb-graphics--add-placement (state params image row column cell-size)
  "Validate and add a placement of IMAGE to STATE."
  (when-let* ((placement (ebb-graphics--placement-candidate
                          state params image row column cell-size)))
    (ebb-graphics--commit-placement state placement)))

(defun ebb-graphics-clear-placements (state &optional all)
  "Remove visible placements from STATE, retaining image data.
When ALL is non-nil, remove virtual placeholder prototypes too."
  (let* ((old (ebb-graphics-state-placements state))
         (new (unless all
                (cl-remove-if-not #'ebb-graphics-placement-virtual old))))
    (unless (= (length old) (length new))
      (setf (ebb-graphics-state-placements state) new)
      (cl-incf (ebb-graphics-state-generation state)))))

(defun ebb-graphics-clear-row-range (state top bottom)
  "Remove non-virtual placements intersecting absolute rows TOP..BOTTOM."
  (ebb-graphics--remove-placements
   state
   (lambda (placement)
     (and (not (ebb-graphics-placement-virtual placement))
          (<= (ebb-graphics-placement-row placement) bottom)
          (> (+ (ebb-graphics-placement-row placement)
                (ebb-graphics-placement-rows placement))
             top)))
   nil))

(defun ebb-graphics--scroll-placement-up (placement top bottom count)
  "Return PLACEMENT moved up within TOP..BOTTOM, or nil if clipped away."
  (if (ebb-graphics-placement-virtual placement)
      placement
    (let* ((row (ebb-graphics-placement-row placement))
           (rows (ebb-graphics-placement-rows placement))
           (end (+ row rows))
           (limit (1+ bottom)))
      (if (not (and (< row limit) (> end top)))
          placement
        ;; A placement crossing a region boundary cannot follow only the
        ;; moved rows without being split.  Remove it rather than leave it
        ;; attached to unrelated text.
        (if (not (and (>= row top) (<= end limit)))
            nil
          (let ((cutoff (+ top count)))
          (cond
           ((<= end cutoff) nil)
           ((< row cutoff)
            (cl-incf (ebb-graphics-placement-row-offset placement) (- cutoff row))
            (setf (ebb-graphics-placement-row placement) top
                  (ebb-graphics-placement-rows placement) (- end cutoff))
            placement)
           (t
            (cl-decf (ebb-graphics-placement-row placement) count)
            placement))))))))

(defun ebb-graphics--scroll-placement-down (placement top bottom count)
  "Return PLACEMENT moved down within TOP..BOTTOM, or nil if clipped away."
  (if (ebb-graphics-placement-virtual placement)
      placement
    (let* ((row (ebb-graphics-placement-row placement))
           (rows (ebb-graphics-placement-rows placement))
           (end (+ row rows))
           (limit (1+ bottom)))
      (if (not (and (< row limit) (> end top)))
          placement
        ;; See `ebb-graphics--scroll-placement-up' for boundary crossings.
        (if (not (and (>= row top) (<= end limit)))
            nil
          (let ((new-row (+ row count)))
          (cond
           ((>= new-row limit) nil)
           ((> (+ end count) limit)
            (setf (ebb-graphics-placement-row placement) new-row
                  (ebb-graphics-placement-rows placement) (- limit new-row))
            placement)
           (t
            (setf (ebb-graphics-placement-row placement) new-row)
            placement))))))))

(defun ebb-graphics-scroll (state direction top bottom count)
  "Move placements in STATE for a terminal scroll operation.
DIRECTION is `up' or `down'; TOP and BOTTOM are inclusive row bounds."
  (when (> count 0)
    (let* ((old (ebb-graphics-state-placements state))
           (changed nil)
           (new (mapcar
                 (lambda (placement)
                   (let ((before
                          (list (ebb-graphics-placement-row placement)
                                (ebb-graphics-placement-rows placement)
                                (ebb-graphics-placement-row-offset placement)))
                         (result
                          (pcase direction
                            ('up (ebb-graphics--scroll-placement-up
                                  placement top bottom count))
                            ('down (ebb-graphics--scroll-placement-down
                                    placement top bottom count)))))
                     (when (or (null result)
                               (not (equal before
                                           (list
                                            (ebb-graphics-placement-row result)
                                            (ebb-graphics-placement-rows result)
                                            (ebb-graphics-placement-row-offset
                                             result)))))
                       (setq changed t))
                     result))
                 old)))
      (setq new (delq nil new))
      (when changed
        (setf (ebb-graphics-state-placements state) new)
        (cl-incf (ebb-graphics-state-generation state))))))

(defun ebb-graphics-history-grew (state old-base bottom count)
  "Adjust placements after COUNT rows were inserted into history.
Placements wholly below the top-anchored region move with unchanged viewport
content.  A placement crossing the region's bottom cannot follow both sides
without splitting, so it is removed."
  (let ((threshold (+ old-base bottom 1))
        changed new)
    (dolist (placement (ebb-graphics-state-placements state))
      (cond
       ((ebb-graphics-placement-virtual placement)
        (push placement new))
       ((and (< (ebb-graphics-placement-row placement) threshold)
             (> (+ (ebb-graphics-placement-row placement)
                   (ebb-graphics-placement-rows placement))
                threshold))
        (setq changed t))
       ((>= (ebb-graphics-placement-row placement) threshold)
        (cl-incf (ebb-graphics-placement-row placement) count)
        (setq changed t)
        (push placement new))
       (t (push placement new))))
    (when changed
      (setf (ebb-graphics-state-placements state) (nreverse new))
      (cl-incf (ebb-graphics-state-generation state)))))

(defun ebb-graphics-trim-history (state count)
  "Remove COUNT oldest physical history rows from placement coordinates."
  (when (> count 0)
    (let ((old (ebb-graphics-state-placements state))
          new changed)
      (dolist (placement old)
        (if (ebb-graphics-placement-virtual placement)
            (push placement new)
          (let* ((row (ebb-graphics-placement-row placement))
                 (rows (ebb-graphics-placement-rows placement))
                 (end (+ row rows)))
            (cond
             ((<= end count) (setq changed t))
             ((< row count)
              (setq changed t)
              (cl-incf (ebb-graphics-placement-row-offset placement) (- count row))
              (setf (ebb-graphics-placement-row placement) 0
                    (ebb-graphics-placement-rows placement) (- end count))
              (push placement new))
             (t
              (setq changed t)
              (cl-decf (ebb-graphics-placement-row placement) count)
              (push placement new))))))
      (setq new (nreverse new))
      (when changed
        (setf (ebb-graphics-state-placements state) new)
        (cl-incf (ebb-graphics-state-generation state))))))

(defun ebb-graphics--placement-intersects-p (placement row column)
  "Return non-nil when PLACEMENT covers ROW and COLUMN."
  (and (not (ebb-graphics-placement-virtual placement))
       (<= (ebb-graphics-placement-row placement) row)
       (< row (+ (ebb-graphics-placement-row placement)
                 (ebb-graphics-placement-rows placement)))
       (<= (ebb-graphics-placement-column placement) column)
       (< column (+ (ebb-graphics-placement-column placement)
                    (ebb-graphics-placement-columns placement)))))

(defun ebb-graphics--remove-placements (state predicate free-data)
  "Remove placements in STATE matching PREDICATE.
When FREE-DATA is non-nil, delete newly unreferenced backing images."
  (let ((old (ebb-graphics-state-placements state))
        removed-ids)
    (setf (ebb-graphics-state-placements state)
          (cl-delete-if
           (lambda (placement)
             (when (funcall predicate placement)
               (cl-pushnew (ebb-graphics-placement-image-id placement)
                           removed-ids)
               t))
           old))
    (when removed-ids
      (cl-incf (ebb-graphics-state-generation state)))
    (when free-data
      (dolist (image-id removed-ids)
        (unless (cl-find image-id (ebb-graphics-state-placements state)
                         :key #'ebb-graphics-placement-image-id :test #'eql)
          (ebb-graphics--delete-image state image-id))))))

(defun ebb-graphics--newest-image-by-number (state number)
  "Return newest image ID in STATE having image NUMBER."
  (cl-find-if
   (lambda (image-id)
     (eql number
          (ebb-graphics-image-number
           (gethash image-id (ebb-graphics-state-images state)))))
   (ebb-graphics-state-image-order state)))

(defun ebb-graphics--find-image (state params)
  "Return the image in STATE addressed by the i or I key of PARAMS."
  (let ((image-id (ebb-graphics--param-integer params ?i nil))
        (number (ebb-graphics--param-integer params ?I nil)))
    (when-let* ((id (cond
                     ((integerp image-id) image-id)
                     ((integerp number)
                      (ebb-graphics--newest-image-by-number state number)))))
      (gethash id (ebb-graphics-state-images state)))))

;;;; ---- Delete ---------------------------------------------------------

(defun ebb-graphics--handle-delete
    (state params row column row-base viewport-height viewport-width)
  "Apply a Kitty delete command to STATE using PARAMS at ROW and COLUMN."
  (setf (ebb-graphics-state-upload state) nil)
  (let* ((selector (ebb-graphics--param-char params ?d ?a))
         (kind (and selector (downcase selector)))
         (free-data (and selector (<= ?A selector ?Z)))
         (image-id (ebb-graphics--param-integer params ?i nil))
         (image-number (ebb-graphics--param-integer params ?I nil))
         (placement-id (ebb-graphics--param-integer params ?p nil))
         (x (ebb-graphics--param-integer params ?x 1))
         (y (ebb-graphics--param-integer params ?y 1))
         (z (ebb-graphics--param-integer params ?z 0 t)))
    (when (and (integerp x) (integerp y) (integerp z)
               (or (not (memq kind '(?x ?p ?q ?r)))
                   (and (eq kind ?p)
                        (integerp image-id) (integerp placement-id))
                   (> x 0))
               (or (not (memq kind '(?y ?p ?q ?r)))
                   (and (eq kind ?p)
                        (integerp image-id) (integerp placement-id))
                   (> y 0))
               (or (not (memq kind '(?x ?p ?q)))
                   (and (eq kind ?p)
                        (integerp image-id) (integerp placement-id))
                   (<= x viewport-width))
               (or (not (memq kind '(?y ?p ?q)))
                   (and (eq kind ?p)
                        (integerp image-id) (integerp placement-id))
                   (<= y viewport-height)))
      (setq x (1- x)
            y (+ row-base (1- y)))
      (pcase kind
        (?a
         (ebb-graphics--remove-placements
          state
          (lambda (placement)
            (and (not (ebb-graphics-placement-virtual placement))
                 (< (ebb-graphics-placement-row placement)
                    (+ row-base viewport-height))
                 (> (+ (ebb-graphics-placement-row placement)
                       (ebb-graphics-placement-rows placement))
                    row-base)))
          free-data))
        (?i
         (when (integerp image-id)
           (if (integerp placement-id)
               (ebb-graphics--remove-placements
                state
                (lambda (placement)
                  (and (= image-id (ebb-graphics-placement-image-id placement))
                       (eql placement-id
                            (ebb-graphics-placement-placement-id placement))))
                free-data)
             (if free-data
                 (ebb-graphics--delete-image state image-id)
               (ebb-graphics--remove-placements
                state
                (lambda (placement)
                  (= image-id (ebb-graphics-placement-image-id placement)))
                nil)))))
        (?n
         (when (integerp image-number)
           (when-let* ((id (ebb-graphics--newest-image-by-number
                            state image-number)))
             (if free-data
                 (ebb-graphics--delete-image state id)
               (ebb-graphics--remove-placements
                state
                (lambda (placement)
                  (and (= id (ebb-graphics-placement-image-id placement))
                       (or (not (integerp placement-id))
                           (eql placement-id
                                (ebb-graphics-placement-placement-id placement)))))
                nil)))))
        (?c
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (ebb-graphics--placement-intersects-p placement row column))
          free-data))
        (?p
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (if (and (integerp image-id) (integerp placement-id))
                      (and (not (ebb-graphics-placement-virtual placement))
                           (= image-id (ebb-graphics-placement-image-id placement))
                           (eql placement-id
                                (ebb-graphics-placement-placement-id placement)))
                    (ebb-graphics--placement-intersects-p placement y x)))
          free-data))
        (?q
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (and (= z (ebb-graphics-placement-z-index placement))
                       (ebb-graphics--placement-intersects-p placement y x)))
          free-data))
        (?x
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (and (not (ebb-graphics-placement-virtual placement))
                       (<= (ebb-graphics-placement-column placement) x)
                       (< x (+ (ebb-graphics-placement-column placement)
                               (ebb-graphics-placement-columns placement)))))
          free-data))
        (?y
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (and (not (ebb-graphics-placement-virtual placement))
                       (<= (ebb-graphics-placement-row placement) y)
                       (< y (+ (ebb-graphics-placement-row placement)
                               (ebb-graphics-placement-rows placement)))))
          free-data))
        (?z
         (ebb-graphics--remove-placements
          state (lambda (placement)
                  (and (not (ebb-graphics-placement-virtual placement))
                       (= z (ebb-graphics-placement-z-index placement))))
          free-data))
        (?r
         (let ((minimum (1+ x))
               (maximum (ebb-graphics--param-integer params ?y 1)))
           (dolist (id (copy-sequence (ebb-graphics-state-image-order state)))
             (when (and (integerp maximum) (<= minimum id maximum))
               (if free-data
                   (ebb-graphics--delete-image state id)
                 (ebb-graphics--remove-placements
                  state
                  (lambda (placement)
                    (= id (ebb-graphics-placement-image-id placement)))
                  nil))))))))))

;;;; ---- Command dispatch -----------------------------------------------

(defun ebb-graphics--invalid-parameter-p (params)
  "Return non-nil when known control data is malformed or out of range."
  (cl-labels ((integer (key &optional signed)
                (ebb-graphics--param-integer params key nil signed))
              (present-invalid (key &optional signed)
                (and (gethash key params)
                     (eq (integer key signed) 'invalid)))
              (outside (key allowed)
                (and (gethash key params)
                     (not (memq (integer key) allowed))))
              (nonpositive (key)
                (and (gethash key params)
                     (let ((value (integer key)))
                       (or (not (integerp value)) (<= value 0))))))
    (or (cl-some (lambda (key) (present-invalid key (eq key ?z)))
                 '(?i ?I ?p ?q ?f ?s ?v ?c ?r ?z ?C ?U ?m ?O ?S ?x ?y ?w
                   ?h ?X ?Y ?P ?Q ?H ?V))
        (cl-some (lambda (key)
                   (and (gethash key params)
                        (null (ebb-graphics--param-char params key nil))))
                 '(?a ?t ?o ?d))
        (cl-some #'nonpositive '(?i ?I ?p))
        (outside ?q '(0 1 2))
        (outside ?m '(0 1))
        (outside ?f '(24 32 100))
        (outside ?C '(0 1))
        (outside ?U '(0 1))
        (and (gethash ?a params)
             (not (memq (ebb-graphics--param-char params ?a nil)
                        '(?q ?t ?T ?p ?d))))
        (and (gethash ?t params)
             (not (memq (ebb-graphics--param-char params ?t nil)
                        '(?d ?f ?t ?s))))
        (and (gethash ?o params)
             (not (eq (ebb-graphics--param-char params ?o nil) ?z)))
        (and (gethash ?d params)
             (not (memq (downcase (ebb-graphics--param-char params ?d ?a))
                        '(?a ?i ?n ?c ?p ?q ?x ?y ?z ?r))))
        (and (memq (ebb-graphics--param-char params ?a ?t) '(?q ?t ?T ?p))
             (cl-some (lambda (key) (gethash key params))
                      '(?x ?y ?w ?h ?X ?Y ?P ?Q ?H ?V))))))

(defun ebb-graphics--handle-transmit
    (state params medium payload respond-fn row column cell-size
           file-media-enabled)
  "Handle an a=t or a=T command and return its new placement, if any."
  (pcase (ebb-graphics--load-image
          state params medium payload file-media-enabled row column cell-size)
    ('nil nil)
    ((and (pred ebb-graphics--error-p) failure)
     (ebb-graphics--respond respond-fn params 'error (cdr failure))
     nil)
    (`(,final-params ,format ,width ,height ,data
                     ,upload-row ,upload-column ,upload-cell-size)
     (let* ((prospective-id
             (ebb-graphics--prospective-image-id state final-params))
            (display
             (eq (ebb-graphics--param-char final-params ?a ?t) ?T))
            (candidate-image
             (make-ebb-graphics-image
              :id prospective-id
              :number (let ((number
                             (ebb-graphics--param-integer final-params ?I nil)))
                        (and (integerp number) number))
              :format format :width width :height height :data data))
            (placement
             (and display
                  (ebb-graphics--placement-candidate
                   state final-params candidate-image
                   upload-row upload-column upload-cell-size t))))
       (cond
        ((and display (null placement))
         (ebb-graphics--respond respond-fn final-params 'error
                                "EINVAL:invalid placement parameters"
                                prospective-id)
         nil)
        ((not (ebb-graphics--store-image
               state final-params format width height data))
         (ebb-graphics--respond respond-fn final-params 'error
                                "ENOSPC:image quota exceeded")
         nil)
        (t
         (when placement
           (ebb-graphics--commit-placement state placement))
         (when (or (gethash ?i final-params) (gethash ?I final-params))
           (ebb-graphics--respond respond-fn final-params 'ok "OK"
                                  prospective-id))
         placement))))))

(defun ebb-graphics--handle-query
    (state params medium payload respond-fn file-media-enabled)
  "Handle an a=q command: load the image described by PARAMS, but never store it."
  (let ((loaded (ebb-graphics--load-image
                 state params medium payload file-media-enabled
                 0 0 ebb-graphics-fallback-cell-size)))
    (cond
     ((null loaded))
     ((ebb-graphics--error-p loaded)
      (setf (ebb-graphics-state-upload state) nil)
      (ebb-graphics--respond respond-fn params 'error (cdr loaded)))
     (t
      (setf (ebb-graphics-state-upload state) nil)
      (ebb-graphics--respond respond-fn (car loaded) 'ok "OK")))))

(defun ebb-graphics-process-apc
    (state payload respond-fn row column &optional cell-size row-base
           viewport-height file-media-enabled viewport-width)
  "Process Kitty APC PAYLOAD against STATE.
RESPOND-FN writes replies to the child.  ROW and COLUMN are the current cursor
coordinates for placement commands.  CELL-SIZE is the terminal cell size in
pixels.  ROW-BASE and VIEWPORT-HEIGHT delimit the visible screen for delete
selectors.  FILE-MEDIA-ENABLED must be non-nil for local file media."
  (when (and (> (length payload) 0) (= (aref payload 0) ?G))
    (let* ((body (substring payload 1))
           (separator (string-search ";" body))
           (header (if separator (substring body 0 separator) body))
           (encoded (if separator (substring body (1+ separator)) ""))
           (params (ebb-graphics--parse-params header))
           (cell-size
            (ebb-graphics-normalize-cell-size
             cell-size ebb-graphics-fallback-cell-size))
           (row-base (or row-base 0))
           (viewport-height (or viewport-height most-positive-fixnum))
           placement)
      (cond
       ((null params)
        (setf (ebb-graphics-state-upload state) nil)
        (ebb-graphics--respond
         respond-fn (make-hash-table :test #'eql) 'error
         "EINVAL:malformed control data"))
       ((ebb-graphics--invalid-parameter-p params)
        (setf (ebb-graphics-state-upload state) nil)
        (ebb-graphics--respond respond-fn params 'error
                               "EINVAL:invalid control data"))
       ((and (gethash ?i params) (gethash ?I params))
        (setf (ebb-graphics-state-upload state) nil)
        (ebb-graphics--respond respond-fn params 'error
                               "EINVAL:cannot specify both image id and number"))
       (t
        (let ((action (ebb-graphics--param-char params ?a ?t))
              (medium (ebb-graphics--param-char params ?t ?d)))
          (pcase action
            ((guard (<= ebb-kitty-graphics-storage-limit 0))
             (setf (ebb-graphics-state-upload state) nil)
             (ebb-graphics--respond respond-fn params 'error
                                    "ENOTSUP:graphics disabled"))
            (?q
             (ebb-graphics--handle-query
              state params medium encoded respond-fn file-media-enabled))
            ((or ?t ?T)
             (setq placement
                   (ebb-graphics--handle-transmit
                    state params medium encoded respond-fn row column cell-size
                    file-media-enabled)))
            (?p
             ;; A non-transmission command aborts any incomplete upload.
             (setf (ebb-graphics-state-upload state) nil)
             (if-let* ((image (ebb-graphics--find-image state params)))
                 (if (setq placement
                           (ebb-graphics--add-placement
                            state params image row column cell-size))
                     (ebb-graphics--respond respond-fn params 'ok "OK"
                                            (ebb-graphics-image-id image))
                   (ebb-graphics--respond respond-fn params 'error
                                          "EINVAL:invalid placement parameters"))
               (ebb-graphics--respond respond-fn params 'error
                                      "ENOENT:image not found")))
            (?d
             (ebb-graphics--handle-delete
              state params row column row-base viewport-height
              (or viewport-width most-positive-fixnum)))
            (_
             (setf (ebb-graphics-state-upload state) nil)
             (ebb-graphics--respond respond-fn params 'error
                                    "EINVAL:unsupported action"))))))
      (cons 'kitty placement))))

(defun ebb-graphics-process-overflow (state payload respond-fn)
  "Reply to an oversized Kitty APC represented by retained PAYLOAD prefix."
  (setf (ebb-graphics-state-upload state) nil)
  (when (and (> (length payload) 0) (= (aref payload 0) ?G))
    (let* ((body (substring payload 1))
           (separator (string-search ";" body))
           (header (if separator (substring body 0 separator) body))
           (params (or (ebb-graphics--parse-params header)
                       (make-hash-table :test #'eql))))
      (ebb-graphics--respond respond-fn params 'error
                             "EFBIG:APC payload too large"))
    (cons 'kitty nil)))

(provide 'ebb-graphics)
;;; ebb-graphics.el ends here
