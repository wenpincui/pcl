;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:id3-parser)

(defun binary-bump ()
  (let* ((seq #(#xff #xfe #x56 #x78 #x9a #xbc #xde #xf0))
         (stream (make-in-memory-input-stream seq)))
    stream))

(defun string-bump ()
  (make-string-input-stream "this is a long string"))

(defun swap-byte (code)
  (let ((low (ldb (byte 8 0) code))
        (high (ldb (byte 8 8) code)))
    (logior high (ash low 8))))

(define-binary-type unsigned-integer (length bits-per-integer)
  (:reader (in)
           (loop with result = 0
              for i downfrom (1- length) to 0
              do (setf (ldb (byte bits-per-integer (* i bits-per-integer)) result)
                       (read-byte in))
              finally (return result)))
  (:writer (out val)
           (loop
                for i from 0 below length
              do (write-byte (ldb (byte bits-per-integer (* i bits-per-integer)) val)
                             out))))

(define-binary-type u1 ()
  (unsigned-integer :length 1 :bits-per-integer 8))
(define-binary-type u2 ()
  (unsigned-integer :length 2 :bits-per-integer 8))
(define-binary-type u3 ()
  (unsigned-integer :length 3 :bits-per-integer 8))
(define-binary-type u4 ()
  (unsigned-integer :length 4 :bits-per-integer 8))

(define-binary-type id3-tag-size ()
  (unsigned-integer :length 4 :bits-per-integer 7))

(define-binary-type general-string (length type)
  (:reader (in)
           (let ((str (make-string length)))
             (loop for i from 0 below length
                do (setf (char str i) (read-value type in))
                finally (return str))))
  (:writer (out value)
           (loop for i from 0 below length
              do (write-value type out (char value i)))))

(define-binary-type general-terminate-string (termintor type)
  (:reader (in)
           (with-output-to-string (str)
             (loop for char = (code-char (read-byte in))
                until (char= char termintor)
                do (write-char char str))))
  (:writer (out val)
           (with-input-from-string (str val)
             (loop for char = (read-char str)
                until (char= char termintor)
                do (write-byte (char-code char) out)
                finally (write-byte (char-code termintor) out)))))

(define-binary-type iso-8859-1 ()
  (:reader (in)
           (code-char (read-byte in)))
  (:writer (out val)
           (write-byte (char-code val) out)))

(define-binary-type ucs-2 (swap)
  (:reader (in)
           (let ((code (read-value 'u2 in)))
             (assert (<= code #xffff))
             (if swap
               (setf code (swap-byte code)))
             (code-char code)))
  (:writer (out val)
           (let ((code (char-code val)))
             (if swap
                 (setf code (swap-byte code)))
             (write-value 'u2 out code))))

(define-binary-type ucs-2-be ()
  (ucs-2 :swap nil))

(define-binary-type ucs-2-le ()
  (ucs-2 :swap t))

(defun ucs-2-char-type (swapmask)
  (ecase swapmask
    (#xfffe 'ucs-2-le)
    (#xfeff 'ucs-2-be)))

(define-binary-type ucs-2-string (length)
  (:reader (in)
           (let* ((swapmask (read-value 'u2 in))
                  (type (ucs-2-char-type swapmask))
                  (len (1- (/ length 2))))
             (read-value 'general-string in :length len :type type)))
  (:writer (out val)
           (progn
             (write-value 'u2 out #xfffe)
             (write-value 'general-string out val
                          :length (length val)
                          :type (ucs-2-char-type #xfffe)))))

(define-binary-type iso-8859-1-string (length)
  (:reader (in)
           (read-value 'general-string in
                       :length length
                       :type 'iso-8859-1))
  (:writer (out val)
           (write-value 'general-string out val
                        :length (length val)
                        :type 'iso-8859-1)))

(define-tagged-binary-class id3-tag ()
  ((identifier (iso-8859-1-string :length 3))
   (major-version u1)
   (minor-version u1)
   (flags u1)
   (tag-size id3-tag-size))
  (:dispatch
   (ecase major-version
     (2 'id3-v2.2-tag)
     (3 'id3-v2.3-tag))))

(defun read-id3 (file)
  (with-open-file (s file :element-type 'unsigned-byte)
    (read-value 'id3-tag s)))

(defun show-header (file)
  (with-slots (identifier major-version minor-version flags tag-size)
      (read-id3 file)
    (format t "file name~10t:~a~%" (enough-namestring file))
    (format t "identify~10t:~a~%" identifier)
    (format t "main ver~10t:~a~%" major-version)
    (format t "mino ver~10t:~a~%" minor-version)
    (format t "flag~10t:~a~%" flags)
    (format t "size~10t:~a~%" tag-size)))

(defun mp3-p (file)
  (equal "mp3" (pathname-type file)))

(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (v) (cons v 0)) '(2 3 4))))
    (flet ((get-ver (file)
             (and (mp3-p file)
                  (incf (cdr (assoc (major-version (read-id3 file)) versions))))))
      (walk-directory dir #'get-ver :test #'mp3-p))
    versions))

(define-tagged-binary-class frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-binary-class generic-frame (frame)
  ((raw-data (raw-bytes :size size))))

(define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (read-sequence buf in)))
  (:writer (out val)
           (write-sequence val out)))

(defun find-frame-class (id)
  (declare (ignore id))
  'generic-frame)

(define-binary-type id3-frames (size frame-type)
  (:reader (in)
           (loop with to-read = size
              for frame = (read-frame in)
              while (plusp frame)
              do (decf (size frame) to-read)
              collect frame
              finally (loop repeat (- size to-read)
                         do (read-byte in))))
  (:writer (out val)
           (loop with to-write = size
              for frame in val
              do (write-value 'frame frame out)
                (decf to-write (size frame))
                finally (loop repeat to-write
                           do (write-byte 0 out)))))

(define-condition in-padding () nil)

(define-binary-type frame-id (length)
  (:reader (in)
           (let ((first-byte (read-byte in)))
             (when (eql 0 first-byte)
               (signal 'in-padding))
             (let ((rest (read-value 'iso-8859-1-string in
                                     :length (1- length))))
               (concatenate 'string first-byte rest))))
  (:writer (out val)
           (write-value 'iso-8859-1-string out val)))

(defun read-frame (in)
  (handler-case (read-value 'frame in)
    (in-padding () nil)))

(define-binary-class id3-v2.2-tag (id3-tag)
  ((frames id3-frames :size size :frame-type 'id3-v2.2-tag)))

(define-binary-type optional (type if)
  (:reader (in)
           (when if
             (read-value type in)))
  (:writer (out val)
           (when if
             (write-value type out val))))

(define-binary-class id3-v2.3-tag (id3-tag)
  ((extended-header-size (optional :type 'u4 :if (extended-p flags)))
   (extra-flags (optional :type 'u2 :if (extended-p flags)))
   (padding-size (optional :type 'u4 :if (extended-p flags)))
   (crc (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames id3-frames :size size :frame-type 'id3-v2.3-tag)))

(defun extended-p (flag)
  (logbitp 6 flag))

(defun crc-p (flags extra)
  (and (extended-p flags) (logbitp 15 extra)))
