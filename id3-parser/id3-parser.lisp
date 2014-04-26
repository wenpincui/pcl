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

(define-binary-type general-terminate-string (terminator type)
  (:reader (in)
           (with-output-to-string (str)
             (loop for char = (code-char (read-byte in))
                until (char= char terminator)
                do (write-char char str))))
  (:writer (out val)
           (with-input-from-string (str val)
             (loop for char = (read-char str)
                until (char= char terminator)
                do (write-byte (char-code char) out)
                finally (write-byte (char-code terminator) out)))))

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

(define-binary-type ucs-2-terminated-string (terminator)
  (:reader (in)
           (let ((byte-order-mark (read-value 'u2 in)))
             (read-value
              'general-terminate-string in
              :terminator terminator
              :type (ucs-2-char-type byte-order-mark))))
  (:writer (out string)
           (write-value 'u2 out #xfeff)
           (write-value
            'general-terminate-string out string
            :terminator terminator
            :type (ucs-2-char-type #xfeff))))

(define-binary-type iso-8859-1-terminated-string (terminator)
  (general-terminate-string :terminator terminator :type 'iso-8859-1))

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

(define-tagged-binary-class id3-v2.2-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-tagged-binary-class id3-v2.3-frame ()
  ((id (frame-id :length 4))
   (size u4)
   (flags u2)
   (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags)))
   (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))

(defun frame-compressed-p (flags)
  (logbitp 7 flags))

(defun frame-encrypted-p (flags)
  (logbitp 6 flags))

(defun frame-grouped-p (flags)
  (logbitp 5 flags))

(defmethod frame-header-size ((frame id3-v2.2-frame)) 6)
(defmethod frame-header-size ((frame id3-v2.3-frame)) 10)

(define-binary-class generic-frame-v2.2 (id3-v2.2-frame generic-frame) ())
(define-binary-class generic-frame-v2.3 (id3-v2.3-frame generic-frame) ())

(define-binary-class generic-frame ()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))

(defgeneric data-bytes (frame))

(defmethod data-bytes ((frame id3-v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3-v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

(define-binary-type raw-bytes (size)
  (:reader (in)
           (let ((buf (make-array size :element-type '(unsigned-byte 8))))
             (read-sequence buf in)
             buf))
  (:writer (out val)
           (write-sequence val out)))

(defun find-frame-class (id)
  (cond
    ((and (char= (char id 0) #\T)
          (not (member id '("TXX" "TXXX") :test #'string=)))
     (ecase (length id)
       (3 'text-info-frame-v2.2)
       (4 'text-info-frame-v2.3)))
    ((string= id "COM") 'comment-frame-v2.2)
    ((string= id "COMM") 'comment-frame-v2.3)
    (t (ecase (length id)
         (3 'generic-frame-v2.2)
         (4 'generic-frame-v2.3)))))

(define-binary-type id3-frames (size frame-type)
  (:reader (in)
           (loop with to-read = size
              while (plusp to-read)
              for frame = (read-frame frame-type in)
              while frame
              do (decf to-read (+ (frame-header-size frame)
                                  (size frame)))
              collect frame
              finally (loop repeat (- size to-read)
                         do (read-byte in))))
  (:writer (out val)
           (loop with to-write = size
              for frame in val
              do (write-value 'frame out frame)
                (decf to-write (+ (frame-header-size frame)
                                  (size frame)))
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
               (concatenate 'string (string (code-char first-byte)) rest))))
  (:writer (out val)
           (write-value 'iso-8859-1-string out val)))

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding () nil)))

(define-binary-class id3-v2.2-tag (id3-tag)
  ((frames (id3-frames :size tag-size :frame-type 'id3-v2.2-frame))))

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
   (frames (id3-frames :size tag-size :frame-type 'id3-v2.3-frame))))

(defun extended-p (flag)
  (logbitp 6 flag))

(defun crc-p (flags extra)
  (and (extended-p flags) (logbitp 15 extra)))

(defgeneric frame-header-size (frame))

(defun frame-types (file)
  (delete-duplicates (mapcar #'id (frames (read-id3 file))) :test #'string=))

(defun non-terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-string)
    (1 'ucs-2-string)))

(defun terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-terminated-string)
    (1 'ucs-2-terminated-string)))

(defun string-args (encoding length terminator)
  (cond
    (length
     (values (non-terminated-type encoding) :length length))
    (terminator
     (values (terminated-type encoding) :terminator terminator))))

(define-binary-type id3-encoded-string (encoding length terminator)
  (:reader (in)
           (multiple-value-bind (type keyword arg)
               (string-args encoding length terminator)
             (read-value type in keyword arg)))
  (:writer (out val)
           (multiple-value-bind (type keyword arg)
               (string-args encoding length terminator)
             (write-value type out val keyword arg))))

(define-binary-class text-info-frame ()
  ((encoding u1)
   (information (id3-encoded-string :encoding encoding :length (bytes-left 1)))))

(defun bytes-left (bytes-read)
  (- (size (current-binary-object)) bytes-read))

(define-binary-class text-info-frame-v2.2 (id3-v2.2-frame text-info-frame) ())
(define-binary-class text-info-frame-v2.3 (id3-v2.3-frame text-info-frame) ())

(define-binary-class comment-frame ()
  ((encoding u1)
   (language (iso-8859-1-string :length 3))
   (description (id3-encoded-string :encoding encoding :terminator +null+))
   (text (id3-encoded-string
          :encoding encoding
          :length (bytes-left
                   (+ 1 3 (encoded-string-length description encoding t)))))))

(defun encoded-string-length (string encoding terminated)
  (let ((characters (+ (length string)
                       (if terminated 1 0)
                       (ecase encoding (0 0) (1 1)))))
    (* characters (ecase encoding (0 1) (1 2)))))

(define-binary-class comment-frame-v2.2 (id3-v2.2-frame comment-frame) ())
(define-binary-class comment-frame-v2.3 (id3-v2.3-frame comment-frame) ())

(defun find-frame (id3 ids)
  (find-if #'(lambda (x) (find (id x) ids :test #'string=)) (frames id3)))

(defun get-text-info (id3 &rest ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (information frame))))

(defun song   (id3) (get-text-info id3 "TIT2"))
(defun album  (id3) (get-text-info id3 "TAL" "TALB"))
(defun artist (id3) (get-text-info id3 "TP1" "TPE1"))
(defun track  (id3) (get-text-info id3 "TRK" "TRCK"))
(defun year   (id3) (get-text-info id3 "TYE" "TYER" "TDRC"))
(defun genre  (id3) (get-text-info id3 "TCO" "TCON"))

(defun all-info (id3)
  (let ((f-list '(song album artist track year genre)))
    (mapcar #'(lambda (f)
                        (format t "~a~15t~a~%" (symbol-name f)
                                (funcall (symbol-function f) id3)))
            f-list)))

(defun translated-genre (id3)
  (let ((genre (genre id3)))
    (if (and genre (char= #\( (char genre 0)))
        (translate-v1-genre genre))))

(defun translate-v1-genre (genre)
  (aref *id3-v1-genres* (parse-integer genre :start 1 :junk-allowed t)))

(defparameter *id3-v1-genres*
  #(
    ;; These are the official ID3v1 genres.
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instrumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychadelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"

    ;; These were made up by the authors of Winamp but backported into
    ;; the ID3 spec.
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"

    ;; These were also invented by the Winamp folks but ignored by the
    ;; ID3 authors.
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))
