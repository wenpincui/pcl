;;;; this file contains form for testing binary parser.

(define-binary-accessor generic-byte (length)
  (:reader
   (loop with value = 0
      for i from (* 8 (1- length)) downto 0 by 8
      do (setf (ldb (byte 8 i) value) (read-byte stream))
      finally (return value)))
  (:writer
   (loop
      for i from (* 8 (1- length)) downto 0 by 8
        do (write-byte (ldb (byte 8 i) value) stream))))

(define-binary-accessor u2 ()
  (generic-byte :length 2))

(define-binary-accessor ascii (length)
  (:reader
   (with-output-to-string (out)
     (loop repeat length
        do (write-char (code-char (read-byte stream)) out))))
  (:writer
   (with-input-from-string (str value)
     (loop repeat length
        do (write-byte (char-code (read-char str)) stream)))))

(define-binary-class header ()
  ((header-name (ascii :length 8))))

(define-binary-class jpeg-header (header)
  ((major-version u2)
   (minor-version u2)
   (name (ascii :length 4))))

(define-binary-class jpeg-header-son (jpeg-header)
  ((son-name (ascii :length 12))))
