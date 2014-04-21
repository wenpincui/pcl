;;;; this file contains form for testing binary parser.

(define-binary-accessor generic-byte (length)
  (:reader (in)
   (loop with value = 0
      for i from (* 8 (1- length)) downto 0 by 8
      do (setf (ldb (byte 8 i) value) (read-byte in))
      finally (return value)))
  (:writer (out value)
   (loop
      for i from (* 8 (1- length)) downto 0 by 8
        do (write-byte (ldb (byte 8 i) value) out))))

(define-binary-accessor u2 ()
  (generic-byte :length 2))

(define-binary-accessor ascii (length)
  (:reader (in)
   (with-output-to-string (out)
     (loop repeat length
        do (write-char (code-char (read-byte in)) out))))
  (:writer (out value)
   (with-input-from-string (str value)
     (loop repeat length
        do (write-byte (char-code (read-char str)) out)))))

(define-binary-class header ()
  ((header-name (ascii :length 7))))

(define-binary-class jpeg-header (header)
  ((major-version u2)
   (minor-version u2)
   (name (ascii :length 4))))

(define-binary-class jpeg-header-son (jpeg-header)
  ((son-name (ascii :length 12))))
