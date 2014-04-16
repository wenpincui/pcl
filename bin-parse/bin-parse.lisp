;;;; binary parse library, ch24
(in-package org.wenpin.pcl.bin-parse)

(defmacro with-gensyms (name &body body)
  `(let ,(loop for i in name collect `(,i (gensym)))
     ,@body))

(defconstant +null+ (code-char 0))

(defun read-null-terminated-ascii (s)
  (with-output-to-string (c)
    (loop for i = (code-char (read-byte s nil 0))
       until (char= +null+ i) do (write-char i c))))

(defun write-null-terminated-ascii (str s)
  (loop for i across str do (write-byte (char-code i) s))
  (write-byte 0 s))

(defun as-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->defclass-slot (spec)
  (let ((name (car spec)))
    `(,name :initarg ,(as-keyword name)
            :accessor ,name)))

(defmacro define-binary-class (name (&rest super-class) slots)
  (with-gensyms (typevar objectvar streamvar)
    `(progn
       (defclass ,name ,super-class
         ,(mapcar #'slot->defclass-slot slots))
       (defmethod read-object ((,typevar (eql ',name)) ,streamvar &key)
         (let ((,objectvar (make-instance ',name)))
           (with-slots ,(mapcar #'first slots) ,objectvar
             ,@(mapcar #'(lambda (x) (slot->read-value x streamvar)) slots))
           ,objectvar))
       (defmethod write-object ((,typevar (eql ',name)) ,streamvar ,objectvar &key)
         (with-slots ,(mapcar #'first slots) ,objectvar
           ,@(mapcar #'(lambda (x) (slot->write-value x streamvar)) slots))))))

(defgeneric read-value (type stream &key)
  (:documentation "read a value of the give type stream."))

(defmethod read-value ((type (eql 'iso-8859-1-string)) stream &key length)
  (with-output-to-string (s)
    (loop for i to (or length 1)
         do (write-char (code-char (read-byte stream)) s))))

(defmethod read-value ((type (eql 'u1)) stream &key)
  (read-byte stream))

(defun slot->read-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(setf ,name (read-value ',type ,stream ,@args))))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun mklist (x)
  (if (listp x)
      x
      (list x)))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defgeneric write-value (type stream value &key)
  (:documentation "Write a value as the give type to the stream"))

(defmethod write-value ((type (eql 'iso-8859-1-string)) stream value &key length)
  nil)

(defmethod write-value ((type (eql 'u1)) stream value &key)
  nil)

(defmethod write-value ((type (eql 'u2)) stream value &key)
  nil)

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last &key)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream value &key)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object from value."))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object steam)
    object))

(defmethod write-value ((type symbol) stream &key)
  (assert (typep value type))
  (write-object value stream))
