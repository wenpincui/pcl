;;;; my clone of binary parser (chapter 24)

;; This is what user use this macro
;;(define-binary-class jpeg-header ()
;;  ((major-version u2)
;;   (minor-version u2)
;;   (name (ascii :length 4))))
;;
;; This is what I want to expand
;;(defclass jpeg-header ()
;;  ((major-version :initarg :major-version :accessor major-version)
;;   (minor-version :initarg :minor-version :accessor minor-version)
;;   (name :initarg :name :accessor name)))

(defun to-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->class-slot (slot)
  (let ((name (car slot)))
    `(,name :initarg ,(to-keyword name) :accessor ,name)))

(defun mklist (arg)
  (if (listp arg)
      arg
      (list arg)))

(defun slot->read-value (slot)
  (destructuring-bind (slot-name type-and-arg) slot
    (let* ((type-and-arg-list (mklist type-and-arg))
           (type (car type-and-arg-list))
           (args (cdr type-and-arg-list)))
      `(setf (,slot-name object) (read-value ',type stream ,@args)))))

(defun slot->write-value (slot)
  (destructuring-bind (slot-name type-and-arg) slot
    (let* ((type-and-arg-list (mklist type-and-arg))
           (type (car type-and-arg-list))
           (args (cdr type-and-arg-list)))
      `(write-value ',type (,slot-name object) stream ,@args))))

(defgeneric read-value (type stream &key)
  (:documentation "read value for type object from stream"))

(defgeneric write-value (type value stream &key)
  (:documentation "write value to stream for type object"))

(defmacro define-binary-class (name (&rest super-class) slot)
  `(progn
     (defclass ,name ,super-class
       ,(mapcar #'slot->class-slot slot))

     (defmethod read-value ((type (eql ',name)) stream &key)
       (let ((object (make-instance ',name)))
         (progn ,@(mapcar #'slot->read-value slot))))

     (defmethod write-value ((type (eql ',name)) stream value &key)
       (progn ,@(mapcar #'slot->write-value slot)))))


;;; below are stub method for debug.
(defmethod read-value ((type (eql 'u2)) stream &key)
  (format t "u2 read value ~%"))

(defmethod write-value ((type (eql 'u2)) value stream &key)
  (format t "u2 write value ~%"))

(defmethod read-value ((type (eql 'ascii)) stream &key length)
  (format t "ascii read value ~a~%" length))

(defmethod write-value ((type (eql 'ascii)) value stream &key length)
  (format t "ascii write value ~a~%" length))
