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

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun to-keyword (sym)
  (intern (string sym) :keyword))

(defun slot->class-slot (slot)
  (let ((name (car slot)))
    `(,name :initarg ,(to-keyword name) :accessor ,name)))

(defun mklist (arg)
  (if (listp arg)
      arg
      (list arg)))

(defun normalized-slot (slot)
  (cons (car slot) (mklist (cadr slot))))

(defun slot->read-object (slot object stream)
  (destructuring-bind (slot-name type &rest args) (normalized-slot slot)
    `(setf (,slot-name ,object) (read-value ',type ,stream ,@args))))

(defun slot->write-object (slot object stream)
  (destructuring-bind (slot-name type &rest args) (normalized-slot slot)
    `(write-value ',type (,slot-name ,object) ,stream ,@args)))

(defgeneric read-value (type stream &key)
  (:documentation "read value for type object from stream"))

(defgeneric write-value (type value stream &key)
  (:documentation "write value to stream for type object"))

(defgeneric read-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "fill the object from stream"))

(defgeneric write-object (object stream)
  (:method-combination progn :most-specific-last)
  (:documentation "fill the stream with the slots of the object"))

(defmethod read-value ((type symbol) stream &key)
  (let ((object (make-instance type)))
    (read-object object stream)
    object))

(defmethod write-value ((type symbol) value stream &key)
  (assert (typep value type))
  (write-object value stream))

(defun current-slots (name)
  (get name :slots))

(defun find-all-slots (parent)
  (if (null parent)
      nil
      (append (current-slots parent)
              (find-all-slots (car (get parent :parent))))))

(defun find-inherited-slot (super-class)
  (if super-class
      (append (get (car super-class) :slots)
              (find-inherited-slot (get (car super-class) :parent)))
      nil))

(defmacro define-binary-class (name (&rest super-class) slot)
  (with-gensyms (objectvar streamvar)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name :slots) ',(mapcar #'car slot))
         (setf (get ',name :parent) ',super-class))

       (defclass ,name ,super-class
         ,(mapcar #'slot->class-slot slot))

       (defmethod read-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slot) ,objectvar
                    (progn
                      ,@(mapcar #'(lambda (x) (slot->read-object x objectvar streamvar)) slot))))

       (defmethod write-object progn ((,objectvar ,name) ,streamvar)
                  (with-slots ,(mapcar #'first slot) ,objectvar
                    (progn
                      ,@(mapcar #'(lambda (x) (slot->write-object x objectvar streamvar)) slot)))))))

;; binary class accessor
;;(define-binary-accessor ascii (length)
;;  (:reader (logic...))
;;  (:writer (logic...)))
;;
;; what we want expand to
;;(defmethod read-value ((type (eql 'ascii)) stream &key length)
;;  (logic...))
;;
;; another form we allowed
;; (define-binary-accessor type2 (length)
;;   (ascii length)))
;;
;; which expands to
;; (progn
;; (defmethod read-value ((type (eql 'type2)) stream &key length)
;;   (read-value 'ascii &key length))
;; (defmethod write-value ((type (eql 'type2)) value stream &key length)
;;   (write-value 'ascii &key length)))
;;

;;; TODO: gensym, and lexcical binding for user defined body.
(defmacro define-binary-accessor (type args &body body)
  (ecase (length body)
    (1 `(progn (defmethod read-value ((type (eql ',type)) stream &key ,@args)
                 (read-value ',(caar body) stream ,@(rest (car body))))
               (defmethod write-value ((type (eql ',type)) value stream &key ,@args)
                 (write-value ',(caar body) value stream ,@(rest (car body))))))
    (2 `(progn (defmethod read-value ((type (eql ',type)) stream &key ,@args)
                 ,@(rest (assoc :reader body)))
               (defmethod write-value ((type (eql ',type)) value stream &key ,@args)
                 ,@(rest (assoc :writer body)))))))
