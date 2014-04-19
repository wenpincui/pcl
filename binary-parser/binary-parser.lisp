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

(defun slot->read-value (slot object)
  (destructuring-bind (slot-name type-and-arg) slot
    (let* ((type-and-arg-list (mklist type-and-arg))
           (type (car type-and-arg-list))
           (args (cdr type-and-arg-list)))
      `(setf (,slot-name ,object) (read-value ',type stream ,@args)))))

(defun slot->write-value (slot object)
  (destructuring-bind (slot-name type-and-arg) slot
    (let* ((type-and-arg-list (mklist type-and-arg))
           (type (car type-and-arg-list))
           (args (cdr type-and-arg-list)))
      `(write-value ',type (,slot-name ,object) stream ,@args))))

(defgeneric read-value (type stream &key)
  (:documentation "read value for type object from stream"))

(defgeneric write-value (type value stream &key)
  (:documentation "write value to stream for type object"))

(defun has-parent-p (list)
  (if (null list)
      nil
      t))

(defmacro define-binary-class (name (&rest super-class) slot)
  (with-gensyms (all-slot)
    (setf all-slot (append (and super-class (get (car super-class) :slots)) slot))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (get ',name :slots) ',slot)
         (when ',super-class
           (setf (get ',name :parent) ',super-class)))

       (defclass ,name ,super-class
         ,(mapcar #'slot->class-slot slot))

       (defmethod read-value ((type (eql ',name)) stream &key)
         (let ((object (make-instance ',name)))
           (progn
             ,@(mapcar #'(lambda (x) (slot->read-value x 'object)) all-slot)
             object)))

       (defmethod write-value ((type (eql ',name)) stream value &key)
         (progn ,@(mapcar #'(lambda (x) (slot->write-value x 'value)) slot))))))

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
;; but ... how to expand the symbol doesn't exist in macro form??

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

;; TODO: add inherition support.
