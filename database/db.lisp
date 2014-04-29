(defclass table ()
  ((rows   :accessor rows   :initarg :rows :initform (make-rows))
   (schema :accessor schema :initarg :schema)))

(defparameter *default-table-size* 100)

(defun make-rows (&optional (size *default-table-size*))
  (make-array size :adjustable t :fill-pointer 0))

(defclass column ()
  ((name
    :reader name
    :initarg :name)
   (equality-predicate
    :reader equality-predicate
    :initarg :equality-predicate)
   (comparator
    :reader comparator
    :initarg :comparator)
   (default-value
    :reader default-value
     :initarg :default-value
    :initform nil)
   (value-normalizer
    :reader value-normalizer
    :initarg :value-normalizer
    :initform #'(lambda (v column)
                  (declare (ignore column))))))

(defgeneric make-column (name type &optional default-value))

(defmethod make-column (name (type (eql 'string)) &optional default-value)
  (make-instance 'column
                 :name name
                 :comparator #'string<
                 :equality-predicate #'string=
                 :default-value default-value
                 :value-normalizer #'not-nullable))

(defmethod make-column (name (type (eql 'number)) &optional default-value)
  (make-instance 'column
                 :name name
                 :comparator #'<
                 :equality-predicate #'=
                 :default-value default-value))

(defun not-nullable (value column)
  (or value (error "Column ~a can't be null" (name column))))

(defclass interned-values-column (column)
  ((interned-values
    :reader interned-values
    :initform (make-hash-table :test #'equal))
   (equality-predicate :initform #'eql)
   (value-normalizer :initform #'intern-for-column)))

(defun intern-for-column (value column)
  (let ((hash (interned-values column)))
    (or (gethash (not-nullable value column) hash)
        (setf (gethash value hash) value))))

(defmethod make-column
    (name (type (eql 'interned-string)) &optional default-value)
  (make-instance 'interned-values-column
                 :name name
                 :comparator #'string<
                 :default-value default-value))

(defun make-schema (spec)
  (mapcar #'(lambda (column-spec) (apply #'make-column column-spec)) spec))

(defparameter *mp3-schema*
  (make-schema
   '((:file string)
     (:genre interned-string "Unknown")
     (:artist interned-string "Unknown")
     (:album interned-string "Unknown")
     (:song string)
     (:track number 0)
     (:year number 0)
     (:id3-size number))))

(defparameter *mp3s* (make-instance 'table :schema *mp3-schema*))
