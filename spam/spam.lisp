(in-package :org.wenpin.pcl.spam)

(defun classify (text)
  (classification (score (extract-features text))))

(defparameter *max-ham-score* .4)
(defparameter *min-spam-score* .6)

(defun classification (score)
  (values
   (cond ((>= score *min-spam-score*) 'spam)
         ((<= score *max-ham-score*) 'ham)
         (t 'uncertian))
   score))

(defclass word-feature ()
  ((word :initform (error "Must supply word")
         :initarg :word
         :accessor word
         :documentation "The word this feature stands for")
   (spam-count :initform 0
               :initarg :spam-count
               :accessor spam-count
               :documentation "number of spams we have seen this feature in")
   (ham-count :initform 0
              :initarg :ham-count
              :accessor ham-count
              :documentation "number of hams we have seen this feature in")))

(defvar *feature-database* (make-hash-table :test #'equal))

(defun clear-database ()
  (progn
    (setf *feature-database* (make-hash-table :test #'equal))
    (setf *total-hams* 0)
    (setf *total-spams* 0)))

(defun intern-feature (word)
  (or (gethash word *feature-database*)
      (setf (gethash word *feature-database*)
            (make-instance 'word-feature :word word))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[a-zA-Z]{3,}" text)
   :test #'string=))

(defun extract-features (text)
  (mapcar #'intern-feature (extract-words text)))

(defmethod print-object ((object word-feature) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (word spam-count ham-count) object
      (format stream "~s: spam-count ~d ham-count ~d" word spam-count ham-count))))

(defun train (text type)
  (dolist (feature (extract-features text))
    (increment-count feature type)
    (increment-total-count type)))

(defun increment-count (feature type)
  (ecase type
    (spam (incf (spam-count feature)))
    (ham (incf (ham-count feature)))))

(defun increment-total-count (type)
  (ecase type
    (spam (incf *total-spams*))
    (ham (incf *total-hams*))))

(defvar *total-spams* 0)
(defvar *total-hams* 0)

(defun spam-probability (feature)
  (with-slots (spam-count ham-count) feature
    (let ((spam-frequency (/ spam-count (max 1 *total-spams*)))
          (ham-frequency (/ ham-count (max 1 *total-hams*))))
      (/ spam-frequency (+ spam-frequency ham-frequency)))))

(defun bayesian-spam-probability (feature &optional (assumed-probability 1/2) (weight 1))
  (let ((basic-probability (spam-probability feature))
        (data-points (+ (spam-count feature) (ham-count feature))))
    (/ (+ (* weight assumed-probability)
          (* data-points basic-probability))
       (+ weight data-points))))

(defun score (features)
  (let (spam-probs ham-probs (number-of-probs 0))
    (dolist (feature features)
      (unless (untrained-p feature)
        (let ((spam-prob (float (bayesian-spam-probability feature) 0.0d0)))
          (push spam-prob spam-probs)
          (push (- 1.0 spam-prob) ham-probs)
          (incf number-of-probs))))
    (let ((h (- 1 (fisher spam-probs number-of-probs)))
          (s (- 1 (fisher ham-probs number-of-probs))))
      (/ (+ (- 1 h) s) 2.0d0))))

(defun untrained-p (feature)
  (with-slots (spam-count ham-count) feature
    (and (zerop spam-count) (zerop ham-count))))

(defun fisher (probs number-of-probs)
  "The Fisher computation described by Robinson"
  (inverse-chi-square
   (* -1 (reduce #'+ probs :key #'log))
   (* 2 number-of-probs)))

(defun inverse-chi-square (value degrees-of-freedom)
  (assert (evenp degrees-of-freedom))
  (min
   (loop with m = (/ value 2)
        for i below (/ degrees-of-freedom)
        for prob = (exp (- m)) then (* prob (/ m i))
        summing prob)
   1.0))

(defun add-file-to-corpus (filename type corpus)
  (vector-push-extend (list filename type) corpus))

(defparameter *corpus* (make-array 1000 :adjustable t :fill-pointer 0))

(defun add-directory-to-corpus (dir type corpus)
  (dolist (filename (directory dir))
    (add-file-to-corpus filename type corpus)))
