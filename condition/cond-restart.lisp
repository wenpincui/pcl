(in-package :org.wenpin.pcl.cond-restart)

(define-condition malformed-log-entry-error (error)
  ((test :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (evenp text)
      (list text "even")
      (error 'malformed-log-entry-error :text text)))

(defun parse-log-file-cond ()
  (loop for i from 1 to 10
     for entry = (handler-case
                     (parse-log-entry i)
                   (malformed-log-entry-error (c)
                     (format t "caught error ~s~%" (text c))))
     when entry collect it))

(defun parse-log-file ()
  (loop for i from 1 to 10
     for entry = (restart-case
                     (parse-log-entry i)
                   (skip-log-entry () "skip")
                   (use-value (v) (list v "odd"))
                   (reparse-entry (fixed-text) (parse-log-entry fixed-text)))
     when entry collect it))

(defun analyze-log ()
  (print (parse-log-file)))

(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart
      (invoke-restart restart))))

(defun use-value-p (c)
  (let ((restart (find-restart 'use-value)))
    (when restart
      (use-value (text c)))))

(defun reparse-log-entry (c)
  (let ((reparse (find-restart 'reparse-entry)))
    (when reparse
      (invoke-restart reparse (+ (text c) 11)))))

(defun log-analyzer-skip ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (analyze-log)))

(defun log-analyzer-use ()
  (handler-bind ((malformed-log-entry-error #'use-value-p))
    (analyze-log)))

(defun log-analyzer-reparse ()
  (handler-bind ((malformed-log-entry-error #'reparse-log-entry))
    (analyze-log)))
