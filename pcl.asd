;;;; pcl.asd

(defpackage #:org.wenpin.pcl
  (:use #:cl :asdf))
(in-package :org.wenpin.pcl)

(defsystem #:pcl
  :serial t
  :description "pcl's examples"
  :author "wenpin cui <wenpincui224@gmail.com>"
  :license "GPLv2"
  :depends-on (:cl-ppcre)
  :components ((:file "condition/package")
               (:file "condition/cond-restart" :depends-on ("condition/package"))
               (:file "spam/package")
               (:file "spam/spam" :depends-on ("spam/package"))))

