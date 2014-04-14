;;;; pcl.asd

(asdf:defsystem #:pcl
  :serial t
  :description "pcl's examples"
  :author "wenpin cui <wenpincui224@gmail.com>"
  :license "GPLv2"
  :depends-on (:cl-ppcre)
  :components ((:file "ch23/package")
               (:file "ch23/spam" :depends-on ("ch23/package"))))

