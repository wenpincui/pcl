;;;; pcl.asd

(asdf:defsystem #:pcl
  :serial t
  :description "pcl's examples"
  :author "wenpin cui <wenpincui224@gmail.com>"
  :license "GPLv2"
  :depends-on (:cl-ppcre)
  :components ((:file "package")
               (:file "pcl")))

