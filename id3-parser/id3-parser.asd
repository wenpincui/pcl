(defpackage #:id3-parser-system (:use #:cl #:asdf))

(in-package #:id3-parser-system)

(asdf:defsystem id3-parser
  :version "0"
  :description "parse mp3 id3 tags"
  :maintainer "wenpin cui <wenpincui224@gmail.com>"
  :author "wenpin cui <wenpincui224@gmail.com>"
  :licence "BSD-style"
  :depends-on (:binary-data :pathnames :flexi-streams)
  :serial t
  :components ((:file packages)
               (:file id3-parser))
  )
