(asdf:defsystem foo
  :version "0"
  :maintainer "wenpin cui <wenpincui224@gmail.com>"
  :author "wenpin cui <wenpincui224@gmail.com>"
  :licence "BSD-style"
  :serial t
  :components ((:file package)
               (:file foo :depends-on (package)))
  )
