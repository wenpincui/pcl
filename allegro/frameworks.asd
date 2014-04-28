(asdf:defsystem frameworks
  :version "0"
  :description "light weight web framework"
  :maintainer "wenpin cui <wenpincui@mac>"
  :author "wenpin cui <wenpincui@mac>"
  :licence "BSD-style"
  :depends-on (:html
                :macro-utilities
                :aserve)
  :serial t
  :components ((:file "package")
               (:file "framework" :depends-on ("package")))
  )
