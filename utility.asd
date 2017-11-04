(asdf:defsystem #:utility
  :name "Utility package"
  :author "Soós Péter Levente"
  :license "BSD-style"
  :description "Utility functions and macros."
  :components ((:file "package")
               (:file "general" :pathname "utility"
                      :depends-on ("package"))))
