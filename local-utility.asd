(asdf:defsystem #:local-utility
  :name "Local Utility Metapackage"
  :author "Soós Péter Levente"
  :license "MIT / various"
  :description "Metapackage for locally defined functions and macros."
  :components ((:file "package")
               (:file "general" :pathname "luf/local-utility-function-general"
                      :depends-on ("package"))
               (:file "strings" :pathname "luf/local-utility-function-strings"
                      :depends-on ("package"))
               (:file "lum/local-utility-macro"
                      :depends-on ("general"))))
