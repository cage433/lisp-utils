(asdf:defsystem :cage433-lisp-utils
  :description "Some lisp utilities"
  :version "0.1"
  :author "Alex McGuire"
  :components ( (:file "package")
                (:file "anaphors" :depends-on ("package")))
                )
