(asdf:defsystem :cage433-lisp-utils
  :description "Some lisp utilities"
  :version "0.1"
  :author "Alex McGuire"
  :components ( (:file "package")
                (:file "utils0" :depends-on ("package"))
                (:file "unit-testing" :depends-on ("utils0"))
                (:file "utils" :depends-on ("unit-testing"))
                (:file "tests/utils-tests" :depends-on ("utils"))
                (:file "readers" :depends-on ("package"))
                (:file "tests/readers-tests" :depends-on ("readers" "unit-testing"))
                (:file "anaphors" :depends-on ("package"))
                ))
