(in-package :common-lisp-user)

(defpackage :cage433-lisp-utils
  (:use :common-lisp)
  (:export :deftest
           :combine-results
           :===
           :dbind
           :mbind
           :foldr
           :check
           :with-gensyms
           :for-all
           :take-while
           :def-rstruct
           :info
           :spec
           :aif
           :awhen
           :until
           :it
           :group-by
           :hash-to-list
           :hash-values
           :cross-product
           :mappend
           ))
