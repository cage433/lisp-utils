(in-package :common-lisp-user)

(require :random-state)
(defpackage :cage433-lisp-utils
  (:use :common-lisp)
  (:export :===
           :acond
           :aif
           :awhen
           :cross-product
           :dbind
           :def-rstruct
           :foldl
           :foldr
           :for-all
           :group-by
           :hash-to-list
           :hash-values
           :info
           :it
           :mappend
           :mbind
           :spec
           :take-while
           :until
           :with-gensyms
           :spec
           :random-spec
           :run-tests
           ))
