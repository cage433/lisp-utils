(in-package :common-lisp-user)

(require :random-state)
(require :array-operations)
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
           :random-spec
           :range
           :run-tests
           :shuffle-vector
           :spec
           :spec
           :take-while
           :until
           :with-gensyms
           ))
