(in-package :common-lisp-user)

(require :array-operations)
(require :local-time)
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
           :run-random-test
           :shuffle-vector
           :spec
           :spec
           :take-while
           :until
           :with-gensyms
           :with-time
           ))
