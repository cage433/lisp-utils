(load "package.lisp")
(require :cage433-ci)
(in-package :cage433-lisp-utils)

(proclaim '(optimize (debug 3)))
(defun load-and-compile-source()
  (and
    (cage433-ci:load-and-compile-if-necessary "package")
    (cage433-ci:load-and-compile-if-necessary "utils0")
    (cage433-ci:load-and-compile-if-necessary "unit-testing")
    (cage433-ci:load-and-compile-if-necessary "utils")
    (cage433-ci:load-and-compile-if-necessary "random-utils")
    (cage433-ci:load-and-compile-if-necessary "tests/utils-tests")
    (cage433-ci:load-and-compile-if-necessary "tests/random-utils-tests")
    (cage433-ci:load-and-compile-if-necessary "readers")
    (cage433-ci:load-and-compile-if-necessary "tests/readers-tests")
    (cage433-ci:load-and-compile-if-necessary "anaphors")
    ))

(defun compile-and-run-tests()
  (declare #+sbcl(sb-ext:muffle-conditions style-warning))
  (if (load-and-compile-source)
        
    (run-tests
      (info "project tests"
            (utils-full-suite)
            (random-utils-suite)
            (readers-full-suite)))))


(in-package :common-lisp-user)
(defun ci()
  (cage433-ci:run-ci-function #'cage433-lisp-utils::compile-and-run-tests)
  )
