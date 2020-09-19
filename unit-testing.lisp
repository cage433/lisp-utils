(in-package :cage433-lisp-utils)

(defvar *test-name* nil)

(defun === (expected actual &key (test #'eql))
  (if (funcall test expected actual)
    t
    (progn
      (format t "Expected ~a but got ~a~%" expected actual))))

(defun ascii-color (color)
  (ecase color
    (:red 31)
    (:green 32)
    (:blue 34)))

(defun colored-text (text color &key bold)
  (format nil "~c[~a~:[~;;1~]m~a~c[0m"
          #\Esc
          (ascii-color color)
          bold
          text #\Esc))

(defun run-test (doc-string test-and-forms)
  (if (null test-and-forms)
    (progn 
      (format t "  ~A~%" (colored-text doc-string :green))
      t)
    (destructuring-bind (test form) (car test-and-forms)
      (if test
        (run-test doc-string (cdr test-and-forms))
        (progn 
          (format t "  ~A~%" (colored-text doc-string :red))
          (format t "    Failing form~%     ~A~%" form)
          nil)))))

(defmacro spec (doc-string &body forms)
  "Run each expression in 'forms' as a test case."
  `(run-test ,doc-string 
    (list ,@(mapcar (lambda (f) `(list ,f ',f)) forms))))

(defmacro info (feature &rest specs)
  `(progn 
    (format t "~A~%" (colored-text ,feature :blue))
    (and ,@specs)))
;(defun info (feature &rest specs)
  ;(progn 
    ;(format t "~A~%" (colored-text feature :blue))
    ;(and specs)))

(defstruct simple-test name form test)
(defmacro make-test (name test)
  `(make-simple-test :name ,name :form ',test :test (lambda () ,test)))

(defun run-simple-test (test &key indent)
  (if (not (funcall (simple-test-test test)))
      (progn
        (format t "~A~A failed~%" indent (simple-test-name test))
        (format t "~AForm is ~A~%" indent (simple-test-form test))
        nil)
      t))

(defstruct rng-test name form test seed num-runs num-allowed-failures)
(defmacro make-random-test (name test &key num-runs num-allowed-failures)
  `(make-rng-test :name ,name :form ',test :test ,test :num-runs ,num-runs :num-allowed-failures ,num-allowed-failures))

(defun run-random-test (test &key seed (indent ""))
  (let* ((num-runs (if seed 1 (or (rng-test-num-runs test) 1)))
         (num-allowed-failures (if seed 0 (or (rng-test-num-allowed-failures test) 0)))
         (failing-seeds '()))
    (do ((i 0 (incf i)))
        ((or (>= i num-runs) (> (length failing-seeds) num-allowed-failures)))
        (let* ((seed (or seed (random-state:random-int (random-state:make-generator :mersenne-twister-64) 0 10000000)))
              (rng (random-state:make-generator :mersenne-twister-64 seed))
              (test-result (funcall (rng-test-test test) rng)))
          (if (not test-result)
            (setf failing-seeds (cons seed failing-seeds)))
          ))
    (if (> (length failing-seeds) num-allowed-failures)
      (progn
        (format t "~A~A failed~%" indent (rng-test-name test))
        (format t "~AForm is ~A~%" indent (rng-test-form test))
        (format t "~AFailing seed(s) ~A~%" indent (reverse failing-seeds))
        nil)
      t)))

(defstruct test-suite name tests-or-suites)
(defun make-suite (name &rest testish)
  (make-test-suite name testish))

(defun run-tests (testish &key (indent ""))
  (cond ((test-suite-p testish)
         (progn 
           (format t "~A~A~%" indent (test-suite-name testish))
           (do ((has-failed nil)
                (remaining (test-suite-tests-or-suites testish) (cdr remaining)))
             ((or has-failed (null remaining)))
             (if (not (run-tests (car remaining) :indent (concatenate 'string "  " indent)))
               (setf has-failed t)))))
        ((simple-test-p testish)
        (progn
          (format t "~A~A~%" indent (simple-test-name testish))
          (run-simple-test testish :indent (concatenate 'string "  " indent))))
        ((rng-test-p testish)
        (progn
          (format t "~A~A~%" indent (rng-test-name testish))
          (run-random-test testish :indent (concatenate 'string "  " indent))))))

