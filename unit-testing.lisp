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

(defstruct test-suite name tests-or-suites)

(defun info (name &rest testish)
  (make-test-suite :name name :tests-or-suites testish))

(defstruct simple-test name test)
(defmacro spec (doc-string &body forms)
    "Run each expression in 'forms' as a test case."
      `(make-test-suite :name ,doc-string
                     :tests-or-suites 
                     (list ,@(mapcar (lambda (f) `(cage433-lisp-utils::make-simple-test :name ',f :test (lambda () ,f))) forms))))



(defun run-simple-test (test &key indent)
  (if (not (funcall (simple-test-test test)))
      (progn
        (format t (colored-text 
          (format nil "~A~A failed~%" indent (simple-test-name test)) :red))
        nil)
      t))

(defstruct rng-test name form test seed num-runs num-allowed-failures)

(defmacro random-spec (name test &key num-runs num-allowed-failures)
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
        (format t 
                (colored-text 
                  (format nil "~A  Failed with seed(s) ~A~%" 
                          indent (reverse failing-seeds))
                  :red)
                )
        nil)
      t)))



(defun run-tests (testish &key (indent ""))
  (cond ((test-suite-p testish)
         (progn 
           (format t "~A~A~%" indent (colored-text (test-suite-name testish) :blue))
           (do ((has-failed nil)
                (remaining (test-suite-tests-or-suites testish) (cdr remaining)))
             ((or has-failed (null remaining)) (not has-failed))
             (if (not (run-tests (car remaining) :indent (concatenate 'string "  " indent)))
               (setf has-failed t)))))
        ((simple-test-p testish)
          (progn
            ;(format t "~A~A~%" indent (colored-text (simple-test-name testish) :green))
            (run-simple-test testish :indent (concatenate 'string "  " indent))))
        ((rng-test-p testish)
          (progn
            (format t "~A~A~%" indent (colored-text (rng-test-name testish) :blue))
            (run-random-test testish :indent (concatenate 'string "  " indent))))))

