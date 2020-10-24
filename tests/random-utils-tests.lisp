(in-package :cage433-lisp-utils)

(defun random-utils-suite ()
  (info "shuffle"
        (random-spec "has same-elements"
                     (lambda (rng) 
                       (let ((vec (aops:generate* 'integer #'identity 20 :position)))
                         (shuffle-vector rng vec)
                         (equal 190 (apply #'+ (coerce vec 'list))))
                       ) 
                     :num-runs 10 
                     )))

