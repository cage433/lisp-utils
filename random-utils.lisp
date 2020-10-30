(in-package :cage433-lisp-utils)

(defun random-choice (random-state things)
    (let ((n (random (1- (length things)) random-state)))
          (if (vectorp things)
                      (aref things n)
                      (nth n things))))


(defun shuffle-vector (random-state vec)
  (let ((n (length vec)))
    (dotimes (i n)
      (let ((index 
              (+ i (random (- n i) random-state))
              ))
        (unless (= i index)
          (rotatef (svref vec i) (svref vec index)))))
    vec))
