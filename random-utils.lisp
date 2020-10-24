(in-package :cage433-lisp-utils)

(defun random-choice (rng things)
    (let ((n (random-state:random-int rng 0 (1- (length things)))))
          (if (vectorp things)
                      (aref things n)
                      (nth n things))))


(defun shuffle-vector (rng vec)
  (let ((n (length vec)))
    (dotimes (i n)
      (let ((index (random-state:random-int rng i (1- n))))
        (unless (= i index)
          (rotatef (svref vec i) (svref vec index)))))
    vec))
