(in-package :cage433-lisp-utils)

(defvar *cross-product-suite*
  (info "A cross product"
    (spec "Should work on non-empty sets"
      (=== nil (cross-product))
      (=== '((x)) (cross-product '(x)) :test #'equalp)
      (=== '((x) (y)) (cross-product '(x y)) :test #'equalp)
      (=== '( (1 x) )
           (cross-product '(1) '(x))
           :test #'equalp)
      (=== '( (a b c))
           (cross-product '(a) '(b) '(c))
           :test #'equalp)
      (=== '( (1 b) (2 b))
           (cross-product '(1 2) '(b) )
           :test #'equalp)
      (=== '( (1 b c) (2 b c) )
           (cross-product '(1 2) '(b) '(c) )
           :test #'equalp)
      (=== '( (1 b) (2 b) (1 c) (2 c))
           (cross-product '(1 2) '(b c) )
           :test #'equalp)
      )))
     
(defvar *take-while-suite*
  (info "take-while"
    (spec "Returns nil when passed an empty list"
      (equalp nil (take-while #'evenp nil)))
    (spec "Returns nil when predicate failes on the first element"
      (equalp nil (take-while #'evenp '(1 2 4 6 8))))
    (spec "Returns whole list if predicate never fails"
      (equalp '(1 2 4 6 8) (take-while #'numberp '(1 2 4 6 8))))
    (spec "Returns a proper sublist at the point predicate fails"
      (=== '(1 2 3) (take-while (lambda (i) (< i 4)) '(1 2 3 4 0)) 
              :test #'equal))))

(defvar *span-suite*
  (info "Span function" 
                    (spec "Returns nested nil when passed nil as an argument" 
                              (equalp '(()) (span #'evenp nil)))
                    (spec "Works when predicate fails on first element"
                              (equalp '(() 1 2 3) (span #'evenp '(1 2 3))))
                    )
          )

(defun range-test-suite ()
  (info "Range"
        (spec "from 1 until 3"
              (equalp '(1 2) (range 1 3)))
        (spec "from 1 until 1"
              (equalp nil (range 1 1)))))

(defun utils-full-suite ()
  (info "utils tests"
        *cross-product-suite*
        *take-while-suite*
        *span-suite*
        (range-test-suite)))
