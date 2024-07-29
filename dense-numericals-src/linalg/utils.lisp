(in-package :dense-numericals/linalg)

(defun float-close-p (x y)
  (or (= x y)
      (progn
        ;; (print (list x y))
        (< (/ (abs (- x y))
              (+ (abs x) (abs y)))
           0.01))))

(declaim (inline eigen-array-layout))
(defun eigen-array-layout (array)
  (declare (type simple-array array))
  (ecase (array-layout array)
    (:row-major 82)
    (:column-major 67)))

(declaim (inline fixnum-dot))
(defun fixnum-dot (n x incx y incy)
  #+sbcl
  (nth-value 0 (floor (bmas:i64dot n x incx y incy) 4))
  #+ccl
  (bmas:i64dot n x incx y incy)
  #-(or sbcl ccl)
  (error "FIXNUM-DOT does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(define-polymorphic-function out-shape-compatible-p (function-name &rest args)
  :overwrite t)

(define-polymorphic-function out-shape (function-name &rest args)
  :overwrite t)
