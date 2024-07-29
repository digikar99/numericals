(in-package :dense-numericals/basic-math/impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +fixnum-shift-pointer+ (progn
                                   (when (boundp '+fixnum-shift-pointer+)
                                     ;; Should we be doing this?
                                     (cffi:foreign-free +fixnum-shift-pointer+))
                                   (cffi:foreign-alloc :long :initial-element 1))))

#-(or ccl sbcl)
(warn "FIXNUM operations are untested on non-SBCL/CCL platforms")

(declaim (inline fixnum-mul))
(defun fixnum-mul (n x incx y incy out inc-out)
  (bmas:i64mul n x incx y incy out inc-out)
  #+sbcl
  (bmas:i64sra n out inc-out +fixnum-shift-pointer+ 0 out inc-out))

(declaim (inline fixnum-sum))
(defun fixnum-sum (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64sum n x incx) 2))
  #+ccl
  (bmas:i64sum n x incx)
  #-(or ccl sbcl)
  (error "FIXNUM-SUM does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(declaim (inline fixnum-hmax))
(defun fixnum-hmax (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64hmax n x incx) 2))
  #+ccl
  (bmas:i64hmax n x incx)
  #-(or ccl sbcl)
  (error "FIXNUM-HMAX does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(declaim (inline fixnum-hmin))
(defun fixnum-hmin (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64hmin n x incx) 2))
  #+ccl
  (bmas:i64hmin n x incx)
  #-(or sbcl ccl)
  (error "FIXNUM-HMIN does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(declaim (inline fixnum-dot))
(defun fixnum-dot (n x incx y incy)
  #+sbcl
  (nth-value 0 (floor (bmas:i64dot n x incx y incy) 4))
  #+ccl
  (bmas:i64dot n x incx y incy)
  #-(or sbcl ccl)
  (error "FIXNUM-DOT does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))
