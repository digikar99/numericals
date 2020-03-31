(in-package :sbcl-numericals.internals)

;; Storage classes: https://github.com/sbcl/sbcl/blob/master/src/compiler/x86-64/vm.lisp
;; Primitive Types: https://github.com/sbcl/sbcl/blob/3188c07e865ec5774e98ed1ccf49f91f0c8ef855/src/compiler/generic/primtype.lisp

;; It's not clear whether vdpps would be faster; it likely requires another addition instruction:
;; https://stackoverflow.com/a/36798752/8957330

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown (%s4dot)
      ((simd-pack single-float) (simd-pack single-float))
      (simd-pack single-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%s4dot)
    (:translate %s4dot)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::single-sse-reg))
           (y :scs (sb-vm::single-sse-reg)))
    (:arg-types sb-vm::simd-pack-single
                sb-vm::simd-pack-single)
    (:results (r :scs (sb-vm::single-sse-reg)))
    (:result-types sb-vm::simd-pack-single)
    (:generator 1 
                (sb-vm::inst dpps x y 255)
                (move r x))))

(declaim (inline s4dot))
(defun s4dot (x y)
  (declare (optimize (speed 3)))
  (nth-value 0 (%simd-pack-singles (%s4dot x y))))

(defun sbcl-numericals:sdot (x y)
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) x y))
  (if (not (equalp (array-dimensions x)
                   (array-dimensions y)))
      (error "Arrays cannot have different dimensions!"))
  (let ((x (array-storage-vector x))
        (y (array-storage-vector y))
        (sum 0.0))
    (declare (type single-float sum))
    (loop for loop-var fixnum
       below (- (length x) 4)
       by 4
       do (setq sum (the single-float
                         (+ sum (s4dot (s4-ref x loop-var)
                                       (s4-ref y loop-var)))))
       finally
         (return (+ sum
                    (loop for loop-final-var from loop-var below (length x)
                       summing (* (aref x loop-final-var)
                                  (aref y loop-final-var))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown (%d2dot)
      ((simd-pack double-float) (simd-pack double-float))
      (simd-pack double-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%d2dot)
    (:translate %d2dot)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::double-sse-reg))
           (y :scs (sb-vm::double-sse-reg)))
    (:arg-types sb-vm::simd-pack-double
                sb-vm::simd-pack-double)
    (:results (r :scs (sb-vm::double-sse-reg)))
    (:result-types sb-vm::simd-pack-double)
    (:generator 1 
                (sb-vm::inst dppd x y 255)
                (move r x))))

(declaim (inline d2dot))
(defun d2dot (x y)
  (declare (optimize (speed 3)))
  (nth-value 0 (%simd-pack-doubles (%d2dot x y))))

(defun sbcl-numericals:ddot (x y)
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) x y))
  (if (not (equalp (array-dimensions x)
                   (array-dimensions y)))
      (error "Arrays cannot have different dimensions!"))
  (let ((x (array-storage-vector x))
        (y (array-storage-vector y))
        (sum 0.0d0))
    (declare (type double-float sum))
    (loop for loop-var fixnum
       below (- (length x) 2)
       by 2
       do (setq sum (the double-float
                         (+ sum (d2dot (d2-ref x loop-var)
                                       (d2-ref y loop-var)))))
       finally
         (return (+ sum
                    (loop for loop-final-var from loop-var below (length x)
                       summing (* (aref x loop-final-var)
                                  (aref y loop-final-var))))))))


(defparameter a (%make-simd-pack-double 2.0d0 3.0d0))
(defparameter b (%make-simd-pack-double 5.0d0 4.0d0))
