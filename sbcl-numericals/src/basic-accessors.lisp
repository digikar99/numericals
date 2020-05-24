(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; d-ref | d-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defknown (%d-ref) ((simple-array double-float (*))
                      (integer 0 #.most-positive-fixnum))
      double-float
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%d-ref)
    (:translate %d-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-reg)))
    (:result-types double-float)
    (:policy :fast-safe)
    ;; EA below stands for effective address. 
    ;; To understand the role of each arg, see the source code for float-ref-ea at
    ;; https://github.com/sbcl/sbcl/blob/master/src/compiler/x86/array.lisp
    ;; and consider the (EA = base + index * scale + offset * element_size) model.
    (:generator 1
                (inst movq
                      dest
                      (float-ref-ea v i 0 0
                                    :scale (ash 8  (- n-fixnum-tag-bits))))))

  (defknown %d-set ((simple-array double-float (*))
                    (integer 0 #.most-positive-fixnum)
                    double-float)
      double-float
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%d-set)
    (:translate %d-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-reg)))
    (:arg-types simple-array-double-float
                tagged-num
                double-float)
    (:policy :fast-safe)
    (:generator 1
                (inst movq
                      (float-ref-ea v i 0 0
                                    :scale (ash 8 (- n-fixnum-tag-bits)))
                      x)))  )

(in-package :sbcl-numericals.internals)

(declaim (inline d-ref))
(defun d-ref (v i)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (*)) v)
           (type fixnum i))
  (sb-vm::%d-ref v i))

(declaim (inline (setf d-ref)))
(defun (setf d-ref) (new-value v i)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array double-float (*)) v)
           (type fixnum i)
           (type double-float new-value))
  (sb-vm::%d-set v i new-value))

(defparameter a (make-array 5 :element-type 'double-float
                            :initial-contents (loop for i below 5 collect (+ 0.1d0 i))))






