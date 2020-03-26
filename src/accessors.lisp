(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel :execute)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; d2-ref | d2-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defknown (d2-ref) ((simple-array double-float (*))
                      (integer 0 #.most-positive-fixnum))
      (simd-pack double-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d2-ref)
    (:translate d2-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-sse-reg)))
    (:result-types simd-pack-double)
    (:policy :fast-safe)
    ;; EA below stands for effective address. 
    ;; To understand the role of each arg, see the source code for float-ref-ea at
    ;; https://github.com/sbcl/sbcl/blob/master/src/compiler/x86/array.lisp
    ;; and consider the (EA = base + index * scale + offset * element_size) model.
    (:generator 1
                (inst movups
                      dest
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 8 (- n-fixnum-tag-bits))))))

  (defknown d2-set ((simple-array double-float (*))
                    (integer 0 #.most-positive-fixnum)
                    (simd-pack double-float))
      (simd-pack double-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d2-set)
    (:translate d2-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-sse-reg)))
    (:arg-types simple-array-double-float
                tagged-num
                simd-pack-double)
    (:policy :fast-safe)
    (:generator 1
                (inst movups
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 8 (- n-fixnum-tag-bits)))
                      x)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; s4-ref | s4-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defknown (s4-ref) ((simple-array single-float (*))
                      (integer 0 #.most-positive-fixnum))
      (simd-pack single-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s4-ref)
    (:translate s4-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-single-float
                tagged-num)
    (:results (dest :scs (single-sse-reg)))
    (:result-types simd-pack-single)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      dest
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 4 (- n-fixnum-tag-bits))))))
  
  (defknown s4-set ((simple-array single-float (*))
                    (integer 0 #.most-positive-fixnum)
                    (simd-pack single-float))
      (simd-pack single-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s4-set)
    (:translate s4-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (single-sse-reg)))
    (:arg-types simple-array-single-float
                tagged-num
                simd-pack-single)
    (:results (result :scs (single-sse-reg)))
    (:result-types simd-pack-single)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 4 (- n-fixnum-tag-bits)))
                      x)
                (move result x)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; d4-ref | d4-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defknown (d4-ref) ((simple-array double-float (*))
                      (integer 0 #.most-positive-fixnum))
      (simd-pack-256 double-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d4-ref)
    (:translate d4-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-avx2-reg)))
    (:result-types simd-pack-256-double)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      dest
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 8 (- n-fixnum-tag-bits))))))

  (defknown d4-set ((simple-array double-float (*))
                    (integer 0 #.most-positive-fixnum)
                    (simd-pack-256 double-float))
      (simd-pack-256 double-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d4-set)
    (:translate d4-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-avx2-reg)))
    (:arg-types simple-array-double-float
                tagged-num
                simd-pack-256-double)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 8 (- n-fixnum-tag-bits)))
                      x)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;; s8-ref | s8-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defknown (s8-ref) ((simple-array single-float (*))
                      (integer 0 #.most-positive-fixnum))
      (simd-pack-256 single-float)
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s8-ref)
    (:translate s8-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-single-float
                tagged-num)
    (:results (dest :scs (single-avx2-reg)))
    (:result-types simd-pack-256-single)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      dest
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 4 (- n-fixnum-tag-bits))))))
  (defknown s8-set ((simple-array single-float (*))
                    (integer 0 #.most-positive-fixnum)
                    (simd-pack-256 single-float))
      (simd-pack-256 single-float)
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s8-set)
    (:translate s8-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (single-avx2-reg)))
    (:arg-types simple-array-single-float
                tagged-num
                simd-pack-256-single)
    (:results (result :scs (single-avx2-reg)))
    (:result-types simd-pack-256-single)
    (:policy :fast-safe)
    (:generator 1
                (inst vmovups
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 4 (- n-fixnum-tag-bits)))
                      x)
                (move result x))))

(in-package :sbcl-numericals.internals)

(declaim (inline d2-ref))
(defun d2-ref (vec i)
  "Access I and I+1 elements of VEC in the form of (SIMD-PACK DOUBLE-FLOAT)."
  (declare (optimize (speed 3))
           (type (simple-array double-float) vec)
           (type fixnum i))
  (sb-vm::d2-ref vec i))
(declaim (inline (setf d2-ref)))
(defun (setf d2-ref) (new-value vec i)
  (declare (optimize (speed 3))
           (type (simple-array double-float) vec)
           (type fixnum i))
  (sb-vm::d2-set vec i new-value))

(declaim (inline s4-ref))
(defun s4-ref (vec i)
  "Access I, I+1, I+2, I+3 elements of VEC in the form of (SIMD-PACK SINGLE-FLOAT)."
  (declare (optimize (speed 3))
           (type (simple-array single-float) vec)
           (type fixnum i))
  (sb-vm::s4-ref vec i))

(declaim (inline (setf s4-ref)))
(defun (setf s4-ref) (new-value vec i)
  (declare (optimize (speed 3))
           (type (simple-array single-float) vec)
           (type fixnum i))
  (sb-vm::s4-set vec i new-value))

(declaim (inline d4-ref))
(defun d4-ref (vec i)
  "Access I, I+1, I+2, I+3 elements of VEC in the form of (SIMD-PACK-256 DOUBLE-FLOAT)."
  (declare (optimize (speed 3))
           (type (simple-array double-float) vec)
           (type fixnum i))
  (sb-vm::d4-ref vec i))
(declaim (inline (setf d4-ref)))
(defun (setf d4-ref) (new-value vec i)
  (declare (optimize (speed 3))
           (type (simple-array double-float) vec)
           (type fixnum i))
  (sb-vm::d4-set vec i new-value))

(declaim (inline s8-ref))
(defun s8-ref (vec i)
  "Access I, I+1, ..., I+7 elements of VEC in the form of (SIMD-PACK-256 SINGLE-FLOAT)."
  (declare (optimize (speed 3))
           (type (simple-array single-float) vec)
           (type fixnum i))
  (sb-vm::s8-ref vec i))
(declaim (inline (setf s8-ref)))
(defun (setf s8-ref) (new-value vec i)
  (declare (optimize (speed 3))
           (type (simple-array single-float) vec)
           (type fixnum i))
  (sb-vm::s8-set vec i new-value))
