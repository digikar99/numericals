(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
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

(declaim (inline s8-ref))
(defun s8-ref (vec i)
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

(defmacro define-single-vectorized-op (op prefix assembly-equivalent)
  (let  ((sb-vm-symbol (intern (concatenate 'string "%F8" (symbol-name op))
                               :sbcl-numericals.internals))
         (internals-symbol (intern (concatenate 'string "F8" (symbol-name op))
                                   :sbcl-numericals.internals))
         (sbcl-numericals-symbol (find-symbol (concatenate 'string
                                                           (symbol-name prefix)
                                                           (symbol-name op))
                                              :sbcl-numericals)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defknown (,sb-vm-symbol)
             ((simd-pack-256 single-float) (simd-pack-256 single-float))
             (simd-pack-256 single-float)
             (movable flushable always-translatable)
           :overwrite-fndb-silently t)
         (define-vop (,sb-vm-symbol)
           (:translate ,sb-vm-symbol)
           (:policy :fast-safe)
           (:args (x :scs (sb-vm::single-avx2-reg))
                  (y :scs (sb-vm::single-avx2-reg)))
           (:arg-types sb-vm::simd-pack-256-single
                       sb-vm::simd-pack-256-single)
           (:results (r :scs (sb-vm::single-avx2-reg)))
           (:result-types sb-kernel:simd-pack-256-single)
           (:generator 1 ;; what should be the cost?
                       (sb-vm::inst ,assembly-equivalent r x y))))
       (declaim (inline ,internals-symbol))
       (defun ,internals-symbol (simd-256-a simd-256-b)
         (declare (optimize (speed 3)))
         (,sb-vm-symbol simd-256-a simd-256-b))
       (defun ,sbcl-numericals-symbol (array-a array-b result-array)
         (declare (optimize (speed 3))
                  (type (simple-array single-float) array-a array-b result-array))
         (if (not (and (equalp (array-dimensions array-a) (array-dimensions array-b))
                       (equalp (array-dimensions array-a) (array-dimensions result-array))))
             (error "Arrays cannot have different dimensions!"))
         (let ((vec-a (array-storage-vector array-a))
               (vec-b (array-storage-vector array-b))
               (vec-r (array-storage-vector result-array)))
           (loop for i fixnum below (- (length vec-a) 8) by 8
              do (setf (s8-ref vec-r i)
                       (,internals-symbol (s8-ref vec-a i)
                                          (s8-ref vec-b i)))
              finally
                (loop for j from i below (length vec-a)
                   do (setf (aref vec-r j)
                            (,op (aref vec-a j)
                                 (aref vec-b j))))
                (return result-array)))))))

(define-single-vectorized-op - s vsubps)
(define-single-vectorized-op + s vaddps)
(define-single-vectorized-op * s vmulps)
(define-single-vectorized-op / s vdivps)
