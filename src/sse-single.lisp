(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
                (move result x))))


(in-package :sbcl-numericals.internals)

(declaim (inline s4-ref))
(defun s4-ref (vec i)
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
             ((simd-pack single-float) (simd-pack single-float))
             (simd-pack single-float)
             (movable flushable always-translatable)
           :overwrite-fndb-silently t)
         (define-vop (,sb-vm-symbol)
           (:translate ,sb-vm-symbol)
           (:policy :fast-safe)
           (:args (x :scs (sb-vm::single-sse-reg))
                  (y :scs (sb-vm::single-sse-reg)))
           (:arg-types sb-vm::simd-pack-single
                       sb-vm::simd-pack-single)
           (:results (r :scs (sb-vm::single-sse-reg)))
           (:result-types sb-kernel:simd-pack-single)
           (:generator 1 ;; what should be the cost?
                       (move r x)
                       (sb-vm::inst ,assembly-equivalent r y))))
       (declaim (inline ,internals-symbol))
       (defun ,internals-symbol (simd-a simd-b)
         (declare (optimize (speed 3)))
         (,sb-vm-symbol simd-a simd-b))
       (defun ,sbcl-numericals-symbol (array-a array-b result-array)
         (declare (optimize (speed 3))
                  (type (simple-array single-float) array-a array-b result-array))
         (if (not (and (equalp (array-dimensions array-a) (array-dimensions array-b))
                       (equalp (array-dimensions array-a) (array-dimensions result-array))))
             (error "Arrays cannot have different dimensions!"))
         (let ((vec-a (array-storage-vector array-a))
               (vec-b (array-storage-vector array-b))
               (vec-r (array-storage-vector result-array)))
           (loop for i fixnum below (- (length vec-a) 4) by 4
              do (setf (s4-ref vec-r i)
                       (,internals-symbol (s4-ref vec-a i)
                                          (s4-ref vec-b i)))
              finally
                (loop for j from i below (length vec-a)
                   do (setf (aref vec-r j)
                            (,op (aref vec-a j)
                                 (aref vec-b j))))
                (return result-array)))))))

(define-single-vectorized-op - s2 subps)
(define-single-vectorized-op + s2 addps)
(define-single-vectorized-op * s2 mulps)
(define-single-vectorized-op / s2 divps)
