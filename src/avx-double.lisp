(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel :execute)

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
    ;; EA below stands for effective address. 
    ;; To understand the role of each arg, see the source code for float-ref-ea at
    ;; https://github.com/sbcl/sbcl/blob/master/src/compiler/x86/array.lisp
    ;; and consider the (EA = base + index * scale + offset * element_size) model.
    (:generator 1
                (inst vmovups
                      dest
                      (make-ea-for-float-ref v i 0 0
                                             :scale (ash 8 (- n-fixnum-tag-bits))))))

  (defknown d4-set ((simple-array double-float (*))
                    (integer 0 #.most-positive-fixnum)
                    (simd-pack-256 double-float))
      ;; (integer 0 #.most-positive-fixnum)
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
                      x))))

(in-package :sbcl-numericals.internals)
(declaim (inline d4-ref))
(defun d4-ref (vec i)
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

(defmacro define-double-vectorized-op (op prefix assembly-equivalent)
  (let  ((sb-vm-symbol (intern (concatenate 'string "%D4" (symbol-name op))
                               :sbcl-numericals.internals))
         (internals-symbol (intern (concatenate 'string "D4" (symbol-name op))
                                   :sbcl-numericals.internals))
         (sbcl-numericals-symbol (find-symbol (concatenate 'string
                                                           (symbol-name prefix)
                                                           (symbol-name op))
                                              :sbcl-numericals)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defknown (,sb-vm-symbol)
             ((simd-pack-256 double-float) (simd-pack-256 double-float))
             (simd-pack-256 double-float)
             (movable flushable always-translatable)
           :overwrite-fndb-silently t)
         (define-vop (,sb-vm-symbol)
           (:translate ,sb-vm-symbol)
           (:policy :fast-safe)
           (:args (x :scs (sb-vm::double-avx2-reg))
                  (y :scs (sb-vm::double-avx2-reg)))
           (:arg-types sb-vm::simd-pack-256-double
                       sb-vm::simd-pack-256-double)
           (:results (r :scs (sb-vm::double-avx2-reg)))
           (:result-types sb-kernel:simd-pack-256-double)
           (:generator 1 ;; what should be the cost?
                       (sb-vm::inst ,assembly-equivalent r x y))))
       (declaim (inline ,internals-symbol))
       (defun ,internals-symbol (simd-256-a simd-256-b)
         (declare (optimize (speed 3)))
         (,sb-vm-symbol simd-256-a simd-256-b))
       (defun ,sbcl-numericals-symbol (array-a array-b result-array)
         (declare (optimize (speed 3))
                  (type (simple-array double-float) array-a array-b result-array))
         (if (not (and (equalp (array-dimensions array-a) (array-dimensions array-b))
                       (equalp (array-dimensions array-a) (array-dimensions result-array))))
             (error "Arrays cannot have different dimensions!"))
         (let* ((vec-a (array-storage-vector array-a))
                (vec-b (array-storage-vector array-b))
                (vec-r (array-storage-vector result-array)))
           (loop for i fixnum below (- (length vec-a) 4) by 4
              do (setf (d4-ref vec-r i)
                       (,internals-symbol (d4-ref vec-a i)
                                          (d4-ref vec-b i)))
              finally
                (loop for j from i below (length vec-a)
                   do (setf (aref vec-r j)
                            (,op (aref vec-a j)
                                 (aref vec-b j))))
                (return result-array)))))))

(define-double-vectorized-op - d vsubpd)
(define-double-vectorized-op + d vaddpd)
(define-double-vectorized-op * d vmulpd)
(define-double-vectorized-op / d vdivpd)
