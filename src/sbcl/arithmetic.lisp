;;; SIMD arithmetic operations (+-/*) that can take any number of operands.

(in-package :numericals.sbcl)

;;; The code for SINGLE and DOUBLE is almost identical; but I didn't find it wiser
;;; to abstract the commonalities one layer further.

(macro-when (member :avx2 sb-impl:+internal-features+)

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defun simd-pack-256-single-= (a b)
      (equalp (multiple-value-list (%simd-pack-256-singles a))
              (multiple-value-list (%simd-pack-256-singles b))))
    (defun simd-pack-256-double-= (a b)
      (equalp (multiple-value-list (%simd-pack-256-doubles a))
              (multiple-value-list (%simd-pack-256-doubles b)))))
  
  (define-constant +single-simd-zeros+
      (apply #'%make-simd-pack-256-single (make-list 8 :initial-element 0.0))
    :test #'simd-pack-256-single-=)
  
  (define-constant +double-simd-zeros+
      (apply #'%make-simd-pack-256-double (make-list 4 :initial-element 0.0d0))
    :test #'simd-pack-256-double-=)

  (define-constant +single-simd-ones+
      (apply #'%make-simd-pack-256-single (make-list 8 :initial-element 1.0))
    :test #'simd-pack-256-single-=)
  
  (define-constant +double-simd-ones+
      (apply #'%make-simd-pack-256-double (make-list 4 :initial-element 1.0d0))
    :test #'simd-pack-256-double-=)

  (macrolet ((define-single-binary-vop-operation (name avx2-operation)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defknown (,name)
                      ((simd-pack-256 single-float) (simd-pack-256 single-float))
                      (simd-pack-256 single-float)
                      (movable flushable always-translatable)
                    :overwrite-fndb-silently t)
                  (define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (a :scs (single-avx2-reg))
                           (b :scs (single-avx2-reg)))
                    (:arg-types simd-pack-256-single simd-pack-256-single)
                    (:results (dest :scs (single-avx2-reg)))
                    (:result-types simd-pack-256-single)
                    (:generator 1 ;; TODO: what should be the cost?
                                (inst ,avx2-operation dest a b)))))
             (define-double-binary-vop-operation (name avx2-operation)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defknown (,name)
                      ((simd-pack-256 double-float) (simd-pack-256 double-float))
                      (simd-pack-256 double-float)
                      (movable flushable always-translatable)
                    :overwrite-fndb-silently t)
                  (define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (a :scs (double-avx2-reg))
                           (b :scs (double-avx2-reg)))
                    (:arg-types simd-pack-256-double simd-pack-256-double)
                    (:results (dest :scs (double-avx2-reg)))
                    (:result-types simd-pack-256-double)
                    (:generator 1 ;; TODO: what should be the cost?
                                (inst ,avx2-operation dest a b)))))) 

    (define-single-binary-vop-operation %simd-single-+ vaddps)
    (define-single-binary-vop-operation %simd-single-- vsubps)
    (define-single-binary-vop-operation %simd-single-* vmulps)
    (define-single-binary-vop-operation %simd-single-/ vdivps)

    (define-double-binary-vop-operation %simd-double-+ vaddpd)
    (define-double-binary-vop-operation %simd-double-- vaddpd)
    (define-double-binary-vop-operation %simd-double-* vaddpd)
    (define-double-binary-vop-operation %simd-double-/ vaddpd))

  (macrolet ((define-single-unary-vop-operation (name avx2-operation)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defknown (,name)
                      ((simd-pack-256 single-float))
                      (simd-pack-256 single-float)
                      (movable flushable always-translatable)
                    :overwrite-fndb-silently t)
                  (define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (a :scs (single-avx2-reg)))
                    (:arg-types simd-pack-256-single)
                    (:results (dest :scs (single-avx2-reg)))
                    (:result-types simd-pack-256-single)
                    (:generator 1 ;; TODO: what should be the cost?
                                (inst ,avx2-operation dest a)))))
             (define-double-unary-vop-operation (name avx2-operation)
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (defknown (,name)
                      ((simd-pack-256 double-float))
                      (simd-pack-256 double-float)
                      (movable flushable always-translatable)
                    :overwrite-fndb-silently t)
                  (define-vop (,name)
                    (:translate ,name)
                    (:policy :fast-safe)
                    (:args (a :scs (double-avx2-reg)))
                    (:arg-types simd-pack-256-double)
                    (:results (dest :scs (double-avx2-reg)))
                    (:result-types simd-pack-256-double)
                    (:generator 1 ;; TODO: what should be the cost?
                                (inst ,avx2-operation dest a))))))
    (define-single-unary-vop-operation %simd-single-sqrt vsqrtps)
    (define-double-unary-vop-operation %simd-double-sqrt vsqrtpd))

  (defun simd-single-sqrt (arg)
    (declare (optimize (speed 3)))
    (%simd-single-sqrt arg))

  (defun simd-double-sqrt (arg)
    (declare (optimize (speed 3)))
    (%simd-double-sqrt arg)))

;; (defparameter a (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.1))))
;; (defparameter b (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.2))))
;; (defparameter c (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.3))))
