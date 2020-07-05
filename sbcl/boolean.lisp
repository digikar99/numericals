(in-package :numericals.sbcl)

;;; These can be used to implement bitwise boolean operators. However

(macro-when (member :avx2 sb-impl:+internal-features+)

  (define-constant +simple-simd-nil+
      (let ((svec #(nil nil nil nil)))
        (simd-svref svec 0))
    :test (lambda (a b)
            (equalp (multiple-value-list (%simd-pack-256-ub64s a))
                    (multiple-value-list (%simd-pack-256-ub64s b)))))

  (define-constant +simple-simd-ones+
      (apply #'%make-simd-pack-256-ub64 (make-list 4 :initial-element (1- (expt 2 64))))
    :test (lambda (a b)
            (equalp (multiple-value-list (%simd-pack-256-ub64s a))
                    (multiple-value-list (%simd-pack-256-ub64s b)))))
  
  (defmacro define-simd-log (simd-operation-name vop-name inst)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defknown (,vop-name)
             ((simd-pack-256 integer) (simd-pack-256 integer))
             (simd-pack-256 integer)
             (movable flushable always-translatable)
           :overwrite-fndb-silently t)
         (define-vop (,vop-name)
           (:translate ,vop-name)
           (:policy :fast-safe)
           (:args (x :scs (int-avx2-reg))
                  (y :scs (int-avx2-reg)))
           (:arg-types simd-pack-256-int
                       simd-pack-256-int)
           (:results (r :scs (int-avx2-reg)))
           (:result-types simd-pack-256-int)
           (:generator 1 
                       (inst ,inst r x y))))
       (defun ,simd-operation-name (a b)
         (declare (optimize (speed 3)))
         (,vop-name a b))))

  (define-simd-log simd-and %and vpand)
  (define-simd-log simd-or %or vpor)
  (define-simd-log simd-xor %xor vpxor)

  (defun simd-not (a)
    (declare (optimize (speed 3)))
    (simd-xor a +simple-simd-ones+)))

;; (defparameter a (%make-simd-pack-256-ub64 0 0 0 0))
;; (defparameter b (%make-simd-pack-256-ub64 1 1 1 1))
;; (defparameter c (%make-simd-pack-256-ub64 0 0 0 0))
;; (defparameter svec #(nil nil nil nil 0 1 2 3))


