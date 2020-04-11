;;; SIMD arithmetic operations (+-/*) that can take any number of operands.

(in-package :numericals.sbcl)

(macro-when (member :avx2 sb-impl:+internal-features+)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defknown %simd-single-+ ((simd-pack-256 single-float) (simd-pack-256 single-float))
        (simd-pack-256 single-float)
        (movable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (%simd-single-+)
      (:translate %simd-single-+)
      (:policy :fast-safe)
      (:args (a :scs (single-avx2-reg))
             (b :scs (single-avx2-reg)))
      (:arg-types simd-pack-256-single simd-pack-256-single)
      (:results (dest :scs (single-avx2-reg)))
      (:result-types simd-pack-256-single)
      (:generator 1 ;; what should be the cost?
                  (inst vaddps dest a b))))

  #.(defconstant +single-simd-zeros+
      (apply #'%make-simd-pack-256-single (make-list 8 :initial-element 0.0)))

  (defparameter simd
    (apply #'%make-simd-pack-256-single (loop for i below 8 collect (coerce i 'single-float))))

  (declaim (inline single-+))
  (defun simd-single-+ (&rest args)
    (declare (optimize (speed 3)))
    (cond ((null args) +single-simd-zeros+)
          ((null (cdr args)) (car args))
          ((null (cddr args))
           (%simd-single-+ (car args) (cadr args)))
          (t
           (%simd-single-+ (car args)
                           (apply #'simd-single-+ (cdr args))))))

  (define-compiler-macro simd-single-+ (&whole whole &rest args &environment env)
    ;; This doesn't work well with apply and it's not clear how to get it to work.
    (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
           (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
        (let ((args (case (car whole)
                      (apply (nconc (butlast (cdr args))
                                    (car (last (cdr args)))))
                      (funcall args)
                      (t args))))
          (cond ((null args) +single-simd-zeros+)
                ((null (cdr args)) (car args))
                ((null (cddr args))
                 `(%simd-single-+ ,(car args) ,(cadr args)))
                (t
                 `(%simd-single-+ ,(car args)
                                  ,(funcall (compiler-macro-function 'simd-single-+)
                                            `(funcall #'simd-single-+ ,@(cdr args))
                                            env)))))
        whole)))
