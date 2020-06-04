;;; SIMD arithmetic operations (+-/*) that can take any number of operands.

(in-package :numericals/sbcl)

(macro-when (member :avx2 sb-impl:+internal-features+)
  
  (define-constant +single-simd-zeros+
      (apply #'%make-simd-pack-256-single (make-list 8 :initial-element 0.0))
    :test (lambda (a b)
            (equalp (multiple-value-list (%simd-pack-256-singles a))
                    (multiple-value-list (%simd-pack-256-singles b)))))
  
  (define-constant +double-simd-zeros+
      (apply #'%make-simd-pack-256-double (make-list 4 :initial-element 0.0d0))
    :test (lambda (a b)
            (equalp (multiple-value-list (%simd-pack-256-doubles a))
                    (multiple-value-list (%simd-pack-256-doubles b)))))

  (define-constant +single-simd-ones+
      (apply #'%make-simd-pack-256-single (make-list 8 :initial-element 1.0))
    :test (lambda (a b)
            (equalp (multiple-value-list (%simd-pack-256-singles a))
                    (multiple-value-list (%simd-pack-256-singles b)))))

  (macrolet ((define-binary-vop-operation (name avx2-operation)
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
                                (inst ,avx2-operation dest a b)))))) 

    (define-binary-vop-operation %simd-single-+ vaddps)
    (define-binary-vop-operation %simd-single-- vsubps)
    (define-binary-vop-operation %simd-single-* vmulps)
    (define-binary-vop-operation %simd-single-/ vdivps))

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
        whole))

  (defun simd-single-- (arg &rest args)
    (declare (optimize (speed 3)))
    (cond ((null args) (%simd-single-- +single-simd-zeros+ arg))
          ((null (cdr args)) (%simd-single-- arg (car args)))
          (t
           (apply #'simd-single--
                  (%simd-single-- arg (car args))
                  (cdr args)))))

  (define-compiler-macro simd-single-- (&whole whole arg &rest args &environment env)
    ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
    (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
           (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
        (cond ((null args) `(%simd-single-- ,+single-simd-zeros+ ,arg))
              ((null (cdr args)) `(%simd-single-- ,arg ,(car args)))
              (t
               (funcall (compiler-macro-function 'simd-single--)
                        `(funcall #'simd-single--
                                  (%simd-single-- arg (car args))
                                  ,@(cdr args))
                        env)))
        whole))

  (defun simd-single-* (&rest args)
    (declare (optimize (speed 3)))
    (cond ((null args) +single-simd-ones+)
          ((null (cdr args)) (car args))
          ((null (cddr args))
           (%simd-single-* (car args) (cadr args)))
          (t
           (%simd-single-* (car args)
                           (apply 'simd-single-* (cdr args))))))
  (define-compiler-macro simd-single-* (&whole whole &rest args &environment env)
    ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
    (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
           (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
        (let ((args (case (car whole)
                      (apply (nconc (butlast (cdr args))
                                    (car (last (cdr args)))))
                      (funcall args)
                      (t args))))
          (cond ((null args) +single-simd-ones+)
                ((null (cdr args)) (car args))
                ((null (cddr args))
                 `(%simd-single-* ,(car args) ,(cadr args)))
                (t
                 `(%simd-single-* ,(car args)
                                  ,(funcall (compiler-macro-function 'simd-single-*)
                                            `(funcall #'simd-single-* ,@(cdr args))
                                            env)))))
        whole))

  (defun simd-single-/ (arg &rest args)
    (declare (optimize (speed 3)))
    (cond ((null args) (%simd-single-/ +single-simd-ones+ arg))
          ((null (cdr args)) (%simd-single-/ arg (car args)))
          (t
           (apply #'simd-single-/
                  (%simd-single-/ arg (car args))
                  (cdr args)))))

  (define-compiler-macro simd-single-/ (&whole whole arg &rest args &environment env)
    ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
    (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
           (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
        (cond ((null args) `(%simd-single-/ ,+single-simd-ones+ ,arg))
              ((null (cdr args)) `(%simd-single-/ ,arg ,(car args)))
              (t
               (funcall (compiler-macro-function 'simd-single-/)
                        `(funcall #'simd-single-/
                                  (%simd-single-/ arg (car args))
                                  ,@(cdr args))
                        env)))
        whole))

  (macrolet ((define-unary-vop-operation (name avx2-operation)
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
                                (inst ,avx2-operation dest a))))))
    (define-unary-vop-operation %simd-single-sqrt vsqrtps))

  (defun simd-single-sqrt (arg)
    (declare (optimize (speed 3)))
    (%simd-single-sqrt arg)))

;; (defparameter a (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.1))))
;; (defparameter b (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.2))))
;; (defparameter c (apply '%make-simd-pack-256-single (loop for i below 8 collect (+ i 0.3))))
