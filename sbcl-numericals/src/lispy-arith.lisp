;;; SIMD arithmetic operations (+-/*) that can take any number of operands.



(in-package :sb-vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown single-simd-+ ((simd-pack-256 single-float) (simd-pack-256 single-float))
      (simd-pack-256 single-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (single-simd-+)
    (:translate single-simd-+)
    (:policy :fast-safe)
    (:args (a :scs (single-avx2-reg))
           (b :scs (single-avx2-reg)))
    (:arg-types simd-pack-256-single simd-pack-256-single)
    (:results (dest :scs (single-avx2-reg)))
    (:result-types simd-pack-256-single)
    (:generator 1 ;; what should be the cost?
                (inst vaddps dest a b))))

(in-package :sbcl-numericals.internals)

(defconstant +single-simd-zeros+
  #.(apply #'%make-simd-pack-256-single (make-list 8 :initial-element 0.0)))

(defparameter simd
  (apply #'%make-simd-pack-256-single (loop for i below 8 collect (coerce i 'single-float))))

(declaim (inline single-+))
(defun single-+ (&rest args)
  (declare (optimize (speed 3)))
  (cond ((null args) +single-simd-zeros+)
        ((null (cdr args)) (car args))
        ((null (cddr args))
         (sb-vm::single-simd-+ (car args) (cadr args)))
        (t
         (sb-vm::single-simd-+ (car args)
                               (apply #'single-+ (cdr args))))))

(define-compiler-macro single-+ (&whole whole &rest args &environment env)
  ;; This doesn't work well with apply and it's not clear how to get it to work.
  (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
         (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
      (let ((args (case (car whole)
                    (apply (nconc (butlast (cdr args))
                                  (car (last (cdr args)))))
                    (funcall args)
                    (t args))))
        (print args)
        (cond ((null args) +single-simd-zeros+)
              ((null (cdr args)) (car args))
              ((null (cddr args))
               `(sb-vm::single-simd-+ ,(car args) ,(cadr args)))
              (t
               `(sb-vm::single-simd-+ ,(car args)
                                      ,(funcall (compiler-macro-function 'single-+)
                                                `(funcall #'single-+ ,@(cdr args))
                                                env)))))
      whole))
