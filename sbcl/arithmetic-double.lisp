(in-package :numericals.sbcl)

(defun simd-double-+ (&rest args)
  (declare (optimize (speed 3)))
  (cond ((null args) +double-simd-zeros+)
        ((null (cdr args)) (car args))
        ((null (cddr args))
         (%simd-double-+ (car args) (cadr args)))
        (t
         (%simd-double-+ (car args)
                         (apply #'simd-double-+ (cdr args))))))

(define-compiler-macro simd-double-+ (&whole whole &rest args &environment env)
  ;; This doesn't work well with apply and it's not clear how to get it to work.
  (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
         (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
      (let ((args (case (car whole)
                    (apply (nconc (butlast (cdr args))
                                  (car (last (cdr args)))))
                    (funcall args)
                    (t args))))
        (cond ((null args) +double-simd-zeros+)
              ((null (cdr args)) (car args))
              ((null (cddr args))
               `(%simd-double-+ ,(car args) ,(cadr args)))
              (t
               `(%simd-double-+ ,(car args)
                                ,(funcall (compiler-macro-function 'simd-double-+)
                                          `(funcall #'simd-double-+ ,@(cdr args))
                                          env)))))
      whole))

(defun simd-double-- (arg &rest args)
  (declare (optimize (speed 3)))
  (cond ((null args) (%simd-double-- +double-simd-zeros+ arg))
        ((null (cdr args)) (%simd-double-- arg (car args)))
        (t
         (apply #'simd-double--
                (%simd-double-- arg (car args))
                (cdr args)))))

(define-compiler-macro simd-double-- (&whole whole arg &rest args &environment env)
  ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
  (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
         (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
      (cond ((null args) `(%simd-double-- ,+double-simd-zeros+ ,arg))
            ((null (cdr args)) `(%simd-double-- ,arg ,(car args)))
            (t
             (funcall (compiler-macro-function 'simd-double--)
                      `(funcall #'simd-double--
                                (%simd-double-- arg (car args))
                                ,@(cdr args))
                      env)))
      whole))

(defun simd-double-* (&rest args)
  (declare (optimize (speed 3)))
  (cond ((null args) +double-simd-ones+)
        ((null (cdr args)) (car args))
        ((null (cddr args))
         (%simd-double-* (car args) (cadr args)))
        (t
         (%simd-double-* (car args)
                         (apply 'simd-double-* (cdr args))))))
(define-compiler-macro simd-double-* (&whole whole &rest args &environment env)
  ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
  (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
         (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
      (let ((args (case (car whole)
                    (apply (nconc (butlast (cdr args))
                                  (car (last (cdr args)))))
                    (funcall args)
                    (t args))))
        (cond ((null args) +double-simd-ones+)
              ((null (cdr args)) (car args))
              ((null (cddr args))
               `(%simd-double-* ,(car args) ,(cadr args)))
              (t
               `(%simd-double-* ,(car args)
                                ,(funcall (compiler-macro-function 'simd-double-*)
                                          `(funcall #'simd-double-* ,@(cdr args))
                                          env)))))
      whole))

(defun simd-double-/ (arg &rest args)
  (declare (optimize (speed 3)))
  (cond ((null args) (%simd-double-/ +double-simd-ones+ arg))
        ((null (cdr args)) (%simd-double-/ arg (car args)))
        (t
         (apply #'simd-double-/
                (%simd-double-/ arg (car args))
                (cdr args)))))

(define-compiler-macro simd-double-/ (&whole whole arg &rest args &environment env)
  ;; TODO: This doesn't work well with apply and it's not clear how to get it to work.
  (if (> (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'speed)
         (sb-c::policy-quality (slot-value env 'sb-c::%policy) 'space))
      (cond ((null args) `(%simd-double-/ ,+double-simd-ones+ ,arg))
            ((null (cdr args)) `(%simd-double-/ ,arg ,(car args)))
            (t
             (funcall (compiler-macro-function 'simd-double-/)
                      `(funcall #'simd-double-/
                                (%simd-double-/ ,arg ,(car args))
                                ,@(cdr args))
                      env)))
      whole))

