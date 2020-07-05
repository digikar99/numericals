(in-package :numericals.sbcl)

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
                                (%simd-single-/ ,arg ,(car args))
                                ,@(cdr args))
                      env)))
      whole))

