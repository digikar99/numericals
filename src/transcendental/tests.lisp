(in-package :numericals/transcendental/impl)

(macrolet ((def (name
                 (single-float-error &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-error &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(define-numericals-one-arg-test ,name :numericals
                (,single-float-error ,sf-min ,sf-max)
                (,double-float-error ,df-min ,df-max))))
  (def nu:sin (2f-7) (1d-15))
  (def nu:cos (2f-7) (1d-15))
  (def nu:tan (2f-7) (1d-15))

  (def nu:asin (2f-7) (1d-15))
  (def nu:acos (2f-7) (1d-15))
  ;; (def nu:atan (2f-7) (1d-15)) ; Handle atan case specially

  (def nu:sinh (2f-7) (1d-15))
  (def nu:cosh (2f-7) (1d-15))
  (def nu:tanh (2f-7) (1d-15))

  (def nu:asinh (2f-7) (1d-15))
  (def nu:acosh (2f-7 1.0f0 2.0f0) (1d-15 1.0d0 2.0d0))
  (def nu:atanh (2f-7) (1d-15))

  (def nu:exp (2f-7) (1d-15))
  ;; (def nu:sqrt (bmas:ssqrt 2f-7) (bmas:dsqrt 1d-15))
  )

(define-numericals-one-arg-test nu:atan :numericals (2f-7) (1d-15))

(define-numericals-two-arg-test nu:expt :numericals t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))

(define-numericals-two-arg-test nu::atan2 :numericals t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))
