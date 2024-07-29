(in-package :dense-numericals/transcendental/impl)

(5am:in-suite :dense-numericals)

(macrolet ((def (name)
             `(progn
                (define-polymorphic-function ,name (x &key out broadcast)
                  :overwrite t :documentation +one-arg-fn-float-doc+)
                (defpolymorph ,name (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/float ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
                  (one-arg-fn/float ',name x :out out :broadcast broadcast)))))
  (def nu:sin)
  (def nu:cos)
  (def nu:tan)

  (def nu:asin)
  (def nu:acos)
  ;; (def nu:atan (2f-7) (1d-15)) ; Handle atan case specially

  (def nu:sinh)
  (def nu:cosh)
  (def nu:tanh)

  (def nu:asinh)
  (def nu:acosh)
  (def nu:atanh)

  (def nu:exp)
  (def nu:sqrt)
  )

;; Handle atan case specially
(define-polymorphic-function nu:atan (x &rest args)
  :overwrite t :documentation +one-arg-fn-float-doc+)
(defpolymorph nu:atan (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
  (declare (ignore out))
  (one-arg-fn/float 'nu:atan x :broadcast broadcast))
(defpolymorph nu:atan (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
  (one-arg-fn/float 'nu:atan x :out out :broadcast broadcast))
