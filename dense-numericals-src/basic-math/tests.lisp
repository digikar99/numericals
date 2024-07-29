(in-package :dense-numericals/basic-math/impl)

(define-numericals-one-arg-test nu:abs :dense-numericals (0.0f0) (0.0d0))
(define-numericals-one-arg-test/integers nu:abs :dense-numericals)

(macrolet ((def (name
                 (single-float-error)
                 (double-float-error))
             `(define-numericals-one-arg-test ,name :dense-numericals
                  (,single-float-error) (,double-float-error))))
  (def nu:fround    (0.0f0) (0.0d0))
  (def nu:ftruncate (0.0f0) (0.0d0))
  (def nu:ffloor    (0.0f0) (0.0d0))
  (def nu:fceiling  (0.0f0) (0.0d0)))



(macrolet ((def (name)
             `(define-numericals-two-arg-test/integers ,name :dense-numericals)))

  ;; FIXME: These tests fail.
  (def nu:two-arg-logior)
  (def nu:two-arg-logand)
  (def nu:two-arg-logxor))



(macrolet ((def (name
                 (single-float-return-type single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-return-type double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-numericals-two-arg-test ,name :dense-numericals nil
                    (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                    (,double-float-error ,df-min ,df-max ,double-float-return-type))
                (define-numericals-two-arg-test/integers ,name :dense-numericals (unsigned-byte 8)))))

  (def nu:two-arg-<  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-<= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-=  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-/= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0)))



(define-numericals-two-arg-test/integers nu:add      :dense-numericals)
(define-numericals-two-arg-test/integers nu:subtract :dense-numericals)
(define-numericals-two-arg-test/integers nu:multiply :dense-numericals)
;; (define-numericals-two-arg-test/integers nu:divide   nu::array)
(macrolet ((def (name
                 (single-float-return-type single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-return-type double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(define-numericals-two-arg-test ,name :dense-numericals nil
                  (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                  (,double-float-error ,df-min ,df-max ,double-float-return-type))))

  (def nu:add      (single-float 1f-7) (double-float 1d-15))
  (def nu:subtract (single-float 1f-7) (double-float 1d-15))
  (def nu:multiply (single-float 1f-7) (double-float 1d-15))
  (def nu:divide   (single-float 1f-7) (double-float 1d-15)))
