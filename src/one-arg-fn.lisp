(in-package :numericals.impl)

;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

(define-polymorphic-function one-arg-fn (name x &key out) :overwrite t)

(defpolymorph (one-arg-fn :inline t)
    ((name symbol) (x (cl:array single-float))
     &key ((out (cl:array single-float)) (nu:zeros-like x)))
    (cl:array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svx svo))
      (with-thresholded-multithreading (array-total-size out)
          (x out)
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 4 (cl-array-offset x))))
          (cffi:incf-pointer ptr-o (the-size (* 4 (cl-array-offset out))))
          (funcall single-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-o 1)))))
  out)

(defpolymorph (one-arg-fn :inline t)
    ((name symbol) (x (cl:array double-float))
     &key ((out (cl:array double-float)) (nu:zeros-like x)))
    (cl:array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svx svo))
      (with-thresholded-multithreading (array-total-size out)
          (x out)
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 8 (cl-array-offset x))))
          (cffi:incf-pointer ptr-o (the-size (* 8 (cl-array-offset out))))
          (funcall double-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-o 1)))))
  out)


;; pure number
(defpolymorph (one-arg-fn :inline t) ((name symbol) (x number) &key ((out null)))
    (values number &optional)
  (declare (ignorable out name))
  (funcall (cl-name name) x))

;; lists - 2 polymorphs
(defpolymorph (one-arg-fn :inline t) ((name symbol) (x list) &key ((out null)))
    (values cl:array &optional)
  (declare (ignorable out))
  (let ((array (nu:asarray x)))
    (one-arg-fn name array :out array)))

(defpolymorph (one-arg-fn :inline t) ((name symbol) (x list) &key ((out cl:array)))
    (values cl:array &optional)
  (declare (ignorable out))
  (one-arg-fn name (nu:asarray x :type (array-element-type out)) :out out))


(macrolet ((def (name
                 (single-float-error &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-error &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-polymorphic-function ,name (x &key out) :overwrite t)
                (defpolymorph ,name (x &key ((out null))) t
                  (declare (ignore out))
                  (one-arg-fn ',name x))
                (defpolymorph ,name (x &key ((out (not null)))) t
                  (one-arg-fn ',name x :out out))
                (define-numericals-one-arg-test ,name nu::array
                    (,single-float-error ,sf-min ,sf-max)
                    (,double-float-error ,df-min ,df-max)))))
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
  (def nu:abs (0.0f0) (0.0f0)))

;; Handle atan case specially
(define-polymorphic-function nu:atan (x &rest args) :overwrite t)
(defpolymorph nu:atan (x &key ((out null))) t
  (declare (ignore out))
  (one-arg-fn 'nu:atan x))
(defpolymorph nu:atan (x &key ((out (not null)))) t
  (one-arg-fn 'nu:atan x :out out))
(define-numericals-one-arg-test nu:atan nu::array (2f-7) (1d-15))


(macrolet ((def (name
                 (single-float-error)
                 (double-float-error))
             (eval `(define-polymorphic-function ,name (value &rest args) :overwrite t))
             `(progn
                (define-polymorphic-function ,name (value &rest args))
                (defpolymorph ,name (x &key ((out null))) t
                  (declare (ignore out))
                  (one-arg-fn ',name x))
                (defpolymorph ,name (x &key ((out (not null)))) t
                  (one-arg-fn ',name x :out out))
                (define-numericals-one-arg-test ,name nu::array
                    (,single-float-error) (,double-float-error)))))
  (def nu:log       (2f-7)  (1d-15))
  (def nu:fround    (0.0f0) (0.0d0))
  (def nu:ftruncate (0.0f0) (0.0d0))
  (def nu:ffloor    (0.0f0) (0.0d0))
  (def nu:fceiling  (0.0f0) (0.0d0)))
