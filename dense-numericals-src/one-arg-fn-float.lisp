(in-package :dense-numericals.impl)

(5am:in-suite array)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

;; FIXME: No multithreading for broadcasting; see numericals/src/one-arg-fn.lisp

;;; Yes, this does lead to a lot of code-bloat while inlining, but inlining is only
;;; primarily useful for very small arrays, and in those cases, the performance difference
;;; is of the order of an magnitude.

;;; TODO: Use ARRAY or STATIC-ARRAY

(define-polymorphic-function one-arg-fn/float (name x &key out broadcast) :overwrite t
  :documentation "These functions have a single array as input and a single array as output.
If the output array is not supplied, its element-type is given by *DEFAULT-FLOAT-FORMAT*")

(defpolymorph (one-arg-fn/float :inline t)
    ((name symbol) (x (array single-float))
     &key ((out (array single-float)) (zeros-like x))
     (broadcast dn:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name))
  (when (and broadcast
             (not (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (%broadcast-compatible-p (narray-dimensions x)
                                 (narray-dimensions out))
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (setq x (broadcast-array x broadcast-dimensions))))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions out)))))
    (with-thresholded-multithreading (array-total-size out)
        (x out)
      (ptr-iterate-but-inner n ((ptr-x   4 ix   x)
                                (ptr-out 4 iout out))
        (funcall (single-float-c-name name) n ptr-x ix ptr-out iout))))
  out)

(defpolymorph (one-arg-fn/float :inline t)
    ((name symbol) (x (array double-float))
     &key ((out (array double-float)) (zeros-like x))
     (broadcast dn:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name))
  (when (and broadcast
             (not (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (%broadcast-compatible-p (narray-dimensions x)
                                 (narray-dimensions out))
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (setq x (broadcast-array x broadcast-dimensions))))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions out)))))
    (with-thresholded-multithreading (array-total-size out)
        (x out)
      (ptr-iterate-but-inner n ((ptr-x   8 ix   x)
                                (ptr-out 8 iout out))
        (funcall (double-float-c-name name) n ptr-x ix ptr-out iout))))
  out)

;; pure number
(defpolymorph (one-arg-fn/float :inline t) ((name symbol) (x number)
                                            &key ((out null))
                                            (broadcast dn:*broadcast-automatically*))
    (values number &optional)
  (declare (ignorable out name broadcast))
  (funcall (cl-name name) x))

;; lists - 2 polymorphs
(defpolymorph (one-arg-fn/float :inline t) ((name symbol) (x list)
                                            &key ((out null))
                                            (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; Why didn't we create ARRAY in the lambda-list itself?
  ;;   We can do, once ZEROS-LIKE starts taking TYPE as an argument
  ;;   Also think over the implication of being required to allocate a separate
  ;; array in the second case below; perhaps we also need a way to copy from a
  ;; array-like to an array.
  (let ((array (asarray x :type dn:*default-float-format*)))
    (one-arg-fn/float name array :out array :broadcast broadcast)))

(defpolymorph (one-arg-fn/float :inline t) ((name symbol) (x list)
                                            &key ((out array))
                                            (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  (one-arg-fn/float name (asarray x :type (array-element-type out)) :out out :broadcast broadcast))

;; non-float arrays
(defpolymorph (one-arg-fn/float :inline nil) ; this is recursive
    ((name symbol) (x array)
     &key ((out (or (array single-float) (array double-float)))
           (zeros (array-dimensions x) :type dn:*default-float-format*))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (dn:copy x :out out)
  (one-arg-fn/float name out :out out :broadcast broadcast))

(macrolet ((def (name
                 (single-float-error &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-error &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-polymorphic-function ,name (x &key out broadcast) :overwrite t)
                (defpolymorph ,name (x &key ((out null)) (broadcast dn:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/float ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast dn:*broadcast-automatically*)) t
                  (one-arg-fn/float ',name x :out out :broadcast broadcast))
                (define-numericals-one-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max)
                    (,double-float-error ,df-min ,df-max)))))
  (def dn:sin (2f-7) (1d-15))
  (def dn:cos (2f-7) (1d-15))
  (def dn:tan (2f-7) (1d-15))

  (def dn:asin (2f-7) (1d-15))
  (def dn:acos (2f-7) (1d-15))
  ;; (def dn:atan (2f-7) (1d-15)) ; Handle atan case specially

  (def dn:sinh (2f-7) (1d-15))
  (def dn:cosh (2f-7) (1d-15))
  (def dn:tanh (2f-7) (1d-15))

  (def dn:asinh (2f-7) (1d-15))
  (def dn:acosh (2f-7 1.0f0 2.0f0) (1d-15 1.0d0 2.0d0))
  (def dn:atanh (2f-7) (1d-15))

  (def dn:exp (2f-7) (1d-15))
  ;; (def dn:sqrt (bmas:ssqrt 2f-7) (bmas:dsqrt 1d-15))
  )

;; Handle atan case specially
(define-polymorphic-function dn:atan (x &rest args) :overwrite t)
(defpolymorph dn:atan (x &key ((out null))) t
  (declare (ignore out))
  (one-arg-fn/float 'dn:atan x))
(defpolymorph dn:atan (x &key ((out (not null)))) t
  (one-arg-fn/float 'dn:atan x :out out))
(define-numericals-one-arg-test dn:atan array (2f-7) (1d-15))


(macrolet ((def (name
                 (single-float-error)
                 (double-float-error))
             (eval `(define-polymorphic-function ,name (value &rest args) :overwrite t))
             `(progn
                (define-polymorphic-function ,name (value &rest args))
                (defpolymorph ,name (x &key ((out null))) t
                  (declare (ignore out))
                  (one-arg-fn/float ',name x))
                (defpolymorph ,name (x &key ((out (not null)))) t
                  (one-arg-fn/float ',name x :out out))
                (define-numericals-one-arg-test ,name array
                    (,single-float-error) (,double-float-error)))))
  (def dn:log       (2f-7)  (1d-15))
  (def dn:fround    (0.0f0) (0.0d0))
  (def dn:ftruncate (0.0f0) (0.0d0))
  (def dn:ffloor    (0.0f0) (0.0d0))
  (def dn:fceiling  (0.0f0) (0.0d0)))
