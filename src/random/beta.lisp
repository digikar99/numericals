(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-polymorphic-function rand:beta (a b &key size shape out type)
  :documentation "Returns a scalar or an array of shape SHAPE (or SIZE) filled with random numbers drawn from a beta distribution with parameters A (alpha) and B (beta).

If SHAPE (or SIZE) is NIL (default) and OUT is NIL, then only a scalar is returned.

Exactly one of SIZE or SHAPE must be supplied; both mean the same thing.

For more information and examples, see: https://numpy.org/doc/stable/reference/random/generated/numpy.random.Generator.beta.html")

(defpolymorph rand:beta (a b &key ((size null)) ((shape null))
                           (type nu:*default-float-format*) ((out null)))
    number
  (declare (ignore size shape out))
  (imlet ((a (coerce a 'double-float))
          (b (coerce b 'double-float)))
    ;; If there were a way to stack allocate a foreign value (portably),
    ;; then we could have used the CEIGEN-LITE function.
    (coerce (cffi:foreign-funcall "gsl_ran_beta"
                                  :pointer +gsl-rng+
                                  :double a
                                  :double b
                                  :double)
            type)))

(defpolymorph rand:beta (a b &key ((size list) nil sizep) ((shape list) nil shapep)
                           (type nu:*default-float-format*) ((out null)))
    simple-array
  (declare (ignorable out sizep shapep))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied"))
    (let* ((a (coerce a type))
           (b (coerce b type))
           (shape (if shapep shape size))
           (out   (nu:empty shape :type type))
           (c-name (c-name type 'rand:beta))
           (sv    (array-storage out))
           (len   (nu:array-total-size out)))
      (print c-name)
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr a b))
      out)))

(defpolymorph rand:beta (a b &key ((size list) nil sizep) ((shape list) nil shapep)
                           (type nu:*default-float-format* typep)
                           ((out (simple-array <type>))))
    (simple-array <type>)
  (declare (ignorable sizep shapep typep type shape size))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied")
       (assertion (if typep
                      (type= type <type>)
                      t)
                  ()
                  "OUT was expected to be a simple array with element type ~S" type)
       (assertion (if (or shapep sizep)
                      (let ((shape (if shapep shape size)))
                        (equal (narray-dimensions out) shape))
                      t)
                  ()
                  "OUT was expected to be a simple array with shape ~S"
                  (if shapep shape size)))
    (let* ((a (coerce a <type>))
           (b (coerce b <type>))
           (c-name (c-name <type> 'rand:beta))
           (sv    (array-storage out))
           (len   (nu:array-total-size out)))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr a b))
      out)))
