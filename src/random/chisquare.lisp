(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-polymorphic-function rand:chisquare (&key size shape out type (ndof 1))
  :documentation "Returns a scalar or an array of shape SHAPE (or SIZE) filled with random numbers drawn from a chisquare distribution with NDOF as the degrees of freedom.

If SHAPE (or SIZE) is NIL (default) and OUT is NIL, then only a scalar is returned.

Exactly one of SIZE or SHAPE must be supplied; both mean the same thing.

For more information and examples, see:
https://numpy.org/doc/stable/reference/random/generated/numpy.random.Generator.chisquare.html")

(defpolymorph rand:chisquare (&key (ndof 1)
                                   ((size null)) ((shape null))
                                   (type nu:*default-float-format*) ((out null)))
    number
  (declare (ignore size shape out))
  (imlet ((ndof (coerce ndof 'double-float)))
    ;; If there were a way to stack allocate a foreign value (portably),
    ;; then we could have used the CEIGEN-LITE function.
    (coerce (cffi:foreign-funcall "gsl_ran_chisq"
                                  :pointer +gsl-rng+
                                  :double ndof
                                  :double)
            type)))

(defpolymorph rand:chisquare (&key ((size list) nil sizep) ((shape list) nil shapep)
                                   (type nu:*default-float-format*) ((out null))
                                   (ndof 1))
    simple-array
  (declare (ignorable out sizep shapep))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied"))
    (let* ((ndof (coerce ndof type))
           (shape (if shapep shape size))
           (out   (nu:empty shape :type type))
           (c-name (c-name type 'rand:chisquare))
           (sv    (array-storage out))
           (len   (nu:array-total-size out)))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr ndof))
      out)))

(defpolymorph rand:chisquare (&key ((size list) nil sizep) ((shape list) nil shapep)
                           (type nu:*default-float-format* typep)
                           ((out (simple-array <type>)))
                           (ndof 1))
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
    (let* ((ndof (coerce ndof <type>))
           (c-name (c-name <type> 'rand:chisquare))
           (sv    (array-storage out))
           (len   (nu:array-total-size out)))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr ndof))
      out)))
