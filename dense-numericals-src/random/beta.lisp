(in-package :dense-numericals/random)

(define-polymorphic-function beta-scalar (c-type a b)
  :overwrite t)

(defpolymorph beta-scalar ((c-type (eql :float)) a b) single-float
  (declare (ignore c-type))
  (imlet ((c-name (c-name 'single-float 'beta))
          (a (coerce a 'single-float))
          (b  (coerce b 'single-float)))
    (with-foreign-object (r :float)
      (inline-or-funcall c-name 1 r a b)
      (cffi:mem-ref r :float))))

(defpolymorph beta-scalar ((c-type (eql :double)) a b)
    double-float
  (declare (ignore c-type))
  (imlet ((c-name (c-name 'double-float 'beta))
          (a (coerce a 'double-float))
          (b (coerce b 'double-float)))
    (with-foreign-object (r :double)
      (inline-or-funcall c-name 1 r a b)
      (cffi:mem-ref r :double))))

(define-polymorphic-function beta (a b &key size shape out type)
  :documentation "Returns a scalar or an array of shape SHAPE (or SIZE) filled with random numbers drawn from a beta distribution with parameters A (alpha) and B (beta).

If SHAPE (or SIZE) is NIL (default) and OUT is NIL, then only a scalar is returned.

Exactly one of SIZE or SHAPE must be supplied; both mean the same thing.

For more information and examples, see: https://numpy.org/doc/stable/reference/random/generated/numpy.random.Generator.beta.html")

(defpolymorph beta (a b &key ((size null)) ((shape null))
                      (type *default-float-format*) ((out null)))
    number
  (declare (ignore size shape out))
  (imlet ((c-type (c-type type)))
    (beta-scalar c-type a b)))

(defpolymorph beta (a b &key ((size list) nil sizep) ((shape list) nil shapep)
                      (type *default-float-format*) ((out null)))
    simple-array
  (declare (ignorable out sizep shapep))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied"))
    (let* ((a (coerce a type))
           (b (coerce b type))
           (shape (if shapep shape size))
           (out   (empty shape :type type))
           (c-name (c-name type 'beta))
           (sv    (array-storage out))
           (len   (array-total-size out)))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr a b))
      out)))

(defpolymorph beta (a b &key ((size list) nil sizep) ((shape list) nil shapep)
                      (type *default-float-format* typep)
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
           (c-name (c-name <type> 'beta))
           (sv    (array-storage out))
           (len   (array-total-size out)))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr a b))
      out)))
