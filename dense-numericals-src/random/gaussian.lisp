(in-package :dense-numericals/random)

(define-polymorphic-function gaussian-scalar (c-type meanp mean loc stdp std scale)
  :overwrite t)

(defpolymorph gaussian-scalar ((c-type (eql :float)) meanp mean loc stdp std scale)
    single-float
  (declare (ignore c-type))
  (imlet ((c-name (c-name 'single-float 'gaussian))
          (mean (coerce (the real (if meanp mean loc)) 'single-float))
          (std  (coerce (the real (if stdp std scale)) 'single-float)))
    (with-foreign-object (r :float)
      (inline-or-funcall c-name 1 r mean std)
      (cffi:mem-ref r :float))))

(defpolymorph gaussian-scalar ((c-type (eql :double)) meanp mean loc stdp std scale)
    double-float
  (declare (ignore c-type))
  (imlet ((c-name (c-name 'double-float 'gaussian))
          (mean (coerce (the real (if meanp mean loc)) 'double-float))
          (std  (coerce (the real (if stdp std scale)) 'double-float)))
    (with-foreign-object (r :double)
      (inline-or-funcall c-name 1 r mean std)
      (cffi:mem-ref r :double))))

(define-polymorphic-function gaussian (&key loc scale size shape (mean 0) (std 1) out type)
  :documentation "Returns a scalar or an array of shape SHAPE (or SIZE) filled with random numbers drawn from a gaussian/normal distribution centered at LOC (or MEAN) and standard deviation SCALE (or STD).

If SHAPE (or SIZE) is NIL (default) and OUT is NIL, then only a scalar is returned.

The following are analogous pairs of arguments. Supply only one of these.

- LOC and MEAN
- SCALE and STD
- SIZE and SHAPE

For more information and examples, see: https://numpy.org/doc/stable/reference/random/generated/numpy.random.normal.html")


(defpolymorph gaussian (&key
                        ((size null)) ((shape null))
                        (mean 0.0d0 meanp) (loc 0.0d0 locp)
                        (std 1.0d0 stdp) (scale 1.0d0 scalep)
                        (type *default-float-format*) ((out null)))
    real
  (declare (ignore size shape out)
           (ignorable meanp locp scalep stdp))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and meanp locp))
                  ()
                  "Only one of MEAN or LOC should be supplied")
       (assertion (not (and stdp scalep))
                  ()
                  "Only one of STD or SCALE should be supplied"))
    (gaussian-scalar (c-type type) meanp mean loc stdp std scale)))

(defpolymorph gaussian (&key
                        ((size list) nil sizep) ((shape list) nil shapep)
                        (mean 0.0d0 meanp) (loc 0.0d0 locp)
                        (std 1.0d0 stdp) (scale 1.0d0 scalep)
                        (type *default-float-format*) ((out null)))
    simple-array
  (declare (ignorable out sizep shapep meanp locp stdp scalep))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied")
       (assertion (not (and meanp locp))
                  ()
                  "Only one of MEAN or LOC should be supplied")
       (assertion (not (and stdp scalep))
                  ()
                  "Only one of STD or SCALE should be supplied"))
    (let* ((mean  (coerce (the real (if meanp mean loc)) type))
           (std   (coerce (the real (if stdp std scale)) type))
           (shape (if shapep shape size))
           (out   (empty shape :type type))
           (c-name (c-name type 'gaussian))
           (sv    (array-storage out))
           (len   (array-total-size out)))
      (declare (ignorable mean std))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr mean std))
      out)))

(defpolymorph gaussian (&key
                             ((size list) nil sizep) ((shape list) nil shapep)
                             (mean 0.0d0 meanp) (loc 0.0d0 locp)
                             (std 1.0d0 stdp) (scale 1.0d0 scalep)
                             (type *default-float-format* typep)
                             ((out (simple-array <type>))))
    (simple-array <type>)
  (declare (ignorable sizep shapep meanp locp stdp scalep typep type shape size))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (not (and shapep sizep))
                  ()
                  "Only one of SHAPE or SIZE should be supplied")
       (assertion (not (and meanp locp))
                  ()
                  "Only one of MEAN or LOC should be supplied")
       (assertion (not (and stdp scalep))
                  ()
                  "Only one of STD or SCALE should be supplied")
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
    (let* ((mean  (coerce (the real (if meanp mean loc)) <type>))
           (std   (coerce (the real (if stdp std scale)) <type>))
           (c-name (c-name <type> 'gaussian))
           (sv    (array-storage out))
           (len   (array-total-size out)))
      (declare (ignorable mean std))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name len ptr mean std))
      out)))
