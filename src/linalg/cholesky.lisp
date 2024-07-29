(in-package :numericals/linalg)

(5am:in-suite :numericals)

(define-polymorphic-function cholesky (array-like &key out) :overwrite t
  :documentation "Compute the cholesky decomposition of positive definite 2D
matrices given by ARRAY-LIKE. This uses the Eigen::LLT to perform the computation.

For a matrix A, it returns L such that A = L * L^C where L is lower triangular, and
L^C is the conjugate of L.

References:

- http://www.eigen.tuxfamily.org/dox/classEigen_1_1LLT.html
")

(defpolymorph out-shape-compatible-p ((name (eql cholesky)) a out) boolean
  (declare (ignore name)
           (type simple-array a out)
           (optimize speed))
  (let ((rank (array-rank a)))
    (and (cl:= rank (array-rank out))
         (if (< rank 2)
             nil
             (and (equal (narray-dimensions a)
                         (narray-dimensions out))
                  (cl:= (array-dimension a (- rank 1))
                        (array-dimension a (- rank 2))))))))

(defpolymorph (cholesky :inline t)
    ((a (simple-array <type>)) &key ((out (simple-array <type>))))
    (simple-array <type>)
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (out-shape-compatible-p 'cholesky a out)
                  ()
                  "Cannot compute cholesky of array of shape ~A~%into array of shape ~A."
                  (narray-dimensions a) (narray-dimensions out)))
    (let* ((c-size (c-size <type>))
           (a-layout (eigen-array-layout a))
           (o-layout (eigen-array-layout out))
           (rank   (array-rank a))
           (m (array-dimension a (- rank 2)))
           (c-name (c-name <type> 'cholesky)))
      (flet ((cholesky (a-size a-ptr a-dims o-ptr o-dims)
               (declare (ignore a-size a-dims o-dims))
               (inline-or-funcall c-name m a-ptr a-layout o-ptr o-layout)))
        (with-simple-array-broadcast (cholesky 2 2) (a c-size) (out c-size)))
      out)))

(defpolymorph out-shape ((name (eql cholesky)) a) list
  (declare (ignore name)
           (type simple-array a)
           (optimize speed))
  (array-dimensions a))

(defpolymorph (cholesky :inline t) ((a (simple-array <type>)) &key ((out null)))
    (or number (simple-array <type>))
  (declare (ignore out))
  (pflet ((out (empty (out-shape 'cholesky a) :type <type>)))
    (declare (type (simple-array <type>) out))
    (cholesky a :out out)))

(defpolymorph (cholesky :inline t) ((a list) &key ((out null)))
    (or number simple-array)
  (declare (ignore out))
  (cholesky (asarray a :type *default-float-format*)))

(defpolymorph (cholesky :inline t) ((a list) &key ((out (simple-array <type>))))
    (simple-array <type>)
  (cholesky (asarray a :type <type>) :out out))
