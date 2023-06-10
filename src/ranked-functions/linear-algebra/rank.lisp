(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function la:rank (array-like &key out (tol 0)) :overwrite t
  :documentation "Use Eigen::ColPivHouseholderQR to calculate the rank of the matrix given by ARRAY-LIKE.

The tolerance or threshold is given by TOL. If not supplied or given as zero,
it is the default value set by the eigen's methods.")

(defpolymorph la:rank ((a (simple-array <type> 2)) &key ((out null)) (tol 0))
    number
  (declare (ignore out))
  (let* ((layout (eigen-array-layout a))
         (m (nu:array-dimension a 0))
         (n (nu:array-dimension a 1))
         (c-name (c-name <type> 'la:rank)))
    (pflet ((sv (array-storage a)))
      (declare (type (cl:simple-array <type> 1) sv))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name m n ptr layout (coerce tol <type>))))))

(defun out-shape-compatible-for-rank-p (a out)
  (declare (type simple-array a out)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (and (= (- rank 2) (nu:array-rank out))
         (if (< rank 2)
             nil
             (and (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions a)
                        :for d2 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions out)
                        :repeat (- rank 2)
                        :always (= d1 d2))
                  (= (array-dimension a (- rank 1))
                     (array-dimension a (- rank 2))))))))

(defpolymorph la:rank ((a (simple-array <type>))
                       &key ((out (simple-array (signed-byte 32))))
                       (tol 0))
    (simple-array <type>)
  (policy-cond:with-expectations (= 0 safety)
      ((assertion (out-shape-compatible-for-rank-p a out)
                  ()
                  "Cannot compute determinant of array of shape ~A~%into array of shape ~A."
                  (narray-dimensions a) (narray-dimensions out)))
    (let* ((c-size (c-size <type>))
           (a-layout (eigen-array-layout a))
           (rank   (nu:array-rank a))
           (m (nu:array-dimension a (- rank 2)))
           (n (nu:array-dimension a (- rank 1)))
           (c-name (c-name <type> 'la:rank))
           (tol (coerce tol <type>)))
      (flet ((solve (a-size a-ptr a-dims o-ptr o-dims)
               (declare (ignore a-size a-dims o-dims))
               (inline-or-funcall #'(setf fref)
                                  (inline-or-funcall c-name m n a-ptr a-layout tol)
                                  o-ptr :int32)))
        (with-simple-array-broadcast (solve 2 0) (a c-size) (out c-size)))
      out)))

(defun out-shape-for-rank (a)
  (declare (type simple-array a)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (if (< rank 2)
        (error "Computing rank needs an array of at least two dimensions")
        (subseq (narray-dimensions a) 0 (- rank 2)))))

(defpolymorph (la:rank :inline t) ((a (simple-array <type>)) &key ((out null)) (tol 0))
    (or number (simple-array <type>))
  (declare (ignore out))
  (if (= 2 (nu:array-rank a))
      (pflet ((a a))
        (declare (type (simple-array <type> 2) a))
        (la:rank a :tol tol))
      (pflet ((out (nu:empty (out-shape-for-rank a) :type '(signed-byte 32))))
        (declare (type (simple-array (signed-byte 32)) out))
        (la:rank a :out out :tol tol))))

(defpolymorph (la:rank :inline t) ((a list) &key ((out null)) (tol 0))
    (or number simple-array)
  (declare (ignore out))
  (la:rank (nu:asarray a :type nu:*default-float-format*) :tol tol))

(defpolymorph (la:rank :inline t) ((a list)
                                   &key ((out (simple-array (signed-byte 32))))
                                   (tol 0))
    (simple-array (signed-byte 32))
  (la:rank (nu:asarray a :type nu:*array-element-type*) :out out :tol tol))
