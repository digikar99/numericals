(in-package :numericals/linalg)

(5am:in-suite :numericals)

(define-polymorphic-function inv (array-like &key out) :overwrite t
  :documentation "Calculate the inverse of 2D matrices given by ARRAY-LIKE")

(defpolymorph out-shape-compatible-p ((name (eql inv)) a out) boolean
  (declare (type simple-array a out)
           (optimize speed))
  (let ((rank (array-rank a)))
    (and (cl:= rank (array-rank out))
         (if (< rank 2)
             nil
             (and (equal (narray-dimensions a)
                         (narray-dimensions out))
                  (cl:= (array-dimension a (- rank 1))
                        (array-dimension a (- rank 2))))))))

(defpolymorph (inv :inline t) ((a (simple-array <type>)) &key ((out (simple-array <type>))))
    (simple-array <type>)
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (out-shape-compatible-p 'inv a out)
                  ()
                  "Cannot compute inverse of array of shape ~A~%into array of shape ~A."
                  (narray-dimensions a) (narray-dimensions out)))
    (let* ((c-size (c-size <type>))
           (a-layout (eigen-array-layout a))
           (o-layout (eigen-array-layout out))
           (rank   (array-rank a))
           (m (array-dimension a (- rank 2)))
           (c-name (c-name <type> 'inv)))
      (flet ((inv (a-size a-ptr a-dims o-ptr o-dims)
               (declare (ignore a-size a-dims o-dims))
               (inline-or-funcall c-name m a-ptr a-layout o-ptr o-layout)))
        (with-simple-array-broadcast (inv 2 2) (a c-size) (out c-size)))
      out)))

(defpolymorph out-shape ((name (eql inv)) a) list
  (declare (type simple-array a)
           (optimize speed))
  (array-dimensions a))

(defpolymorph (inv :inline t) ((a (simple-array <type>)) &key ((out null)))
    (or number (simple-array <type>))
  (declare (ignore out))
  (pflet ((out (empty (out-shape 'inv a) :type <type>)))
    (declare (type (simple-array <type>) out))
    (inv a :out out)))

(defpolymorph (inv :inline t) ((a list) &key ((out null)))
    (or number simple-array)
  (declare (ignore out))
  (inv (asarray a :type *default-float-format*)))

(defpolymorph (inv :inline t) ((a list) &key ((out (simple-array <type>))))
    (simple-array <type>)
  (inv (asarray a :type <type>) :out out))
