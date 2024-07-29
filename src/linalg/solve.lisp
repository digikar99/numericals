(in-package :numericals/linalg)

(5am:in-suite :numericals)

(define-polymorphic-function solve (a b &key out)
  :overwrite t
  :documentation "Solves a system of linear equation A*X = B and returns X as the output.
At the time of this writing, it uses the Eigen::partialPivQr for square matrices Eigen::householderQR for non-square matrices.

References:

1. Eigen::ColPivHouseholderQR documentation:
https://eigen.tuxfamily.org/dox/classEigen_1_1ColPivHouseholderQR.html#ad4825c3d43dffdf0c883de09ba891646 for more details

2. Eigen Linear Algebra Tutorial:
https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html")

(defpolymorph out-shape-compatible-p ((name (eql solve)) a b out) boolean
  (declare (ignore name)
           (type simple-array a b out)
           (optimize speed))
  (let ((rank (array-rank a)))
    (and (cl:= (- rank 1) (array-rank b) (array-rank out))
         (if (< rank 2)
             nil
             (and (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions a)
                        :for d2 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions b)
                        :for d3 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions out)
                        :repeat (- rank 2)
                        :always (cl:= d1 d2 d3))
                  (cl:= (array-dimension a (- rank 1))
                        (array-dimension out (- rank 2)))
                  (cl:= (array-dimension a (- rank 2))
                        (array-dimension b (- rank 2))))))))

(defpolymorph (solve :inline t) ((a (simple-array <type>))
                                 (b (simple-array <type>))
                                 &key ((out (simple-array <type>))))
    (simple-array <type>)
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (out-shape-compatible-p 'solve a b out)
                  ()
                  "Arrays of shape ~A and ~A cannot be~%solved to produce an array of shape ~A."
                  (narray-dimensions a) (narray-dimensions b) (narray-dimensions out)))
    (let* ((c-size (c-size <type>))
           (a-layout (ecase (array-layout a)
                       (:row-major 82)
                       (:column-major 67)))
           (rank   (array-rank a))
           (m (array-dimension a (- rank 2)))
           (n (array-dimension a (- rank 1)))
           (c-name (if (cl:= m n)
                       (c-name <type> 'solve/square)
                       (c-name <type> 'solve))))
      (flet ((solve (a-size a-ptr a-dims b-ptr b-dims o-ptr o-dims)
               (declare (ignore a-size a-dims b-dims o-dims))
               (inline-or-funcall c-name m n
                                  a-ptr a-layout
                                  b-ptr o-ptr)))
        (with-simple-array-broadcast (solve 2 1 1) (a c-size) (b c-size) (out c-size)))
      out)))

(defpolymorph out-shape ((name (eql solve)) a b) list
  (declare (ignore name)
           (type simple-array a b)
           (optimize speed))
  (let ((rank (array-rank a)))
    (assert (cl:= (- rank 1) (array-rank b)))
    (if (< rank 2)
        (error "Solve requires arrays of at least rank 2")
        (nconc (if (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                           :in (narray-dimensions a)
                         :for d2 :of-type (integer 0 #.array-dimension-limit)
                           :in (narray-dimensions b)
                         :repeat (- rank 2)
                         :always (cl:= d1 d2))
                   (subseq (array-dimensions a) 0 (- rank 2))
                   (error "Incompatible dimensions for matmul"))
               (list (array-dimension a (- rank 1)))))))

(defpolymorph (solve :inline t) ((a (simple-array <type>))
                                 (b (simple-array <type>))
                                 &key ((out null)))
    (simple-array <type>)
  (declare (ignore out))
  (pflet ((out (empty (out-shape 'solve a b) :type <type>)))
    (declare (type (simple-array <type>) out))
    (solve a b :out out)
    out))

(defpolymorph (solve :inline nil) (a b &key out)
    simple-array
  (solve (asarray a :type *default-float-format*)
            (asarray b :type *default-float-format*)
            :out out))

(5am:def-test solve ()
  (let ((cl-array-p (eq 'array 'cl:array)))
    (dolist (*array-element-type* `(single-float double-float))
      (dolist (*array-layout* (if cl-array-p
                                  `(:row-major)
                                  `(:row-major :column-major)))
        (5am:is (array= (asarray '(-2 1 1))
                           (solve (asarray '((1 2 3)
                                                   (4 5 6)
                                                   (7 8 10)))
                                     (asarray '(3 3 4)))
                           :test #'float-close-p))
        (5am:is (array= (asarray '(-2.5 2.667))
                           (solve (asarray '((1 2)
                                                   (4 5)
                                                   (7 8)))
                                     (asarray '(3 3 4)))
                           :test #'float-close-p))))))
