(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-polymorphic-function la:svd (array-like &key u s v) :overwrite t
  :documentation "Calculate the SVD decomposition of ARRAY-LIKE using Eigen::BDCSVD.
For input m-by-n input A, it returns U, S, and V such that

                         A = U S V^H

where
  U is a m-by-m unitary,
  V is a n-by-n unitary,
  and S is a m-by-n real positive matrix which is zero outside of its main diagonal

The diagonal entries of S are known as the singular values of A and the columns of U and V are known as the left and right singular vectors of A respectively.

The following code illustrates the decomposition and the reconstruction (FIXME: Update):

```lisp
    (let ((a (nu:asarray '((1 2 3) (4 5 6)) :type 'single-float)))
      (multiple-value-bind (p lu q) (lu a)
        (print lu)
        (nu:matmul (la:inv p)
                   (nu:asarray '((1 0 0) (0.5 1 0)) :type 'single-float)   ; unit lower triangular
                   (nu:asarray '((6 4 5) (0 -1 -0.5)) :type 'single-float) ; upper triangular
                   (la:inv q))))

    #|

    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  6.000       4.000       5.000    )
      (  0.500      -1.000      -0.500    )
       {101110E403}>
    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  1.000       2.000       3.000    )
      (  4.000       5.000       6.000    )
       {101110E893}>

    |#
```

References:

- https://eigen.tuxfamily.org/dox/classEigen_1_1BDCSVD.html
- https://eigen.tuxfamily.org/dox/classEigen_1_1JacobiSVD.html
")

(defun out-shape-compatible-for-svd-p (a u s v)
  (declare (type simple-array a u s v)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (and (= rank (nu:array-rank u) (nu:array-rank s) (nu:array-rank v))
         (if (< rank 2)
             nil
             (and (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions a)
                        :for d2 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions u)
                        :for d3 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions s)
                        :for d4 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions v)
                        :repeat (- rank 2)
                        :always (= d1 d2 d3 d4))
                  (= (array-dimension u (- rank 1))
                     (array-dimension u (- rank 2)))
                  (= (array-dimension v (- rank 1))
                     (array-dimension v (- rank 2)))
                  (= (array-dimension a  (- rank 2))
                     (array-dimension s (- rank 2)))
                  (= (array-dimension a  (- rank 1))
                     (array-dimension s (- rank 1))))))))

(defpolymorph la:svd ((a (simple-array <type>))
                     &key ((s (simple-array <type>)))
                     ((u (simple-array <type>)))
                     ((v (simple-array <type>))))
    (values (simple-array <type>)
            (simple-array <type>)
            (simple-array <type>))
  (let* ((c-size (c-size <type>))
         (a-layout (eigen-array-layout a))
         (u-layout (eigen-array-layout u))
         (v-layout (eigen-array-layout v))
         (rank   (nu:array-rank a))
         (m (nu:array-dimension a (- rank 2)))
         (n (nu:array-dimension a (- rank 1)))
         (c-name (c-name <type> 'la:svd))
         (zero (coerce 0 <type>)))
    (policy-cond:with-expectations (= 0 safety)
      ((assertion (out-shape-compatible-for-svd-p a u s v)
                  ()
                  "Incompatible shapes of U, S, V to compute S decomposition of array of shape ~A"
                  (narray-dimensions a)))
      (flet ((s (a-size a-ptr a-dims u-ptr u-dims s-ptr s-dims v-ptr v-dims)
               (declare (ignore a-size a-dims u-dims s-dims v-dims))
               (inline-or-funcall c-name m n
                                  a-ptr a-layout
                                  u-ptr u-layout
                                  v-ptr v-layout
                                  s-ptr)))
        (with-simple-array-broadcast (s 2 2 2 2)
          (a c-size) (u c-size) (s c-size) (v c-size)))
      (pflet ((sv (array-storage s)))
        (declare (type (cl:simple-array <type> 1) sv))
        (dotimes (i (min m n))
          (funcall #'(setf nu:aref) (nu:aref sv i)
                   s i i)
          (unless (zerop i)
            (funcall #'(setf nu:aref) zero sv i)))))
    (values u s v)))

(defun out-shape-for-svd (a)
  (declare (type simple-array a)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (if (< rank 2)
        (error "S decomposition requires array of rank at least 2")
        (values
         (nconc (subseq (narray-dimensions a) 0 (- rank 2))
                (list (array-dimension a (- rank 2))
                      (array-dimension a (- rank 2))))
         (array-dimensions a)
         (nconc (subseq (narray-dimensions a) 0 (- rank 2))
                (list (array-dimension a (- rank 1))
                      (array-dimension a (- rank 1))))))))

(defpolymorph (la:svd :inline t) ((a (simple-array <type>))
                                 &key ((u null)) ((s null)) ((v null)))
    (values (simple-array <type>) (simple-array <type>) (simple-array <type>))
  (declare (ignore u s v))
  (multiple-value-bind (u-shape s-shape v-shape)
      (out-shape-for-lu a)
    (pflet ((s (nu:empty s-shape :type <type>))
            (u (nu:empty u-shape :type <type>))
            (v (nu:empty v-shape :type <type>)))
      (declare (type (simple-array <type>) s u v))
      (la:svd a :s s :u u :v v))))

(defpolymorph (la:svd :inline t) ((a list) &key ((u null)) ((s null)) ((v null)))
    simple-array
  (declare (ignore u s v))
  (la:svd (nu:asarray a :type nu:*default-float-format*)))

(defpolymorph (la:svd :inline t) ((a list)
                                 &key ((u (simple-array <type>)))
                                 ((s (simple-array <type>)))
                                 ((v (simple-array <type>))))
    (simple-array <type>)
  (la:svd (nu:asarray a :type <type>) :u u :s s :v v))
