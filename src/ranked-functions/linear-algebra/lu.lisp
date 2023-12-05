(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-polymorphic-function la:lu (array-like &key p lu q) :overwrite t
  :documentation "Calculate the LU decomposition of ARRAY-LIKE using Eigen::FullPivLU.
For input A, it returns three matrices P, LU, and Q such that

                         A=P^{−1} L U Q^{−1}

where L is unit-lower-triangular, U is upper-triangular, and P and Q are permutation matrices.
The matrix LU contains L below the diagonal and U above the diagonal.

The following code illustrates the decomposition and the reconstruction

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

- https://eigen.tuxfamily.org/dox/classEigen_1_1FullPivLU.html")

(defun out-shape-compatible-for-lu-p (a p lu q)
  (declare (type simple-array a p lu q)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (and (cl:= rank (nu:array-rank p) (nu:array-rank lu) (nu:array-rank q))
         (if (< rank 2)
             nil
             (and (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions a)
                        :for d2 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions p)
                        :for d3 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions lu)
                        :for d4 :of-type (integer 0 #.array-dimension-limit)
                          :in (narray-dimensions q)
                        :repeat (- rank 2)
                        :always (cl:= d1 d2 d3 d4))
                  (cl:= (array-dimension p (- rank 1))
                        (array-dimension p (- rank 2)))
                  (cl:= (array-dimension q (- rank 1))
                        (array-dimension q (- rank 2)))
                  (cl:= (array-dimension a  (- rank 2))
                        (array-dimension lu (- rank 2)))
                  (cl:= (array-dimension a  (- rank 1))
                        (array-dimension lu (- rank 1))))))))

(defpolymorph la:lu ((a (simple-array <type>))
                     &key ((lu (simple-array <type>)))
                     ((p (simple-array <type>)))
                     ((q (simple-array <type>))))
    (values (simple-array <type>)
            (simple-array <type>)
            (simple-array <type>))
  (let* ((c-size (c-size <type>))
         (layout (eigen-array-layout lu))
         (rank   (nu:array-rank a))
         (m (nu:array-dimension a (- rank 2)))
         (n (nu:array-dimension a (- rank 1)))
         (c-name (c-name <type> 'la:lu)))
    (policy-cond:with-expectations (cl:= 0 safety)
        ((assertion (out-shape-compatible-for-lu-p a p lu q)
                    ()
                    "Incompatible shapes of P, LU, Q to compute LU decomposition of array of shape ~A"
                    (narray-dimensions a))
         (assertion (cl:= layout
                          (eigen-array-layout a)
                          (eigen-array-layout p)
                          (eigen-array-layout q))
                    ()
                    "Support for different array layouts is not provided"))
      (flet ((lu (a-size a-ptr a-dims p-ptr p-dims lu-ptr lu-dims q-ptr q-dims)
               (declare (ignore a-size a-dims p-dims lu-dims q-dims))
               (inline-or-funcall c-name m n layout
                                  a-ptr p-ptr lu-ptr q-ptr)))
        (with-simple-array-broadcast (lu 2 2 2 2)
          (a c-size) (p c-size) (lu c-size) (q c-size))))
    (values p lu q)))

(defun out-shape-for-lu (a)
  (declare (type simple-array a)
           (optimize speed))
  (let ((rank (nu:array-rank a)))
    (if (< rank 2)
        (error "LU decomposition requires array of rank at least 2")
        (values
         (nconc (subseq (narray-dimensions a) 0 (- rank 2))
                (list (array-dimension a (- rank 2))
                      (array-dimension a (- rank 2))))
         (array-dimensions a)
         (nconc (subseq (narray-dimensions a) 0 (- rank 2))
                (list (array-dimension a (- rank 1))
                      (array-dimension a (- rank 1))))))))

(defpolymorph (la:lu :inline t) ((a (simple-array <type>))
                                 &key ((p null)) ((lu null)) ((q null)))
    (values (simple-array <type>) (simple-array <type>) (simple-array <type>))
  (declare (ignore p lu q))
  (multiple-value-bind (p-shape lu-shape q-shape)
      (out-shape-for-lu a)
    (pflet ((lu (nu:empty lu-shape :type <type>))
            (p  (nu:empty p-shape  :type <type>))
            (q  (nu:empty q-shape  :type <type>)))
      (declare (type (simple-array <type>) lu p q))
      (la:lu a :lu lu :p p :q q))))

(defpolymorph (la:lu :inline t) ((a list) &key ((p null)) ((lu null)) ((q null)))
    simple-array
  (declare (ignore p lu q))
  (la:lu (nu:asarray a :type nu:*default-float-format*)))

(defpolymorph (la:lu :inline t) ((a list)
                                 &key ((p (simple-array <type>)))
                                 ((lu (simple-array <type>)))
                                 ((q (simple-array <type>))))
    (simple-array <type>)
  (la:lu (nu:asarray a :type <type>) :p p :lu lu :q q))
