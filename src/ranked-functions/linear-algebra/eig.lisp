(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function la:eigvals (array-like &key eigvals)
  :overwrite t
  :documentation "Use Eigen::EigenSolver to compute the eigenvalues of the 2D square matrix
given by ARRAY-LIKE.")

(defun out-shape-compatible-for-eigvals-p (array eigvals)
  (declare (optimize speed)
           (type nu:simple-array array eigvals))
  (let ((rank (nu:array-rank array)))
    (and (= (- rank 1) (nu:array-rank eigvals))
         (loop :repeat (- rank 1)
               :for d1 :of-type size :in (narray-dimensions array)
               :for d2 :of-type size :in (narray-dimensions eigvals)
               :always (= d1 d2))
         (= (array-dimension array (- rank 1))
            (array-dimension array (- rank 2))))))

(defun out-shape-for-eigvals (array)
  (declare (optimize speed)
           (type nu:simple-array array))
  (let ((rank (nu:array-rank array)))
    (if (= (array-dimension array (- rank 1))
           (array-dimension array (- rank 2)))
        (subseq (narray-dimensions array) 0 (- rank 1))
        (error "Cannot compute eigenvalues of non-square matrices"))))

(macrolet
    ((def (c-name type size)
       `(progn
          (defpolymorph la:eigvals
              ((a (simple-array ,type))
               &key ((eigvals (simple-array (complex ,type)))))
              (simple-array (complex ,type))
            (policy-cond:with-expectations (= 0 safety)
                ((assertion (out-shape-compatible-for-eigvals-p a eigvals)
                            ()
                            "Cannot compute eigenvalues of array of shape ~A~%into array of shape ~A."
                            (narray-dimensions a) (narray-dimensions eigvals)))
              (let* ((a-layout (eigen-array-layout a))
                     (rank (nu:array-rank a))
                     (n (nu:array-dimension a (- rank 2))))
                (flet ((eigvals (a-size a-ptr a-dims e-ptr e-dims)
                         (declare (ignore a-size a-dims e-dims))
                         (,c-name n a-ptr a-layout e-ptr)))
                  (with-simple-array-broadcast (eigvals 2 1) (a ,size) (eigvals ,(* 2 size))))
                eigvals)))
          (defpolymorph (la:eigvals :inline t) ((a (simple-array ,type))
                                                &key ((eigvals null)))
              (simple-array (complex ,type))
            (declare (ignore eigvals))
            (let ((eigvals (nu:empty (out-shape-for-eigvals a) :type '(complex ,type))))
              (declare (type (simple-array (complex ,type)) eigvals))
              (la:eigvals a :eigvals eigvals))))))
  (def ceigen-lite:seigvals single-float 4)
  (def ceigen-lite:deigvals double-float 8))

(defun float-type-from-complex-type (complex-type)
  (declare (optimize speed))
  (cond ((cl:subtypep complex-type '(complex single-float))
         'single-float)
        ((cl:subtypep complex-type '(complex double-float))
         'double-float)
        (t t)))

(defpolymorph (la:eigvals :inline t) ((a list) &key ((eigvals null)))
    simple-array
  (declare (ignore eigvals))
  (la:eigvals (nu:asarray a :type nu:*default-float-format*)))

(defpolymorph (la:eigvals :inline t) ((a list) &key ((eigvals (simple-array <type>))))
    (simple-array <type>)
  (la:eigvals (nu:asarray a :type (float-type-from-complex-type <type>)) :eigvals eigvals))



(define-polymorphic-function la:eigvecs (array-like &key eigvals eigvecs)
  :overwrite t
  :documentation "Use Eigen::EigenSolver to compute the eigenvalues and
eigvectors of the 2D square matrix given by ARRAY-LIKE.")

(defun out-shape-compatible-for-eigvecs-p (array eigvals eigvecs)
  (declare (optimize speed)
           (type nu:simple-array array eigvals eigvecs))
  (let ((rank (nu:array-rank array)))
    (and (equal (narray-dimensions array)
                (narray-dimensions eigvecs))
         (= (- rank 1) (nu:array-rank eigvals))
         (loop :repeat (- rank 1)
               :for d1 :of-type size :in (narray-dimensions array)
               :for d2 :of-type size :in (narray-dimensions eigvals)
               :always (= d1 d2))
         (= (array-dimension array (- rank 1))
            (array-dimension array (- rank 2))))))

(defun out-shape-for-eigvecs (array)
  (declare (optimize speed)
           (type nu:simple-array array))
  (let ((rank (nu:array-rank array)))
    (if (= (array-dimension array (- rank 1))
           (array-dimension array (- rank 2)))
        (values (subseq (narray-dimensions array) 0 (- rank 1))
                (array-dimensions array))
        (error "Cannot compute eigenvalues of non-square matrices"))))

(macrolet
    ((def (c-name type size)
       `(progn

          (defpolymorph la:eigvecs

              ((a (simple-array ,type))
               &key ((eigvals (simple-array (complex ,type))))
               ((eigvecs (simple-array (complex ,type)))))

              (values (simple-array (complex ,type))
                      (simple-array (complex ,type)))

            (policy-cond:with-expectations (= 0 safety)
                ((assertion (out-shape-compatible-for-eigvecs-p a eigvals eigvecs)
                            ()
                            "Cannot compute eigenvectors and eigenvalues of array~%of shape ~A~%into arrays of shape ~A and ~A."
                            (narray-dimensions a) (narray-dimensions eigvecs) (narray-dimensions eigvals)))
              (let* ((a-layout (eigen-array-layout a))
                     (v-layout (eigen-array-layout eigvecs))
                     (rank (nu:array-rank a))
                     (n (nu:array-dimension a (- rank 2))))
                (flet ((eigvecs (a-size a-ptr a-dims e-ptr e-dims v-ptr v-dims)
                         (declare (ignore a-size a-dims e-dims v-dims))
                         (,c-name n a-ptr a-layout e-ptr v-ptr v-layout)))
                  (with-simple-array-broadcast (eigvecs 2 1 2)
                    (a ,size) (eigvals ,(* 2 size)) (eigvecs ,(* 2 size))))
                (values eigvecs eigvals))))

          (defpolymorph (la:eigvecs :inline t) ((a (simple-array ,type))
                                                &key ((eigvals null))
                                                ((eigvecs null)))
              (values (simple-array (complex ,type))
                      (simple-array (complex ,type)))
            (declare (ignore eigvecs eigvals))
            (multiple-value-bind (eval-shape evec-shape)
                (out-shape-for-eigvecs a)
              (let ((eigvals (nu:empty eval-shape :type '(complex ,type)))
                    (eigvecs (nu:empty evec-shape :type '(complex ,type))))
                (declare (type (simple-array (complex ,type)) eigvecs eigvals))
                (la:eigvecs a :eigvals eigvals :eigvecs eigvecs)))))))
  (def ceigen-lite:seigvecs single-float 4)
  (def ceigen-lite:deigvecs double-float 8))

(defun float-type-from-complex-type (complex-type)
  (declare (optimize speed))
  (cond ((cl:subtypep complex-type '(complex single-float))
         'single-float)
        ((cl:subtypep complex-type '(complex double-float))
         'double-float)
        (t (error "Expected COMPLEX type to be a COMPLEX SINGLE-FLOAT or COMPLEX DOUBLE-FLOAT"))))

(defpolymorph (la:eigvecs :inline t) ((a list) &key ((eigvecs null)) ((eigvals null)))
    simple-array
  (declare (ignore eigvecs eigvals))
  (la:eigvecs (nu:asarray a :type nu:*default-float-format*)))

(defpolymorph (la:eigvecs :inline t) ((a list) &key
                                      ((eigvals (simple-array <type>)))
                                      ((eigvecs (simple-array <type>))))
    (values (simple-array <type>) (simple-array <type>))
  (la:eigvecs (nu:asarray a :type (float-type-from-complex-type <type>))
              :eigvals eigvals
              :eigvecs eigvecs))
