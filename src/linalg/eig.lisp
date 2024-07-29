(in-package :numericals/linalg)

(5am:in-suite :numericals)

(define-polymorphic-function eigvals (array-like &key eigvals)
  :overwrite t
  :documentation "Use Eigen::EigenSolver to compute the eigenvalues of the 2D square matrix
given by ARRAY-LIKE.")

(defpolymorph out-shape-compatible-p ((name (eql eigvals)) array eigvals) boolean
  (declare (optimize speed)
           (ignore name)
           (type simple-array array eigvals))
  (let ((rank (array-rank array)))
    (and (cl:= (- rank 1) (array-rank eigvals))
         (loop :repeat (- rank 1)
               :for d1 :of-type size :in (narray-dimensions array)
               :for d2 :of-type size :in (narray-dimensions eigvals)
               :always (cl:= d1 d2))
         (cl:= (array-dimension array (- rank 1))
               (array-dimension array (- rank 2))))))

(defpolymorph out-shape ((name (eql eigvals)) array) list
  (declare (optimize speed)
           (type simple-array array))
  (let ((rank (array-rank array)))
    (if (cl:= (array-dimension array (- rank 1))
              (array-dimension array (- rank 2)))
        (subseq (narray-dimensions array) 0 (- rank 1))
        (error "Cannot compute eigenvalues of non-square matrices"))))

(macrolet
    ((def (c-name type size)
       `(progn
          (defpolymorph (eigvals :inline t)
              ((a (simple-array ,type))
               &key ((eigvals (simple-array (complex ,type)))))
              (simple-array (complex ,type))
            (policy-cond:with-expectations (cl:= 0 safety)
                ((assertion (out-shape-compatible-p 'eigvals a eigvals)
                            ()
                            "Cannot compute eigenvalues of array of shape ~A~%into array of shape ~A."
                            (narray-dimensions a) (narray-dimensions eigvals)))
              (let* ((a-layout (eigen-array-layout a))
                     (rank (array-rank a))
                     (n (array-dimension a (- rank 2))))
                (flet ((eigvals (a-size a-ptr a-dims e-ptr e-dims)
                         (declare (ignore a-size a-dims e-dims))
                         (,c-name n a-ptr a-layout e-ptr)))
                  (with-simple-array-broadcast (eigvals 2 1) (a ,size) (eigvals ,(* 2 size))))
                eigvals)))
          (defpolymorph (eigvals :inline t) ((a (simple-array ,type))
                                                &key ((eigvals null)))
              (simple-array (complex ,type))
            (declare (ignore eigvals))
            (let ((eigvals (empty (out-shape 'eigvals a) :type '(complex ,type))))
              (declare (type (simple-array (complex ,type)) eigvals))
              (eigvals a :eigvals eigvals))))))
  (def ceigen-lite:seigvals single-float 4)
  (def ceigen-lite:deigvals double-float 8))

(defun float-type-from-complex-type (complex-type)
  (declare (optimize speed))
  (cond ((cl:subtypep complex-type '(complex single-float))
         'single-float)
        ((cl:subtypep complex-type '(complex double-float))
         'double-float)
        (t t)))

(defpolymorph (eigvals :inline t) ((a list) &key ((eigvals null)))
    simple-array
  (declare (ignore eigvals))
  (eigvals (asarray a :type *default-float-format*)))

(defpolymorph (eigvals :inline t) ((a list) &key ((eigvals (simple-array <type>))))
    (simple-array <type>)
  (eigvals (asarray a :type (float-type-from-complex-type <type>)) :eigvals eigvals))



(define-polymorphic-function eigvecs (array-like &key eigvals eigvecs)
  :overwrite t
  :documentation "Use Eigen::EigenSolver to compute the eigenvalues and
eigvectors of the 2D square matrix given by ARRAY-LIKE.")

(defpolymorph out-shape-compatible-p ((name (eql eigvecs)) array eigvals eigvecs)
    boolean
  (declare (optimize speed)
           (ignore name)
           (type simple-array array eigvals eigvecs))
  (let ((rank (array-rank array)))
    (and (equal (narray-dimensions array)
                (narray-dimensions eigvecs))
         (cl:= (- rank 1) (array-rank eigvals))
         (loop :repeat (- rank 1)
               :for d1 :of-type size :in (narray-dimensions array)
               :for d2 :of-type size :in (narray-dimensions eigvals)
               :always (cl:= d1 d2))
         (cl:= (array-dimension array (- rank 1))
               (array-dimension array (- rank 2))))))

(defpolymorph out-shape ((name (eql eigvecs)) array) boolean
  (declare (optimize speed)
           (ignore name)
           (type simple-array array))
  (let ((rank (array-rank array)))
    (if (cl:= (array-dimension array (- rank 1))
              (array-dimension array (- rank 2)))
        (values (subseq (narray-dimensions array) 0 (- rank 1))
                (array-dimensions array))
        (error "Cannot compute eigenvalues of non-square matrices"))))

(macrolet
    ((def (c-name type size)
       `(progn

          (defpolymorph (eigvecs :inline t)

              ((a (simple-array ,type))
               &key ((eigvals (simple-array (complex ,type))))
               ((eigvecs (simple-array (complex ,type)))))

              (values (simple-array (complex ,type))
                      (simple-array (complex ,type)))

            (policy-cond:with-expectations (cl:= 0 safety)
                ((assertion (out-shape-compatible-p 'eigvecs a eigvals eigvecs)
                            ()
                            "Cannot compute eigenvectors and eigenvalues of array~%of shape ~A~%into arrays of shape ~A and ~A."
                            (narray-dimensions a) (narray-dimensions eigvecs) (narray-dimensions eigvals)))
              (let* ((a-layout (eigen-array-layout a))
                     (v-layout (eigen-array-layout eigvecs))
                     (rank (array-rank a))
                     (n (array-dimension a (- rank 2))))
                (flet ((eigvecs (a-size a-ptr a-dims e-ptr e-dims v-ptr v-dims)
                         (declare (ignore a-size a-dims e-dims v-dims))
                         (,c-name n a-ptr a-layout e-ptr v-ptr v-layout)))
                  (with-simple-array-broadcast (eigvecs 2 1 2)
                    (a ,size) (eigvals ,(* 2 size)) (eigvecs ,(* 2 size))))
                (values eigvecs eigvals))))

          (defpolymorph (eigvecs :inline t) ((a (simple-array ,type))
                                                &key ((eigvals null))
                                                ((eigvecs null)))
              (values (simple-array (complex ,type))
                      (simple-array (complex ,type)))
            (declare (ignore eigvecs eigvals))
            (multiple-value-bind (eval-shape evec-shape)
                (out-shape 'eigvecs a)
              (let ((eigvals (empty eval-shape :type '(complex ,type)))
                    (eigvecs (empty evec-shape :type '(complex ,type))))
                (declare (type (simple-array (complex ,type)) eigvecs eigvals))
                (eigvecs a :eigvals eigvals :eigvecs eigvecs)))))))
  (def ceigen-lite:seigvecs single-float 4)
  (def ceigen-lite:deigvecs double-float 8))

(defpolymorph (eigvecs :inline t) ((a list) &key ((eigvecs null)) ((eigvals null)))
    simple-array
  (declare (ignore eigvecs eigvals))
  (eigvecs (asarray a :type *default-float-format*)))

(defpolymorph (eigvecs :inline t) ((a list) &key
                                      ((eigvals (simple-array <type>)))
                                      ((eigvecs (simple-array <type>))))
    (values (simple-array <type>) (simple-array <type>))
  (eigvecs (asarray a :type (float-type-from-complex-type <type>))
              :eigvals eigvals
              :eigvecs eigvecs))
