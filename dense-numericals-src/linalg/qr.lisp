(in-package :dense-numericals/linalg)

(define-polymorphic-function qr (array-like &key q r) :overwrite t
  :documentation "Calculate the QR decomposition of ARRAY-LIKE.")

(defpolymorph out-shape-compatible-p ((name (eql qr)) a q r) boolean
  (declare (ignore name)
           (type simple-array a q r)
           (optimize speed))
  (and (equal (narray-dimensions a) (narray-dimensions r))
       (let ((rank (array-rank a)))
         (and (cl:= rank (array-rank q))
              (if (< rank 2)
                  nil
                  (and (loop :for d1 :of-type (integer 0 #.array-dimension-limit)
                               :in (narray-dimensions a)
                             :for d2 :of-type (integer 0 #.array-dimension-limit)
                               :in (narray-dimensions q)
                             :repeat (- rank 2)
                             :always (cl:= d1 d2))
                       (cl:= (array-dimension a (- rank 2))
                             (array-dimension q (- rank 2)))
                       (cl:= (array-dimension q (- rank 1))
                             (array-dimension q (- rank 2)))))))))

(defpolymorph (qr :inline t) ((a (simple-array <type>))
                              &key ((q (simple-array <type>)))
                              ((r (simple-array <type>))))
    (values (simple-array <type>) (simple-array <type>))
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (out-shape-compatible-p 'qr a q r)
                  ()
                  "Incompatible shapes of Q ~A and R ~A~%to compute QR decomposition of array of shape ~A"
                  (narray-dimensions q) (narray-dimensions r) (narray-dimensions a)))
    (let* ((c-size (c-size <type>))
           (a-layout (eigen-array-layout a))
           (q-layout (eigen-array-layout q))
           (r-layout (eigen-array-layout r))
           (rank   (array-rank a))
           (m (array-dimension a (- rank 2)))
           (n (array-dimension a (- rank 1)))
           (c-name (c-name <type> 'qr)))
      (flet ((qr (a-size a-ptr a-dims q-ptr q-dims r-ptr r-dims)
               (declare (ignore a-size a-dims q-dims r-dims))
               (inline-or-funcall c-name m n
                                  a-ptr a-layout
                                  q-ptr q-layout
                                  r-ptr r-layout)))
        (with-simple-array-broadcast (qr 2 2 2) (a c-size) (q c-size) (r c-size)))
      (values q r))))

(defpolymorph out-shape ((name (eql q)) a) list
  (declare (ignore name)
           (type simple-array a)
           (optimize speed))
  (let ((rank (array-rank a)))
    (if (< rank 2)
        (error "QR decomposition requires array of rank at least 2")
        (nconc (subseq (narray-dimensions a) 0 (- rank 2))
               (list (array-dimension a (- rank 2))
                     (array-dimension a (- rank 2)))))))

(defpolymorph out-shape ((name (eql r)) a) list
  (declare (ignore name)
           (type simple-array a)
           (optimize speed))
  (array-dimensions a))

(defpolymorph (qr :inline t) ((a (simple-array <type>))
                                 &key ((q null)) ((r null)))
    (or number (simple-array <type>))
  (declare (ignore q r))
  (pflet ((q (empty (out-shape 'q a) :type <type>))
          (r (empty (out-shape 'r a) :type <type>)))
    (declare (type (simple-array <type>) q r))
    (qr a :q q :r r)))

(defpolymorph (qr :inline t) ((a list) &key ((q null)) ((r null)))
    simple-array
  (declare (ignore q r))
  (qr (asarray a :type *default-float-format*)))

(defpolymorph (qr :inline t) ((a list)
                                 &key ((q (simple-array <type>)))
                                 ((r (simple-array <type>))))
    (simple-array <type>)
  (qr (asarray a :type <type>) :q q :r r))
