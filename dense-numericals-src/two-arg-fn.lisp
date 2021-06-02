(in-package :dense-numericals.impl)

(define-polymorphic-function two-arg-fn (name x y &key out) :overwrite t)

(defpolymorph two-arg-fn ((name symbol)
                          (x number) (y number) &key ((out null) nil))
    (values number &optional)
  (declare (ignore out)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn name (asarray x) (asarray y) :out out))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn name x (asarray y) :out out))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y number) &key ((out array)))
    (values array &optional)
  (two-arg-fn name (asarray x) y :out out))

(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name (asarray x) (asarray y)))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name x (asarray y)))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y number) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name (asarray x) y))



;; single-float - 4 polymorphs

(defpolymorph two-arg-fn
    ((name symbol) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (zeros (narray-dimensions x) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (with-thresholded-multithreading (array-total-size x)
      (x out)
    (cffi:with-foreign-pointer (ptr-y 4)
      (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
      (let ((single-float-c-name (single-float-c-name name)))
        (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                  (ptr-o 4 io out))
          (funcall single-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io)))))
  out)

(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y (array single-float))
     &key ((out (array single-float))
           (zeros (narray-dimensions y) :type 'single-float)))
    (array single-float)
  (two-arg-fn name y x :out out)
  out)


(defpolymorph two-arg-fn
    ((name symbol) (x (array single-float)) (y (array single-float))
     &key ((out (or null (array single-float)))))
    (array single-float)
  (declare (ignorable name))
  (unless (and out
               (equalp (narray-dimensions x)
                       (narray-dimensions y)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x y)
      (assert broadcast-compatible-p (x y)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x y))
              :array-likes (list x y))
      (setq x (broadcast-array x broadcast-dimensions))
      (setq y (broadcast-array y broadcast-dimensions))
      (setq out (or out (zeros broadcast-dimensions :type 'single-float)))))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((single-float-c-name (single-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                (ptr-y 4 iy y)
                                (ptr-o 4 io out))
        (funcall single-float-c-name
                 n
                 ptr-x ix
                 ptr-y iy
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn
    ((name symbol) (x (simple-array single-float)) (y (simple-array single-float))
     &key ((out (or null (simple-array single-float)))))
    (simple-array single-float)
  (declare (ignorable name))
  (let ((single-float-c-name (single-float-c-name name)))
    (if (and out
             (equalp (narray-dimensions x)
                     (narray-dimensions y))
             (equalp (narray-dimensions out)
                     (narray-dimensions y)))
        (progn
          (with-thresholded-multithreading (array-total-size (the array out))
              (:simple x y out)
            (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                            (ptr-y (array-storage y))
                                            (ptr-o (array-storage out)))
              (cffi:incf-pointer ptr-x (* 4 (array-total-offset x)))
              (cffi:incf-pointer ptr-y (* 4 (array-total-offset y)))
              (cffi:incf-pointer ptr-o (* 4 (array-total-offset out)))
              (funcall single-float-c-name
                       (array-total-size (the array out))
                       ptr-x 1
                       ptr-x 1
                       ptr-o 1)))
          out)
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y)
          (assert broadcast-compatible-p (x y)
                  'incompatible-broadcast-dimensions
                  :dimensions (mapcar #'narray-dimensions (list x y))
                  :array-likes (list x y))
          (let ((x (broadcast-array x broadcast-dimensions))
                (y (broadcast-array y broadcast-dimensions))
                (out (or out (zeros broadcast-dimensions :type 'single-float))))
            (declare (type (array single-float) x y out))
            (with-thresholded-multithreading (array-total-size (the array out))
                (x y out)
              (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                        (ptr-y 4 iy y)
                                        (ptr-o 4 io out))
                (funcall single-float-c-name
                         n
                         ptr-x ix
                         ptr-y iy
                         ptr-o io)))
            out)))))

;;; double-float - 4 polymorphs

(defpolymorph two-arg-fn
    ((name symbol) (x (array double-float)) (y number)
     &key ((out (array double-float))
           (zeros (narray-dimensions x) :type 'double-float)))
    (array double-float)
  (with-thresholded-multithreading (array-total-size x)
      (x out)
    (cffi:with-foreign-pointer (ptr-y 8)
      (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
      (let ((double-float-c-name (double-float-c-name name)))
        (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                  (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io)))))
  out)

(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y (array double-float))
     &key ((out (array double-float))
           (zeros (narray-dimensions y) :type 'double-float)))
    (array double-float)
  (two-arg-fn name y x :out out)
  out)


(defpolymorph two-arg-fn
    ((name symbol) (x (array double-float)) (y (array double-float))
     &key ((out (or null (array double-float)))))
    (array double-float)
  (unless (and out
               (equalp (narray-dimensions x)
                       (narray-dimensions y)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x y)
      (assert broadcast-compatible-p (x y)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x y))
              :array-likes (list x y))
      (setq x (broadcast-array x broadcast-dimensions))
      (setq y (broadcast-array y broadcast-dimensions))
      (setq out (or out (zeros broadcast-dimensions :type 'double-float)))))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((double-float-c-name (double-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                (ptr-y 8 iy y)
                                (ptr-o 8 io out))
        (funcall double-float-c-name
                 n
                 ptr-x ix
                 ptr-y iy
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn
    ((name symbol) (x (simple-array double-float)) (y (simple-array double-float))
     &key ((out (or null (simple-array double-float)))))
    (simple-array double-float)
  (let ((double-float-c-name (double-float-c-name name)))
    (if (and out
             (equalp (narray-dimensions x)
                     (narray-dimensions y))
             (equalp (narray-dimensions out)
                     (narray-dimensions y)))
        (progn
          (with-thresholded-multithreading (array-total-size (the array out))
              (:simple x y out)
            (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                            (ptr-y (array-storage y))
                                            (ptr-o (array-storage out)))
              (cffi:incf-pointer ptr-x (* 8 (array-total-offset x)))
              (cffi:incf-pointer ptr-y (* 8 (array-total-offset y)))
              (cffi:incf-pointer ptr-o (* 8 (array-total-offset out)))
              (funcall double-float-c-name
                       (array-total-size (the array out))
                       ptr-x 1
                       ptr-y 1
                       ptr-o 1)))
          out)
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y)
          (assert broadcast-compatible-p (x y)
                  'incompatible-broadcast-dimensions
                  :dimensions (mapcar #'narray-dimensions (list x y))
                  :array-likes (list x y))
          (let ((x (broadcast-array x broadcast-dimensions))
                (y (broadcast-array y broadcast-dimensions))
                (out (or out (zeros broadcast-dimensions :type 'double-float))))
            (declare (type (array double-float) x y out))
            (with-thresholded-multithreading (array-total-size (the array out))
                (x y out)
              (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                        (ptr-y 8 iy y)
                                        (ptr-o 8 io out))
                (funcall double-float-c-name
                         n
                         ptr-x ix
                         ptr-y iy
                         ptr-o io)))
            out)))))


(define-polymorphic-function dn:expt (base power &key out) :overwrite t)
(defpolymorph dn:expt (base power &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'dn:expt base power))
(defpolymorph dn:expt (base power &key ((out (not null)))) t
  (two-arg-fn 'dn:expt base power :out out))
(define-numericals-two-arg-test dn:expt array
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))


(defpolymorph dn:atan (x y &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'dn::atan2 x y))
(defpolymorph dn:atan (x y &key ((out (not null)))) t
  (two-arg-fn 'dn::atan2 x y :out out))

(define-polymorphic-function dn::atan2 (x y &key out) :overwrite t)
(defpolymorph dn::atan2 (x y &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'dn::atan2 x y))
(defpolymorph dn::atan2 (x y &key ((out (not null)))) t
  (two-arg-fn 'dn::atan2 x y :out out))
(define-numericals-two-arg-test dn::atan2 array
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))
