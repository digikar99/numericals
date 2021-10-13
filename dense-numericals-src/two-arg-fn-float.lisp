(in-package :dense-numericals.impl)

(define-polymorphic-function two-arg-fn/float (name x y &key out broadcast) :overwrite t)

(defpolymorph two-arg-fn/float ((name symbol) (x number) (y number)
                                &key ((out null)) broadcast)
    (values number &optional)
  (declare (ignore out broadcast)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y list)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name (asarray x) (asarray y) :out out :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x number) (y list)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name x (asarray y :type (array-element-type out))
                    :out out
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y number)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name (asarray x :type (array-element-type out))
                    y :out
                    out :broadcast broadcast))

(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y list)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name
                    (asarray x :type dn:*default-float-format*)
                    (asarray y :type dn:*default-float-format*)
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x number) (y list)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name x (asarray y :type dn:*default-float-format*)
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y number)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name (asarray x :type dn:*default-float-format*) y
                    :broadcast broadcast))



;; single-float - 4 polymorphs

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (zeros (narray-dimensions x) :type 'single-float))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) dn:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (let ((single-float-c-name (single-float-c-name name)))
    (unless (equalp (narray-dimensions x)
                    (narray-dimensions out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x out)
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (setq x   (broadcast-array x   broadcast-dimensions))
      (setq out (broadcast-array out broadcast-dimensions))))
    (cffi:with-foreign-pointer (ptr-y 4)
      (setf (cffi:mem-ref ptr-y :float) (trivial-coerce:coerce y 'single-float))
      (with-thresholded-multithreading (array-total-size out)
          (x out)
        (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                  (ptr-o 4 io out))
          (funcall single-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array single-float)) 
     &key ((out (array single-float))
           (zeros (narray-dimensions y) :type 'single-float))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) dn:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (let ((single-float-c-name (single-float-c-name name)))
    (unless (equalp (narray-dimensions y)
                    (narray-dimensions out))
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p y out)
        (assert broadcast-compatible-p (y out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list y out))
                :array-likes (list y out))
        (setq y   (broadcast-array y   broadcast-dimensions))
        (setq out (broadcast-array out broadcast-dimensions))))
    (cffi:with-foreign-pointer (ptr-x 4)
      (setf (cffi:mem-ref ptr-x :float) (trivial-coerce:coerce X 'single-float))
      (with-thresholded-multithreading (array-total-size out)
          (y out)
        (ptr-iterate-but-inner n ((ptr-y 4 iy y)
                                  (ptr-o 4 io out))
          (funcall single-float-c-name
                   n
                   ptr-x 0
                   ptr-y iy
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array single-float)) (y (array single-float))
     ;; We are not producing OUT in the lambda-list because
     ;; it is non-trivial to work out the dimensions in a short space.
     ;; This work has been done just below.
     &key ((out (or null (array single-float))))
     (broadcast dn:*broadcast-automatically*))
    (array single-float)
  ;; We are not broadcasting OUT because doing so would mean
  ;; OUT would be written multiple times leading to all sorts of bad things
  (declare (ignorable name))
  (if (or (not broadcast)
          (equalp (narray-dimensions x)
                  (narray-dimensions y)))
      (setq out (or out (zeros (narray-dimensions x) :type 'single-float)))
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y out)
        (assert broadcast-compatible-p (x y out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y out))
                :array-likes (list x y out))
        (setq x (broadcast-array x broadcast-dimensions))
        (setq y (broadcast-array y broadcast-dimensions))
        (setq out (or out (zeros broadcast-dimensions :type 'single-float)))))
  ;; Why didn't we use multithreading here?
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions y))))
       (assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((single-float-c-name (single-float-c-name name)))
      (with-thresholded-multithreading (array-total-size (the array out))
          (x y out)
        (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                  (ptr-y 4 iy y)
                                  (ptr-o 4 io out))
          (funcall single-float-c-name
                   n
                   ptr-x ix
                   ptr-y iy
                   ptr-o io)))))
  out)

;; TODO: Measure the benefits of doing this
;; (defpolymorph two-arg-fn/float
;;     ((name symbol) (x (simple-array single-float)) (y (simple-array single-float))
;;      &key ((out (or null (simple-array single-float))))
;;      (broadcast dn:*broadcast-automatically*))
;;     (simple-array single-float)
;;   (declare (ignorable name))
;;   (let ((single-float-c-name (single-float-c-name name)))
;;     (if (and out
;;              (equalp (narray-dimensions x)
;;                      (narray-dimensions y))
;;              (equalp (narray-dimensions out)
;;                      (narray-dimensions y)))
;;         (progn
;;           (with-thresholded-multithreading (array-total-size (the array out))
;;               (:simple x y out)
;;             (with-pointers-to-vectors-data ((ptr-x (array-storage x))
;;                                             (ptr-y (array-storage y))
;;                                             (ptr-o (array-storage out)))
;;               (cffi:incf-pointer ptr-x (* 4 (array-total-offset x)))
;;               (cffi:incf-pointer ptr-y (* 4 (array-total-offset y)))
;;               (cffi:incf-pointer ptr-o (* 4 (array-total-offset out)))
;;               (funcall single-float-c-name
;;                        (array-total-size (the array out))
;;                        ptr-x 1
;;                        ptr-x 1
;;                        ptr-o 1)))
;;           out)
;;         (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
;;             (broadcast-compatible-p x y)
;;           (assert broadcast-compatible-p (x y)
;;                   'incompatible-broadcast-dimensions
;;                   :dimensions (mapcar #'narray-dimensions (list x y))
;;                   :array-likes (list x y))
;;           (let ((x (broadcast-array x broadcast-dimensions))
;;                 (y (broadcast-array y broadcast-dimensions))
;;                 (out (or out (zeros broadcast-dimensions :type 'single-float))))
;;             (declare (type (array single-float) x y out))
;;             (with-thresholded-multithreading (array-total-size (the array out))
;;                 (x y out)
;;               (ptr-iterate-but-inner n ((ptr-x 4 ix x)
;;                                         (ptr-y 4 iy y)
;;                                         (ptr-o 4 io out))
;;                 (funcall single-float-c-name
;;                          n
;;                          ptr-x ix
;;                          ptr-y iy
;;                          ptr-o io)))
;;             out)))))

;;; double-float - 4 polymorphs

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array double-float)) (y number)
     &key ((out (array double-float))
           (zeros (narray-dimensions x) :type 'double-float))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) dn:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (let ((double-float-c-name (double-float-c-name name)))
    (unless (equalp (narray-dimensions x)
                    (narray-dimensions out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x out)
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (setq x   (broadcast-array x   broadcast-dimensions))
      (setq out (broadcast-array out broadcast-dimensions))))
    (cffi:with-foreign-pointer (ptr-y 8)
      (setf (cffi:mem-ref ptr-y :double) (trivial-coerce:coerce y 'double-float))
      (with-thresholded-multithreading (array-total-size out)
          (x out)
        (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                  (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array double-float)) 
     &key ((out (array double-float))
           (zeros (narray-dimensions y) :type 'double-float))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) dn:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (let ((double-float-c-name (double-float-c-name name)))
    (unless (equalp (narray-dimensions y)
                    (narray-dimensions out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list y out))
              :array-likes (list y out))
      (setq y   (broadcast-array y   broadcast-dimensions))
      (setq out (broadcast-array out broadcast-dimensions))))
    (cffi:with-foreign-pointer (ptr-x 8)
      (setf (cffi:mem-ref ptr-x :double) (trivial-coerce:coerce X 'double-float))
      (with-thresholded-multithreading (array-total-size out)
          (y out)
        (ptr-iterate-but-inner n ((ptr-y 8 iy y)
                                  (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x 0
                   ptr-y iy
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array double-float)) (y (array double-float))
     ;; We are not producing OUT in the lambda-list because
     ;; it is non-trivial to work out the dimensions in a short space.
     ;; This work has been done just below.
     &key ((out (or null (array double-float))))
     (broadcast dn:*broadcast-automatically*))
    (array double-float)
  ;; We are not broadcasting OUT because doing so would mean
  ;; OUT would be written multiple times leading to all sorts of bad things
  (declare (ignorable name))
  (if (or (not broadcast)
          (equalp (narray-dimensions x)
                  (narray-dimensions y)))
      (setq out (or out (zeros (narray-dimensions x) :type 'double-float)))
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y out)
        (assert broadcast-compatible-p (x y out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y out))
                :array-likes (list x y out))
        (setq x (broadcast-array x broadcast-dimensions))
        (setq y (broadcast-array y broadcast-dimensions))
        (setq out (or out (zeros broadcast-dimensions :type 'double-float)))))
  ;; Why didn't we use multithreading here?
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions y))))
       (assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((double-float-c-name (double-float-c-name name)))
      (with-thresholded-multithreading (array-total-size (the array out))
          (x y out)
        (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                  (ptr-y 8 iy y)
                                  (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x ix
                   ptr-y iy
                   ptr-o io)))))
  out)

(define-polymorphic-function dn:expt (base power &key out broadcast) :overwrite t)
(defpolymorph dn:expt (base power &key ((out null)) (broadcast dn:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'dn:expt base power :broadcast broadcast))
(defpolymorph dn:expt (base power &key ((out (not null))) (broadcast dn:*broadcast-automatically*)) t
  (two-arg-fn/float 'dn:expt base power :out out :broadcast broadcast))
(define-numericals-two-arg-test dn:expt array t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))


(defpolymorph dn:atan (x y &key ((out null)) (broadcast dn:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'dn::atan2 x y :broadcast broadcast))
(defpolymorph dn:atan (x y &key ((out (not null))) (broadcast dn:*broadcast-automatically*)) t
  (two-arg-fn/float 'dn::atan2 x y :out out :broadcast broadcast))

(define-polymorphic-function dn::atan2 (x y &key out broadcast) :overwrite t)
(defpolymorph dn::atan2 (x y &key ((out null)) (broadcast dn:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'dn::atan2 x y :broadcast broadcast))
(defpolymorph dn::atan2 (x y &key ((out (not null))) (broadcast dn:*broadcast-automatically*)) t
  (two-arg-fn/float 'dn::atan2 x y :out out :broadcast broadcast))
(define-numericals-two-arg-test dn::atan2 array t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))

;;; We should rather change TWO-ARG-FN to allow for integer arguments
;; (define-polymorphic-function dn::ash-right (x y &key out) :overwrite t)
;; (defpolymorph dn::ash-right (x y &key ((out null))) t
;;   (declare (ignore out))
;;   (two-arg-fn/non-broadcast 'dn::ash-right x y))
;; (defpolymorph dn::ash-right (x y &key ((out (not null)))) t
;;   (two-arg-fn/non-broadcast 'dn::ash-right x y :out out))
