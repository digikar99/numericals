(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

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
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name (nu:asarray x) (nu:asarray y) :out out :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x number) (y list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name x (nu:asarray y :type (array-element-type out))
                    :out out
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y number)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/float name (nu:asarray x :type (array-element-type out))
                    y :out
                    out :broadcast broadcast))

(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name
                    (nu:asarray x :type nu:*default-float-format*)
                    (nu:asarray y :type nu:*default-float-format*)
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x number) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name x (nu:asarray y :type nu:*default-float-format*)
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x list) (y number)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/float name (nu:asarray x :type nu:*default-float-format*) y
                    :broadcast broadcast))



;; single-float - 4 polymorphs

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (nu:zeros (narray-dimensions x) :type 'single-float))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    (cffi:with-foreign-pointer (ptr-y 4)
      (setf (cffi:mem-ref ptr-y :float) (trivial-coerce:coerce y 'single-float))
      (ptr-iterate-but-inner broadcast-dimensions n
        ((ptr-x 4 ix x)
         (ptr-o 4 io out))
        (funcall (single-float-c-name name)
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array single-float)) 
     &key ((out (array single-float))
           (nu:zeros (narray-dimensions y) :type 'single-float))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (let ((single-float-c-name (single-float-c-name name)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list y out))
              :array-likes (list y out))
      (cffi:with-foreign-pointer (ptr-x 4)
        (setf (cffi:mem-ref ptr-x :float) (trivial-coerce:coerce X 'single-float))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-y 4 iy y)
           (ptr-o 4 io out))
          (funcall single-float-c-name
                   n
                   ptr-x 0
                   ptr-y iy
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array single-float)) (y (array single-float))
     ;; We are not producing OUT in the lambda-list because it is
     ;; non-trivial to work out the dimensions in a short
     ;; human-readable space. This work has been done just below.
     &key ((out (or null (array single-float))))
     (broadcast nu:*broadcast-automatically*))
    (array single-float)
  ;; We are not broadcasting OUT because doing so would mean
  ;; OUT would be written multiple times leading to all sorts of bad things
  (declare (ignorable name))
  (when (or (not broadcast)
            (equalp (narray-dimensions x)
                    (narray-dimensions y)))
    (setq out (or out (nu:zeros (narray-dimensions x) :type 'single-float))))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x y out)
    (assert broadcast-compatible-p (x y out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x y out))
            :array-likes (list x y out))
    (let ((out (or out (nu:zeros broadcast-dimensions :type 'single-float))))
      (declare (type (array single-float) out))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions y))))
           (assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions out)))))
        (let ((single-float-c-name (single-float-c-name name)))
          (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 4 ix x)
                                                         (ptr-y 4 iy y)
                                                         (ptr-o 4 io out))
            (funcall single-float-c-name
                     n
                     ptr-x ix
                     ptr-y iy
                     ptr-o io))))
      out)))

;; TODO: Measure the benefits of doing this
;; (defpolymorph two-arg-fn/float
;;     ((name symbol) (x (simple-array single-float)) (y (simple-array single-float))
;;      &key ((out (or null (simple-array single-float))))
;;      (broadcast nu:*broadcast-automatically*))
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
;;                 (out (or out (nu:zeros broadcast-dimensions :type 'single-float))))
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
           (nu:zeros (narray-dimensions x) :type 'double-float))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    (cffi:with-foreign-pointer (ptr-y 8)
      (setf (cffi:mem-ref ptr-y :float) (trivial-coerce:coerce y 'double-float))
      (ptr-iterate-but-inner broadcast-dimensions n
        ((ptr-x 8 ix x)
         (ptr-o 8 io out))
        (funcall (double-float-c-name name)
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array double-float)) 
     &key ((out (array double-float))
           (nu:zeros (narray-dimensions y) :type 'double-float))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (let ((double-float-c-name (double-float-c-name name)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list y out))
              :array-likes (list y out))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :float) (trivial-coerce:coerce X 'double-float))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-y 8 iy y)
           (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x 0
                   ptr-y iy
                   ptr-o io)))))
  out)

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array double-float)) (y (array double-float))
     ;; We are not producing OUT in the lambda-list because it is
     ;; non-trivial to work out the dimensions in a short
     ;; human-readable space. This work has been done just below.
     &key ((out (or null (array double-float))))
     (broadcast nu:*broadcast-automatically*))
    (array double-float)
  ;; We are not broadcasting OUT because doing so would mean
  ;; OUT would be written multiple times leading to all sorts of bad things
  (declare (ignorable name))
  (when (or (not broadcast)
            (equalp (narray-dimensions x)
                    (narray-dimensions y)))
    (setq out (or out (nu:zeros (narray-dimensions x) :type 'double-float))))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x y out)
    (assert broadcast-compatible-p (x y out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x y out))
            :array-likes (list x y out))
    (let ((out (or out (nu:zeros broadcast-dimensions :type 'double-float))))
      (declare (type (array double-float) out))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions y))))
           (assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions out)))))
        (let ((double-float-c-name (double-float-c-name name)))
          (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 8 ix x)
                                                         (ptr-y 8 iy y)
                                                         (ptr-o 8 io out))
            (funcall double-float-c-name
                     n
                     ptr-x ix
                     ptr-y iy
                     ptr-o io))))
      out)))



(define-polymorphic-function nu:expt (base power &key out broadcast) :overwrite t)
(defpolymorph nu:expt (base power &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'nu:expt base power :broadcast broadcast))
(defpolymorph nu:expt (base power &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
  (two-arg-fn/float 'nu:expt base power :out out :broadcast broadcast))
(define-numericals-two-arg-test nu:expt nu::array t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))


(defpolymorph nu:atan (x y &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'nu::atan2 x y :broadcast broadcast))
(defpolymorph nu:atan (x y &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
  (two-arg-fn/float 'nu::atan2 x y :out out :broadcast broadcast))

(define-polymorphic-function nu::atan2 (x y &key out broadcast) :overwrite t)
(defpolymorph nu::atan2 (x y &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
  (declare (ignore out))
  (two-arg-fn/float 'nu::atan2 x y :broadcast broadcast))
(defpolymorph nu::atan2 (x y &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
  (two-arg-fn/float 'nu::atan2 x y :out out :broadcast broadcast))
(define-numericals-two-arg-test nu::atan2 nu::array t
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))

;;; We should rather change TWO-ARG-FN to allow for integer arguments
;; (define-polymorphic-function nu::ash-right (x y &key out) :overwrite t)
;; (defpolymorph nu::ash-right (x y &key ((out null))) t
;;   (declare (ignore out))
;;   (two-arg-fn/non-broadcast 'nu::ash-right x y))
;; (defpolymorph nu::ash-right (x y &key ((out (not null)))) t
;;   (two-arg-fn/non-broadcast 'nu::ash-right x y :out out))
