(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function two-arg-fn/non-comparison (name x y &key out broadcast) :overwrite t)

;; Pure number
(defpolymorph two-arg-fn/non-comparison ((name symbol)
                                         (x number) (y number)
                                         &key ((out null) nil) broadcast)
    (values number &optional)
  (declare (ignore out broadcast)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))


;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x list) (y list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/non-comparison name (nu:asarray x) (nu:asarray y) :out out :broadcast broadcast))
(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x number) (y list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/non-comparison name x (nu:asarray y :type (array-element-type out))
                             :out out
                             :broadcast broadcast))
(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x list) (y number)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/non-comparison name (nu:asarray x :type (array-element-type out)) y
                             :out out
                             :broadcast broadcast))

(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x list) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/non-comparison name
                             (nu:asarray x)
                             (nu:asarray y)
                             :broadcast broadcast))
(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x number) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (let ((y (nu:asarray y)))
    (two-arg-fn/non-comparison name x y :out y :broadcast broadcast)))
(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x list) (y number)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (let ((x (nu:asarray x)))
    (two-arg-fn/non-comparison name x y :out x :broadcast broadcast)))

;; arbitrary arrays

(defpolymorph (two-arg-fn/non-comparison :suboptimal-note runtime-array-allocation
                                         :inline t)
    ((name symbol) (x array) (y array)
     &key ((out null)) (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (let* ((xtype (array-element-type x))
         (ytype (array-element-type y)))
    (let* ((max-type (max-type xtype ytype))
           (x (if (type= xtype max-type)
                  x
                  (nu:copy x :out (nu:zeros (array-dimensions x) :type max-type))))
           (y (if (type= ytype max-type)
                  y
                  (nu:copy y :out (nu:zeros (array-dimensions y) :type max-type)))))
      ;; TODO: Add type declarations?
      (two-arg-fn/non-comparison name x y :broadcast broadcast :out nil))))

(defpolymorph (two-arg-fn/non-comparison :suboptimal-note runtime-array-allocation
                                         :inline t)
    ((name symbol) (x array) (y array)
     &key ((out array)) (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (let* ((xtype (array-element-type x))
         (ytype (array-element-type y))
         (otype (array-element-type out)))
    (let* ((x (if (type= xtype otype)
                  x
                  (nu:copy x :out (nu:zeros (array-dimensions x) :type otype))))
           (y (if (type= ytype otype)
                  y
                  (nu:copy y :out (nu:zeros (array-dimensions y) :type otype)))))
      ;; TODO: Add type declarations?
      (two-arg-fn/non-comparison name x y :broadcast broadcast :out nil))))


;; 6 parametric polymorphs

(defpolymorph (two-arg-fn/non-comparison :inline t)
    ((name symbol) (x (simple-array <type>))
     (y (simple-array <type>)) &key ((out (simple-array <type>)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion
        (or broadcast
            (and (equalp (narray-dimensions x) (narray-dimensions y))
                 (equalp (narray-dimensions x) (narray-dimensions out))))))
    (pflet ((c-name (c-name <type> name))
            (svx (array-storage x))
            (svy (array-storage y))
            (svo (array-storage out))
            (c-size (c-size <type>)))
      (declare (type (cl:array <type> 1) svx svy svo))
      (with-thresholded-multithreading/cl (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* c-size (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* c-size (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (* c-size (cl-array-offset svo)))
          (funcall c-name (array-total-size svo) ptr-x 1 ptr-y 1
                   ptr-o 1)))))
  out)

(defpolymorph (two-arg-fn/non-comparison :inline t :suboptimal-note
                                         runtime-array-allocation)
    ((name symbol) (x (simple-array <type>))
     (y (simple-array <type>)) &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable name out broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion
        (or broadcast (equalp (narray-dimensions x) (narray-dimensions y)))))
    (pflet ((out (nu:zeros (narray-dimensions x) :type <type>)))
      (declare (type (simple-array <type>) out))
      (two-arg-fn/non-comparison name x y :out out :broadcast nil))))

(defpolymorph (two-arg-fn/non-comparison :inline :maybe
                                         :more-optimal-type-list
                                         (symbol (simple-array <type>)
                                                 (simple-array <type>) &key
                                                 (:out (simple-array <type>))
                                                 (:broadcast null)))
    ((name symbol) (x (array <type>)) (y (array <type>)) &key
     ((out (array <type>))) (broadcast nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name broadcast))
  (let ((c-size (c-size <type>)))
    (if broadcast
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y out)
          (assert broadcast-compatible-p (x y out)
                  'incompatible-broadcast-dimensions
                  :dimensions (list (narray-dimensions x)
                                    (narray-dimensions y)
                                    (narray-dimensions out))
                  :array-likes (list x y out))
          (let ((c-name (c-name <type> name)))
            (ptr-iterate-but-inner broadcast-dimensions
                n
              ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o c-size io out))
              (funcall c-name n ptr-x ix ptr-y iy ptr-o io))))
        (policy-cond:with-expectations (= safety 0)
            ((assertion
              (or broadcast
                  (and (equalp (narray-dimensions x) (narray-dimensions y))
                       (equalp (narray-dimensions x) (narray-dimensions out))))))
          (let ((c-name (c-name <type> name)))
            (ptr-iterate-but-inner (narray-dimensions x)
                n
              ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o c-size io out))
              (funcall c-name n ptr-x ix ptr-y iy ptr-o io))))))
  out)

(defpolymorph (two-arg-fn/non-comparison :inline :maybe :suboptimal-note
                                         runtime-array-allocation)
    ((name symbol) (x (array <type>)) (y (array <type>)) &key
     ((out null)) (broadcast nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name out broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y)
        (assert broadcast-compatible-p (x y)
                'incompatible-broadcast-dimensions
                :dimensions (list (narray-dimensions x)
                                  (narray-dimensions y))
                :array-likes (list x y))
        (pflet ((out (nu:zeros broadcast-dimensions :type <type>)))
          (declare (type (simple-array <type>) out)
                   (compiler-macro-notes:muffle
                    polymorphic-functions:more-optimal-polymorph-inapplicable))
          (two-arg-fn/non-comparison name x y :out out :broadcast t)))
      (policy-cond:with-expectations (= safety 0)
          ((assertion
            (or broadcast
                (equalp (narray-dimensions x) (narray-dimensions y)))))
        (pflet ((out (nu:zeros (narray-dimensions x) :type <type>)))
          (declare (type (simple-array <type>) out))
          (two-arg-fn/non-comparison name x y :out out :broadcast nil)))))

(defpolymorph two-arg-fn/non-comparison
    ((name symbol) (x (array <type>)) (y number) &key
     ((out (array <type>)))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name broadcast))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out) 'incompatible-broadcast-dimensions
            :dimensions (list (narray-dimensions x) (narray-dimensions out))
            :array-likes (list x out))
    (let ((c-size (c-size <type>)))
      (cffi-sys:with-foreign-pointer (ptr-y c-size)
        (setf (cffi:mem-ref ptr-y (c-type <type>))
              (trivial-coerce:coerce y <type>))
        (ptr-iterate-but-inner broadcast-dimensions
            n
          ((ptr-x c-size ix x) (ptr-o c-size io out))
          (funcall (c-name <type> name) n ptr-x ix ptr-y 0 ptr-o io)))))
  out)

(defpolymorph (two-arg-fn/non-comparison :suboptimal-note runtime-array-allocation
                                         :inline t)
    ((name symbol) (x (array <type>)) (y number) &key
     ((out null))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name out broadcast))
  (pflet ((out (nu:zeros (narray-dimensions x) :type (array-element-type x))))
    (declare (type (array <type>) out))
    (two-arg-fn/non-comparison name x y :out out :broadcast t)
    out))

(defpolymorph two-arg-fn/non-comparison
    ((name symbol) (x number) (y (array <type>)) &key
     ((out (array <type>)))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name broadcast))
  (let ((c-name (c-name <type> name))
        (c-size (c-size <type>)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (list (narray-dimensions y)
                                (narray-dimensions out))
              :array-likes (list y out))
      (cffi-sys:with-foreign-pointer (ptr-x c-size)
        (setf (cffi:mem-ref ptr-x (c-type <type>))
              (trivial-coerce:coerce x <type>))
        (ptr-iterate-but-inner broadcast-dimensions
            n
          ((ptr-y c-size iy y) (ptr-o c-size io out))
          (funcall c-name n ptr-x 0 ptr-y iy ptr-o io))))
    out))

(defpolymorph (two-arg-fn/non-comparison :suboptimal-note runtime-array-allocation
                                         :inline t)
    ((name symbol) (x number) (y (array <type>)) &key
     ((out null))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array <type>)
  (declare (ignorable name out broadcast))
  (pflet ((out (nu:zeros (narray-dimensions y) :type (array-element-type y))))
    (declare (type (array <type>) out))
    (two-arg-fn/non-comparison name x y :out out :broadcast t)
    out))

;;; Actual definitions

(macrolet ((def (name
                 (single-float-return-type single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-return-type double-float-error
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (x y &key out broadcast)
                      :overwrite t))
             `(progn
                (define-polymorphic-function ,name (x y &key out broadcast))
                (defpolymorph ,name (x y &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
                  (declare (ignorable out))
                  (two-arg-fn/non-comparison ',name x y :broadcast broadcast))
                (defpolymorph ,name (x y &key ((out (not null)))
                                       (broadcast nu:*broadcast-automatically*))
                    t
                  (two-arg-fn/non-comparison ',name x y :out out :broadcast broadcast))
                (define-numericals-two-arg-test ,name nu::array nil
                  (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                  (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def nu:add      (single-float 1f-7) (double-float 1d-15))
  (def nu:subtract (single-float 1f-7) (double-float 1d-15))
  (def nu:multiply (single-float 1f-7) (double-float 1d-15))
  (def nu:divide   (single-float 1f-7) (double-float 1d-15)))

(define-numericals-two-arg-test/integers nu:add      nu::array)
(define-numericals-two-arg-test/integers nu:subtract nu::array)
(define-numericals-two-arg-test/integers nu:multiply nu::array)
;; (define-numericals-two-arg-test/integers nu:divide   nu::array)
