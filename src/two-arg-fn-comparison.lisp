(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function two-arg-fn/comparison (name x y &key out broadcast) :overwrite t)

;;; Comparison operators return a (UNSIGNED-BYTE 8) element array as output if input is array-like.
;; Pure number
(defpolymorph two-arg-fn/comparison ((name symbol)
                                     (x number) (y number)
                                     &key ((out null) nil) broadcast)
    bit
  (declare (ignore out broadcast)
           (ignorable name))
  (if (funcall (cl-name name) x y)
      1
      0))

;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x list) (y list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/comparison name (nu:asarray x) (nu:asarray y) :out out :broadcast broadcast))
(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x number) (y list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/comparison name x (nu:asarray y :type (array-element-type out))
                         :out out
                         :broadcast broadcast))
(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x list) (y number)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/comparison name (nu:asarray x :type (array-element-type out)) y
                         :out out
                         :broadcast broadcast))

(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x list) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/comparison name
                         (nu:asarray x)
                         (nu:asarray y)
                         :broadcast broadcast))
(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x number) (y list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/comparison name x (nu:asarray y) :broadcast broadcast))
(defpolymorph (two-arg-fn/comparison :inline t)
    ((name symbol) (x list) (y number)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/comparison name (nu:asarray x) y :broadcast broadcast))


;; 6 parametric polymorphs

(defpolymorph (two-arg-fn/comparison :inline t)

    ((name symbol)
     (x (simple-array <type>))
     (y (simple-array <type>))
     &key ((out (simple-array (unsigned-byte 8))))
     ((broadcast null) nu:*broadcast-automatically*))

    (simple-array (unsigned-byte 8))

  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion
        (or broadcast
            (and (equalp (narray-dimensions x) (narray-dimensions y))
                 (equalp (narray-dimensions x) (narray-dimensions out))))))
    (pflet ((c-name (c-name <type> name))
            (c-size (c-size <type>))
            (svx (array-storage x))
            (svy (array-storage y))
            (svo (array-storage out)))
      (declare (type (common-lisp:array (unsigned-byte 8) 1) svo)
               (type (common-lisp:array <type> 1) svx svy))
      (with-thresholded-multithreading/cl (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* c-size (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* c-size (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (cl-array-offset svo))
          (funcall c-name (array-total-size svo) ptr-x 1 ptr-y 1
                   ptr-o 1)))))
  out)



(defpolymorph (two-arg-fn/comparison :inline t :suboptimal-note
                                     runtime-array-allocation)

    ((name symbol)
     (x (simple-array <type>))
     (y (simple-array <type>))
     &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))

    (simple-array (unsigned-byte 8))

  (declare (ignorable name out broadcast))
  (let ((c-size (c-size <type>)))
    (policy-cond:with-expectations (= safety 0)
        ((assertion
          (or broadcast (equalp (narray-dimensions x) (narray-dimensions y)))))
      (pflet* ((out (zeros (narray-dimensions x) :type '(unsigned-byte 8)))
               (c-name (c-name <type> name))
               (svx (array-storage x)) (svy (array-storage y))
               (svo (array-storage out)))
        (declare (type (array (unsigned-byte 8)) out)
                 (type (common-lisp:array (unsigned-byte 8) 1) svo)
                 (type (common-lisp:array <type> 1) svx svy))
        (with-thresholded-multithreading/cl (array-total-size svo)
            (svx svy svo)
          (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                          (ptr-y (array-storage svy))
                                          (ptr-o (array-storage svo)))
            (cffi:incf-pointer ptr-x (* c-size (cl-array-offset svx)))
            (cffi:incf-pointer ptr-y (* c-size (cl-array-offset svy)))
            (cffi:incf-pointer ptr-o (cl-array-offset svo))
            (funcall c-name (array-total-size svo) ptr-x 1 ptr-y 1
                     ptr-o 1)))
        out))))



(defpolymorph (two-arg-fn/comparison :inline :maybe :more-optimal-type-list
                                     (symbol (simple-array <type>)
                                             (simple-array <type>) &key
                                             (:out (simple-array (unsigned-byte 8)))
                                             (:broadcast null)))

    ((name symbol) (x (array <type>)) (y (array <type>)) &key
     ((out (array (unsigned-byte 8))))
     (broadcast nu:*broadcast-automatically*))

    (array (unsigned-byte 8))

  (declare (ignorable name broadcast))
  (let ((c-name (c-name <type> name))
        (c-size (c-size <type>)))
    (if broadcast
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y out)
          (assert broadcast-compatible-p (x y out)
                  'incompatible-broadcast-dimensions
                  :dimensions (mapcar #'narray-dimensions (list x y out))
                  :array-likes (list x y out))
          (ptr-iterate-but-inner broadcast-dimensions
              n
            ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o 1 io out))
            (funcall c-name n ptr-x ix ptr-y iy ptr-o io)))
        (policy-cond:with-expectations (= safety 0)
            ((assertion
              (or broadcast
                  (and (equalp (narray-dimensions x) (narray-dimensions y))
                       (equalp (narray-dimensions x) (narray-dimensions out))))))
          (ptr-iterate-but-inner (narray-dimensions x)
              n
            ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o 1 io out))
            (funcall c-name n ptr-x ix ptr-y iy ptr-o io)))))
  out)



(defpolymorph (two-arg-fn/comparison :inline :maybe :suboptimal-note
                                     runtime-array-allocation)

    ((name symbol) (x (array <type>)) (y (array <type>)) &key
     ((out null)) (broadcast nu:*broadcast-automatically*))

    (array (unsigned-byte 8))

  (declare (ignorable name out broadcast))
  (let ((c-name (c-name <type> name))
        (c-size (c-size <type>)))
    (if broadcast
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y)
          (assert broadcast-compatible-p (x y)
                  'incompatible-broadcast-dimensions
                  :dimensions (mapcar #'narray-dimensions (list x y))
                  :array-likes (list x y))
          (let ((out (zeros broadcast-dimensions :type '(unsigned-byte 8))))
            (declare (type (array (unsigned-byte 8)) out))
            (ptr-iterate-but-inner broadcast-dimensions
                n
              ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o 1 io out))
              (funcall c-name n ptr-x ix ptr-y iy ptr-o io))
            out))
        (policy-cond:with-expectations (= safety 0)
            ((assertion
              (or broadcast
                  (equalp (narray-dimensions x) (narray-dimensions y)))))
          (let ((out (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
            (declare (type (array (unsigned-byte 8)) out))
            (ptr-iterate-but-inner (narray-dimensions x)
                n
              ((ptr-x c-size ix x) (ptr-y c-size iy y) (ptr-o 1 io out))
              (funcall c-name n ptr-x ix ptr-y iy ptr-o io))
            out)))))



(defpolymorph two-arg-fn/comparison

    ((name symbol) (x (array <type>)) (y number) &key
     ((out (array (unsigned-byte 8)))
      (zeros (narray-dimensions x) :type '(unsigned-byte 8)))
     ((broadcast (not null)) nu:*broadcast-automatically*))

    (array (unsigned-byte 8))

  (declare (ignorable name broadcast))
  (let ((c-size (c-size <type>)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x out)
      (assert broadcast-compatible-p (x out) 'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (cffi-sys:with-foreign-pointer (ptr-y c-size)
        (setf (cffi:mem-ref ptr-y :float)
              (trivial-coerce:coerce y <type>))
        (ptr-iterate-but-inner broadcast-dimensions
            n
          ((ptr-x c-size ix x) (ptr-o 1 io out))
          (funcall (c-name <type> name) n ptr-x ix ptr-y 0 ptr-o io)))))
  out)



(defpolymorph two-arg-fn/comparison

    ((name symbol) (x number) (y (array <type>)) &key
     ((out (array (unsigned-byte 8)))
      (zeros (narray-dimensions y) :type '(unsigned-byte 8)))
     ((broadcast (not null)) nu:*broadcast-automatically*))

    (array (unsigned-byte 8))

  (declare (ignorable name broadcast))
  (let ((c-name (c-name <type> name))
        (c-size (c-size <type>)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list y out))
              :array-likes (list y out))
      (cffi-sys:with-foreign-pointer (ptr-x c-size)
        (setf (cffi:mem-ref ptr-x :float)
              (trivial-coerce:coerce x <type>))
        (ptr-iterate-but-inner broadcast-dimensions
            n
          ((ptr-y c-size iy y) (ptr-o 1 io out))
          (funcall c-name n ptr-x 0 ptr-y iy ptr-o io)))))
  out)

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
                  (two-arg-fn/comparison ',name x y :broadcast broadcast))
                (defpolymorph ,name (x y &key ((out (not null)))
                                       (broadcast nu:*broadcast-automatically*))
                    t
                  (two-arg-fn/comparison ',name x y :out out :broadcast broadcast))
                (define-numericals-two-arg-test ,name nu::array nil
                  (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                  (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def nu:two-arg-<  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-<= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-=  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-/= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0)))

(define-numericals-two-arg-test/integers nu:two-arg-<  nu::array (unsigned-byte 8))
(define-numericals-two-arg-test/integers nu:two-arg-<= nu::array (unsigned-byte 8))
(define-numericals-two-arg-test/integers nu:two-arg-=  nu::array (unsigned-byte 8))
(define-numericals-two-arg-test/integers nu:two-arg-/= nu::array (unsigned-byte 8))
(define-numericals-two-arg-test/integers nu:two-arg->  nu::array (unsigned-byte 8))
(define-numericals-two-arg-test/integers nu:two-arg->= nu::array (unsigned-byte 8))
