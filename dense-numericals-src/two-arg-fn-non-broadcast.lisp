(in-package :dense-numericals.impl)

;;; We let the n-arg version handle the broadcasting

(define-polymorphic-function two-arg-fn/non-broadcast
    (name x y &key out)
  :overwrite t)

(deftype comparison-operator ()
  `(member cl:< cl:= cl:<= cl:/= cl:>= cl:>
           dn:< dn:= dn:<= dn:/= dn:>= dn:>
           dn:two-arg-<  dn:two-arg-<= dn:two-arg-=
           dn:two-arg-/= dn:two-arg->= dn:two-arg->))
(deftype non-comparison-operator ()
  `(and symbol (not comparison-operator)))

;; Pure number
(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator)
     (x number) (y number) &key ((out null) nil))
    (values number &optional)
  (declare (ignore out)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator)
     (x number) (y number) &key ((out null) nil))
    bit
  (declare (ignore out)
           (ignorable name))
  (if (funcall (cl-name name) x y)
      1
      0))


;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x list) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn/non-broadcast name (asarray x) (asarray y) :out out))
(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x number) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn/non-broadcast name x (asarray y) :out out))
(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x list) (y number) &key ((out array)))
    (values array &optional)
  (two-arg-fn/non-broadcast name (asarray x) y :out out))

(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x list) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/non-broadcast name (asarray x) (asarray y)))
(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x number) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/non-broadcast name x (asarray y)))
(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x list) (y number) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/non-broadcast name (asarray x) y))



;; single-float - 2+2+1+2 polymorphs

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (zeros (narray-dimensions x) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-y 4)
    (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
    (let ((single-float-c-name (single-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                (ptr-o 4 io out))
        (funcall single-float-c-name
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array single-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-y 4)
    (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
    (let ((single-float-c-name (single-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                (ptr-o 1 io out))
        (funcall single-float-c-name
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x number) (y (array single-float))
     &key ((out (array single-float))
           (zeros (narray-dimensions y)
                  :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-x 4)
    (setf (cffi:mem-ref ptr-x :float) (coerce x 'single-float))
    (let ((single-float-c-name (single-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-y 4 iy y)
                                (ptr-o 1 io out))
        (funcall single-float-c-name
                 n
                 ptr-x 0
                 ptr-y iy
                 ptr-o io))))
  out)


(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array single-float))
           (zeros (narray-dimensions x) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
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

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((single-float-c-name (single-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                                (ptr-y 4 iy y)
                                (ptr-o 1 io out))
        (funcall single-float-c-name
                 n
                 ptr-x ix
                 ptr-y iy
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (simple-array single-float))
     (y (simple-array single-float))
     &key ((out (simple-array single-float))
           (zeros (narray-dimensions x)
                  :type 'single-float)))
    (simple-array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((single-float-c-name (single-float-c-name name)))
      (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                      (ptr-y (array-storage y))
                                      (ptr-o (array-storage out)))
        (funcall single-float-c-name
                 (array-total-size (the array out))
                 ptr-x 1
                 ptr-y 1
                 ptr-o 1))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (simple-array single-float))
     (y (simple-array single-float))
     &key ((out (simple-array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (simple-array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((single-float-c-name (single-float-c-name name)))
      (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                      (ptr-y (array-storage y))
                                      (ptr-o (array-storage out)))
        (funcall single-float-c-name
                 (array-total-size (the array out))
                 ptr-x 1
                 ptr-y 1
                 ptr-o 1))))
  out)



;;; double-float - 2+1+2+2 polymorphs

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array double-float)) (y number)
     &key ((out (array double-float))
           (zeros (narray-dimensions x) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-y 4)
    (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
    (let ((double-float-c-name (double-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                (ptr-o 8 io out))
        (funcall double-float-c-name
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array double-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-y 8)
    (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
    (let ((double-float-c-name (double-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                (ptr-o 1 io out))
        (funcall double-float-c-name
                 n
                 ptr-x ix
                 ptr-y 0
                 ptr-o io))))
  out)

(defpolymorph (two-arg-fn/non-broadcast :inline t)
    ((name symbol) (x number) (y (array double-float))
     &key ((out (array double-float))
           (zeros (narray-dimensions y) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (cffi:with-foreign-pointer (ptr-x 8)
    (setf (cffi:mem-ref ptr-x :double) (coerce x 'double-float))
    (let ((double-float-c-name (double-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-y 8 iy y)
                                (ptr-o 8 io out))
        (funcall double-float-c-name
                 n
                 ptr-x 0
                 ptr-y iy
                 ptr-o io))))
  out)


(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array double-float))
           (zeros (narray-dimensions x) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
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

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((double-float-c-name (double-float-c-name name)))
      (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                                (ptr-y 8 iy y)
                                (ptr-o 1 io out))
        (funcall double-float-c-name
                 n
                 ptr-x ix
                 ptr-y iy
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (simple-array double-float))
     (y (simple-array double-float))
     &key ((out (simple-array double-float))
           (zeros (narray-dimensions x) :type 'double-float)))
    (simple-array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((double-float-c-name (double-float-c-name name)))
      (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                      (ptr-y (array-storage y))
                                      (ptr-o (array-storage out)))
        (funcall double-float-c-name
                 (array-total-size (the array out))
                 ptr-x 1
                 ptr-y 1
                 ptr-o 1))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (simple-array double-float))
     (y (simple-array double-float))
     &key ((out (simple-array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (simple-array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let ((double-float-c-name (double-float-c-name name)))
      (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                      (ptr-y (array-storage y))
                                      (ptr-o (array-storage out)))
        (funcall double-float-c-name
                 (array-total-size (the array out))
                 ptr-x 1
                 ptr-y 1
                 ptr-o 1))))
  out)


;;; Actual definitions


(macrolet ((def (name
                 (single-float-return-type single-float-error
                  &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-return-type double-float-error 
                  &optional (df-min 0.0d0) (df-max 1.0d0)))
             (eval `(define-polymorphic-function ,name (x y &key out)
                      :overwrite t))
             `(progn
                (define-polymorphic-function ,name (x y &key out))
                (defpolymorph ,name (x y &key ((out null))) t
                  (declare (ignore out))
                  (two-arg-fn/non-broadcast ',name x y))
                (defpolymorph ,name (x y &key ((out (not null)))) t
                  (two-arg-fn/non-broadcast ',name x y :out out))
                (define-numericals-two-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                    (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def dn:two-arg-+ (single-float 1f-7) (double-float 1d-15))
  (def dn:two-arg-- (single-float 1f-7) (double-float 1d-15))
  (def dn:two-arg-* (single-float 1f-7) (double-float 1d-15))
  (def dn:two-arg-/ (single-float 1f-7) (double-float 1d-15))

  (def dn:two-arg-<  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def dn:two-arg-<= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def dn:two-arg-=  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def dn:two-arg-/= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def dn:two-arg->= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def dn:two-arg->  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0)))
