(in-package :dense-numericals.impl)

;;; We let the n-arg version handle the broadcasting

(define-polymorphic-function two-arg-fn/non-broadcast (name x y &key out) :overwrite t)

;;; Comparison operators return a (UNSIGNED-BYTE 8) element array as output if input is array-like.
(deftype comparison-operator ()
  `(member cl:< cl:= cl:<= cl:/= cl:>= cl:>
           dn:< dn:= dn:<= dn:/= dn:>= dn:>
           dn:two-arg-<  dn:two-arg-<= dn:two-arg-=
           dn:two-arg-/= dn:two-arg->= dn:two-arg->))
(deftype non-comparison-operator ()
  `(and symbol (not comparison-operator)))

;; Pure number
(defpolymorph two-arg-fn/non-broadcast ((name comparison-operator)
                                        (x number) (y number) &key ((out null) nil))
    bit
  (declare (ignore out)
           (ignorable name))
  (if (funcall (cl-name name) x y)
      1
      0))

(defpolymorph two-arg-fn/non-broadcast ((name non-comparison-operator)
                                        (x number) (y number) &key ((out null) nil))
    (values number &optional)
  (declare (ignore out)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))



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

(macrolet ((def (type fn-retriever size)
             ;; TODO: Decide if we need to specialize on simple-array
             `(progn
                ;; Comparison
                (defpolymorph two-arg-fn/non-broadcast
                    ((name comparison-operator) (x (array ,type)) (y (array ,type))
                     &key ((out (array (unsigned-byte 8)))
                           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
                    (array (unsigned-byte 8))
                  (declare (ignorable name))
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions y)))
                       (assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (let ((,fn-retriever (,fn-retriever name)))
                      (with-thresholded-multithreading (array-total-size (the array out))
                          (x y out)
                        (ptr-iterate-but-inner n ((ptr-x ,size ix x)
                                                  (ptr-y ,size iy y)
                                                  (ptr-o 1 io out))
                          (funcall ,fn-retriever
                                   n
                                   ptr-x ix
                                   ptr-y iy
                                   ptr-o io)))))
                  out)
                ;; Non-comparison
                (defpolymorph two-arg-fn/non-broadcast
                    ((name non-comparison-operator) (x (array ,type)) (y (array ,type))
                     &key ((out (array ,type))
                           (zeros (narray-dimensions x) :type ',type)))
                    (array ,type)
                  (declare (ignorable name))
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions y)))
                       (assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (let ((,fn-retriever (,fn-retriever name)))
                      (with-thresholded-multithreading (array-total-size (the array out))
                          (x y out)
                        (ptr-iterate-but-inner n ((ptr-x ,size ix x)
                                                  (ptr-y ,size iy y)
                                                  (ptr-o ,size io out))
                          (funcall ,fn-retriever
                                   n
                                   ptr-x ix
                                   ptr-y iy
                                   ptr-o io)))))
                  out))))

  (def (signed-byte 64)   int64-c-name 8)
  (def (signed-byte 32)   int32-c-name 4)
  (def (signed-byte 16)   int16-c-name 2)
  (def (signed-byte 08)   int8-c-name  1)

  (def (unsigned-byte 64) uint64-c-name 8)
  (def (unsigned-byte 32) uint32-c-name 4)
  (def (unsigned-byte 16) uint16-c-name 2)
  (def (unsigned-byte 08) uint8-c-name  1)

  (def fixnum       fixnum-c-name       8)
  (def single-float single-float-c-name 4)
  (def double-float double-float-c-name 8))


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
                (define-numericals-two-arg-test ,name array nil
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

(define-numericals-two-arg-test/integers dn:two-arg-+ array)
(define-numericals-two-arg-test/integers dn:two-arg-- array)
(define-numericals-two-arg-test/integers dn:two-arg-* array)

(define-numericals-two-arg-test/integers dn:two-arg-<  array (unsigned-byte 8))
(define-numericals-two-arg-test/integers dn:two-arg-<= array (unsigned-byte 8))
(define-numericals-two-arg-test/integers dn:two-arg-=  array (unsigned-byte 8))
(define-numericals-two-arg-test/integers dn:two-arg-/= array (unsigned-byte 8))
(define-numericals-two-arg-test/integers dn:two-arg->  array (unsigned-byte 8))
(define-numericals-two-arg-test/integers dn:two-arg->= array (unsigned-byte 8))
