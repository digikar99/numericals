(in-package :dense-numericals.impl)

(define-polymorphic-function one-arg-fn/all (name x &key out broadcast) :overwrite t
  ;; See the below non-array polymorphs for the element-type deduction procedure
  :documentation "These functions have a single array as input and a single array as output.
If the output array is not supplied, its element-type is obtained from the element-type
as the input array. If the input is not an array, then the element-type is deduced from
the elements as closely as possible.")

(macrolet ((def (type c-type fn-retriever size)

             `(progn

                (defpolymorph one-arg-fn/all
                    ((name symbol) (x (array ,type))
                     &key ((out (array ,type)) (zeros-like x))
                     (broadcast dn:*broadcast-automatically*))
                    (array ,type)
                  (when (and broadcast
                             (not (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
                        (%broadcast-compatible-p (narray-dimensions x)
                                                 (narray-dimensions out))
                      (assert broadcast-compatible-p (x out)
                              'incompatible-broadcast-dimensions
                              :dimensions (mapcar #'narray-dimensions (list x out))
                              :array-likes (list x out))
                      (setq x (broadcast-array x broadcast-dimensions))))
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (or broadcast
                                      (equalp (narray-dimensions x)
                                              (narray-dimensions out)))))
                    (with-thresholded-multithreading (array-total-size out)
                        (x out)
                      (ptr-iterate-but-inner n ((ptr-x   ,size ix x)
                                                (ptr-out ,size iout out))
                        (funcall (,fn-retriever name) n ptr-x ix ptr-out iout))))
                  out)

                (defpolymorph one-arg-fn/all
                    ((name symbol) (x real)
                     &key ((out (array ,type)))
                     ((broadcast (not null)) dn:*broadcast-automatically*))
                    (array ,type)
                  (declare (ignore broadcast))
                  (cffi:with-foreign-pointer (ptr-x ,size)
                    (setf (cffi:mem-ref ptr-x ,c-type)
                          (trivial-coerce:coerce x ',type))
                    (with-thresholded-multithreading (array-total-size out)
                        (out)
                      (ptr-iterate-but-inner n ((ptr-out ,size iout out))
                        (funcall (,fn-retriever name) n ptr-x 0 ptr-out iout))))
                  out))))

  (def (signed-byte 64) :long  int64-c-name 8)
  (def (signed-byte 32) :int   int32-c-name 4)
  (def (signed-byte 16) :short int16-c-name 2)
  (def (signed-byte 08) :char  int8-c-name  1)

  (def (unsigned-byte 64) :unsigned-long  uint64-c-name 8)
  (def (unsigned-byte 32) :unsigned-int   uint32-c-name 4)
  (def (unsigned-byte 16) :unsigned-short uint16-c-name 2)
  (def (unsigned-byte 08) :unsigned-char  uint8-c-name  1)

  (def fixnum       :long   fixnum-c-name       8)
  (def single-float :float  single-float-c-name 4)
  (def double-float :double double-float-c-name 8))

;; pure number
(defpolymorph (one-arg-fn/all :inline t) ((name symbol) (x number)
                                          &key ((out null))
                                          (broadcast dn:*broadcast-automatically*))
    (values number &optional)
  (declare (ignorable out name broadcast))
  (funcall (cl-name name) x))

;; lists - 2 polymorphs
(defpolymorph (one-arg-fn/all :inline t) ((name symbol) (x list)
                                          &key ((out null))
                                          (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; Why didn't we create ARRAY in the lambda-list itself?
  ;;   We can do, once ZEROS-LIKE starts taking TYPE as an argument
  ;;   Also think over the implication of being required to allocate a separate
  ;; array in the second case below; perhaps we also need a way to copy from a
  ;; array-like to an array.
  (let ((array (asarray x :type :auto)))
    (one-arg-fn/all name array :out array :broadcast broadcast)))

(defpolymorph (one-arg-fn/all :inline t) ((name symbol) (x list)
                                          &key ((out array))
                                          (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  (one-arg-fn/all name (asarray x :type (array-element-type out)) :out out :broadcast broadcast))

;; non-float arrays
(defpolymorph (one-arg-fn/all :inline nil) ; this is recursive
    ((name symbol) (x array)
     &key ((out (or (array single-float) (array double-float)))
           (zeros (array-dimensions x) :type dn:*default-float-format*))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (dn:copy x :out out)
  (one-arg-fn/all name out :out out :broadcast broadcast))

(macrolet ((def (name)
             `(progn
                (define-polymorphic-function ,name (x &key out broadcast) :overwrite t)
                (defpolymorph ,name (x &key ((out null)) (broadcast dn:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/all ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast dn:*broadcast-automatically*)) t
                  (one-arg-fn/all ',name x :out out :broadcast broadcast))
                (define-numericals-one-arg-test ,name array (0.0f0) (0.0d0))
                (define-numericals-one-arg-test/integers ,name array))))

  (def dn:abs))
