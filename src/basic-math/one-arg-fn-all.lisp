(in-package :numericals/basic-math/impl)

(5am:in-suite :numericals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +one-arg-fn-all-doc+
    ;; See the below non-array polymorphs for the element-type deduction procedure
    "
These functions have a single array as input and a single array as output.
If the output array is not supplied, its element-type is computed :AUTO-matically

Declarations become necessary for smaller arrays. An (OPTIMIZE SPEED) declaration
should help you with optimization by providing optimization notes.
Optimization for small arrays essentially involves inlining, along with:
  (i)  providing the OUT parameter to allow the user to eliminate allocation wherever possible
  (ii) eliminating work involved in broadcasting and multithreading

TODO: Provide more details
  "
    :test #'string=))

(define-polymorphic-function one-arg-fn/all (name x &key out broadcast) :overwrite t
  :documentation +one-arg-fn-all-doc+)

(defpolymorph (one-arg-fn/all :inline t)
    ((name symbol) (x (simple-array <type>))
     &key ((out (simple-array <type>)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion
        (or broadcast
            (equalp (narray-dimensions x)
                    (narray-dimensions out)))))
    (pflet ((svx (array-storage x))
            (svo (array-storage out))
            (c-size (c-size <type>))
            (c-name (c-name <type> name)))
      (declare (type (common-lisp:simple-array <type> 1) svx svo))
      (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                      (ptr-o (array-storage svo)))
        (cffi:incf-pointer ptr-x (* c-size (cl-array-offset svx)))
        (cffi:incf-pointer ptr-o (* c-size (cl-array-offset svo)))
        (funcall c-name
                 (array-total-size svo)
                 ptr-x 1
                 ptr-o 1))))
  out)

(defpolymorph (one-arg-fn/all :inline t :suboptimal-note
                              runtime-array-allocation)
    ((name symbol) (x (simple-array <type>)) &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable name out broadcast))
  (pflet ((out (nu:zeros (narray-dimensions x) :type <type>)))
    (declare (type (array <type>) out))
    (pflet ((svx (array-storage x))
            (svo (array-storage out))
            (c-size (c-size <type>))
            (c-name (c-name <type> name)))
      (declare (type (common-lisp:array <type> 1) svx svo))
      (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                      (ptr-o (array-storage svo)))
        (cffi:incf-pointer ptr-x (* c-size (cl-array-offset svx)))
        (cffi:incf-pointer ptr-o (* c-size (cl-array-offset svo)))
        (funcall c-name (array-total-size svo)
                 ptr-x 1
                 ptr-o 1)))
    out))

(defpolymorph (one-arg-fn/all :inline :maybe :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (array <type>)) &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable name out broadcast))
  (pflet ((out (nu:zeros-like x))
          (c-size (c-size <type>))
          (c-name (c-name <type> name)))
    (declare (type (array <type>) out))
    (ptr-iterate-but-inner (narray-dimensions out)
        n
      ((ptr-x c-size ix x)
       (ptr-out c-size iout out))
      (funcall c-name n ptr-x ix ptr-out iout))
    out))

(defpolymorph (one-arg-fn/all
               :inline :maybe
               :more-optimal-type-list
               (symbol (simple-array <type>) &key
                       (:out (simple-array <type>))
                       (:broadcast null)))
    ((name symbol) (x (array <type>)) &key ((out (array <type>)))
     (broadcast nu:*broadcast-automatically*))
    (array <type>)
  (let ((c-size (c-size <type>))
        (c-name (c-name <type> name)))
    (if broadcast
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (%broadcast-compatible-p (narray-dimensions x)
                                     (narray-dimensions out))
          (assert broadcast-compatible-p (x out)
                  'incompatible-broadcast-dimensions :dimensions
                  (mapcar #'narray-dimensions (list x out)) :array-likes
                  (list x out))
          (ptr-iterate-but-inner broadcast-dimensions
              n
            ((ptr-x c-size ix x)
             (ptr-out c-size iout out))
            (funcall c-name n
                     ptr-x ix ptr-out iout)))
        (policy-cond:with-expectations (= safety 0)
            ((assertion
              (or broadcast
                  (equalp (narray-dimensions x) (narray-dimensions out)))))
          (ptr-iterate-but-inner (narray-dimensions out)
              n
            ((ptr-x c-size ix x) (ptr-out c-size iout out))
            (funcall c-name n
                     ptr-x ix ptr-out iout)))))
  out)

(defpolymorph (one-arg-fn/all :inline t)
    ((name symbol) (x real) &key ((out (array <type>)))
     ((broadcast (not null))))
    (array <type>)
  (declare (ignore broadcast))
  (let ((c-size (c-size <type>))
        (c-name (c-name <type> name)))
    (cffi-sys:with-foreign-pointer (ptr-x c-size)
      (setf (cffi:mem-ref ptr-x :float) (coerce x <type>))
      (pflet ((svo (array-storage out)))
        (declare (type (common-lisp:simple-array <type> 1) svo))
        (with-pointers-to-vectors-data ((ptr-o svo))
          (funcall c-name
                   (array-total-size svo)
                   ptr-x 0
                   ptr-o 1)))))
  out)

;; pure number
(defpolymorph (one-arg-fn/all :inline t) ((name symbol) (x number)
                                          &key ((out null))
                                          (broadcast nu:*broadcast-automatically*))
    (values number &optional)
  (declare (ignorable out name broadcast))
  (funcall (cl-name name) x))

;; lists - 2 polymorphs
(defpolymorph (one-arg-fn/all :inline t :suboptimal-note runtime-array-allocation)
    ((name symbol) (x list)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; Why didn't we create ARRAY in the lambda-list itself?
  ;;   We can do, once NU:ZEROS-LIKE starts taking TYPE as an argument
  ;;   Also think over the implication of being required to allocate a separate
  ;; array in the second case below; perhaps we also need a way to copy from a
  ;; array-like to an array.
  (let ((array (nu:asarray x :type :auto)))
    (one-arg-fn/all name array :out array :broadcast broadcast)))

(defpolymorph (one-arg-fn/all :inline t :suboptimal-note runtime-array-allocation)
    ((name symbol) (x list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; TODO: Define copy from list to array directly
  (one-arg-fn/all name (nu:asarray x :type (array-element-type out)) :out out :broadcast broadcast))

(macrolet ((def (name)
             `(progn
                (define-polymorphic-function ,name (x &key out broadcast)
                  :overwrite t :documentation +one-arg-fn-all-doc+)
                (defpolymorph ,name (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/all ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*))
                    t
                  (one-arg-fn/all ',name x :out out :broadcast broadcast)))))

  (def nu:abs))
