(in-package :numericals/basic-math/impl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-constant +logical-doc+
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

(define-polymorphic-function nu:lognot (x &key out broadcast) :overwrite t
  :documentation +logical-doc+)

(defpolymorph (nu:lognot :inline t)
    ((x (simple-array <type>))
     &key ((out (simple-array <type>)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion
        (or broadcast
            (equalp (narray-dimensions x)
                    (narray-dimensions out)))))
    (pflet ((svx (array-storage x))
            (svo (array-storage out))
            (c-size (c-size <type>))
            (c-name (c-name <type> 'nu:lognot)))
      (declare (type (common-lisp:simple-array <type> 1) svx svo))
      (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                      (ptr-o (array-storage svo)))
        (cffi:incf-pointer ptr-x (cl-array-offset svx))
        (cffi:incf-pointer ptr-o (cl-array-offset svo))
        (funcall c-name
                 (* c-size (array-total-size svo))
                 ptr-x 1
                 ptr-o 1))))
  out)

(defpolymorph (nu:lognot :inline t :suboptimal-note
                         runtime-array-allocation)
    ((x (simple-array <type>)) &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable out broadcast))
  (pflet ((out (nu:zeros (narray-dimensions x) :type <type>)))
    (declare (type (array <type>) out))
    (pflet ((svx (array-storage x))
            (svo (array-storage out))
            (c-size (c-size <type>))
            (c-name (c-name <type> 'nu:lognot)))
      (declare (type (common-lisp:array <type> 1) svx svo))
      (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                      (ptr-o (array-storage svo)))
        (cffi:incf-pointer ptr-x (cl-array-offset svx))
        (cffi:incf-pointer ptr-o (cl-array-offset svo))
        (funcall c-name (* c-size (array-total-size svo))
                 ptr-x 1
                 ptr-o 1)))
    out))

(defpolymorph (nu:lognot :inline :maybe :suboptimal-note runtime-array-allocation)
    ((x (array <type>)) &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignorable out broadcast))
  (pflet ((out (nu:zeros-like x))
          (c-size (c-size <type>))
          (c-name (c-name <type> 'nu:lognot)))
    (declare (type (array <type>) out))
    (ptr-iterate-but-inner (narray-dimensions out)
        n
      ((ptr-x c-size ix x)
       (ptr-out c-size iout out))
      (funcall c-name (* c-size n) ptr-x ix ptr-out iout))
    out))

(defpolymorph (nu:lognot
               :inline :maybe
               :more-optimal-type-list
               (symbol (simple-array <type>) &key
                       (:out (simple-array <type>))
                       (:broadcast null)))
    ((x (array <type>)) &key ((out (array <type>)))
     (broadcast nu:*broadcast-automatically*))
    (array <type>)
  (let ((c-size (c-size <type>))
        (c-name (c-name <type> 'nu:lognot)))
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
            (funcall c-name (* c-size n)
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

(defpolymorph (nu:lognot :inline t)
    ((x real) &key ((out (array <type>)))
     ((broadcast (not null))))
    (array <type>)
  (declare (ignore broadcast))
  (let ((c-size (c-size <type>))
        (c-name (c-name <type> 'nu:lognot)))
    (cffi-sys:with-foreign-pointer (ptr-x c-size)
      (setf (cffi:mem-ref ptr-x :float) (coerce x <type>))
      (pflet ((svo (array-storage out)))
        (declare (type (common-lisp:simple-array <type> 1) svo))
        (with-pointers-to-vectors-data ((ptr-o svo))
          (funcall c-name
                   (* c-size (array-total-size svo))
                   ptr-x 0
                   ptr-o 1)))))
  out)

;; pure number
(defpolymorph (nu:lognot :inline t) ((x number)
                                     &key ((out null))
                                     (broadcast nu:*broadcast-automatically*))
    (values number &optional)
  (declare (ignorable out broadcast))
  (lognot x))

;; lists - 2 polymorphs
(defpolymorph (nu:lognot :inline t :suboptimal-note runtime-array-allocation)
    ((x list)
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
    (nu:lognot array :out array :broadcast broadcast)))

(defpolymorph (nu:lognot :inline t :suboptimal-note runtime-array-allocation)
    ((x list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; TODO: Define copy from list to array directly
  (nu:lognot (nu:asarray x :type (array-element-type out))
             :out out :broadcast broadcast))
