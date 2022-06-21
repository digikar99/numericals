(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;; Basic Concept:
;; (c:dn-ssin (array-total-size x) (ptr x) 1 (ptr out) 1)

;; FIXME: No multithreading for broadcasting; see numericals/src/one-arg-fn.lisp

;;; Yes, this does lead to a lot of code-bloat while inlining, but inlining is only
;;; primarily useful for very small arrays, and in those cases, the performance difference
;;; is of the order of an magnitude.

;;; TODO: Use ARRAY or STATIC-ARRAY

(define-constant +one-arg-fn-float-doc+
  "
These functions have a single array as input and a single array as output.
If the output array is not supplied, its element-type is given by *DEFAULT-FLOAT-FORMAT*

For large arrays, you should be happy without any declarations. You can
use *MULTITHREADED-THRESHOLD* to a large enough value to disable lparallel
based multithreading.

Declarations become necessary for smaller arrays. An (OPTIMIZE SPEED) declaration
should help you with optimization by providing optimization notes.
Optimization for small arrays essentially involves inlining, along with:
  (i)  providing the OUT parameter to allow the user to eliminate allocation wherever possible
  (ii) eliminating work involved in broadcasting and multithreading

TODO: Provide more details
  "
  :test #'string=)

(define-polymorphic-function one-arg-fn/float (name x &key out broadcast) :overwrite t
  :documentation +one-arg-fn-float-doc+)

(define-condition runtime-array-allocation (suboptimal-polymorph-note)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unable to avoid array allocation at run time. Consider supplying
the OUT argument, and/or ensuring all the appropriate arguments are
arrays of appropriate types."))))

;;; Combined with the above, then, we have the following
;;; cases of optimality of polymorphs:
;;; - OUT is supplied and BROADCAST is NIL; this can in fact be inlined
;;;   - no note is necessary in this case; this is the most optimal case
;;; - OUT is supplied, but BROADCAST is not known to be NIL;
;;;   - this necessitates code that handles broadcasting
;;;   - we point to the more optimal polymorph in this case
;;; - OUT is not supplied, BROADCAST is whatever: no benefits of inlining
;;;   - it will help to signal a OUT not supplied note in this case

;; If OUT is not supplied aka is definitely NIL, then we don't need to worry about broadcasting

(defpolymorph (one-arg-fn/float :inline t)
    ;; most optimal case
    ((name symbol) (x (simple-array single-float))
     &key ((out (simple-array single-float)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array single-float)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:array single-float 1) svx svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svo)
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-o svo))
          (funcall (single-float-c-name name)
                   (array-total-size out)
                   ptr-x 1
                   ptr-o 1)))))
  out)

(defpolymorph (one-arg-fn/float
               :inline :maybe
               :more-optimal-type-list (symbol (simple-array single-float)
                                               &key (:out (simple-array single-float))
                                               (:broadcast null)))
    ;; OUT is supplied, but code may need to handle broadcasting
    ((name symbol) (x (array single-float))
     &key ((out (array single-float)))
     (broadcast nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (%broadcast-compatible-p (narray-dimensions x)
                                   (narray-dimensions out))
        (assert broadcast-compatible-p (x out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x out))
                :array-likes (list x out))
        ;; It is possible for us to use multithreading along with broadcasting for
        ;; DENSE-ARRAYS:ARRAY
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-x   4 ix   x)
           (ptr-out 4 iout out))
          (funcall (single-float-c-name name) n ptr-x ix ptr-out iout)))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (equalp (narray-dimensions x)
                              (narray-dimensions out))))
        (ptr-iterate-but-inner (narray-dimensions out) n
          ((ptr-x   4 ix   x)
           (ptr-out 4 iout out))
          (funcall (single-float-c-name name) n ptr-x ix ptr-out iout))))
  out)

(defpolymorph (one-arg-fn/float :inline :maybe :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (array single-float))
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (simple-array single-float)
  (declare (ignorable name broadcast out))
  (let ((out (nu:zeros-like x)))
    (declare (type (array single-float) out))
    (ptr-iterate-but-inner (narray-dimensions x) n
      ((ptr-x   4 ix   x)
       (ptr-out 4 iout out))
      (funcall (single-float-c-name name) n ptr-x ix ptr-out iout))
    out))

;; Fourth case involving REAL to array; BROADCAST necessarily has to be non-NIL
(defpolymorph (one-arg-fn/float :inline t)
    ((name symbol) (x real)
     &key ((out (array single-float)))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignore broadcast))
  (cffi:with-foreign-pointer (ptr-x 4)
    (setf (cffi:mem-ref ptr-x :float)
          (trivial-coerce:coerce x 'single-float))
    (let ((svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svo))
      (with-thresholded-multithreading/cl
          (cl:array-total-size svo)
          (svo)
        (with-pointers-to-vectors-data ((ptr-o svo))
          (funcall (single-float-c-name name)
                   (array-total-size svo)
                   ptr-x 0
                   ptr-o 1)))))
  out)

;;; The same but for DOUBLE-FLOAT arrays

(defpolymorph (one-arg-fn/float :inline t)
    ;; most optimal case
    ((name symbol) (x (simple-array double-float))
     &key ((out (simple-array double-float)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array double-float)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:array double-float 1) svx svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svo)
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-o svo))
          (funcall (double-float-c-name name)
                   (array-total-size out)
                   ptr-x 1
                   ptr-o 1)))))
  out)

(defpolymorph (one-arg-fn/float
               :inline nil
               :more-optimal-type-list (symbol (simple-array double-float)
                                               &key (:out (simple-array double-float))
                                               (:broadcast null)))
    ;; OUT is supplied, but code needs to handle broadcasting
    ((name symbol) (x (array double-float))
     &key ((out (array double-float)))
     (broadcast nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (%broadcast-compatible-p (narray-dimensions x)
                               (narray-dimensions out))
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    ;; It is possible for us to use multithreading along with broadcasting for
    ;; DENSE-ARRAYS:ARRAY
    (ptr-iterate-but-inner broadcast-dimensions n
      ((ptr-x   8 ix   x)
       (ptr-out 8 iout out))
      (funcall (double-float-c-name name) n ptr-x ix ptr-out iout)))
  out)

(defpolymorph (one-arg-fn/float :inline :maybe :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (array double-float))
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (simple-array double-float)
  (declare (ignorable name broadcast out))
  (let ((out (nu:zeros-like x)))
    (declare (type (array double-float) out))
    (ptr-iterate-but-inner (narray-dimensions x) n
      ((ptr-x   8 ix   x)
       (ptr-out 8 iout out))
      (funcall (double-float-c-name name) n ptr-x ix ptr-out iout))
    out))

;; Fourth case involving REAL to array; BROADCAST necessarily has to be non-NIL
(defpolymorph (one-arg-fn/float :inline t)
    ((name symbol) (x real)
     &key ((out (array double-float)))
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignore broadcast))
  (cffi:with-foreign-pointer (ptr-x 8)
    (setf (cffi:mem-ref ptr-x :double)
          (trivial-coerce:coerce x 'double-float))
    (let ((svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svo))
      (with-thresholded-multithreading/cl
          (cl:array-total-size svo)
          (svo)
        (with-pointers-to-vectors-data ((ptr-o svo))
          (funcall (double-float-c-name name)
                   (array-total-size svo)
                   ptr-x 0
                   ptr-o 1)))))
  out)


;; pure number
(defpolymorph (one-arg-fn/float :inline t) ((name symbol) (x number)
                                            &key ((out null))
                                            (broadcast nu:*broadcast-automatically*))
    (values number &optional)
  (declare (ignorable out name broadcast))
  (nth-value 0 (funcall (cl-name name) x)))

;; lists - 2 polymorphs
(defpolymorph (one-arg-fn/float :inline t :suboptimal-note runtime-array-allocation)
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
  (let ((array (nu:asarray x :type nu:*default-float-format*)))
    (one-arg-fn/float name array :out array :broadcast broadcast)))

(defpolymorph (one-arg-fn/float :inline t :suboptimal-note runtime-array-allocation)
    ((name symbol) (x list)
     &key ((out array))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignorable out))
  ;; TODO: Define copy from list to array directly
  (one-arg-fn/float name (nu:asarray x :type (array-element-type out)) :out out :broadcast broadcast))

;; non-float arrays - these two are recursive
(defpolymorph (one-arg-fn/float :inline nil :suboptimal-note runtime-array-allocation)
    ((name symbol) (x array)
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (let ((out (nu:zeros (array-dimensions x) :type nu:*default-float-format*)))
    (declare (type array out))
    (nu:copy x :out out)
    (one-arg-fn/float name out :out out :broadcast broadcast)))

(defpolymorph (one-arg-fn/float :inline nil)
    ((name symbol) (x array)
     &key ((out (or (array single-float) (array double-float))))
     (broadcast nu:*broadcast-automatically*))
    (values array &optional)
  (nu:copy x :out out)
  (one-arg-fn/float name out :out out :broadcast broadcast))

(macrolet ((def (name
                 (single-float-error &optional (sf-min 0.0f0) (sf-max 1.0f0))
                 (double-float-error &optional (df-min 0.0d0) (df-max 1.0d0)))
             `(progn
                (define-polymorphic-function ,name (x &key out broadcast)
                  :overwrite t :documentation +one-arg-fn-float-doc+)
                (defpolymorph ,name (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/float ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
                  (one-arg-fn/float ',name x :out out :broadcast broadcast))
                (define-numericals-one-arg-test ,name nu::array
                  (,single-float-error ,sf-min ,sf-max)
                  (,double-float-error ,df-min ,df-max)))))
  (def nu:sin (2f-7) (1d-15))
  (def nu:cos (2f-7) (1d-15))
  (def nu:tan (2f-7) (1d-15))

  (def nu:asin (2f-7) (1d-15))
  (def nu:acos (2f-7) (1d-15))
  ;; (def nu:atan (2f-7) (1d-15)) ; Handle atan case specially

  (def nu:sinh (2f-7) (1d-15))
  (def nu:cosh (2f-7) (1d-15))
  (def nu:tanh (2f-7) (1d-15))

  (def nu:asinh (2f-7) (1d-15))
  (def nu:acosh (2f-7 1.0f0 2.0f0) (1d-15 1.0d0 2.0d0))
  (def nu:atanh (2f-7) (1d-15))

  (def nu:exp (2f-7) (1d-15))
  ;; (def nu:sqrt (bmas:ssqrt 2f-7) (bmas:dsqrt 1d-15))
  )

;; Handle atan case specially
(define-polymorphic-function nu:atan (x &rest args)
  :overwrite t :documentation +one-arg-fn-float-doc+)
(defpolymorph nu:atan (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
  (declare (ignore out))
  (one-arg-fn/float 'nu:atan x :broadcast broadcast))
(defpolymorph nu:atan (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
  (one-arg-fn/float 'nu:atan x :out out :broadcast broadcast))
(define-numericals-one-arg-test nu:atan nu::array (2f-7) (1d-15))


(macrolet ((def (name
                 (single-float-error)
                 (double-float-error))
             (eval `(define-polymorphic-function ,name (value &rest args)
                      :overwrite t
                      :documentation +one-arg-fn-float-doc+))
             `(progn
                (define-polymorphic-function ,name (value &rest args))
                (defpolymorph ,name (x &key ((out null)) (broadcast nu:*broadcast-automatically*)) t
                  (declare (ignore out))
                  (one-arg-fn/float ',name x :broadcast broadcast))
                (defpolymorph ,name (x &key ((out (not null))) (broadcast nu:*broadcast-automatically*)) t
                  (one-arg-fn/float ',name x :out out :broadcast broadcast))
                (define-numericals-one-arg-test ,name nu::array
                    (,single-float-error) (,double-float-error)))))
  (def nu:log       (2f-7)  (1d-15))
  (def nu:fround    (0.0f0) (0.0d0))
  (def nu:ftruncate (0.0f0) (0.0d0))
  (def nu:ffloor    (0.0f0) (0.0d0))
  (def nu:fceiling  (0.0f0) (0.0d0)))
