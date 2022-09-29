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

;;; As with one arg float functions, following are the
;;; cases of optimality of polymorphs:
;;; - OUT is supplied and BROADCAST is NIL; this can in fact be inlined
;;;   - no note is necessary in this case; this is the most optimal case
;;; - OUT is supplied, but BROADCAST is not known to be NIL;
;;;   - this necessitates code that handles broadcasting
;;;   - we point to the more optimal polymorph in this case
;;; - OUT is not supplied, but operating on SIMPLE-ARRAY with BROADCAST as NIL
;;;   - sometimes the cost of creating the OUT is comparable to BROADCAST being non-NIL
;;;   - it will help to signal a OUT not supplied note in this case
;;; - OUT is not supplied, BROADCAST is whatever: no benefits of inlining
;;;   - it will help to signal a OUT not supplied note in this case

;;; In addition to these, there are 2 other polymorphs dealing with REAL to arrays

;; Most optimal case - OUT is supplied, BROADCAST is NIL
(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x (simple-array single-float)) (y (simple-array single-float))
     &key ((out (simple-array single-float)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array single-float)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y)))
       (assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:array single-float 1) svx svy svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* 4 (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* 4 (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (* 4 (cl-array-offset svo)))
          (funcall single-float-c-name
                   (array-total-size svo)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)

(defpolymorph (two-arg-fn/float :inline t :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (simple-array single-float)) (y (simple-array single-float))
     &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array single-float)
  ;; OUT is unsupplied, but BROADCAST is known to be NIL
  (declare (ignorable name out broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let* ((out (nu:zeros (narray-dimensions x) :type 'single-float))
           (single-float-c-name (single-float-c-name name))
           (svx (array-storage x))
           (svy (array-storage y))
           (svo (array-storage out)))
      (declare (type (cl:array single-float 1) svx svy svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* 4 (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* 4 (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (* 4 (cl-array-offset svo)))
          (funcall single-float-c-name
                   (array-total-size svo)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))
      out)))

(defpolymorph (two-arg-fn/float
               :inline :maybe
               :more-optimal-type-list (symbol (simple-array single-float)
                                               (simple-array single-float)
                                               &key (:out (simple-array single-float))
                                               (:broadcast null)))
    ;; Slightly unoptimal case - OUT is supplied, BROADCAST is not known to be NIL
    ((name symbol) (x (array single-float)) (y (array single-float))
     &key ((out (array single-float)))
     (broadcast nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y out)
        (assert broadcast-compatible-p (x y out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y out))
                :array-likes (list x y out))
        (let ((single-float-c-name (single-float-c-name name)))
          (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 4 ix x)
                                                         (ptr-y 4 iy y)
                                                         (ptr-o 4 io out))
                                 (funcall single-float-c-name
                                          n
                                          ptr-x ix
                                          ptr-y iy
                                          ptr-o io))))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (and (equalp (narray-dimensions x)
                                       (narray-dimensions y))
                               (equalp (narray-dimensions x)
                                       (narray-dimensions out))))))
        (let ((single-float-c-name (single-float-c-name name)))
          (ptr-iterate-but-inner (narray-dimensions x) n ((ptr-x 4 ix x)
                                                          (ptr-y 4 iy y)
                                                          (ptr-o 4 io out))
                                 (funcall single-float-c-name
                                          n
                                          ptr-x ix
                                          ptr-y iy
                                          ptr-o io)))))
  out)

(defpolymorph (two-arg-fn/float
               :inline :maybe
               :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (array single-float)) (y (array single-float))
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name out broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y)
        (assert broadcast-compatible-p (x y)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y))
                :array-likes (list x y))
        (let ((out (nu:zeros broadcast-dimensions :type 'single-float)))
          (declare (type (array single-float) out))
          (let ((single-float-c-name (single-float-c-name name)))
            (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 4 ix x)
                                                           (ptr-y 4 iy y)
                                                           (ptr-o 4 io out))
                                   (funcall single-float-c-name
                                            n
                                            ptr-x ix
                                            ptr-y iy
                                            ptr-o io)))
          out))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions y)))))
        (let ((out (nu:zeros (narray-dimensions x) :type 'single-float)))
          (declare (type (array single-float) out))
          (let ((single-float-c-name (single-float-c-name name)))
            (ptr-iterate-but-inner (narray-dimensions x) n ((ptr-x 4 ix x)
                                                            (ptr-y 4 iy y)
                                                            (ptr-o 4 io out))
                                   (funcall single-float-c-name
                                            n
                                            ptr-x ix
                                            ptr-y iy
                                            ptr-o io)))
          out))))

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array single-float)) (y number)
     &key ((out (or null (array single-float))))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (let ((out (or out (nu:zeros (narray-dimensions x) :type 'single-float)))
        (single-float-c-name (single-float-c-name name)))
    (declare (type (array single-float) out))
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
          (funcall single-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io))))
    out))

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array single-float)) 
     &key ((out (or null (array single-float))))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array single-float)
  (declare (ignorable name broadcast))
  (let ((out (or out (nu:zeros (narray-dimensions y) :type 'single-float)))
        (single-float-c-name (single-float-c-name name)))
    (declare (type (array single-float) out))
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
                   ptr-o io))))
    out))

;;; double-float - 6 polymorphs

(defpolymorph (two-arg-fn/float :inline t)
    ((name symbol) (x (simple-array double-float)) (y (simple-array double-float))
     &key ((out (simple-array double-float)))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array double-float)
  (declare (ignorable name broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y)))
       (assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:array double-float 1) svx svy svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* 8 (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* 8 (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (* 8 (cl-array-offset svo)))
          (funcall double-float-c-name
                   (array-total-size svo)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)

(defpolymorph (two-arg-fn/float :inline t :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (simple-array double-float)) (y (simple-array double-float))
     &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array double-float)
  ;; OUT is unsupplied, but BROADCAST is known to be NIL
  (declare (ignorable name out broadcast))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions y))))
    (let* ((out (nu:zeros (narray-dimensions x) :type 'double-float))
           (double-float-c-name (double-float-c-name name))
           (svx (array-storage x))
           (svy (array-storage y))
           (svo (array-storage out)))
      (declare (type (cl:array double-float 1) svx svy svo))
      (with-thresholded-multithreading/cl
          (array-total-size svo)
          (svx svy svo)
        (with-pointers-to-vectors-data ((ptr-x (array-storage svx))
                                        (ptr-y (array-storage svy))
                                        (ptr-o (array-storage svo)))
          (cffi:incf-pointer ptr-x (* 8 (cl-array-offset svx)))
          (cffi:incf-pointer ptr-y (* 8 (cl-array-offset svy)))
          (cffi:incf-pointer ptr-o (* 8 (cl-array-offset svo)))
          (funcall double-float-c-name
                   (array-total-size svo)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))
      out)))

(defpolymorph (two-arg-fn/float
               :inline :maybe
               :more-optimal-type-list (symbol (simple-array double-float)
                                               (simple-array double-float)
                                               &key (:out (simple-array double-float))
                                               (:broadcast null)))
    ;; Slightly unoptimal case - OUT is supplied, BROADCAST is not known to be NIL
    ((name symbol) (x (array double-float)) (y (array double-float))
     &key ((out (array double-float)))
     (broadcast nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y out)
        (assert broadcast-compatible-p (x y out)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y out))
                :array-likes (list x y out))
        (let ((double-float-c-name (double-float-c-name name)))
          (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 8 ix x)
                                                         (ptr-y 8 iy y)
                                                         (ptr-o 8 io out))
                                 (funcall double-float-c-name
                                          n
                                          ptr-x ix
                                          ptr-y iy
                                          ptr-o io))))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (and (equalp (narray-dimensions x)
                                       (narray-dimensions y))
                               (equalp (narray-dimensions x)
                                       (narray-dimensions out))))))
        (let ((double-float-c-name (double-float-c-name name)))
          (ptr-iterate-but-inner (narray-dimensions x) n ((ptr-x 8 ix x)
                                                          (ptr-y 8 iy y)
                                                          (ptr-o 8 io out))
                                 (funcall double-float-c-name
                                          n
                                          ptr-x ix
                                          ptr-y iy
                                          ptr-o io)))))
  out)

(defpolymorph (two-arg-fn/float
               :inline :maybe
               :suboptimal-note runtime-array-allocation)
    ((name symbol) (x (array double-float)) (y (array double-float))
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name out broadcast))
  (if broadcast
      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
          (broadcast-compatible-p x y)
        (assert broadcast-compatible-p (x y)
                'incompatible-broadcast-dimensions
                :dimensions (mapcar #'narray-dimensions (list x y))
                :array-likes (list x y))
        (let ((out (nu:zeros broadcast-dimensions :type 'double-float)))
          (declare (type (array double-float) out))
          (let ((double-float-c-name (double-float-c-name name)))
            (ptr-iterate-but-inner broadcast-dimensions n ((ptr-x 8 ix x)
                                                           (ptr-y 8 iy y)
                                                           (ptr-o 8 io out))
                                   (funcall double-float-c-name
                                            n
                                            ptr-x ix
                                            ptr-y iy
                                            ptr-o io)))
          out))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions y)))))
        (let ((out (nu:zeros (narray-dimensions x) :type 'double-float)))
          (declare (type (array double-float) out))
          (let ((double-float-c-name (double-float-c-name name)))
            (ptr-iterate-but-inner (narray-dimensions x) n ((ptr-x 8 ix x)
                                                            (ptr-y 8 iy y)
                                                            (ptr-o 8 io out))
                                   (funcall double-float-c-name
                                            n
                                            ptr-x ix
                                            ptr-y iy
                                            ptr-o io)))
          out))))

(defpolymorph two-arg-fn/float
    ((name symbol) (x (array double-float)) (y number)
     &key ((out (or null (array double-float))))
     ;; The very fact that we are allowing Y to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (let ((double-float-c-name (double-float-c-name name))
        (out (or out (nu:zeros (narray-dimensions x) :type 'double-float))))
    (declare (type (array double-float) out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x out)
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (cffi:with-foreign-pointer (ptr-y 8)
        (setf (cffi:mem-ref ptr-y :double) (trivial-coerce:coerce y 'double-float))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-x 8 ix x)
           (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io))))
    out))

(defpolymorph two-arg-fn/float
    ((name symbol) (x number) (y (array double-float)) 
     &key ((out (or null (array double-float))))
     ;; The very fact that we are allowing X to be NUMBER implies
     ;; BROADCAST must be non-NIL
     ((broadcast (not null)) nu:*broadcast-automatically*))
    (array double-float)
  (declare (ignorable name broadcast))
  (let ((out (or out (nu:zeros (narray-dimensions y) :type 'double-float)))
        (double-float-c-name (double-float-c-name name)))
    (declare (type (array double-float) out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p y out)
      (assert broadcast-compatible-p (y out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list y out))
              :array-likes (list y out))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :double) (trivial-coerce:coerce X 'double-float))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-y 8 iy y)
           (ptr-o 8 io out))
          (funcall double-float-c-name
                   n
                   ptr-x 0
                   ptr-y iy
                   ptr-o io))))
    out))



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
