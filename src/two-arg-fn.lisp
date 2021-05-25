(in-package :numericals.internals)

(define-polymorphic-function two-arg-fn (name x y &key out) :overwrite t)

;; Pure number
(defpolymorph two-arg-fn
    ((name non-comparison-operator)
     (x number) (y number) &key ((out null) nil))
    (values number &optional)
  (declare (ignore out)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

(defpolymorph two-arg-fn
    ((name comparison-operator)
     (x number) (y number) &key ((out null) nil))
    bit
  (declare (ignore out)
           (ignorable name))
  (if (funcall (cl-name name) x y)
      1
      0))


;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT may be initialized in the lambda-list itself
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn name (nu:asarray x) (nu:asarray y) :out out))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn name x (nu:asarray y) :out out))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y number) &key ((out array)))
    (values array &optional)
  (two-arg-fn name (nu:asarray x) y :out out))

(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name (nu:asarray x) (nu:asarray y)))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x number) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name x (nu:asarray y)))
(defpolymorph (two-arg-fn :inline t)
    ((name symbol) (x list) (y number) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn name (nu:asarray x) y))



;;; single-float - 2+2 + 2+2 polymorphs
;;; TODO: Perform broadcasting even for NUMBER cases

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (nu:zeros (array-dimensions x) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svx svo))
      (cffi:with-foreign-pointer (ptr-y 4)
        (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (* 4 (cl-array-offset x)))
            (cffi:incf-pointer ptr-o (* 4 (cl-array-offset out)))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array single-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svx)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-y 4)
        (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (* 4 (cl-array-offset x)))
            (cffi:incf-pointer ptr-o (* 1 (cl-array-offset out)))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

;;; There are 2x2 of these (above and below) because functions may not be commutative

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x number) (y (array single-float))
     &key ((out (array single-float))
           (nu:zeros (array-dimensions y) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svy)
               (type (cl:simple-array single-float 1) svo))
      (cffi:with-foreign-pointer (ptr-x 4)
        (setf (cffi:mem-ref ptr-x :float) (coerce x 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (* 4 (cl-array-offset y)))
            (cffi:incf-pointer ptr-o (* 4 (cl-array-offset out)))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn
    ((name comparison-operator) (x number) (y (array single-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-x 4)
        (setf (cffi:mem-ref ptr-x :float) (coerce x 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (* 4 (cl-array-offset y)))
            (cffi:incf-pointer ptr-o (* 1 (cl-array-offset out)))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)



(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out null)))
    (values (array single-float) &optional)
  (declare (ignorable name out))
  (let ((single-float-c-name (single-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y)))
    (cond ((equalp dim-x dim-y)
           (two-arg-fn/non-broadcast name x y))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (%broadcast-compatible-p dim-x dim-y)
             (assert broadcast-compatible-p (x y)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y)
                     :array-likes (list x y))
             (let ((out (nu:zeros broadcast-dimensions :type 'single-float)))
               (declare (type (array single-float) out))
               (with-thresholded-multithreading (array-total-size (the array out))
                   (x y out)
                 (ptr-iterate-but-inner broadcast-dimensions n
                   ((ptr-x 4 ix x)
                    (ptr-y 4 iy y)
                    (ptr-o 4 io out))
                   (funcall single-float-c-name
                            n
                            ptr-x ix
                            ptr-y iy
                            ptr-o io)))
               out))))))

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array single-float))))
    (values (array single-float) &optional)
  (declare (ignorable name))
  (let ((single-float-c-name (single-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y))
        (dim-o (array-dimensions out)))
    (cond ((and (equalp dim-x dim-y)
                (equalp dim-y dim-o))
           (two-arg-fn/non-broadcast name x y :out out))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p dim-x dim-y dim-o)
             (assert broadcast-compatible-p (x y out)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y dim-o)
                     :array-likes (list x y out))
             (with-thresholded-multithreading (array-total-size out)
                 (x y out)
               (ptr-iterate-but-inner broadcast-dimensions n
                 ((ptr-x 4 ix x)
                  (ptr-y 4 iy y)
                  (ptr-o 4 io out))
                 (funcall single-float-c-name
                          n
                          ptr-x ix
                          ptr-y iy
                          ptr-o io)))
             out)))))

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out null)))
    (values (array (unsigned-byte 8)) &optional)
  (declare (ignorable name out))
  (let ((single-float-c-name (single-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y)))
    (cond ((equalp dim-x dim-y)
           (two-arg-fn/non-broadcast name x y))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (%broadcast-compatible-p dim-x dim-y)
             (assert broadcast-compatible-p (x y)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y)
                     :array-likes (list x y))
             (let ((out (nu:zeros broadcast-dimensions :type '(unsigned-byte 8))))
               (declare (type (array (unsigned-byte 8)) out))
               (with-thresholded-multithreading (array-total-size (the array out))
                   (x y out)
                 (ptr-iterate-but-inner broadcast-dimensions n
                   ((ptr-x 4 ix x)
                    (ptr-y 4 iy y)
                    (ptr-o 1 io out))
                   (funcall single-float-c-name
                            n
                            ptr-x ix
                            ptr-y iy
                            ptr-o io)))
               out))))))

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array (unsigned-byte 8)))))
    (values (array (unsigned-byte 8)) &optional)
  (declare (ignorable name))
  (let ((single-float-c-name (single-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y))
        (dim-o (array-dimensions out)))
    (cond ((and (equalp dim-x dim-y)
                (equalp dim-y dim-o))
           (two-arg-fn/non-broadcast name x y :out out))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p dim-x dim-y dim-o)
             (assert broadcast-compatible-p (x y out)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y dim-o)
                     :array-likes (list x y out))
             (with-thresholded-multithreading (array-total-size out)
                 (x y out)
               (ptr-iterate-but-inner broadcast-dimensions n
                 ((ptr-x 4 ix x)
                  (ptr-y 4 iy y)
                  (ptr-o 1 io out))
                 (funcall single-float-c-name
                          n
                          ptr-x ix
                          ptr-y iy
                          ptr-o io)))
             out)))))



;;; double-float - 2+2 + 2+2 polymorphs
;;; TODO: Perform broadcasting even for NUMBER cases

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array double-float)) (y number)
     &key ((out (array double-float))
           (nu:zeros (array-dimensions x) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svx svo))
      (cffi:with-foreign-pointer (ptr-y 8)
        (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (* 8 (cl-array-offset x)))
            (cffi:incf-pointer ptr-o (* 8 (cl-array-offset out)))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array double-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svx)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-y 8)
        (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (* 8 (cl-array-offset x)))
            (cffi:incf-pointer ptr-o (* 1 (cl-array-offset out)))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

;;; There are 2x2 of these (above and below) because functions may not be commutative

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x number) (y (array double-float))
     &key ((out (array double-float))
           (nu:zeros (array-dimensions y) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svy)
               (type (cl:simple-array double-float 1) svo))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :double) (coerce x 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (* 8 (cl-array-offset y)))
            (cffi:incf-pointer ptr-o (* 8 (cl-array-offset out)))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn
    ((name comparison-operator) (x number) (y (array double-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :double) (coerce x 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (* 8 (cl-array-offset y)))
            (cffi:incf-pointer ptr-o (* 1 (cl-array-offset out)))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)



(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out null)))
    (values (array double-float) &optional)
  (declare (ignorable name out))
  (let ((double-float-c-name (double-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y)))
    (cond ((equalp dim-x dim-y)
           (two-arg-fn/non-broadcast name x y))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (%broadcast-compatible-p dim-x dim-y)
             (assert broadcast-compatible-p (x y)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y)
                     :array-likes (list x y))
             (let ((out (nu:zeros broadcast-dimensions :type 'double-float)))
               (declare (type (array double-float) out))
               (with-thresholded-multithreading (array-total-size (the array out))
                   (x y out)
                 (ptr-iterate-but-inner broadcast-dimensions n
                   ((ptr-x 8 ix x)
                    (ptr-y 8 iy y)
                    (ptr-o 8 io out))
                   (funcall double-float-c-name
                            n
                            ptr-x ix
                            ptr-y iy
                            ptr-o io)))
               out))))))

(defpolymorph two-arg-fn
    ((name non-comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array double-float))))
    (values (array double-float) &optional)
  (declare (ignorable name))
  (let ((double-float-c-name (double-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y))
        (dim-o (array-dimensions out)))
    (cond ((and (equalp dim-x dim-y)
                (equalp dim-y dim-o))
           (two-arg-fn/non-broadcast name x y :out out))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p dim-x dim-y dim-o)
             (assert broadcast-compatible-p (x y out)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y dim-o)
                     :array-likes (list x y out))
             (with-thresholded-multithreading (array-total-size out)
                 (x y out)
               (ptr-iterate-but-inner broadcast-dimensions n
                 ((ptr-x 8 ix x)
                  (ptr-y 8 iy y)
                  (ptr-o 8 io out))
                 (funcall double-float-c-name
                          n
                          ptr-x ix
                          ptr-y iy
                          ptr-o io)))
             out)))))

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out null)))
    (values (array (unsigned-byte 8)) &optional)
  (declare (ignorable name out))
  (let ((double-float-c-name (double-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y)))
    (cond ((equalp dim-x dim-y)
           (two-arg-fn/non-broadcast name x y))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (%broadcast-compatible-p dim-x dim-y)
             (assert broadcast-compatible-p (x y)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y)
                     :array-likes (list x y))
             (let ((out (nu:zeros broadcast-dimensions :type '(unsigned-byte 8))))
               (declare (type (array (unsigned-byte 8)) out))
               (with-thresholded-multithreading (array-total-size (the array out))
                   (x y out)
                 (ptr-iterate-but-inner broadcast-dimensions n
                   ((ptr-x 8 ix x)
                    (ptr-y 8 iy y)
                    (ptr-o 1 io out))
                   (funcall double-float-c-name
                            n
                            ptr-x ix
                            ptr-y iy
                            ptr-o io)))
               out))))))

(defpolymorph two-arg-fn
    ((name comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array (unsigned-byte 8)))))
    (values (array (unsigned-byte 8)) &optional)
  (declare (ignorable name))
  (let ((double-float-c-name (double-float-c-name name))
        (dim-x (array-dimensions x))
        (dim-y (array-dimensions y))
        (dim-o (array-dimensions out)))
    (cond ((and (equalp dim-x dim-y)
                (equalp dim-y dim-o))
           (two-arg-fn/non-broadcast name x y :out out))
          (t
           (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
               (broadcast-compatible-p dim-x dim-y dim-o)
             (assert broadcast-compatible-p (x y out)
                     'incompatible-broadcast-dimensions
                     :dimensions (list dim-x dim-y dim-o)
                     :array-likes (list x y out))
             (with-thresholded-multithreading (array-total-size out)
                 (x y out)
               (ptr-iterate-but-inner broadcast-dimensions n
                 ((ptr-x 8 ix x)
                  (ptr-y 8 iy y)
                  (ptr-o 1 io out))
                 (funcall double-float-c-name
                          n
                          ptr-x ix
                          ptr-y iy
                          ptr-o io)))
             out)))))



;;; Actual definitions


(define-polymorphic-function nu:expt (base power &key out) :overwrite t)
(defpolymorph nu:expt (base power &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'nu:expt base power))
(defpolymorph nu:expt (base power &key ((out (not null)))) t
  (two-arg-fn 'nu:expt base power :out out))
(define-numericals-two-arg-test nu:expt array
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))


(defpolymorph nu:atan (x y &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'nu::atan2 x y))
(defpolymorph nu:atan (x y &key ((out (not null)))) t
  (two-arg-fn 'nu::atan2 x y :out out))

(define-polymorphic-function nu::atan2 (x y &key out) :overwrite t)
(defpolymorph nu::atan2 (x y &key ((out null))) t
  (declare (ignore out))
  (two-arg-fn 'nu::atan2 x y))
(defpolymorph nu::atan2 (x y &key ((out (not null)))) t
  (two-arg-fn 'nu::atan2 x y :out out))
(define-numericals-two-arg-test nu::atan2 array
    (2f-7  0.0f0 10.0f0 single-float)
    (1d-15 0.0d0 10.0d0 double-float))


;;; As against dense-numericals, the broadcast information cannot be stored
;;; "away" from the operating site; thus, we are forced to check for the
;;; the possibility of broadcasting in each of these functions.

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
                  (two-arg-fn ',name x y))
                (defpolymorph ,name (x y &key ((out (not null)))) t
                  (two-arg-fn ',name x y :out out))
                (define-numericals-two-arg-test ,name array
                    (,single-float-error ,sf-min ,sf-max ,single-float-return-type)
                    (,double-float-error ,df-min ,df-max ,double-float-return-type)))))

  (def nu:two-arg-+ (single-float 1f-7) (double-float 1d-15))
  (def nu:two-arg-- (single-float 1f-7) (double-float 1d-15))
  (def nu:two-arg-* (single-float 1f-7) (double-float 1d-15))
  (def nu:two-arg-/ (single-float 1f-7) (double-float 1d-15))

  (def nu:two-arg-<  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-<= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-=  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg-/= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->= ((unsigned-byte 8) 0) ((unsigned-byte 8) 0))
  (def nu:two-arg->  ((unsigned-byte 8) 0) ((unsigned-byte 8) 0)))
