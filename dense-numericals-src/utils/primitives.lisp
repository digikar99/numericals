(in-package :dense-numericals/utils/impl)

(declaim (inline fast-empty))
(defun fast-empty (shape &key (type default-element-type)
                           (layout *array-layout*))
  (declare (type (member :row-major :column-major) layout)
           (optimize speed))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let* ((total-size (loop :for s :of-type size :in shape
                           :with total-size :of-type size := 1
                           :do (setf total-size (cl:* total-size s))
                           :finally (return total-size)))
         (rank       (length (the list shape)))
         (storage ;; Allocate, but do not fill
           #+sbcl
           (multiple-value-bind (widetag shift) (sb-vm::%vector-widetag-and-n-bits-shift type)
             (sb-vm::allocate-vector-with-widetag widetag total-size shift))
           #-sbcl
           (cl:make-array total-size :element-type type)))
    (make-instance 'standard-dense-array
                   :storage storage
                   :element-type (cl:array-element-type storage)
                   :dimensions shape
                   :strides (dimensions->strides shape layout)
                   :offsets (make-list rank :initial-element 0)
                   :total-size total-size
                   :root-array nil
                   :rank rank
                   :layout layout)))

(defun fast-full (shape &key (type default-element-type)
                          (layout *array-layout*)
                          value)
  (declare (type (member :row-major :column-major) layout)
           (optimize speed))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let* ((total-size (loop :for s :of-type size :in shape
                           :with total-size :of-type size := 1
                           :do (setf total-size (cl:* total-size s))
                           :finally (return total-size)))
         (rank       (length (the list shape)))
         (storage    (cl:make-array total-size :element-type type
                                               :initial-element value)))
    (make-instance 'standard-dense-array
                   :storage storage
                   :element-type (cl:array-element-type storage)
                   :dimensions shape
                   :strides (dimensions->strides shape layout)
                   :offsets (make-list rank :initial-element 0)
                   :total-size total-size
                   :root-array nil
                   :rank rank
                   :layout layout)))

(define-splice-list-fn full (shape &key (type default-element-type)
                                      (layout *array-layout*)
                                      value)
  (fast-full shape :type type :layout layout :value value))

(define-splice-list-fn empty (shape &key (type default-element-type)
                                       (layout *array-layout*))
  (fast-empty shape :type type :layout layout))

(define-splice-list-fn zeros (shape &key (type default-element-type) (layout *array-layout*))
  (fast-full shape :type type :layout layout :value (coerce 0 type)))

(define-splice-list-fn ones (shape &key (type default-element-type) (layout *array-layout*))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (full (the list shape) :type type :layout layout :value (coerce 1 type)))

(define-splice-list-fn rand (shape &key (type default-element-type)
                                   (layout *array-layout*)
                                   (min (coerce 0 type))
                                   (max (coerce 1 type)))
  (when (listp (first shape))
    (assert (null (rest shape)))
    (setq shape (first shape)))
  (let ((a     (full shape :type type :layout layout :value (coerce 0 type)))
        (range (- max min))
        (min   (coerce min type)))
    (declare (type simple-array a))
    (do-arrays ((a-elt a))
      (setf a-elt (+ min (random range))))
    a))

(defun empty-like (array-like)
  (empty (dimensions array-like) :type (element-type array-like)))

(defun zeros-like (array-like)
  (zeros (dimensions array-like) :type (element-type array-like)))

(defun ones-like (array-like)
  (ones (dimensions array-like) :type (element-type array-like)))

(defun rand-like (array-like)
  (rand (dimensions array-like) :type (element-type array-like)))

(defun full-like (array-like value)
  (full (dimensions array-like) :value value :type (element-type array-like)))

(declaim (inline fill))
(defun fill (array value)
  (declare (type array array))
  (let* ((type  (array-element-type array))
         (value (coerce value type)))
    (if (typep array 'simple-array)
        (let* ((size    (array-total-size array))
               (storage (array-storage array)))
          (cl:fill storage value :start 0 :end size))
        (do-arrays ((elt array))
          (setf elt value)))
    array))
