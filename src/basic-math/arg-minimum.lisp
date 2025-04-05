(in-package :numericals/basic-math/impl)

(5am:in-suite :numericals)

(define-polymorphic-function nu:arg-minimum
    (array-like &key out axis keep-dims) :overwrite t
  :documentation "Find the index of the minimum element along the AXIS.")

(defpolymorph out-shape-compatible-p ((name (eql arg-minimum)) out in axis keep-dims)
    boolean
  (declare (optimize speed)
           (type nu:array out in)
           (type (integer 0 #.array-rank-limit) axis))
  (if keep-dims
      (loop :for i :below (nu:array-rank in)
            :for d1 :of-type size :in (narray-dimensions in)
            :for d2 :of-type size :in (narray-dimensions out)
            :always (if (cl:= i axis)
                        (cl:= 1 d2)
                        (cl:= d1 d2)))
      (loop :for i :below (nu:array-rank in)
            :for d1 :of-type size :in (narray-dimensions in)
            :with out-dims := (narray-dimensions out)
            :for (d2 . rem-dims) := out-dims
            :always (locally (declare (type (or null size) d2))
                      (if (cl:= i axis)
                          t
                          (cl:= d1 d2)))
            :do (unless (cl:= i axis) (setq out-dims rem-dims)))))

(defpolymorph nu:arg-minimum ((array (nu:simple-array <type>))
                              &key ((axis integer))
                              ((keep-dims boolean))
                              ((out (nu:simple-array (signed-byte 64)))))
    (nu:simple-array (signed-byte 64))
  (declare (ignorable keep-dims))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (out-shape-compatible-p 'arg-minimum out array axis keep-dims)
                  (array out)
                  "To find arg-minimum of an array of dimensions ~A on axis ~D~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions array)
                  axis
                  (if keep-dims
                      (loop :for i :below (nu:array-rank array)
                            :for d :of-type size :in (narray-dimensions array)
                            :collect (if (cl:= i axis) 1 d))
                      (loop :for i :below (nu:array-rank array)
                            :for d :of-type size :in (narray-dimensions array)
                            :if (cl:/= i axis)
                              :collect d))
                  keep-dims
                  (narray-dimensions out)))
    (pflet* ((c-name-hmin (c-name <type> 'nu:arg-minimum))
             (c-size (c-size <type>))
             (stride (array-stride array axis))
             (out-rank (- (nu:array-rank array) axis))
             (in-rank  (- (nu:array-rank out) axis)))
      (declare (type (simple-array (signed-byte 64)) out)
               (type (unsigned-byte 16) c-size))
      (flet ((arg-minimum (in-size in-ptr in-dims out-ptr out-dims)
               (declare (ignorable out-dims)
                        (cl:type cffi:foreign-pointer in-ptr out-ptr)
                        (type (unsigned-byte 32) in-size))
               (let* ((n (first in-dims))
                      (out-size (cl:floor in-size n)))
                 (declare (type (unsigned-byte 32) n out-size))
                 ;; TODO: Find an alternative path (look at minimum)
                 ;; to optimize the case when array is of dimension (10000 1)
                 ;; and axis to compute argmin over is 1.
                 (loop :repeat out-size
                       :do (locally (declare (#+sbcl sb-ext:muffle-conditions style-warning))
                             (funcall #'(setf fref)
                                      (funcall c-name-hmin n in-ptr stride)
                                      out-ptr
                                      :int64))
                           (cffi:incf-pointer out-ptr 8)
                           (cffi:incf-pointer in-ptr c-size)))))
        (with-simple-array-broadcast (arg-minimum out-rank in-rank)
          (array c-size) (out 8))
        out))))

(defpolymorph out-shape ((name (eql arg-minimum)) in-array axis keep-dims) list
  (declare (optimize speed)
           (type nu:array in-array)
           (type (integer 0 #.array-rank-limit) axis))
  (if keep-dims
      (loop :for i :below (nu:array-rank in-array)
            :for d :of-type size :in (narray-dimensions in-array)
            :collect (if (cl:= i axis) 1 d))
      (loop :for i :below (nu:array-rank in-array)
            :for d :of-type size :in (narray-dimensions in-array)
            :if (cl:/= i axis)
              :collect d)))

(defpolymorph (nu:arg-minimum :inline t
                              :suboptimal-note runtime-array-allocation)
    ((array (nu:simple-array <type>))
     &key ((axis integer))
     ((keep-dims boolean))
     ((out null)))
    t
  (declare (ignore out))
  (pflet* ((out (nu:zeros (out-shape 'arg-minimum array axis keep-dims)
                          :type '(signed-byte 64))))
    (declare (type (simple-array (signed-byte 64)) out))
    (nu:arg-minimum array :out out :keep-dims keep-dims :axis axis)))

(defpolymorph (nu:arg-minimum :inline t) ((array (nu:simple-array <type>))
                                          &key ((axis null))
                                          ((keep-dims null))
                                          ((out null)))
    t
  (declare (ignore out keep-dims axis))
  (pflet ((c-name (c-name <type> 'nu:arg-minimum))
          (array-storage (array-storage array)))
    (declare (type (cl:simple-array <type> 1) array-storage))
    (with-pointers-to-vectors-data ((ptr array-storage))
      (funcall c-name (array-total-size array) ptr 1))))

(defpolymorph (nu:arg-minimum :inline t :suboptimal-note runtime-array-allocation)
    ((array list) &key axis keep-dims out)
    t
  (declare (notinline nu:asarray))
  (nu:arg-minimum (nu:asarray array) :axis axis :keep-dims keep-dims :out out))

(defpolymorph (nu:arg-minimum :inline t) ((x (array <type>))
                                          &key ((axis null))
                                          ((out null))
                                          ((keep-dims null)))
    (signed-byte 64)
  (declare (ignore axis out keep-dims))
  (pflet ((acc     -1)
          (c-name  (c-name <type> 'nu:arg-minimum))
          (c-size  (c-size <type>)))
    (declare (type (signed-byte 64) acc))
    (ptr-iterate-but-inner (narray-dimensions x)
        n
      ((ptr-x c-size inc-x x))
      (setq acc
            (cl:min acc (inline-or-funcall c-name n ptr-x inc-x))))
    acc))

(defpolymorph (nu:arg-minimum :inline t) ((x (array <type>))
                                          &key ((axis null))
                                          ((out null))
                                          ((keep-dims (not null))))
    (array (signed-byte 64))
  (declare (ignore axis out keep-dims))
  (let ((result (nu:arg-minimum x :axis nil :out nil :keep-dims nil)))
    (make-array (make-list (array-rank x) :initial-element 1)
                :initial-element result :element-type '(signed-byte 64))))


;; (defpolymorph (nu:arg-minimum :inline t)

;;     ((x (array <type>))
;;      &key ((axis integer))
;;      keep-dims
;;      ((out (array <type>))
;;       (nu:full
;;        (let ((dim (narray-dimensions x)))
;;          (append (subseq dim 0 axis)
;;                  (if keep-dims (list 1) ())
;;                  (nthcdr (+ 1 axis) dim)))
;;        :type (array-element-type x)
;;        :value (type-min (array-element-type x)))))

;;     t

;;   (assert (< axis (array-rank x)))
;;   (pflet* ((rank (array-rank x))
;;            (perm
;;             (loop :for i :below rank
;;                   :collect (cond ((= i (1- rank)) axis)
;;                                  ((= i axis) (1- rank))
;;                                  (t i))))
;;            (out
;;             (nu:transpose
;;              (nu:reshape out
;;                          (let ((dim
;;                                  (array-dimensions (the array x))))
;;                            (setf (nth axis dim) 1)
;;                            dim))
;;              :axes perm))
;;            (x (nu:transpose x :axes perm))
;;            (c-fn (c-name <type> 'nu:arg-minimum))
;;            (c-size (c-size <type>))
;;            (c-type (c-type <type>)))
;;     (declare (type (array <type>) x out))
;;     (ptr-iterate-but-inner (narray-dimensions x)
;;         n
;;       ((ptr-x c-size inc-x x)
;;        (ptr-out c-size inc-out out))
;;       (setf (cffi:mem-ref ptr-out c-type)
;;             (funcall c-fn n ptr-x inc-x))))
;;   (if (zerop (array-rank out))
;;       (row-major-aref out 0)
;;       out))

;; FIXME: These tests are not exhaustive.
(5am:def-test nu:arg-minimum ()
  (flet ((our-arg-minimum (array)
           (let ((min most-positive-double-float)
                 (min-id -1)
                 (id 0))
             (nu:do-arrays ((x array))
               (when (<= x min)
                 (setq min x
                       min-id id))
               (incf id))
             min-id)))
    (loop :for *array-element-type* :in `(single-float
                                          double-float
                                          (signed-byte 64)
                                          (signed-byte 32)
                                          (signed-byte 16)
                                          (signed-byte 08)
                                          (unsigned-byte 64)
                                          (unsigned-byte 32)
                                          (unsigned-byte 16)
                                          (unsigned-byte 08)
                                          fixnum)
          :do
             (5am:is (= 0 (nu:arg-minimum (nu:asarray '(1 2 3)))))
             (5am:is (= 0 (nu:arg-minimum (nu:asarray '((1 2 3)
                                                        (4 5 6)
                                                        (7 8 9))))))
             (5am:is (nu:array= (nu:asarray '(0 0 0))
                                (nu:arg-minimum (nu:asarray '((1 2 3)
                                                              (4 5 6)))
                                                :axis 0
                                                :out (nu:zeros 3 :type 'int64))))
             (5am:is (nu:array= (nu:asarray '((0 0 0)))
                                (nu:arg-minimum (nu:asarray '((1 2 3)
                                                              (4 5 6)))
                                                :axis 0
                                                :out (nu:zeros 1 3 :type 'int64)
                                                :keep-dims t)))
             (5am:is (nu:array= (nu:asarray '(0 0))
                                (nu:arg-minimum (nu:asarray '((1 2 3)
                                                              (4 5 6)))
                                                :axis 1
                                                :out (nu:zeros 2 :type 'int64))))
             (5am:is (nu:array= (nu:asarray '((0) (0)))
                                (nu:arg-minimum (nu:asarray '((1 2 3)
                                                              (4 5 6)))
                                                :axis 1
                                                :out (nu:zeros 2 1 :type 'int64)
                                                :keep-dims t)))
             (5am:is (nu:array= (nu:asarray '((0 0 0)
                                              (1 1 1)))
                                (nu:arg-minimum (nu:asarray '(((1 2 3)
                                                               (4 5 6))
                                                              ((7 8 9)
                                                               (0 1 2))))
                                                :axis 1
                                                :out (nu:zeros 2 3 :type 'int64))))
             (5am:is (nu:array= (nu:asarray '((0 0)
                                              (0 0)))
                                (nu:arg-minimum (nu:asarray '(((1 2 3)
                                                               (4 5 6))
                                                              ((7 8 9)
                                                               (0 1 2))))
                                                :axis 2
                                                :out (nu:zeros 2 2 :type 'int64))))
             (5am:is (nu:array= (nu:asarray '((0)))
                                (nu:arg-minimum (nu:asarray '((1 2 3)
                                                              (4 5 6)))
                                                :keep-dims t)))

             (loop for size from 1000
                   repeat 32
                   do (let* ((array
                               (nu:rand size
                                        :min (cl:round
                                              (type-min *array-element-type*)
                                              2)
                                        :min (cl:round
                                              (type-max *array-element-type*)
                                              2)))
                             (actual-idx   (nu:arg-minimum array))
                             (expected-idx (our-arg-minimum array)))
                        (5am:is (cl:= (aref array actual-idx)
                                      (aref array expected-idx))
                                "value ~a at actual index ~a~%  BUT~%value ~a at expected index ~a"
                                (aref array actual-idx) actual-idx
                                (aref array expected-idx) expected-idx))))))
