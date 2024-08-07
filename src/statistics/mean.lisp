(in-package :numericals/statistics)

(5am:in-suite :numericals)

;; TODO: Add documentation

(define-polymorphic-function nu:mean (array-like &key out axes keep-dims) :overwrite t
  :documentation "See https://numpy.org/doc/stable/reference/generated/numpy.mean.html")

(defpolymorph out-shape-compatible-p ((name (eql mean)) out in axes keep-dims) boolean
  (declare (optimize speed)
           (type nu:array out in)
           (type (integer 0 #.array-rank-limit) axes))
  (if keep-dims
      (loop :for i :below (nu:array-rank in)
            :for d1 :of-type size :in (narray-dimensions in)
            :for d2 :of-type size :in (narray-dimensions out)
            :always (if (cl:= i axes)
                        (cl:= 1 d2)
                        (cl:= d1 d2)))
      (loop :for i :below (nu:array-rank in)
            :for d1 :of-type size :in (narray-dimensions in)
            :with out-dims := (narray-dimensions out)
            :for (d2 . rem-dims) := out-dims
            :always (locally (declare (type (or null size) d2))
                      (if (cl:= i axes)
                          t
                          (cl:= d1 d2)))
            :do (unless (cl:= i axes) (setq out-dims rem-dims)))))

(defpolymorph out-shape ((name (eql mean)) in-array axes keep-dims) list
  (declare (optimize speed)
           (type nu:array in-array)
           (type (integer 0 #.array-rank-limit) axes))
  (if keep-dims
      (loop :for i :below (nu:array-rank in-array)
            :for d :of-type size :in (narray-dimensions in-array)
            :collect (if (cl:= i axes) 1 d))
      (loop :for i :below (nu:array-rank in-array)
            :for d :of-type size :in (narray-dimensions in-array)
            :if (cl:/= i axes)
              :collect d)))

(defpolymorph nu:mean ((array (nu:simple-array <type>))
                       &key ((axes integer))
                       ((keep-dims boolean))
                       ((out (nu:simple-array <type>))))
    (nu:simple-array <type>)
  (declare (ignorable keep-dims))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (out-shape-compatible-p 'mean out array axes keep-dims)
                  (array out)
                  "To mean an array of dimensions ~A on axes ~D~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions array)
                  axes
                  (out-shape 'mean array axes keep-dims)
                  keep-dims
                  (narray-dimensions out)))
    (pflet* ((c-name-add (c-name <type> 'nu:add))
             (c-name-sum (c-name <type> 'nu:sum))
             (c-name-divide (c-name <type> 'nu:divide))
             (c-size (c-size <type>))
             (c-type (c-type <type>))
             (stride (array-stride array axes))
             (out-rank (- (nu:array-rank array) axes))
             (in-rank  (- (nu:array-rank out) axes))
             (axis-size (array-dimension array axes)))
      (declare (type (simple-array <type>) out)
               (type (unsigned-byte 16) c-size))
      (cffi:with-foreign-object (c-axis-size c-type)
        (funcall #'(setf fref) (nu:coerce axis-size <type>) c-axis-size c-type)
        (flet ((%mean (in-size in-ptr in-dims out-ptr out-dims)
                 (declare (ignorable out-dims)
                          (cl:type cffi:foreign-pointer in-ptr out-ptr)
                          (type (unsigned-byte 32) in-size))
                 (let* ((n (first in-dims))
                        (out-size (cl:floor in-size n)))
                   (declare (type (unsigned-byte 32) n out-size))
                   (if (< out-size n)
                       (loop :repeat out-size
                             :do (locally (declare (#+sbcl sb-ext:muffle-conditions style-warning))
                                   (funcall #'(setf fref)
                                            (funcall c-name-sum n in-ptr stride)
                                            out-ptr
                                            c-type))
                                 (cffi:incf-pointer out-ptr c-size)
                                 (cffi:incf-pointer in-ptr c-size)
                             :finally
                                (cffi:incf-pointer out-ptr (* c-size (- out-size)))
                                (inline-or-funcall c-name-divide out-size
                                                   out-ptr 1 c-axis-size 0 out-ptr 1))
                       (loop :repeat n
                             :do (funcall c-name-add out-size out-ptr 1 in-ptr 1 out-ptr 1)
                                 (cffi:incf-pointer in-ptr (* c-size out-size))
                             :finally
                                (inline-or-funcall c-name-divide out-size
                                                   out-ptr 1 c-axis-size 0 out-ptr 1))))))
          (with-simple-array-broadcast (%mean out-rank in-rank)
            (array c-size) (out c-size))
          out)))))

(defpolymorph (nu:mean :inline t
                      :suboptimal-note runtime-array-allocation)
    ((array (nu:simple-array <type>))
     &key ((axes integer))
     ((keep-dims boolean))
     ((out null)))
    t
  (declare (ignore out))
  (pflet* ((out (nu:zeros (out-shape 'mean array axes keep-dims) :type <type>)))
    (declare (type (simple-array <type>) out))
    (nu:mean array :out out :keep-dims keep-dims :axes axes)))

(defpolymorph (nu:mean :inline t) ((array (nu:simple-array <type>))
                                  &key ((axes cons))
                                  ((keep-dims boolean))
                                  ((out null)))
    t
  (declare (ignore out))
  (let ((axes (sort (copy-list axes) #'cl:<)))
    (loop :with axis-diff :of-type size := 0
          :for axis :of-type size :in axes
          :do (setq array (nu:mean array :axes (the-size (- axis axis-diff))
                                         :keep-dims keep-dims))
              (unless keep-dims (incf axis-diff))
          :finally (return array))))

(defpolymorph (nu:mean :inline t) ((array (nu:simple-array <type>))
                                  &key ((axes null))
                                  ((keep-dims null))
                                  ((out null)))
    t
  (declare (ignore out keep-dims axes))
  (pflet ((c-name (c-name <type> 'nu:sum))
          (array-storage (array-storage array))
          (array-size (nu:array-total-size array)))
    (declare (type (cl:simple-array <type> 1) array-storage)
             (type size array-size))
    (cl:/ (with-pointers-to-vectors-data ((ptr array-storage))
            (funcall c-name array-size ptr 1))
          array-size)))

(defpolymorph (nu:mean :inline t :suboptimal-note runtime-array-allocation)
    ((array list) &key axes keep-dims out)
    t
  (declare (notinline nu:asarray))
  (nu:mean (nu:asarray array) :axes axes :keep-dims keep-dims :out out))

(defpolymorph (nu:mean :inline t) ((x (array <type>))
                                  &key ((axes null))
                                  ((out null))
                                  ((keep-dims null)))
    real
  (declare (ignore axes out keep-dims))
  (let ((acc     (type-zero <type>))
        (c-name  (c-name <type> 'nu:mean))
        (c-size  (c-size <type>)))
    (declare (type real acc))
    (ptr-iterate-but-inner (narray-dimensions x)
        n
      ((ptr-x c-size inc-x x))
      (setq acc
            (cl:+ acc (inline-or-funcall c-name n ptr-x inc-x))))
    (setq acc (cl:/ acc (nu:array-total-size x)))
    (let ((result (if (typep acc <type>)
                      acc
                      (locally (declare (notinline nu:coerce))
                        (nu:coerce acc <type>)))))
      result)))

(defpolymorph (nu:mean :inline t) ((x (array <type>))
                                  &key ((axes null))
                                  ((out null))
                                  ((keep-dims (not null))))
    (array <type>)
  (declare (ignore axes out keep-dims))
  (let ((result (nu:mean x :axes nil :out nil :keep-dims nil)))
    (make-array (make-list (array-rank x) :initial-element 1)
                :initial-element result :element-type (nu:array-element-type x))))


(defpolymorph (nu:mean :inline t)

    ((x (array <type>))
     &key ((axes integer))
     keep-dims
     ((out (array <type>))
      (nu:full
       (let ((dim (narray-dimensions x)))
         (append (subseq dim 0 axes)
                 (if keep-dims (list 1) ())
                 (nthcdr (+ 1 axes) dim)))
       :type (array-element-type x)
       :value (type-zero (array-element-type x)))))

    t

  (assert (< axes (array-rank x)))
  (pflet* ((rank (array-rank x))
           (perm
            (loop :for i :below rank
                  :collect (cond ((= i (1- rank)) axes)
                                 ((= i axes) (1- rank))
                                 (t i))))
           (out
            (nu:transpose
             (nu:reshape out
                         (let ((dim
                                 (array-dimensions (the array x))))
                           (setf (nth axes dim) 1)
                           dim))
             :axes perm))
           (x (nu:transpose x :axes perm))
           (c-fn (c-name <type> 'nu:sum))
           (c-size (c-size <type>))
           (c-type (c-type <type>)))
    (declare (type (array <type>) x out))
    (ptr-iterate-but-inner (narray-dimensions x)
        n
      ((ptr-x c-size inc-x x)
       (ptr-out c-size inc-out out))
      (setf (cffi:mem-ref ptr-out c-type)
            (nu:coerce (cl:/ (funcall c-fn n ptr-x inc-x)
                             n)
                       <type>))))
  (if (zerop (array-rank out))
      (row-major-aref out 0)
      out))

;; FIXME: These tests are not exhaustive.
(5am:def-test nu:mean ()
  (flet ((float-close-p (x y)
           (or (= x y)
               (progn
                 ;; (print (list x y))
                 (< (/ (abs (- x y))
                       (+ (abs x) (abs y)))
                    0.01)))))
    (loop :for *array-element-type* :in `(single-float
                                          double-float)
          :do
             (5am:is (= 02 (nu:mean (nu:asarray '(1 2 3)))))
             (5am:is (= 4 (nu:mean (nu:asarray '((2 3 4)
                                                 (4 5 6))))))
             (5am:is (nu:array= (nu:asarray '(3 4 5))
                                (nu:mean (nu:asarray '((2 3 4)
                                                       (4 5 6)))
                                         :axes 0
                                         :out (nu:zeros 3))))
             (5am:is (nu:array= (nu:asarray '((3 4 5)))
                                (nu:mean (nu:asarray '((2 3 4)
                                                       (4 5 6)))
                                         :axes 0
                                         :out (nu:zeros 1 3)
                                         :keep-dims t)))
             (5am:is (nu:array= (nu:asarray '(4 6))
                                (nu:mean (nu:asarray '((2 4 6)
                                                       (4 6 8)))
                                         :axes 1
                                         :out (nu:zeros 2))))
             (5am:is (nu:array= (nu:asarray '((3 4 5)
                                              (4 5 6)))
                                (nu:mean (nu:asarray '(((2 3 4)
                                                        (4 5 6))
                                                       ((8 9 10)
                                                        (0 1 2))))
                                         :axes 1
                                         :out (nu:zeros 2 3))))
             (5am:is (nu:array= (nu:asarray '((2 5)
                                              (8 1)))
                                (nu:mean (nu:asarray '(((1 2 3)
                                                        (4 5 6))
                                                       ((7 8 9)
                                                        (0 1 2))))
                                         :axes 2
                                         :out (nu:zeros 2 2))))
             (5am:is (nu:array= (nu:asarray '(5 3))
                                (nu:mean (nu:asarray '(((1 2 3)
                                                        (4 5 6))
                                                       ((7 8 9)
                                                        (0 1 2))))
                                         :axes '(0 2))))
             (5am:is (nu:array= (nu:asarray '(((5) (3))))
                                (nu:mean (nu:asarray '(((1 2 3)
                                                        (4 5 6))
                                                       ((7 8 9)
                                                        (0 1 2))))
                                         :axes '(0 2)
                                         :keep-dims t)))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (nu:mean array)
                                      (let ((sum 0))
                                        (nu:do-arrays ((x array))
                                          (incf sum x))
                                        (/ sum 100))))))))
