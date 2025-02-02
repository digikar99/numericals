(in-package :dense-numericals/basic-math/impl)

(5am:in-suite :dense-numericals)

(define-polymorphic-function nu:sum (array-like &key out axes keep-dims) :overwrite t)

(defpolymorph (nu:sum :inline t :suboptimal-note runtime-array-allocation)
    ((array list) &key axes keep-dims out)
    nu:array
  (declare (notinline nu:asarray))
  (nu:sum (nu:asarray array) :axes axes :keep-dims keep-dims :out out))

(defpolymorph out-shape-compatible-p ((name (eql nu:sum)) out in axes keep-dims) boolean
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

(defpolymorph out-shape ((name (eql nu:sum)) (in-array nu:array) axes keep-dims) list
  (declare (optimize speed)
           (type (or list (integer 0 #.array-rank-limit)) axes))
  (etypecase axes
    (null
     (if keep-dims
         (make-list (array-rank in-array) :initial-element 1)
         nil))
    ((integer 0 #.array-rank-limit)
     (if keep-dims
         (loop :for i :of-type (integer 0 #.array-rank-limit)
                 :below (nu:array-rank in-array)
               :for d :of-type size :in (narray-dimensions in-array)
               :collect (if (cl:= i axes) 1 d))
         (loop :for i :of-type (integer 0 #.array-rank-limit)
                 :below (nu:array-rank in-array)
               :for d :of-type size :in (narray-dimensions in-array)
               :if (cl:/= i axes)
                 :collect d)))
    (cons
     (if keep-dims
         (loop :for i :of-type (integer 0 #.array-rank-limit)
                 :below (nu:array-rank in-array)
               :for d :of-type size :in (narray-dimensions in-array)
               :collect (if (member i axes :test #'cl:=) 1 d))
         (loop :for i :of-type (integer 0 #.array-rank-limit)
                 :below (nu:array-rank in-array)
               :for d :of-type size :in (narray-dimensions in-array)
               :if (not (member i axes :test #'cl:=))
                 :collect d)))))

(defpolymorph (nu:sum :inline t :suboptimal-note runtime-array-allocation)
    ((array (nu:array <type>)) &key axes ((keep-dims boolean)) ((out null)))
    (nu:simple-array <type>)
  (declare (ignore out))
  (pflet* ((out (nu:zeros (out-shape 'nu:sum array axes keep-dims) :type <type>)))
    (declare (type (simple-array <type>) out))
    (nu:sum array :out out :keep-dims keep-dims :axes axes)))

;;; CASE 1: NULL AXES

(defpolymorph (nu:sum :inline t)
    ((x (nu:simple-array <type>))
     &key ((axes null))
     ((keep-dims boolean))
     ((out (nu:array <type>))))
    (nu:array <type>)

  (declare (ignore axes))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (if keep-dims
                      (and (= (array-rank x) (array-rank out))
                           (= 1 (array-total-size out)))
                      (equal () (narray-dimensions out)))
                  (x out keep-dims)
                  "To sum an array of dimensions ~A along all axes~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions x)
                  (if keep-dims
                      (make-list (array-rank x) :initial-element 1)
                      nil)
                  keep-dims
                  (narray-dimensions out)))
    (pflet ((c-name (c-name <type> 'nu:sum))
            (c-type (c-type <type>))
            (c-size (c-type <type>))
            (array-storage (array-storage x))
            (out-storage   (array-storage out)))
      (declare (type (cl:simple-array <type> 1) array-storage))
      (with-pointers-to-vectors-data ((ptr array-storage)
                                      (optr out-storage))
        (unless (array-layout out)        ; non-null for simple arrays
          (cffi:incf-pointer optr (* c-size (array-total-offset out))))
        (funcall #'(setf fref)
                 (funcall c-name (array-total-size x) ptr 1)
                 optr
                 c-type))
      out)))

(defpolymorph (nu:sum :inline t)
    ((x (array <type>))
     &key ((axes null))
     ((out (array <type>)))
     ((keep-dims boolean)))
    (array <type>)
  (declare (ignore axes))

  (policy-cond:with-expectations (= safety 0)
      ((assertion (if keep-dims
                      (and (= (array-rank x) (array-rank out))
                           (= 1 (array-total-size out)))
                      (equal () (narray-dimensions out)))
                  (x out keep-dims)
                  "To sum an array of dimensions ~A along all axes~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions x)
                  (if keep-dims
                      (make-list (array-rank x) :initial-element 1)
                      nil)
                  keep-dims
                  (narray-dimensions out)))
    (let ((acc     (type-zero <type>))
          (c-name  (c-name <type> 'nu:sum))
          (c-size  (c-size <type>))
          (c-type  (c-type <type>)))
      (declare (type real acc))
      (ptr-iterate-but-inner (narray-dimensions x)
          n
        ((ptr-x c-size inc-x x))
        (setq acc
              (cl:+ acc (inline-or-funcall c-name n ptr-x inc-x))))
      (let ((result (if (typep acc <type>)
                        acc
                        (locally (declare (notinline nu:coerce))
                          (nu:coerce acc <type>))))
            (out-storage   (array-storage out)))
        (with-pointers-to-vectors-data ((optr out-storage))
          (unless (array-layout out)      ; non-null for simple arrays
            (cffi:incf-pointer optr (* c-size (array-total-offset out))))
          (funcall #'(setf fref)
                   result
                   optr
                   c-type))
        out))))

;;; CASE 2: INTEGER AXES

(defpolymorph nu:sum ((array (nu:simple-array <type>))
                      &key ((axes integer))
                      ((keep-dims boolean))
                      ((out (nu:simple-array <type>))))
    (nu:simple-array <type>)
  (declare (ignorable keep-dims))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (out-shape-compatible-p 'nu:sum out array axes keep-dims)
                  (array out)
                  "To sum an array of dimensions ~A on axes ~D~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions array)
                  axes
                  (out-shape 'nu:sum array axes keep-dims)
                  keep-dims
                  (narray-dimensions out)))
    (pflet* ((c-name-add (c-name <type> 'nu:add))
             (c-name-sum (c-name <type> 'nu:sum))
             (c-size (c-size <type>))
             (c-type (c-type <type>))
             (stride (array-stride array axes))
             (out-rank (- (nu:array-rank array) axes))
             (in-rank  (- (nu:array-rank out) axes)))
      (declare (type (simple-array <type>) out)
               (type (unsigned-byte 16) c-size))
      (flet ((sum (in-size in-ptr in-dims out-ptr out-dims)
               (declare (ignorable out-dims)
                        (cl:type cffi:foreign-pointer in-ptr out-ptr)
                        (type (unsigned-byte 32) in-size))
               (let* ((n (first in-dims))
                      (out-size (cl:floor in-size n)))
                 (declare (type (unsigned-byte 32) n out-size))
                 ;; (print (list n in-size in-dims out-size out-dims))
                 (if (< out-size n)
                     (loop :repeat out-size
                           :do (locally (declare (#+sbcl sb-ext:muffle-conditions style-warning))
                                 (funcall #'(setf fref)
                                          (funcall c-name-sum n in-ptr stride)
                                          out-ptr
                                          c-type))
                               (cffi:incf-pointer out-ptr c-size)
                               (cffi:incf-pointer in-ptr c-size))
                     (loop :repeat n
                           :do (funcall c-name-add out-size out-ptr 1 in-ptr 1 out-ptr 1)
                               (cffi:incf-pointer in-ptr (* c-size out-size)))))))
        (with-simple-array-broadcast (sum out-rank in-rank)
          (array c-size) (out c-size))
        out))))

(defpolymorph nu:sum ((array (nu:simple-array <type>))
                      &key ((axes integer))
                      ((keep-dims boolean))
                      ((out null)))
    (nu:simple-array <type>)
  (declare (ignore out))
  (pflet ((out (nu:empty (out-shape 'nu:sum array axes keep-dims)
                         :type <type>)))
    (declare (type (nu:simple-array <type>) out))
    (nu:sum array :axes axes :keep-dims keep-dims :out out)))



(defpolymorph (nu:sum :inline t)

    ((x (array <type>))
     &key ((axes integer))
     keep-dims
     ((out null)))

    (nu:simple-array <type>)

  (assert (< axes (array-rank x)))
  (pflet* ((rank (array-rank x))
           ;; (redims
           ;;  (loop :for i :below rank
           ;;        :for d :in (narray-dimensions x)
           ;;        :collect (cond ((= i (1- rank)) (array-dimension x axes))
           ;;                       ((= i axes) 1)
           ;;                       (t d))))
           ;; (perm-out (nu:empty redims :type <type>))
           (perm
            (loop :for i :below rank
                  :collect (cond ((= i (1- rank)) axes)
                                 ((= i axes) (1- rank))
                                 (t i))))
           (out (nu:empty (let ((dims (array-dimensions x)))
                            (setf (nth axes dims) 1)
                            dims)
                          :type <type>))
           (perm-out (nu:transpose out :axes perm))
           (x (nu:transpose x :axes perm))
           (c-fn (c-name <type> 'nu:sum))
           (c-size (c-size <type>))
           (c-type (c-type <type>)))
    (declare (type (array <type>) x perm-out))
    (ptr-iterate-but-inner (narray-dimensions x)
        n
      ((ptr-x c-size inc-x x)
       (ptr-out c-size inc-out perm-out))
      (setf (cffi:mem-ref ptr-out c-type)
            (funcall c-fn n ptr-x inc-x)))
    #.(if (eq 'nu:array 'cl:array)
          `(nu:transpose perm-out :axes perm)
          `out)))



(defpolymorph (nu:sum :inline t)
    ((x (array <type>))
     &key ((axes integer))
     keep-dims
     ((out (array <type>))))
    (nu:array <type>)
  (let ((result (nu:sum x :axes axes :keep-dims keep-dims :out nil)))
    (nu:copy result :out out)))

;; CASE 3: CONS AXES

(defpolymorph (nu:sum :inline t :suboptimal-note runtime-array-allocation)
    ((array (nu:array <type>))
     &key ((axes cons))
     ((keep-dims boolean))
     ((out (nu:array <type>))))
    (nu:array <type>)
  (let ((axes (remove-duplicates axes)))
    (if (= (length axes) (array-rank array))
        (nu:sum array :axes nil :keep-dims keep-dims :out out)
        (loop :with intermediate := array
              :for axis :of-type size :in axes
              :do (setq intermediate (nu:sum intermediate :axes axis :keep-dims t))
              :finally (setq intermediate (nu:reshape intermediate (array-dimensions out)))
                       (nu:copy intermediate :out out)
                       (return out)))))


;; FIXME: These tests are not exhaustive.
(5am:def-test nu:sum ()
  (flet ((float-close-p (x y)
           (or (= x y)
               (progn
                 ;; (print (list x y))
                 (< (/ (abs (- x y))
                       (+ (abs x) (abs y)))
                    0.01)))))
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
                                          ;; fixnum
                                          )
          :do
             (5am:is (nu:array= (ensure-array 06)
                                (nu:sum (nu:asarray '(1 2 3)))))
             (5am:is (nu:array= (ensure-array 21)
                                (nu:sum (nu:asarray '((1 2 3)
                                                      (4 5 6))))))
             (5am:is (nu:array= (nu:asarray '(5 7 9))
                                (nu:sum (nu:asarray '((1 2 3)
                                                      (4 5 6)))
                                        :axes 0
                                        :out (nu:zeros 3))))
             (5am:is (nu:array= (nu:asarray '((5 7 9)))
                                (nu:sum (nu:asarray '((1 2 3)
                                                      (4 5 6)))
                                        :axes 0
                                        :out (nu:zeros 1 3)
                                        :keep-dims t)))
             (5am:is (nu:array= (nu:asarray '(6 15))
                                (nu:sum (nu:asarray '((1 2 3)
                                                      (4 5 6)))
                                        :axes 1
                                        :out (nu:zeros 2))))
             (5am:is (nu:array= (nu:asarray '((5 7 9)
                                              (7 9 11)))
                                (nu:sum (nu:asarray '(((1 2 3)
                                                       (4 5 6))
                                                      ((7 8 9)
                                                       (0 1 2))))
                                        :axes 1
                                        :out (nu:zeros 2 3))))
             (5am:is (nu:array= (nu:asarray '((6 15)
                                              (24 3)))
                                (nu:sum (nu:asarray '(((1 2 3)
                                                       (4 5 6))
                                                      ((7 8 9)
                                                       (0 1 2))))
                                        :axes 2
                                        :out (nu:zeros 2 2))))
             (5am:is (nu:array= (nu:asarray '(30 18))
                                (nu:sum (nu:asarray '(((1 2 3)
                                                       (4 5 6))
                                                      ((7 8 9)
                                                       (0 1 2))))
                                        :axes '(0 2))))
             (5am:is (nu:array= (nu:asarray '(((30) (18))))
                                (nu:sum (nu:asarray '(((1 2 3)
                                                       (4 5 6))
                                                      ((7 8 9)
                                                       (0 1 2))))
                                        :axes '(0 2)
                                        :keep-dims t)))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (row-major-aref (nu:sum array) 0)
                                      (let ((sum 0))
                                        (nu:do-arrays ((x array))
                                          (incf sum x))
                                        sum)))))))
