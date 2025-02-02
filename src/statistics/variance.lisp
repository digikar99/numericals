(in-package :numericals/statistics)

(5am:in-suite :numericals)

(define-polymorphic-function nu:variance (array-like &key out axes keep-dims (ddof 0))
  :overwrite t
  :documentation "See https://numpy.org/doc/stable/reference/generated/numpy.var.html")

(defpolymorph out-shape-compatible-p ((name (eql variance)) out in axes keep-dims)
    boolean
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

(defpolymorph nu:variance ((array (nu:simple-array <type>))
                           &key ((out (simple-array <type>)))
                           ((axes (or cons integer)))
                           ((keep-dims boolean))
                           ((ddof integer) 0))
    (nu:simple-array <type>)
  (policy-cond:with-expectations (= 0 safety)
      ((assertion (out-shape-compatible-p 'variance out array axes keep-dims)
                  (array out)
                  "To variance an array of dimensions ~A on axes ~D~%requires an array of dimension ~D with :KEEP-DIMS ~A,~%but an array of dimensions ~A was supplied"
                  (narray-dimensions array)
                  axes
                  (out-shape 'mean array axes keep-dims)
                  keep-dims
                  (narray-dimensions out)))
    (pflet* ((axis-size (etypecase axes
                          (cons
                           (loop :for axis :in axes
                                 :with size :of-type (integer 0 #.array-total-size-limit)
                                   := 1
                                 :do (setq size (* size (array-dimension array axis)))
                                 :finally (return size)))
                          (integer
                           (array-dimension array axes))))
             (square    (nu:empty-like array)))
      (declare (type (simple-array <type>) square)
               (type (integer 0 #.array-dimension-limit) axis-size))
      (nu:multiply array array :broadcast nil :out square)
      (locally (declare (compiler-macro-notes:muffle runtime-array-allocation))
        (pflet ((array-sum  (nu:sum array  :axes axes :keep-dims keep-dims))
                (square-sum (nu:sum square :axes axes :keep-dims keep-dims
                                           :out out)))
          (declare (type (simple-array <type>) array-sum square-sum))
          (nu:multiply! array-sum array-sum :broadcast nil)
          (nu:divide array-sum axis-size :broadcast t :out array-sum)
          (nu:subtract! square-sum array-sum :broadcast nil)
          (nu:divide! square-sum (the real (- axis-size ddof)) :broadcast t)
          square-sum)))))

;;; The only difference between the above and below bodies is the
;;; OUT argument being used to hold the SQUARE-SUM

(defpolymorph (nu:variance :inline t) ((array (nu:simple-array <type>))
                                       &key ((axes (or integer cons)))
                                       ((keep-dims boolean))
                                       ((out null))
                                       ((ddof integer) 0))
    t
  (declare (ignore out))
  (pflet* ((axis-size (etypecase axes
                        (cons
                         (loop :for axis :in axes
                               :with size :of-type (integer 0 #.array-total-size-limit)
                                 := 1
                               :do (setq size (* size (array-dimension array axis)))
                               :finally (return size)))
                        (integer
                         (array-dimension array axes))))
           (square    (nu:empty-like array)))
    (declare (type (simple-array <type>) square)
             (type (integer 0 #.array-dimension-limit) axis-size))
    (nu:multiply array array :broadcast nil :out square)
    (locally (declare (compiler-macro-notes:muffle runtime-array-allocation))
      (pflet ((array-sum  (nu:sum array  :axes axes :keep-dims keep-dims))
              (square-sum (nu:sum square :axes axes :keep-dims keep-dims)))
        (declare (type (simple-array <type>) array-sum square-sum))
        (nu:multiply! array-sum array-sum :broadcast nil)
        (nu:divide array-sum axis-size :broadcast t :out array-sum)
        (nu:subtract! square-sum array-sum :broadcast nil)
        (nu:divide! square-sum (the real (- axis-size ddof)) :broadcast t)
        square-sum))))

(defpolymorph (nu:variance :suboptimal-note runtime-array-allocation :inline t)
    ((array-like list) &key out axes (ddof 0) keep-dims) t
  (nu:variance (nu:asarray array-like :type (if out
                                                (array-element-type out)
                                                nu:*default-float-format*))
               :out out :axes axes
               :ddof ddof :keep-dims keep-dims))

(defpolymorph (nu:variance :inline t) ((array (nu:simple-array <type>))
                                       &key ((axes null))
                                       ((keep-dims null))
                                       ((out null))
                                       ((ddof integer) 0))
    t
  (declare (ignore out keep-dims axes))
  (pflet* ((size   (array-total-size array))
           (square (nu:empty-like array)))
    (declare (type (simple-array <type>) square)
             (type (integer 0 #.array-dimension-limit) size))
    (nu:multiply array array :broadcast nil :out square)
    (locally (declare (compiler-macro-notes:muffle runtime-array-allocation))
      (let ((array-sum  (aref (nu:sum array  :axes nil :keep-dims nil)))
            (square-sum (aref (nu:sum square :axes nil :keep-dims nil))))
        (ensure-array (/ (- square-sum
                            (/ (* array-sum array-sum)
                               size))
                         (- size ddof))
                      ()
                      <type>)))))


;; FIXME: These tests are not exhaustive.
(5am:def-test nu:variance ()
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
             (5am:is (nu:array= (ensure-array 2/3)
                                (nu:variance (nu:asarray '(1 2 3)))))
             (5am:is (nu:array= (ensure-array 5/3)
                                (nu:variance (nu:asarray '((2 3 4)
                                                           (4 5 6))))))
             (5am:is (nu:array= (nu:asarray '(1 1 1))
                                (nu:variance (nu:asarray '((2 3 4)
                                                           (4 5 6)))
                                             :axes 0
                                             :out (nu:zeros 3))))
             (5am:is (nu:array= (nu:asarray '((1 1 1)))
                                (nu:variance (nu:asarray '((2 3 4)
                                                           (4 5 6)))
                                             :axes 0
                                             :out (nu:zeros 1 3)
                                             :keep-dims t)))
             (5am:is (nu:array= (nu:asarray '(8/3 8/3))
                                (nu:variance (nu:asarray '((2 4 6)
                                                           (4 6 8)))
                                             :axes 1
                                             :out (nu:zeros 2))))
             (5am:is (nu:array= (nu:asarray '(( 1  1  1)
                                              (16 16 16)))
                                (nu:variance (nu:asarray '(((2 3 4)
                                                            (4 5 6))
                                                           ((8 9 10)
                                                            (0 1 2))))
                                             :axes 1
                                             :out (nu:zeros 2 3))))
             (5am:is (nu:array= (nu:asarray '((2/3 2/3)
                                              (2/3 2/3)))
                                (nu:variance (nu:asarray '(((1 2 3)
                                                            (4 5 6))
                                                           ((7 8 9)
                                                            (0 1 2))))
                                             :axes 2
                                             :out (nu:zeros 2 2))))
             (5am:is (nu:array= (nu:asarray '(29/3 14/3))
                                (nu:variance (nu:asarray '(((1 2 3)
                                                            (4 5 6))
                                                           ((7 8 9)
                                                            (0 1 2))))
                                             :axes '(0 2))))
             (5am:is (nu:array= (nu:asarray '(((29/3) (14/3))))
                                (nu:variance (nu:asarray '(((1 2 3)
                                                            (4 5 6))
                                                           ((7 8 9)
                                                            (0 1 2))))
                                             :axes '(0 2)
                                             :keep-dims t)))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (row-major-aref (nu:variance array) 0)
                                      (let* ((sum 0)
                                             (mean
                                               (progn
                                                 (nu:do-arrays ((x array))
                                                   (incf sum x))
                                                 (/ sum 100)))
                                             (diff-sum 0))
                                        (nu:do-arrays ((x array))
                                          (incf diff-sum (expt (- x mean) 2)))
                                        (/ diff-sum 100))))))))
