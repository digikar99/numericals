(in-package :numericals/utils/impl)

;; Numpy uses an array wrapper, and a slice of the matrix
;; is actually a part of the original matrix - meaning, writing to the
;; slice actually writes to the original array - and this is imaginable
;; to be efficient. And even though make-array doesn't cost much,
;; copying the data over to this new array definitely costs much.

;; The closest thing, perhaps, would be to add an iteration macro do-aref
;; or an iterate clause, or both.

;; The following implementation happens to be about 5 times faster than select:select.
;; However, in the absence of further optimizations (= compiler-macros)
;; this is just as slow in the case reducible to cl:aref.

;; This is also about 5 times slower than numpy. While this does not use SIMD,
;; it's not clear how to use it. And this is regardless of SIMD. At moments,
;; when numpy can use SIMD (= data is local), numpy is about 20 times faster!

;; Use the "dense-numericals" system that uses "dense-arrays" under the hood for speed

(declaim (inline fixnump))
(defun* fixnump (object) (typep object 'fixnum))

(declaim (inline aref-applicable-p))
(defun* aref-applicable-p (array &rest subscripts)
  (and (every 'fixnump subscripts)
       (= (array-rank array) (length subscripts))))

(defun normalize-index (index dimension)
  (declare (optimize speed)
           (type int-index index dimension))
  (the size (if (< index 0)
                (+ index dimension)
                index)))

(defun aref* (&rest args)
  "Accessor function for arrays with semantics similar to numpy's indexing semantics.
See https://numpy.org/doc/stable/user/basics.indexing.html

[Enhanced] Lambda List: (array &rest subscripts &key out)

Each element of SUBSCRIPTS can be
- either an integer denoting the position within the axis which is to be indexed
- or a list of the form (&OPTIONAL START &KEY END STEP) with each of START END
  STEP being integers if supplied. START denotes the start position within the
  axis, END denotes the ending position within the axis, STEP denotes at what
  distance within the axis the next element should come after the previous,
  starting from START

Each of the SUBSCRIPTS, START, END, STEP can also be negative integers, in which
case the last element along the axis is given the index -1, the second last is
given the index -2 and so on. Thus, `(aref ... '(-1 :step -1))` can reverse a one
dimensional array.

Like, CL:AREF or ABSTRACT-ARRAYS:AREF, returns the element corresponding to SUBSCRIPTS
if all the subscripts are integers and there as many subscripts
as the rank of the array.

The performance of this function is slightly different for `cl:array` compared
to `dense-arrays:array`. In particular, numpy-like indexing requires
multidimensional offsets. `cl:array` only have a single dimensional offset,
thus, when using `aref*` a copy of the `cl:array` is created. The copy may be
made into a preallocated array supplied using the `:out` keyword argument. In
contrast, because `dense-arrays:array` support multidimensional offsets and
strides, merely a wrapper object (a \"view\") is created. A view is a window
into the original array and thus avoids copying the elements of the original
array. This occurs when the number (aka length) of SUBSCRIPTS were less than the
array's rank, or if some of the SUBSCRIPTS were lists described above.

Examples illustrating the numpy-equivalent indexes:

    a[::]       (aref a nil)
    a[::2]      (aref a '(0 :step 2))
    a[3, ::-1]  (aref a 3 '(-1 :step -1))
    a[3::, -1]  (aref a '(3) -1)

The SUBSCRIPTS can also be integer or boolean arrays, denoting which elements
to select from each of the axes. But in this case the corresponding elements
of the array are copied over into a new array."
  ;; TODO: Optimize for speed
  (declare (optimize (debug 3)))
  ;; TODO: Implement &KEY OUT for DENSE-ARRAYS:AREF
  (destructuring-bind ((array &rest subscripts) &key out)
      (split-at-keywords args)
    (declare (type list subscripts))
    (if (apply 'aref-applicable-p array subscripts)
        (if (every #'non-negative-fixnum-p subscripts)
            (apply #'cl:aref array subscripts)
            (apply #'cl:aref array (loop :for s :in subscripts
                                         :for d :in (narray-dimensions array)
                                         :collect (normalize-index s d))))
        (let ((out (or out
                       (zeros
                        (loop :for d :in (narray-dimensions array)
                              :with ss := subscripts
                              :for s := (first ss)
                              :do (setq ss (rest ss))
                              :if (listp s)
                                :collect
                              (destructuring-bind (&optional (start 0) &key (end d endp) (step 1))
                                  s
                                (declare (type int-index start end step))
                                (psetq start (normalize-index start d)
                                       end   (normalize-index end   d))
                                (when (and (< step 0) (not endp))
                                  (setq end -1))
                                (ceiling (- end start) step)))
                        :type (array-element-type array)))))
          ;; Traverse OUT in row-major order, and ARRAY appropriately as follows.
          ;; Starting with the STORAGE-ARRAY of OUT, each dimension will have strides
          ;; and offsets as mentioned by the SUBSCRIPT. These subscripts will guide us
          ;; how to travel the OUT's storage array.
          (let ((array-sv (array-storage array))
                (out-sv   (array-storage out))
                (out-i    0))
            (declare (type (simple-array) array-sv)
                     (type (simple-array) out-sv)
                     (type int-index out-i))
            ;; (print (list (array-total-size out-sv)))
            (labels ((traverse (array-i size dimensions subscripts)
                       (declare (type size size)
                                (type int-index array-i))
                       (let ((new-size (if dimensions
                                           (floor size (first dimensions))
                                           0)))
                         (declare (type size new-size))
                         ;; (print size)
                         (if subscripts
                             (let ((s (first subscripts))
                                   (d (first dimensions)))
                               (etypecase s
                                 (list
                                  (destructuring-bind (&optional (start 0) &key (end d endp) (step 1))
                                      s
                                    (declare (type int-index start end step))
                                    (psetq start (normalize-index start d)
                                           end   (normalize-index end   d))
                                    (when (and (< step 0) (not endp))
                                      (setq end -1))
                                    (loop :initially (incf array-i (the-int-index (* new-size start)))
                                          :repeat (ceiling (- end start) step)
                                          :do (traverse array-i
                                                        new-size
                                                        (rest dimensions)
                                                        (rest subscripts))
                                              (incf array-i (the-int-index (* new-size step))))))
                                 (int-index
                                  (locally (declare (type int-index s))
                                    (setq s (normalize-index s d))
                                    (incf array-i (the-int-index (* new-size s)))
                                    (traverse array-i new-size (rest dimensions) (rest subscripts))))))
                             (loop :repeat size
                                   :do (setf (cl:aref out-sv out-i)
                                             (cl:aref array-sv array-i))
                                       ;; (print (list out-i array-i))
                                       (incf out-i)
                                       (incf array-i))))))
              (traverse (cl-array-offset array)
                        (array-total-size array)
                        (narray-dimensions array)
                        subscripts)))
          out))))

(deftype int32 () '(signed-byte 32))

(5am:def-test aref* (:suite :numericals)
  (symbol-macrolet ((array (asarray '((0 1)
                                         (1 2)
                                         (2 3)
                                         (3 4)
                                         (4 5)
                                         (5 6)
                                         (6 7)
                                         (7 8)
                                         (8 9)
                                         (9 10))
                                       :type 'int32)))
    (5am:is (= 9 (aref* array 9 0)))
    (5am:is (= 9 (aref* array 8 1)))
    (5am:is (= 9 (aref* array -2 -1)))
    (5am:is (array= array (aref* array)))
    (5am:is (array= array (aref* array nil)))
    (5am:is (array= array (aref* array nil nil)))
    (5am:is (array= (aref* array 0)
                       (asarray '(0 1) :type 'int32)))
    (5am:is (array= (aref* array 0 nil)
                       (asarray '(0 1) :type 'int32)))
    (5am:is (array= (aref* array nil 0)
                       (asarray '(0 1 2 3 4 5 6 7 8 9) :type 'int32)))
    (5am:is (array= (make-array 5 :initial-contents '(5 4 3 2 1) :element-type 'int32)
                       (aref* (make-array 5 :initial-contents '(1 2 3 4 5) :element-type 'int32)
                                 '(-1 :step -1))))
    (5am:is (array= (aref* (aref* array '(2 :end 4)) () 0)
                       (make-array '(2) :initial-contents '(2 3)
                                        :element-type 'int32))))
  (symbol-macrolet ((array (asarray '((0 1 2) (1 2 3)) :type 'int32)))
    (5am:is (equalp '(2 2) (array-dimensions (aref* array nil '(0 :step 2)))))
    (5am:is (equalp '(1 3) (array-dimensions (aref* array '(0 :step 2)))))
    (5am:is (= 0 (aref* (aref* array '(0 :step 2)) 0 0))))
  (5am:is (array= (aref* (asarray '((0 1 2 3 4)
                                             (1 2 3 4 5)
                                             (2 3 4 5 6)
                                             (3 4 5 6 7)
                                             (4 5 6 7 8))
                                           :type 'int32)
                               '(1) '(1))
                     (asarray '((2 3 4 5)
                                   (3 4 5 6)
                                   (4 5 6 7)
                                   (5 6 7 8))
                                 :type 'int32))))

(defun strides (array-dimensions)
  (let ((reversed-dimensions (nreverse array-dimensions))
        (product 1))
    (nreverse
     (loop for i in reversed-dimensions
        collect product
        do (setq product (* product i))))))
