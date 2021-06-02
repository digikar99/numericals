(in-package :dense-numericals.impl)

(defun concatenate-compatible-p (axis &rest dimensions)
  "Returns two values:
  The first value is a generalized boolean indicating whether the arrays can be concatenated.
  The second value is the dimension of the array resulting from the concatenated."
  (case (length dimensions)
    (0 t)
    (1 (values t dimensions))
    (t (iter (cond ((every #'null dimension-lists) (terminate))
                   ((some #'null dimension-lists) (return nil)))
         (for dimension-lists initially dimensions
              then (mapcar #'cdr dimension-lists))
         (for ax from 0)
         (for dimension-list = (mapcar #'car dimension-lists))
         (collect (cond ((= ax axis) (apply #'+ dimension-list))
                        ((apply #'= dimension-list) (car dimension-list))
                        (t (return nil)))
           into concatenate-dimensions)
         (finally (return (values t concatenate-dimensions)))))))

(define-condition incompatible-concatenate-dimensions (error)
  ((array-likes :initarg :array-likes)
   (dimensions  :initarg :dimensions)
   (axis        :initarg :axis))
  (:report (lambda (c s)
             (with-slots (array-likes dimensions axis) c
               (format s "Cannot concatenate~%  ~S~%derived to be of dimensions~%  ~S~%along axis ~D~%"
                       array-likes dimensions axis)))))

(define-condition incompatible-concatenate-out-dimensions (error)
  ((expected  :initarg :dimensions)
   (actual    :initarg :axis))
  (:report (lambda (c s)
             (with-slots (array-likes dimensions axis) c
               (format s "Expected OUT to be a "
                       array-likes dimensions axis)))))

;; FIXME: Still 3 times slower than numpy when OUT is not given
(define-splice-list-fn dn:concat (args &key (axis 0) (out nil outp))
  "If ARGS is a single argument and is a list, the arrays inside the list
will be concatenated. Otherwise the multiple arguments constituting ARGS
will be concatenated. 
&KEY arguments
- AXIS defaulting to 0
- OUT defaulting to NIL"
  (declare (type size axis))
  (let* ((array-args (mapcar #'ensure-appropriate-dense-array args))
         (dimensions (mapcar #'narray-dimensions array-args)))
    (multiple-value-bind (concatenate-compatible-p result-dimensions)
        (apply #'concatenate-compatible-p axis dimensions)
      (assert concatenate-compatible-p ()
              'incompatible-concatenate-dimensions
              :array-likes args
              :dimensions dimensions
              :axis axis)
      (when outp
        (assert (and (typep out 'array)
                     (equalp result-dimensions
                             (narray-dimensions out)))
                (out)
                "Expected OUT to be a DENSE-ARRAY with dimensions ~S but is ~%  ~S"
                result-dimensions out))
      ;; TODO: Speedup ZEROS!
      ;; A looot of time is spent there!
      (let* ((result (or out (zeros result-dimensions)))
             (result-args
               (let* ((dim (narray-dimensions result))
                      (inner-most-stride
                        (reduce #'* (subseq dim (1+ axis)) :initial-value 1))
                      (result (reshape result
                                       (append (subseq dim 0 axis)
                                               (list (* inner-most-stride
                                                        (nth axis dim)))))))
                 (declare (type size inner-most-stride)
                          (type array result))
                 (loop :for array :in array-args
                       :with axis-offset :of-type size := 0
                       :for axis-length :of-type size
                         := (* inner-most-stride
                               (array-dimension array axis))
                       :collect (apply #'aref
                                       result
                                       (nconc (make-list axis :initial-element nil)
                                              (list (list axis-offset
                                                          :end (+ axis-offset axis-length)))))
                       :do (incf axis-offset axis-length))))
             (last-axis-flattened-args
               (loop :for array :in array-args
                     :for dim :of-type list :in dimensions
                     :collect (reshape array (append (subseq dim 0 axis)
                                                     (list (reduce #'* (subseq dim axis)
                                                                   :initial-value 1)))))))
        (loop :for result-arg :of-type array :in result-args
              :for flattened-arg :of-type array :in last-axis-flattened-args
              :do (dn:copy flattened-arg :out result-arg))
        result))))

(5am:def-test dn:concat ()
  (5am:is (array= (asarray '(1 2 3 4 5 6))
                  (dn:concat '(1 2 3) '(4 5 6) :axis 0)))
  (5am:is (array= (asarray '((1 2 3)
                             (4 5 6)))
                  (dn:concat '((1 2 3)) '((4 5 6)) :axis 0)))
  (5am:is (array= (asarray '((1 2 3 4 5 6)))
                  (dn:concat '((1 2 3)) '((4 5 6)) :axis 1))))
