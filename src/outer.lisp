(in-package :numericals.internals)

;; A common use case I have known is having the data arranged in row-major-order
;; so that the first index is the "data point index". May be we could make this
;; common case fast?


;; TODO: Since this uses displaced arrays, all the other functions need to be
;; checked to work correctly for displaced arrays.
(defun nu:map-outer (result function &rest arrays)
  "Result can be a object of type array or the symbol LIST or the symbol ARRAY."
  (declare (optimize (speed 3)))
  (let ((outer-dimensions (loop for array in arrays
                             collect (the fixnum (array-dimension array 0)))))
    (unless (apply '= outer-dimensions)
      (error "Cannot map over unequal outer dimensions ~D" outer-dimensions))
    (let ((outer-dimension (car outer-dimensions)))
      (declare (type (signed-byte 31) outer-dimension))
      (if (arrayp result)
          ;; How does one efficiently guess the dimension of what the function returns? 
          (let ((out-elt-size (floor (array-total-size result)
                                     (array-dimension result 0))))
            (declare (type (signed-byte 31) out-elt-size))
            (loop for i fixnum below outer-dimension
               for out = (make-array (cdr (array-dimensions result))
                                     :element-type (array-element-type result)
                                     :displaced-to (1d-storage-array result)
                                     :displaced-index-offset (* i out-elt-size))
               collect
                 (apply function
                        (nconc
                         (let ()
                           (declare (type (signed-byte 31) i))
                           (loop for array in arrays
                              for elt-size = (the (signed-byte 31)
                                                  (floor (array-total-size array)
                                                         outer-dimension))
                              collect
                                (make-array (cons 1 (cdr (array-dimensions array)))
                                            :element-type (array-element-type array)
                                            :displaced-to (1d-storage-array array)
                                            :displaced-index-offset
                                            (the fixnum
                                                 (* i elt-size)))))
                         (list :out out)))))
          (let ((result-list
                 (loop :for i fixnum :below outer-dimension
                    :for sub-arrays
                      := (let ()
                           (declare (type (signed-byte 31) i))
                           (loop for array in arrays
                              for elt-size = (the (signed-byte 31)
                                                  (floor (array-total-size array)
                                                         outer-dimension))
                              collect
                                (make-array (cons 1 (cdr (array-dimensions array)))
                                            :element-type (array-element-type array)
                                            :displaced-to (1d-storage-array array)
                                            :displaced-index-offset
                                            (the fixnum
                                                 (* i elt-size)))))
                    :if result :collect (apply function sub-arrays)
                    :else :do (apply function sub-arrays))))
            (print result-list)
            (ecase result
              ((nil) nil)
              (list result-list)
              (array (apply #'nu:concatenate result-list))))))))
