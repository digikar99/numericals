(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

;; A common use case I have known is having the data arranged in row-major-order
;; so that the first index is the "data point index". May be we could make this
;; common case fast?


;; Should MAP-OUTER to use the (&key out)
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
            (multiple-value-bind (result-storage-array result-offset)
                (1d-storage-array result)              
              (loop :for i fixnum :below outer-dimension
                 :for out := (make-array (cdr (array-dimensions result))
                                         :element-type (array-element-type result)
                                         :displaced-to result-storage-array
                                         :displaced-index-offset
                                         (the fixnum (+ result-offset (* i out-elt-size))))
                 :collect
                   (apply function
                          (nconc
                           (let ()
                             (declare (type (signed-byte 31) i))
                             (loop :for array :in arrays
                                :for elt-size = (the (signed-byte 31)
                                                     (floor (array-total-size array)
                                                            outer-dimension))
                                :collect
                                  (multiple-value-bind (1d-storage-array offset)
                                      (1d-storage-array array)
                                    (make-array (cdr (array-dimensions array))
                                                :element-type (array-element-type array)
                                                :displaced-to 1d-storage-array
                                                :displaced-index-offset
                                                (the fixnum
                                                     (+ offset (* i elt-size)))))))
                           (list :out out)))))
            result)
          (let ((result-list
                 (loop :for i fixnum :below outer-dimension
                    :for sub-arrays
                      := (let ()
                           (declare (type (signed-byte 31) i))
                           (loop :for array :in arrays
                              :for elt-size := (the (signed-byte 31)
                                                    (floor (array-total-size array)
                                                           outer-dimension))
                              :collect
                                (multiple-value-bind (1d-storage-array offset)
                                    (1d-storage-array array)
                                  (declare (type (signed-byte 31) offset)
                                           (type (simple-array) 1d-storage-array))
                                  (make-array (cdr (array-dimensions array))
                                              :element-type (array-element-type array)
                                              :displaced-to 1d-storage-array
                                              :displaced-index-offset
                                              (the fixnum
                                                   (+ (* i elt-size) offset))))))
                    :if result :collect ; (apply function sub-arrays)
					  (let ((result-array (apply function sub-arrays)))
						(make-array (array-dimensions result-array)
									:element-type
									(array-element-type result-array)
									:displaced-to
									(1d-storage-array result-array)
									:displaced-index-offset 0))
                    :else :do (apply function sub-arrays))))
            (ecase result
              ((nil) nil)
              (list result-list)
              (array (nu:asarray result-list))))))))

;; (defun nu:map-outer (result function &rest arrays)
;;   "Result can be a object of type array or the symbol LIST or the symbol ARRAY."
;;   (declare (optimize (speed 3)))
;;   (let ((outer-dimensions (loop for array in arrays
;;                              collect (the fixnum (array-dimension array 0)))))
;;     (unless (apply '= outer-dimensions)
;;       (error "Cannot map over unequal outer dimensions ~D" outer-dimensions))
;;     (let ((outer-dimension (car outer-dimensions)))
;;       (declare (type (signed-byte 31) outer-dimension))
;;       (if (arrayp result)
;;           ;; How does one efficiently guess the dimension of what the function returns? 
;;           (let ((out-elt-size (floor (array-total-size result)
;;                                      (array-dimension result 0))))
;;             (declare (type (signed-byte 31) out-elt-size))
;;             (loop :for i fixnum :below outer-dimension
;;                :for out := (aref result i)
;;                :collect
;;                  (apply function
;;                         (nconc
;;                          (let ()
;;                            (declare (type (signed-byte 31) i))
;;                            (loop :for array :in arrays
;;                               :for elt-size = (the (signed-byte 31)
;;                                                    (floor (array-total-size array)
;;                                                           outer-dimension))
;;                               :collect (aref array i)))
;;                          (list :out out))))
;;             result)
;;           (let ((result-list
;;                  (loop :for i fixnum :below outer-dimension
;;                     :for sub-arrays
;;                       := (let ()
;;                            (declare (type (signed-byte 31) i))
;;                            (loop :for array :in arrays
;;                               :for elt-size := (the (signed-byte 31)
;;                                                     (floor (array-total-size array)
;;                                                            outer-dimension))
;;                               :collect (aref array i)))
;;                     :if result :collect (apply function sub-arrays)
;;                     :else :do (apply function sub-arrays))))
;;             (ecase result
;;               ((nil) nil)
;;               (list result-list)
;;               (array (apply #'nu:concatenate result-list))))))))
