(in-package :numericals/more-utils)

;;; TODO: Change &KEY TYPE to &KEY (TYPE DEFAULT-ELEMENT-TYPE)
;;; Requires changes to polymorphic-functions
(define-polymorphic-function asarray
    (array-like &key type layout)
  :overwrite t
  :documentation "ARRAY-LIKE can be a nested sequence of sequences, or an array.")

(declaim (notinline asarray))
(defpolymorph (asarray :inline t) (array-like &key ((type (eql :auto))) (layout :row-major))
    (values cl:array &optional)
  (declare (ignore type))
  (ensure-row-major-layout)
  (asarray array-like :type (element-type array-like)))

(defpolymorph asarray (array-like &key (type default-element-type)
                                     (layout :row-major))
    (values cl:array &optional)
  ;; TODO: Define the predicate array-like-p.
  (ensure-row-major-layout)
  (etypecase array-like
    (array
     (let ((array (zeros (narray-dimensions array-like) :type type)))
       (loop :for index :below (array-total-size array-like)
             :do (setf (row-major-aref array index)
                       (coerce (row-major-aref array-like index) type)))
       array))
    (sequence
     (let* ((result-array (zeros (shape array-like) :type type))
            (index 0)
            (rsv (array-storage result-array)))
       (labels ((%asarray (array-like)
                  (etypecase array-like
                    (sequence
                     (map nil #'%asarray array-like))
                    (array
                     (let ((asv (array-storage array-like))
                           (offset (cl-array-offset array-like)))
                       (loop :for i :from offset
                               :below (+ offset (array-total-size array-like))
                             :do (setf (cl:aref rsv index)
                                       ;; We will leave the optimization for this
                                       ;; to cl-form-types
                                       (coerce
                                        (cl:aref asv i)
                                        type))
                                 (incf index))))
                    (real
                     (setf (cl:aref rsv index)
                           (coerce (the real array-like) type))
                     (incf index)))))
         (%asarray array-like))
       result-array))
    (atom (make-array 1 :element-type type
                        :initial-element (coerce array-like type)))))

