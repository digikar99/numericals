(in-package :numericals.internals)

(defun ensure-appropriate-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (nu:asarray (ensure-list array-like) :type default-element-type)))

(defmacro with-pointers-to-vectors-data (bindings &body body)
  (if bindings
      `(cffi:with-pointer-to-vector-data ,(first bindings)
         (locally (declare (type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

(deftype size () `(unsigned-byte 62))
(defmacro the-size (form)
  #+sbcl `(sb-ext:truly-the size ,form)
  #-sbcl `(the size ,form))

(deftype int-index () `(signed-byte 62))
(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    int-index ,form))

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(declaim (inline cl-array-offset))
(declaim (ftype (function (cl:array) size) cl-array-offset))
(defun cl-array-offset (array)
  (declare (optimize speed)
           (type cl:array array)
           (notinline cl-array-offset))
  (if (typep array 'simple-array)
      0
      (multiple-value-bind (displaced-to offset)
          (array-displacement array)
        (the-size (+ offset (cl-array-offset displaced-to))))))

(declaim (inline array-storage)
         (ftype (function (cl:array) (simple-array * 1))))
(defun array-storage (array)
  (declare (ignorable array)
           (type cl:array array)
           (notinline array-storage))
  (if (typep array 'simple-array)
      #+sbcl (sb-ext:array-storage-vector array)
      #-sbcl (error "ARRAY-STORAGE not implemented for CL:ARRAY!")
      (array-storage (nth-value 0 (array-displacement array)))))

(defpolymorph nu:array= ((array1 cl:array) (array2 cl:array) &key (test #'equalp)) boolean
  (and (equalp (array-dimensions array1)
               (array-dimensions array2))
       (loop :for i :below (array-total-size array1)
             :always (funcall test
                              (row-major-aref array1 i)
                              (row-major-aref array2 i)))))

