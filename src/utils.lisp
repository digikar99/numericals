(in-package :numericals.impl)
;; (numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-condition runtime-array-allocation (suboptimal-polymorph-note)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unable to avoid array allocation at run time. Consider supplying
the OUT argument, and/or ensuring all the appropriate arguments are
arrays of appropriate types."))))

(defvar nu:*default-float-format* 'single-float
  "Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.")
(declaim (type (member single-float double-float) nu:*default-float-format*))

(defvar nu:*broadcast-automatically* t
  "If non-NIL, operations automatically perform broadcasting as necessary.
If NIL, broadcasting is expected to be performed by the user. Such strictness
can be helpful to locate bugs.")

(defmacro with-pointers-to-vectors-data (bindings &body body)
  (if bindings
      `(cffi:with-pointer-to-vector-data ,(first bindings)
         (locally (declare (cl:type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

(defmacro with-foreign-object ((var type &optional (value nil valuep)) &body body)
  "A wrapper around CFFI:WITH-FOREIGN-OBJECT with an additional VALUE option."
  (once-only (value)
    `(cffi:with-foreign-object (,var ,type)
       ,(when valuep
          `(setf (cffi:mem-ref ,var ,type) ,value))
       ,@body)))

(defmacro with-foreign-objects (bindings &body body)
  "A wrapper around CFFI:WITH-FOREIGN-OBJECTS with an additional VALUE option."
  (if bindings
      `(with-foreign-object ,(first bindings)
         (with-foreign-objects ,(rest bindings)
           ,@body))
      `(locally ,@body)))

(declaim (inline blas-trans))
(defun blas-trans (array &optional invertedp)
  (declare (optimize speed)
           (type cl:array array)
           (ignore array))
  (if invertedp "N" "T"))

(declaim (inline narray-dimensions))
(defun narray-dimensions (array)
  (declare (type cl:array array)
           (optimize speed))
  (array-dimensions array))

(declaim (inline array-stride))
(defun array-stride (array axis)
  (loop :for d :in (subseq (narray-dimensions array) (1+ axis))
        :with stride := 1
        :do (setq stride (* d stride))
        :finally (return stride)))

(declaim (inline array-layout))
(defun array-layout (array)
  (declare (type array array)
           (ignore array))
  :row-major)

(defmacro ccall (name &rest args)
  "Calls CFFI:FOREIGN-FUNCALL on NAME with appropriate type declarations on args."
  `(cffi:foreign-funcall
    ,@(loop :for arg :in args
            :appending `(once-only (arg)
                          ((etypecase ,arg
                             (cffi:foreign-pointer :pointer)
                             (single-float :float)
                             (double-float :double))
                           ,arg)))))

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
           (type cl:array array))
  (loop :with total-offset :of-type (signed-byte 61) := 0
        :if (typep array 'cl:simple-array)
          :do (return total-offset)
        :else
          :do (multiple-value-bind (displaced-to offset)
                  (cl:array-displacement array)
                (declare (type (signed-byte 61) offset))
                (incf total-offset offset)
                (setq array displaced-to))))

(declaim (inline array-storage)
         (ftype (function (cl:array) (cl:simple-array * 1))))
(defun array-storage (array)
  (declare (ignorable array)
           (optimize speed))
  (loop :with array := array
        :do (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
              (typecase array
                ((cl:simple-array * (*)) (return array))
                (cl:simple-array (return #+sbcl (sb-ext:array-storage-vector array)
                                         #+ccl (ccl::%array-header-data-and-offset array)
                                         #-(or sbcl ccl)
                                         (error "Don't know how to obtain ARRAY-STORAGE on ~S"
                                                (lisp-implementation-type))))
                (t (setq array (cl:array-displacement array)))))))

(defpolymorph nu:array= ((array1 cl:array) (array2 cl:array) &key (test #'equalp)) boolean
  (and (equalp (array-dimensions array1)
               (array-dimensions array2))
       (loop :for i :below (array-total-size array1)
             :always (funcall test
                              (row-major-aref array1 i)
                              (row-major-aref array2 i)))))


(defun array-type-element-type (array-type &optional env)
  (loop :for type :in '(t
                        single-float
                        double-float
                        fixnum
                        (unsigned-byte 64)
                        (unsigned-byte 32)
                        (unsigned-byte 16)
                        (unsigned-byte 08)
                        (signed-byte 64)
                        (signed-byte 32)
                        (signed-byte 16)
                        (signed-byte 08))
        :if (cl:subtypep array-type `(cl:array ,type) env)
          :do (return type)))

;;; Below functions are taken from DENSE-ARRAYS-PLUS-LITE

(defun dimensions (array-like)
  "Consequences of ARRAY-LIKE having elements of different dimensions is undefined."
  (typecase array-like
    (string      nil)
    (sequence    (let ((len (length array-like)))
                   (cons len
                         (when (> len 0) (dimensions (elt array-like 0))))))
    (cl:array    (cl:array-dimensions array-like))
    (t           ())))

(defun element-type (array-like)
  "Consequences of ARRAY-LIKE having elements of different element-type is undefined."
  (typecase array-like
    (string      t)
    (sequence    (if (< 0 (length array-like))
                     (loop :for i :from 1 :below (length array-like)
                           :with max-type := (element-type (elt array-like 0))
                           :do (setq max-type
                                     (max-type max-type
                                               (element-type (elt array-like i))))
                           :finally (return max-type))
                     'null))
    (cl:array    (cl:array-element-type array-like))
    (t           (type-of array-like))))

(defun max-type (type-1 type-2)
  (cond ((subtypep type-1 type-2)
         type-2)
        ((subtypep type-2 type-1)
         type-1)
        ((or (alexandria:type= type-1 'double-float)
             (alexandria:type= type-2 'double-float))
         'double-float)
        ((or (alexandria:type= type-1 'single-float)
             (alexandria:type= type-2 'single-float))
         'single-float)
        ;; At this point, none of the types are floats
        ;; FIXME: Operate better on impl with other float types
        ((and (subtypep type-1 '(unsigned-byte *))
              (subtypep type-2 '(signed-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-1 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        ((and (subtypep type-1 '(signed-byte *))
              (subtypep type-2 '(unsigned-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-2 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        (t
         (error "Don't know how to find MAX-TYPE of ~S and ~S" type-1 type-2))))
