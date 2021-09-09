(in-package :dense-numericals.impl)

(defun ensure-appropriate-dense-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (asarray (ensure-list array-like) :type default-element-type)))


(defmacro with-pointers-to-vectors-data (bindings &body body)
  (if bindings
      `(cffi:with-pointer-to-vector-data ,(first bindings)
         (locally (declare (type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

(defun type-max (type-1 type-2)
  (cond ((subtypep type-1 type-2)
         type-2)
        ((subtypep type-2 type-1)
         type-1)
        ((or (type= type-1 'double-float)
             (type= type-2 'double-float))
         'double-float)
        ((or (type= type-1 'single-float)
             (type= type-2 'single-float))
         'single-float)
        ;; At this point, none of the types are floats
        ;; FIXME: Operate better on impl with other float types
        ((and (subtypep type-1 '(unsigned-byte *))
              (subtypep type-2 '(signed-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-1 `(signed-byte ,num-bits))
                 :do (return-from type-max `(signed-byte ,num-bits))
               :finally (return-from type-max 'single-float)))
        ((and (subtypep type-1 '(signed-byte *))
              (subtypep type-2 '(unsigned-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-2 `(signed-byte ,num-bits))
                 :do (return-from type-max `(signed-byte ,num-bits))
               :finally (return-from type-max 'single-float)))
        (t
         (error "Don't know how to find TYPE-MAX of ~S and ~S" type-1 type-2))))

