(in-package :dense-numericals/more-utils)

(declaim (inline array-total-offset))
(defun array-total-offset (array)
  (declare (type dense-arrays::dense-array array)
           (optimize speed))
  (loop :for o :of-type size :in (array-offsets array)
        :with sum :of-type size := 0
        :do (incf sum o)
        :finally (return sum)))

(defmacro ptr-iterate-but-inner (broadcast-dimensions-expr n-var &body (bindings &rest expression)
                                 &environment env)
  "Each bindings is of the form (PTR-VAR ELT-SIZE INNER-STRIDE-VAR ARRAY-EXPR).
The loop is necessary because different axes can have different strides and
offsets. Equivalently, the loop (and the use of this macro) is unnecessary if
the arrays were SIMPLE with same strides and offsets."

  ;; Differences wrt numericals.internals::ptr-iterate-but-inner include the handling of
  ;; multi-dimensional offsets

  (let* ((pointers      (mapcar #'first  bindings))
         (elt-sizes     (mapcar #'second bindings))
         (inner-strides (mapcar #'third bindings))
         (array-exprs   (mapcar #'fourth  bindings))
         (num-arrays    (length bindings))
         (expr-types    (mapcar (lm expr (cl-form-types:nth-form-type expr env 0)) array-exprs)))

    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (offsets      (make-gensym-list num-arrays "OFFSETS"))
          (dimensions   (gensym "DIMENSIONS"))
          (ss           inner-strides) ; we use the same to avoid reassignment in nest-loop
          (os           (make-gensym-list num-arrays "OS"))
          (strides      (make-gensym-list num-arrays "STRIDES"))
          (broadcast-dimensions (gensym "BROADCAST-DIMENSIONS")))

      `(let* ((,broadcast-dimensions ,broadcast-dimensions-expr)
              ,@(mappend (lambda (var expr expr-type)
                           `((,var ,expr)
                             (,var (locally (declare (type ,expr-type ,var))
                                     (if (equal (the list ,broadcast-dimensions)
                                                (narray-dimensions ,var))
                                         ,var
                                         (broadcast-array ,expr ,broadcast-dimensions))))))
                        array-vars array-exprs expr-types))
         (declare ,@(mapcar (lm var expr-type `(type ,expr-type ,var))
                            array-vars expr-types))
         (with-thresholded-multithreading (array-total-size ,(lastcar array-vars))
             ,array-vars
           (with-pointers-to-vectors-data
               (,@(mapcar (lm ptr var `(,ptr (array-storage ,var)))
                          pointers array-vars))
             (let (,@(mapcar (lm strides var `(,strides (array-strides ,var)))
                             strides array-vars)
                   ,@(mapcar (lm offsets var `(,offsets (array-offsets ,var)))
                             offsets array-vars))
               (declare (cl:type cffi-sys:foreign-pointer ,@pointers))

               (labels ((nest-loop (,dimensions ,@strides ,@offsets)
                          (let ((,n-var   (first ,dimensions))
                                ,@(mapcar (lm ss strides `(,ss (first ,strides)))
                                          ss strides)
                                ,@(mapcar (lm os offsets `(,os (first ,offsets)))
                                          os offsets))
                            (declare (cl:type int-index ,@os ,@ss)
                                     (cl:type size ,n-var))
                            ;; (mapc #'print (list ,dimensions ,@pointers))
                            (if (null (rest ,dimensions))
                                (progn
                                  ,@(mapcar (lm ptr o elt-size
                                                `(cffi:incf-pointer
                                                     ,ptr (the-int-index (* ,elt-size ,o))))
                                            pointers os elt-sizes)
                                  ,@expression
                                  ,@(mapcar (lm ptr o elt-size
                                                `(cffi:incf-pointer
                                                     ,ptr (the-int-index
                                                           (* ,elt-size
                                                              (- ,o)))))
                                            pointers os elt-sizes)
                                  nil)
                                (loop :initially
                                      ,@(mapcar (lm ptr o elt-size
                                                    `(cffi:incf-pointer
                                                         ,ptr (the-int-index (* ,elt-size ,o))))
                                                pointers os elt-sizes)
                                      :repeat ,n-var
                                      :do (nest-loop
                                           (rest ,dimensions)
                                           ,@(mapcar (lm strides `(rest ,strides)) strides)
                                           ,@(mapcar (lm offsets `(rest ,offsets)) offsets))
                                      ,@(mapcar (lm ptr s elt-size
                                                    `(cffi:incf-pointer
                                                         ,ptr (the-int-index (* ,elt-size ,s))))
                                                pointers ss elt-sizes)
                                      :finally
                                      ,@(mapcar (lm ptr o s elt-size
                                                    `(cffi:incf-pointer
                                                         ,ptr
                                                         (the-int-index
                                                          (* ,elt-size
                                                             (the-int-index
                                                              (- (+ ,o
                                                                    (the-int-index
                                                                     (* ,n-var ,s)))))))))
                                                pointers os ss elt-sizes))))))
                 (nest-loop (narray-dimensions ,(first array-vars))
                            ,@strides ,@offsets)))))))))

(defmacro do-with-broadcasting
    (broadcast-dimensions-expr bindings &body body &environment env)
  "Each bindings is of the form (ELT-VAR ARRAY-EXPR &OPTIONAL ELT-TYPE)."
  (with-gensyms (broadcast-dimensions)
    `(let ((,broadcast-dimensions ,broadcast-dimensions-expr))
       (do-arrays ,(loop :for (elt-var array-expr &optional elt-type) :in bindings
                         :collect `(,elt-var (broadcast-array ,array-expr ,broadcast-dimensions)
                                             ,(if elt-type
                                                  `(array ,elt-type)
                                                  (let ((form-type
                                                          (cl-form-types:nth-form-type array-expr env 0)))
                                                    (if (subtypep form-type 'array env)
                                                        (abstract-arrays:array-type-element-type
                                                         form-type
                                                         env)
                                                        t)))))
         ,@body))))
