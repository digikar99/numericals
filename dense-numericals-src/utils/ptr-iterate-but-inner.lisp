(in-package :dense-numericals/utils/impl)

(defmacro ptr-iterate-but-inner (broadcast-dimensions-expr n-var &body (bindings &rest expression)
                                 &environment env)
  "Each bindings is of the form (PTR-VAR ELT-SIZE INNER-STRIDE-VAR ARRAY-EXPR).
The loop is necessary because different axes can have different
strides. Equivalently, the loop (and the use of this macro) is unnecessary if
the arrays were SIMPLE with same strides."

  ;; Differences wrt numericals.internals::ptr-iterate-but-inner include the handling of
  ;; multi-dimensional strides

  (let* ((pointers      (mapcar #'first  bindings))
         (elt-sizes     (mapcar #'second bindings))
         (inner-strides (mapcar #'third bindings))
         (array-exprs   (mapcar #'fourth  bindings))
         (num-arrays    (length bindings))
         (expr-types    (mapcar (lm expr (cl-form-types:nth-form-type expr env 0)) array-exprs)))

    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (dimensions   (gensym "DIMENSIONS"))
          (ss           inner-strides) ; we use the same to avoid reassignment in nest-loop
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
                             strides array-vars))
               (declare (cl:type cffi-sys:foreign-pointer ,@pointers))

               ,@(mapcar (lm ptr var elt-size
                             `(cffi:incf-pointer
                                  ,ptr (the-int-index (* ,elt-size (array-offset ,var)))))
                         pointers array-vars elt-sizes)

               (labels ((nest-loop (,dimensions ,@strides)
                          (let ((,n-var   (first ,dimensions))
                                ,@(mapcar (lm ss strides `(,ss (first ,strides)))
                                          ss strides))
                            (declare (cl:type int-index ,@ss)
                                     (cl:type size ,n-var))
                            ;; (mapc #'print (list ,dimensions ,@pointers))
                            (if (null (rest ,dimensions))
                                (progn
                                  ,@expression
                                  nil)
                                (loop :repeat ,n-var
                                      :do (nest-loop
                                           (rest ,dimensions)
                                           ,@(mapcar (lm strides `(rest ,strides)) strides))
                                      ,@(mapcar (lm ptr s elt-size
                                                    `(cffi:incf-pointer
                                                         ,ptr (the-int-index (* ,elt-size ,s))))
                                                pointers ss elt-sizes)
                                      :finally
                                      ,@(mapcar (lm ptr s elt-size
                                                    `(cffi:incf-pointer
                                                         ,ptr
                                                         (the-int-index
                                                          (* ,elt-size
                                                             (the-int-index
                                                              (- (the-int-index
                                                                  (* ,n-var ,s))))))))
                                                pointers ss elt-sizes))))))
                 (if-let (,dimensions (narray-dimensions ,(first array-vars)))
                   (nest-loop ,dimensions ,@strides)
                   (let ((,n-var 1)
                         ,@(mapcar (lm ss `(,ss 1)) ss))
                     (declare (cl:type int-index ,@ss)
                              (cl:type size ,n-var))
                     ,@expression nil))))))))))

(defmacro do-with-broadcasting
    (broadcast-dimensions-expr bindings &body body &environment env)
  "Each bindings is of the form (ELT-VAR ARRAY-EXPR &OPTIONAL ELT-TYPE)."
  (with-gensyms (broadcast-dimensions)
    `(let ((,broadcast-dimensions ,broadcast-dimensions-expr))
       (do-arrays ,(mapcar (lambda (elt-var &optional array-expr elt-type)
                             `(,elt-var (broadcast-array ,array-expr ,broadcast-dimensions)
                                        ,(if elt-type
                                             `(array ,elt-type)
                                             (let ((form-type
                                                     (cl-form-types:nth-form-type array-expr env 0)))
                                               (if (subtypep form-type 'array env)
                                                   (abstract-arrays:array-type-element-type
                                                    form-type
                                                    env)
                                                   t)))))
                           bindings)
         ,@body))))
