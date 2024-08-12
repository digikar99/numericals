(in-package :numericals/utils/impl)

(defmacro ptr-iterate-but-inner (broadcast-dimensions-expr n-var &body (bindings &rest expression))
  "Each bindings is of the form (PTR-VAR ELT-SIZE INNER-STRIDE-VAR ARRAY-EXPR)."

  (let* ((pointers      (mapcar #'first  bindings))
         (elt-sizes     (mapcar #'second bindings))
         (inner-strides (mapcar #'third bindings))
         (array-exprs   (mapcar #'fourth  bindings))
         (num-arrays    (length bindings)))

    (let ((array-vars   (make-gensym-list num-arrays "ARRAY"))
          (dimensions   (gensym "DIMENSIONS"))
          (ss           inner-strides) ; we use the same to avoid reassignment in nest-loop
          (strides      (make-gensym-list num-arrays "STRIDES"))
          (broadcast-dimensions (gensym "BROADCAST-DIMENSIONS")))

      `(let ((,broadcast-dimensions ,broadcast-dimensions-expr)
             ,@(mapcar (lm var expr `(,var ,expr))
                       array-vars array-exprs))
         (declare (cl:type cl:array ,@array-vars))
         (with-pointers-to-vectors-data
             (,@(mapcar (lm ptr var `(,ptr (array-storage ,var)))
                        pointers array-vars))
           ,@(mapcar (lm ptr var elt-size `(cffi:incf-pointer ,ptr
                                               (the-size (* ,elt-size
                                                            (cl-array-offset ,var)))))
                     pointers array-vars elt-sizes)
           (let (,@(mapcar (lm strides var `(,strides (strides-for-broadcast
                                                       (array-dimensions ,var)
                                                       ,broadcast-dimensions)))
                           strides array-vars))
             (declare (cl:type cffi-sys:foreign-pointer ,@pointers))

             (labels ((nest-loop (,dimensions ,@strides)
                        (let ((,n-var   (first ,dimensions))
                              ,@(mapcar (lm ss strides `(,ss (first ,strides)))
                                        ss strides))
                          (declare (cl:type int-index ,@ss)
                                   (cl:type size ,n-var)
                                   (ignorable ,n-var ,@ss))
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
               (if (null ,broadcast-dimensions)
                   (let ((,n-var 1)
                         ,@(mapcar (lm ss `(,ss 0)) ss))
                     (declare (ignorable ,n-var ,@ss))
                     ,@expression
                     nil)
                   (nest-loop ,broadcast-dimensions ,@strides)))))))))
