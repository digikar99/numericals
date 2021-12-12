(in-package :numericals.internals)

(defun strides-for-broadcast (original-dimensions broadcast-dimensions)
  (flet ((incompatible ()
           (error "Dimensions ~D cannot be broadcast to dimensions ~D"
                  original-dimensions broadcast-dimensions)))
    (let ((len-diff (- (length broadcast-dimensions)
                       (length original-dimensions))))
      (if (< len-diff 0)
          (incompatible)
          (nconc (make-list len-diff :initial-element 0)
                 (nreverse (loop :for b :in (reverse broadcast-dimensions)
                                 :for d :in (reverse original-dimensions)
                                 :with s := 1
                                 :collect
                                 (cond ((= b d) s)
                                       ((= d 1) 0)
                                       (t (incompatible)))
                                 :do (setq s (* d s)))))))))

(defun strides (dimensions)
  (strides-for-broadcast dimensions dimensions))

(defun %broadcast-compatible-p (dimensions-a dimensions-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two dimensions are broadcast compatible.
  The second value is the dimensions of the array resulting from the broadcast."
  (if (equalp dimensions-a dimensions-b)
      (values t dimensions-a)
      (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1))
        ;; We do not use a "for in" clause because we want to terminate at the
        ;; maximum of the two lists rather than the minimum
        (for dim-a initially (reverse dimensions-a)
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (reverse dimensions-b)
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b) (= a 1) (= b 1))
                     (max a b)
                     (return nil))
          into broadcast-dimensions-reversed)
        (finally (return (values t
                                 (nreverse broadcast-dimensions-reversed)))))))


(defun broadcast-compatible-p (&rest arrays)
  "Returns two values:
  The first value is a generalized boolean indicating whether the dimensions can be broadcasted.
  The second value is the dimension of the array resulting from the broadcast."
  (declare (dynamic-extent arrays)
           (optimize speed))
  (case (length arrays)
    (0 t)
    (1 (values t (array-dimensions (first arrays))))
    (2 (%broadcast-compatible-p (array-dimensions (first arrays))
                                (array-dimensions (second arrays))))
    (t (multiple-value-bind (compatible-p broadcast-dimensions)
           (%broadcast-compatible-p (array-dimensions (first arrays))
                                    (array-dimensions (second arrays)))
         ;; Can this be simplified?
         (when compatible-p
           (multiple-value-bind (compatible-p-rest broadcast-dimensions-rest)
               (apply #'broadcast-compatible-p (cddr arrays))
             (when compatible-p-rest
               (%broadcast-compatible-p broadcast-dimensions
                                        broadcast-dimensions-rest))))))))

(define-condition incompatible-broadcast-dimensions (error)
  ((dimensions :initarg :dimensions :reader condition-dimensions)
   (array-likes :initarg :array-likes :reader condition-array-likes))
  (:report (lambda (c s)
             (pprint-logical-block (s nil)
               (format s "The following array-likes with dimensions~{~%  ~S~}~%cannot be broadcast together:~%" (condition-dimensions c))
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (condition-array-likes c)))))))

(defmacro do-with-broadcasting
    (broadcast-dimensions-expr bindings &body body &environment env)
  "Each bindings is of the form (ELT-VAR ARRAY-EXPR &OPTIONAL ELT-TYPE)."

  (let* ((elt-vars      (mapcar #'first  bindings))
         (array-exprs   (mapcar #'second bindings))
         (elt-types     (mapcar #'third  bindings))
         (num-arrays    (length bindings)))


    (let ((dimensions   (gensym "DIMENSIONS"))
          (array-vars   (make-gensym-list num-arrays "ARRAY"))
          (strides      (make-gensym-list num-arrays "STRIDES"))
          (is           (make-gensym-list num-arrays "I"))
          (svs          (make-gensym-list num-arrays "SV")) ; storage-vector
          (d            (gensym "D"))
          (ss           (make-gensym-list num-arrays "SS"))
          (broadcast-dimensions (gensym "BROADCAST-DIMENSIONS"))
          (storage-types (mapcar (lambda (array-expr elt-type)
                                   (or elt-type
                                       (let ((array-type
                                               (cl-form-types:nth-form-type
                                                array-expr env 0)))
                                         'cl:*
                                         (if (subtypep array-type 'cl:array)
                                             (array-type-element-type array-type)
                                             'cl:*))))
                                 array-exprs elt-types)))
      `(let ((,broadcast-dimensions ,broadcast-dimensions-expr)
             ,@(mapcar (lm var expr `(,var ,expr))
                       array-vars array-exprs))
         (let (,@(mapcar (lm sv var `(,sv (array-storage ,var)))
                         svs array-vars)
               ,@(mapcar (lm s var `(,s (strides-for-broadcast
                                         (narray-dimensions ,var)
                                         ,broadcast-dimensions)))
                         strides array-vars)
               ,@(mapcar (lm i var `(,i (cl-array-offset ,var)))
                         is array-vars))
           (declare (type int-index ,@is)
                    ;; (optimize speed) ; check before finalizing
                    ,@(mapcar (lm st sv `(type (cl:simple-array ,st 1) ,sv))
                              storage-types svs))
           ;; TODO: A proper let form would aid debugging, but doesn't allow setf-ing
           (symbol-macrolet (,@(mapcar (lm elt-var sv i `(,elt-var (cl:aref ,sv ,i)))
                                       elt-vars svs is))
             (labels ((nest-loop (,dimensions ,@strides)
                        (let ((,d  (first ,dimensions))
                              ,@(mapcar (lm s stride `(,s (first ,stride)))
                                        ss strides))
                          (declare (type int-index ,@ss)
                                   (type size ,d))
                          (if (null (rest ,dimensions))
                              (loop :repeat ,d
                                    :do (locally ,@body)
                                    ,@(mapcar (lm i s  `(incf ,i ,s)) is ss)
                                    :finally ,@(mapcar (lm i s `(decf ,i
                                                                      (the-int-index (* ,d ,s))))
                                                       is ss))
                              (loop :repeat ,d
                                    :do (nest-loop (rest ,dimensions)
                                                   ,@(mapcar (lm s `(rest ,s)) strides))
                                    ,@(mapcar (lm i s `(incf ,i ,s)) is ss)
                                    :finally ,@(mapcar (lm i s `(decf ,i
                                                                      (the-int-index (* ,d ,s))))
                                                       is ss))))))
               (nest-loop ,broadcast-dimensions ,@strides))))))))

(defun nu:broadcast-array (array broadcast-dimensions)
  (let ((out (nu:zeros broadcast-dimensions :type (array-element-type array))))
    (do-with-broadcasting broadcast-dimensions ((o out)
                                                (a array))
      (setf o a))
    out))
