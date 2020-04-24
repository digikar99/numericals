(in-package :numericals.internals)

;; Numpy uses an array wrapper, and a slice of the matrix
;; is actually a part of the original matrix - meaning writing to the
;; slice actually writes to the original array - and this is imaginable
;; to be efficient. And even though make-array doesn't cost much,
;; copying the data over to this new array definitely costs much.
;; I don't know of the multi-dimensional displaced array support in lisp.

;; The closest thing, perhaps, would be to add an iteration macro do-aref
;; or an iterate clause, or both.

;; The following implementation happens to be about 5 times faster than select:select.
;; However, in the absence of further optimizations (= compiler-macros)
;; this is just as slow in the case reducible to cl:aref.

;; This is also about 5 times slower than numpy. While this does not use SIMD,
;; it's not clear how to use it. And this is regardless of SIMD. At moments,
;; when numpy can use SIMD (= data is local), numpy is about 20 times faster!

(declaim (inline tp))
(defun-c tp (object) (eq t object))

(declaim (inline fixnump))
(defun-c fixnump (object) (typep object 'fixnum))

(declaim (inline aref-applicable-p))
(defun-c aref-applicable-p (array &rest subscripts)
  ;; assume length= is ensured by caller
  (every 'fixnump subscripts))

(defun-c normalize-subscripts (array &rest subscripts)
  (declare (optimize (speed 3)))
  (let ((num-dim (length (array-dimensions array)))
        (len (length subscripts))
        (subscript nil))
    (declare (type (signed-byte 31) num-dim len))
    (assert (<= len num-dim) nil
            "Using ~D subscripts to index into an array of ~D dimensions"
            len num-dim)
    (loop for dim fixnum in (array-dimensions array)
       do (setq subscript (car subscripts)
                subscripts (cdr subscripts))
       collect
         (etypecase subscript
           (list (destructuring-bind (&optional (start t) (end t) (step nil step-supplied-p))
                     subscript
                   (nconc (list (if (tp start) 0 start)
                                (if (tp end) dim end))
                          (if (or (tp step) (null step-supplied-p))
                              (list 1)
                              (list step)))))
           (number (list subscript (1+ subscript) 1))
           (t (list 0 dim 1))))))

(declaim (inline required-array-size))
(defun-c required-array-size (start end step)
  (ceiling (- end start)
           step))

(progn
  (defmacro define-aref-ref ()
    `(defun nu:aref (&rest args)
       "ARGS: (array &rest subscripts &key out)
Examples: (array '(t t 3) :out out)."
       (declare (optimize (speed 3)))
       (destructuring-bind ((array &rest subscripts) (&key out))
           (split-at-keywords args)
         (declare (type list subscripts))
         (setq subscripts
               (nconc subscripts
                      (make-list (- (length (array-dimensions array))
                                    (length subscripts))
                                 :initial-element t)))
         (if (apply 'aref-applicable-p array subscripts)
             (apply 'aref array subscripts)
             (let ((normalized-subscripts (apply 'normalize-subscripts
                                                 array subscripts))
                   (num-dim (length (array-dimensions array))))
               (declare (type list normalized-subscripts))
               (unless out
                 (setq out
                       (apply 'nu:zeros
                              (nconc (loop for subscript in subscripts
                                        for normalized-subscript in normalized-subscripts
                                        unless (numberp subscript)
                                        collect (apply 'required-array-size
                                                       normalized-subscript))
                                     (list :type (array-element-type array))))))
               (ecase (array-element-type array)
                 ,@(loop for type in '(single-float double-float fixnum)
                      collect
                        `(,type
                          (ecase num-dim
                            ,@(loop for i from 1 to *max-broadcast-dimensions*
                                 collect `(,i
                                           (apply ',(specialized-operation 'aref type i)
                                                  out array
                                                  (nconc ,@(loop for j below i
                                                              collect
                                                                `(elt normalized-subscripts
                                                                      ,j)))))))))))))))
  (define-aref-ref))

(progn
  (defmacro define-aref-set ()
    `(defun (setf nu:aref) (new-value array &rest subscripts)
       "ARGS: (array &rest subscripts &key out)
Examples: (array '(t t 3) :out out)."
       (declare (optimize (speed 3))
                (type list subscripts))
       (setq subscripts
             (nconc subscripts
                    (make-list (- (length (array-dimensions array))
                                  (length subscripts))
                               :initial-element t)))
       (if (apply 'aref-applicable-p array subscripts)
           (setf (apply #'aref array subscripts) new-value)
           (let ((normalized-subscripts (apply 'normalize-subscripts
                                               array subscripts))
                 (num-dim (length (array-dimensions array))))
             (declare (type list normalized-subscripts))
             (unless (equalp (loop for subscript in subscripts
                                for normalized-subscript in normalized-subscripts
                                unless (numberp subscript)
                                collect (apply 'required-array-size
                                               normalized-subscript))
                             (array-dimensions new-value))
               (error "~D is not compatible with stated dimensions!" new-value))
             (ecase (array-element-type array)
               ,@(loop for type in '(single-float double-float fixnum)
                    collect
                      `(,type
                        (ecase num-dim
                          ,@(loop for i from 1 to *max-broadcast-dimensions*
                               collect `(,i
                                         (setf (apply #',(specialized-operation 'aref type i)
                                                      array
                                                      (nconc ,@(loop for j below i
                                                                  collect
                                                                    `(elt normalized-subscripts
                                                                          ,j))))
                                               new-value)))))))))))
  (define-aref-set))

(defmacro nested (mode max-depth current-depth out-array out-array-indices array
                  array-indices
                  start-symbols end-symbols step-symbols oa-stride-symbols)
  (if (= max-depth current-depth)
      (ecase mode
        (:ref `(setf (aref ,out-array (the (signed-byte 31) (+ ,@(subseq out-array-indices
                                                                         0 max-depth))))
                     (aref ,array (the (signed-byte 31) (+ ,@(subseq array-indices
                                                                     0 max-depth))))))
        (:set `(setf (aref ,array (the (signed-byte 31) (+ ,@(subseq array-indices
                                                                     0 max-depth))))
                     (aref ,out-array (the (signed-byte 31) (+ ,@(subseq out-array-indices
                                                                         0 max-depth)))))))
      (let ((start (elt start-symbols current-depth))
            (end (elt end-symbols current-depth))
            (step (elt step-symbols current-depth))
            (oa-step (elt oa-stride-symbols current-depth)))
        `(loop for ,(elt array-indices current-depth) fixnum
            from ,start
            below ,end
            by ,step
            for ,(elt out-array-indices current-depth) fixnum from 0 by ,oa-step
            do ,(macroexpand-1 `(nested ,mode
                                        ,max-depth ,(1+ current-depth)
                                        ,out-array
                                        ,out-array-indices
                                        ,array
                                        ,array-indices
                                        ,start-symbols
                                        ,end-symbols
                                        ,step-symbols
                                        ,oa-stride-symbols))))))

(progn
  (defmacro define-specialized-aref (mode)
    (let ((n *max-broadcast-dimensions*))
      `(progn
         ,@(loop for type in '(single-float double-float fixnum)
              collect
                (let* ((array-indices (make-gensym-list n "AI"))
                       (out-array-indices (make-gensym-list n "OAI"))
                       (start-symbols (make-gensym-list n "START"))
                       (strided-start-symbols (make-gensym-list n "SSTART"))
                       (end-symbols (make-gensym-list n "END"))
                       (strided-end-symbols (make-gensym-list n "SEND"))
                       (step-symbols (make-gensym-list n "STEP"))
                       (strided-step-symbols (make-gensym-list n "SSTEP"))
                       (oa-stride-symbols (make-gensym-list n "OAS"))
                       (a-stride-symbols (make-gensym-list n "AS")))
                  `(progn
                     ,@(loop for i from 1 to *max-broadcast-dimensions*
                          collect
                            (let ((args (iter (for j below i)
                                              (for start in start-symbols)
                                              (for end in end-symbols)
                                              (for step in step-symbols)
                                              (collect start)
                                              (collect end)
                                              (collect step))))
                              `(defun ,(ecase mode
                                         (:ref (specialized-operation 'aref type i))
                                         (:set `(setf ,(specialized-operation 'aref type i))))
                                   (out array ,@args)
                                 (declare (optimize (speed 3))
                                          (type (signed-byte 31) ,@args)
                                          (type (array ,type) out array))
                                 (destructuring-bind (&optional
                                                      ,@(loop for s in oa-stride-symbols
                                                           for j below i
                                                           collect `(,s 1)))
                                     (strides out)
                                   (declare (optimize (speed 3))
                                            (type (signed-byte 31)
                                                  ,@(subseq oa-stride-symbols 0 i)))
                                   (destructuring-bind ,(subseq a-stride-symbols 0 i)
                                       (strides array)
                                     (declare (optimize (speed 3))
                                              (type (signed-byte 31)
                                                    ,@(subseq a-stride-symbols 0 i)))
                                     (let ((out (1d-storage-array out))
                                           (array (1d-storage-array array))
                                           ,@(iter
                                               (for j below i)
                                               (for start in start-symbols)
                                               (for sstart in strided-start-symbols)
                                               (for end in end-symbols)
                                               (for send in strided-end-symbols)
                                               (for step in step-symbols)
                                               (for sstep in strided-step-symbols)
                                               (for a in a-stride-symbols)
                                               (collect `(,sstart (the (signed-byte 31)
                                                                       (* ,a ,start))))
                                               (collect `(,send (the (signed-byte 31)
                                                                     (* ,a ,end))))
                                               (collect `(,sstep (the (signed-byte 31)
                                                                      (* ,a ,step))))))
                                       (declare (type (simple-array ,type) out array)
                                                (type (signed-byte 31)
                                                      ,@(subseq strided-start-symbols 0 i)
                                                      ,@(subseq strided-end-symbols 0 i)
                                                      ,@(subseq strided-step-symbols 0 i)))
                                       ,(macroexpand-1 `(nested ,mode ,i 0
                                                                out ,out-array-indices
                                                                array 
                                                                ,array-indices
                                                                ,strided-start-symbols
                                                                ,strided-end-symbols
                                                                ,strided-step-symbols
                                                                ,oa-stride-symbols)))))
                                 out)))))))))
  (define-specialized-aref :ref)
  (define-specialized-aref :set))

(defun strides (array)
  (let ((reversed-dimensions (nreverse (array-dimensions array)))
        (product 1))
    (nreverse
     (loop for i in reversed-dimensions
        collect product
        do (setq product (* product i))))))

;; EXAMPLE 
;; (defun single-2d-aref (out array i00 i01 i02 i10 i11 i12)
;;   (declare (optimize (speed 3))
;;            (type (signed-byte 31) i00 i01 i02 i10 i11 i12))
;;   (destructuring-bind (&optional (oa0 1) (oa1 1)) (strides out)
;;     (destructuring-bind (a0 a1) (strides array)
;;       (declare (type (signed-byte 31) a0 a1))
;;       (let ((out (1d-storage-array out))
;;             (array (1d-storage-array array)))
;;         (declare (type (simple-array single-float) out array))
;;         (let ((start0 (* a0 i00))
;;               (end0 (* a0 i01))
;;               (step0 (* a0 i02))
;;               (start1 (* a1 i10))
;;               (end1 (* a1 i11))
;;               (step1 (* a1 i12)))
;;           (declare (type (signed-byte 31) start0 end0 step0 start1 step1 end1)
;;                    (optimize (speed 3)))
;;           (loop for ai0 fixnum from start0 below end0 by step0
;;              for oai0 fixnum from 0 by oa0
;;              do (loop for ai1 fixnum from start1 below end1 by step1
;;                    for oai1 fixnum from 0 by oa1
;;                    do ;; (print (list oai0 oai1))
;;                    ;; (print (list ai0 ai1))
;;                      (setf (aref out
;;                                  (the (signed-byte 31)
;;                                       (+ oai0 oai1)))
;;                            (aref array
;;                                  (the (signed-byte 31)
;;                                       (+ ai0 ai1))))))))))
;;   out)
