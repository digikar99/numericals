(in-package :dense-numericals/utils/impl)

(defparameter *inline-with-multithreading* nil
  "Inlining is usually necessary for smaller arrays; for such arrays multithreading
becomes unnecessary. If this parameter is non-NIL, code using multithreading
would be emitted; otherwise, the code would be skipped.")

(defparameter *multithreaded-threshold* 80000
  "The lower bound of the array size beyond which LPARALLEL is used for distributing
operations across multiple threads.
NOTE: It is not defined if this bound is inclusive or exclusive.")
(declaim (cl:type size *multithreaded-threshold*))


;;; While it's tempting to check the compile time value of *MULTITHREADED-THRESHOLD*
;;; while inlining in an attempt to reduce code size, the actual size differences
;;; in the codes are minimal; the size is largely determined by the PTR-ITERATE-BUT-INNER

(defmacro with-thresholded-multithreading (threshold-measure (&rest vars)
                                           &body body &environment env)
  (let* ((simple-p (eq :simple (first vars)))
         (vars     (if simple-p
                       (rest vars)
                       vars))
         (out      (lastcar vars))
         (body-sym (gensym "BODY")))
    (if (and polymorphic-functions:*compiler-macro-expanding-p*
             (not *inline-with-multithreading*))
        `(progn ,@body)
        `(block thresholded-multithreading
           (flet ((,body-sym ,vars
                    (declare ,@(mapcar (lm var `(type ,(cl-form-types:nth-form-type var env 0) ,var))
                                       vars))
                    ,@body))
             (if (< (cl:the size ,threshold-measure)
                    *multithreaded-threshold*)
                 (,body-sym ,@vars)
                 (progn
                   (let* ((worker-count  (lparallel:kernel-worker-count)))
                     (declare (cl:type size worker-count))
                     (multiple-value-bind (long-enough-axis axis/work-size)
                         ,(if simple-p
                              `(values 0 (array-total-size (the array ,out)))
                              `(loop :for i :of-type size :from 0
                                     :for d :of-type size :in (narray-dimensions ,out)
                                     :if (>= d worker-count)
                                       :do (return (values i d))
                                     :finally
                                        (return-from thresholded-multithreading
                                          (,body-sym ,@vars))))
                       (declare (cl:type size long-enough-axis axis/work-size))
                       (let ((max-work-size (ceiling axis/work-size worker-count)))
                         (declare (cl:type size max-work-size))
                         (lparallel:pdotimes (thread-idx worker-count)
                           (declare (cl:type size thread-idx))
                           (let* ((index (nconc (if (zerop long-enough-axis)
                                                    ()
                                                    (make-list long-enough-axis))
                                                (list (list (the-size (* thread-idx max-work-size))
                                                            :end (if (= thread-idx
                                                                        (1- worker-count))
                                                                     axis/work-size
                                                                     (the-size (* (1+ thread-idx)
                                                                                  max-work-size)))))))
                                  ,@(loop :for var :in vars
                                          :collect `(,var
                                                     (apply #'aref*
                                                            ,(if simple-p
                                                                 `(reshape ,var
                                                                           (array-total-size
                                                                            (the array ,var))
                                                                           :view t)
                                                                 var)
                                                            index))))
                             (declare (type array ,@vars))
                             (,body-sym ,@vars)))))))))))))

(defmacro with-thresholded-multithreading/cl (threshold-measure (&rest vars) &body body &environment env)
  (let* ((simple-p (eq :simple (first vars)))
         (vars     (if simple-p
                       (rest vars)
                       vars))
         (out      (lastcar vars)))
    (declare (ignorable out))
    (with-gensyms (worker-count thread-idx body-sym)
      (if (and polymorphic-functions:*compiler-macro-expanding-p*
               (not *inline-with-multithreading*))
          `(progn ,@body)
          `(block thresholded-multithreading
             (flet ((,body-sym ,vars
                      (declare ,@(mapcar (lm var `(type ,(cl-form-types:nth-form-type var env 0) ,var))
                                         vars))
                      ,@body))
               (if (< (cl:the size ,threshold-measure)
                      *multithreaded-threshold*)
                   (,body-sym ,@vars)
                   ,(let ((work-size-vars       (make-gensym-list (length vars) "WORK-SIZE"))
                          (max-work-size-vars   (make-gensym-list (length vars) "MAX-WORK-SIZE"))
                          (local-work-size-vars (make-gensym-list (length vars) "LOCAL-WORK-SIZE")))
                      `(let* ((,worker-count  (lparallel:kernel-worker-count))
                              ,@(loop :for var :in vars
                                      :for work-size-var :in work-size-vars
                                      :collect `(,work-size-var (array-total-size ,var))))
                         (declare (cl:type size ,worker-count ,@work-size-vars))
                         (let (,@(loop :for work-size-var :in work-size-vars
                                       :for max-work-size-var :in max-work-size-vars
                                       :collect `(,max-work-size-var (ceiling ,work-size-var
                                                                              ,worker-count))))
                           (declare (cl:type size ,@max-work-size-vars))
                           (lparallel:pdotimes (,thread-idx ,worker-count)
                             (declare (cl:type size ,thread-idx))
                             (let* (,@(loop :for work-size-var :in work-size-vars
                                            :for max-work-size-var :in max-work-size-vars
                                            :for local-work-size-var :in local-work-size-vars
                                            :collect
                                            `(,local-work-size-var
                                              (if (= ,thread-idx (1- ,worker-count))
                                                  (the-size (- ,work-size-var
                                                               (the-size (* (1- ,worker-count)
                                                                            ,max-work-size-var))))
                                                  ,max-work-size-var)))
                                    ,@(loop :for var :in vars
                                            :for max-work-size-var :in max-work-size-vars
                                            :for local-work-size-var :in local-work-size-vars
                                            :collect
                                            `(,var
                                              (cl:make-array ,local-work-size-var
                                                             :element-type (array-element-type ,var)
                                                             :displaced-to (array-storage ,var)
                                                             :displaced-index-offset
                                                             (the-size (+ (cl-array-offset ,var)
                                                                          (the-size
                                                                           (* ,thread-idx
                                                                              ,max-work-size-var))))))))
                               (declare (type cl:array ,@vars))
                               (,body-sym ,@vars)))))))))))))
