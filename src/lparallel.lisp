(in-package :numericals.internals)

(defparameter nu:*multithreaded-threshold* 80000)
(declaim (type fixnum nu:*multithreaded-threshold*))

(defmacro with-thresholded-multithreading (threshold-measure (&rest vars) &body body)
  (with-gensyms (worker-count thread-idx)
    `(block thresholded-multithreading
       (if (< (the size ,threshold-measure)
              nu:*multithreaded-threshold*)
           (progn
             ,@body)
           ,(let ((work-size-vars       (make-gensym-list (length vars) "WORK-SIZE"))
                  (max-work-size-vars   (make-gensym-list (length vars) "MAX-WORK-SIZE"))
                  (local-work-size-vars (make-gensym-list (length vars) "LOCAL-WORK-SIZE")))
              `(let* ((,worker-count  (lparallel:kernel-worker-count))
                      ,@(loop :for var :in vars
                              :for work-size-var :in work-size-vars
                              :collect `(,work-size-var (array-total-size ,var))))
                 (declare (type size ,worker-count ,@work-size-vars))
                 (let (,@(loop :for work-size-var :in work-size-vars
                               :for max-work-size-var :in max-work-size-vars
                               :collect `(,max-work-size-var (ceiling ,work-size-var
                                                                      ,worker-count))))
                   (declare (type size ,@max-work-size-vars))
                   (lparallel:pdotimes (,thread-idx ,worker-count)
                     (declare (type size ,thread-idx))
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
                       (declare (type array ,@vars))
                       ,@body)))))))))
