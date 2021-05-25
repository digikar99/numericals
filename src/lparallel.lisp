(in-package :numericals.internals)

(defparameter nu:*multithreaded-threshold* 80000)
(declaim (type fixnum nu:*multithreaded-threshold*))

(defmacro with-thresholded-multithreading (threshold-measure (&rest vars) &body body)
  (with-gensyms (local-work-size)
    `(block thresholded-multithreading
       (if (< (the size ,threshold-measure)
              nu:*multithreaded-threshold*)
           (progn
             ,@body)
           ;; FIXME: Can the multiple BODY here be reduced?
           (progn
             (let* ((worker-count  (lparallel:kernel-worker-count))
                    (work-size (array-total-size (the cl:array out))))
               (declare (type size worker-count work-size))
               (let ((max-work-size (ceiling work-size worker-count)))
                 (declare (type size max-work-size))
                 (lparallel:pdotimes (thread-idx worker-count)
                   (declare (type size thread-idx))
                   (let* ((,local-work-size (if (= thread-idx
                                                   (1- worker-count))
                                                (the-size (- work-size
                                                             (* (1- worker-count)
                                                                max-work-size)))
                                                max-work-size))
                          ,@(loop :for var :in vars
                                  :collect
                                  `(,var
                                    (cl:make-array ,local-work-size
                                                   :element-type (array-element-type ,var)
                                                   :displaced-to (array-storage ,var)
                                                   :displaced-index-offset
                                                   (the-size (+ (cl-array-offset ,var)
                                                                (* thread-idx
                                                                   max-work-size)))))))
                     (declare (type array ,@vars))
                     ,@body)))))))))
