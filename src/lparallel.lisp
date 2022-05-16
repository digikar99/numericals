(in-package :numericals.internals)

(defparameter nu:*inline-with-multithreading* nil
  "Inlining is usually necessary for smaller arrays; for such arrays multithreading
becomes unnecessary. If this parameter is non-NIL, code using multithreading
would be emitted; otherwise, the code would be skipped.")

(defparameter nu:*multithreaded-threshold* 80000)
(declaim (type fixnum nu:*multithreaded-threshold*))

;;; On 2nd December 2021:

;;; The original motivation for WITH-THRESHOLDED-MULTITHREADING was
;;; that the same VARS (inside the thread) will be bound to different
;;; sub-parts of the arrays bound to VARS (outside the thread). This
;;; way, the same BODY can execute outside as well as inside the
;;; threads.

;;; However, this necessitates that there be a way to divide up the
;;; original arrays into multiple smaller possibly-continuous arrays
;;; representing subparts of the original arrays. This is always
;;; possible with DENSE-ARRAYS:ARRAY but not with CL:ARRAY. With
;;; CL:ARRAY, it is only posssible when such a division can be
;;; represented using displaced arrays. For example, an array of
;;; dimensions (1000 1000 2) can be divided into 4 continuous arrays
;;; of dimensions (250 1000 2), however, while an array of dimensions
;;; (2 1000 1000) can be divided into 4 NON-continuous arrays of
;;; dimensions (2 250 1000), such a division is only possible with
;;; DENSE-ARRAYS:ARRAY but not with CL:ARRAY.

;;; CONFIRM: We wish each of the smaller arrays to be as continuous as
;;; possible, as this aids a proper speedup.

;;; Overall, this implies:
;;; 1. WITH-THRESHOLDED-MULTITHREADING can be used almost always with DENSE-ARRAYS:ARRAY
;;; 2. For CL:ARRAY
;;;    a. In the absence of broadcasting, since the arrays are continuous, they can almost always
;;;       be divided into continuous sub-arrays. Above, while a (2 1000 1000) array cannot be
;;;       divided into continuous sub-arrays of dimensions (2 250 1000). One can directly work
;;;       with the storage vectors, whose divisions will always be continuous.
;;;    b. In the presence of broadcasting, it is harder to work with storage vectors directly
;;;       and this is yet to be thought about.

;;; FIXME? Confirm that the arrays are one dimensional below?

(defmacro with-thresholded-multithreading (threshold-measure (&rest vars) &body body &environment env)
  (let* ((simple-p (eq :simple (first vars)))
         (vars     (if simple-p
                       (rest vars)
                       vars))
         (out      (lastcar vars)))
    (declare (ignorable out))
    (with-gensyms (worker-count thread-idx body-sym)
      (if (and polymorphic-functions:*compiler-macro-expanding-p*
               (not nu:*inline-with-multithreading*))
          `(progn ,@body)
          `(block thresholded-multithreading
             (flet ((,body-sym ,vars
                      (declare ,@(mapcar (lm var `(type ,(cl-form-types:nth-form-type var env 0) ,var))
                                         vars))
                      ,@body))
               (if (< (the size ,threshold-measure)
                      nu:*multithreaded-threshold*)
                   (,body-sym ,@vars)
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
                               (,body-sym ,@vars)))))))))))))

(defmacro with-thresholded-multithreading/cl (&whole form
                                                threshold-measure (&rest vars) &body body &environment env)
  (declare (ignore threshold-measure vars body env))
  `(with-thresholded-multithreading ,@(rest form)))
