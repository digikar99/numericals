(in-package :dense-numericals.impl)

(defparameter dn:*multithreaded-threshold* 80000)
(declaim (type fixnum dn:*multithreaded-threshold*))

(defmacro with-thresholded-multithreading (threshold-measure (&rest vars) &body body)
  (let* ((simple-p (eq :simple (first vars)))
         (vars     (if simple-p
                       (rest vars)
                       vars)))
    `(block thresholded-multithreading
       (if (< (the size ,threshold-measure)
              dn:*multithreaded-threshold*)
           (progn
             ,@body)
           ;; FIXME: Can the multiple BODY here be reduced?
           (progn
             (let* ((worker-count  (lparallel:kernel-worker-count)))
               (declare (type dense-arrays::size worker-count))
               (multiple-value-bind (long-enough-axis axis/work-size)
                   ,(if simple-p
                        `(values 0 (array-total-size (the array out)))
                        `(loop :for i :of-type size :from 0
                               :for d :of-type size :in (narray-dimensions out)
                               :if (>= d worker-count)
                                 :do (return (values i d))
                               :finally (return-from thresholded-multithreading
                                          (progn ,@body))))
                 (declare (type size long-enough-axis axis/work-size))
                 (let ((max-work-size (ceiling axis/work-size worker-count)))
                   (declare (type size max-work-size))
                   (lparallel:pdotimes (thread-idx worker-count)
                     (declare (type dense-arrays::size thread-idx))
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
                                               (apply #'aref
                                                      ,(if simple-p
                                                           `(reshape ,var
                                                                     (array-total-size
                                                                      (the array ,var))
                                                                     :view t)
                                                           var)
                                                      index))))
                       (declare (type array ,@vars))
                       ,@body))))))))))
