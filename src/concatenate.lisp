(in-package :numericals.internals)

(defun concatenate-compatible-p (axis &rest arrays-or-dimensions)
  "Returns two values:
  The first value is a generalized boolean indicating whether the arrays can be concatenated.
  The second value is the dimension of the array resulting from the concatenated."
  (case (length arrays-or-dimensions)
    (0 t)
    (1 (values t (ensure-array-dimensions (first arrays-or-dimensions))))
    (t (iter (cond ((every #'null dimension-lists) (terminate))
                   ((some #'null dimension-lists) (return nil)))
             (for dimension-lists initially (mapcar #'ensure-array-dimensions
                                                    arrays-or-dimensions)
                  then (mapcar #'cdr dimension-lists))
             (for ax from 0)
             (for dimension-list = (mapcar #'car dimension-lists))
             (collect (cond ((= ax axis) (apply #'+ dimension-list))
                            ((apply #'= dimension-list) (car dimension-list))
                            (t (return nil)))
               into concatenate-dimensions)
             (finally (return (values t concatenate-dimensions)))))))

;;; With numpy it doesn't seem to matter if or not out is supplied. Here
;;; it does matter very much.
(defun nu:concatenate (&rest args)
  "ARGS is of the form: (&rest args &key out (axis 0) (type *type*))
so that (concatenate a b c :axis 0) is valid."
  (destructuring-bind (args (&key (out nil out-supplied-p) (axis 0) (type *type*)))
      (split-at-keywords args)
    (let* ((*print-array* t)
           (arrays (mapcar (curry #'ensure-array type) args))
           (concatenate-dimensions (nth-value 1
                                              (apply #'concatenate-compatible-p axis
                                                     arrays))))
      (when out-supplied-p
        (assert (arrayp out) nil
                "Cannot supply result in non-array type ~D" out)
        (assert (equalp (array-dimensions out) concatenate-dimensions)))
      (unless concatenate-dimensions
        (error "Cannot concatenate ~D together along axis ~D" args axis))
      (unless out-supplied-p
        (setq out (apply #'nu:zeros (nconc concatenate-dimensions (list :type type)))))
      (apply #'%concatenate axis out arrays))))


;;; Due to the specialization on axis=0, we get speeds at par with numpy for that case.
;;; However, in the general case, we are 20 times as slow! Of course, SIMD can save
;;; up on about factor of 5-8; but a factor of 2-3 still remains!
(defun %concatenate (axis out &rest arrays)
  (declare (optimize (speed 3))
           (type (array single-float) out)
           (type (signed-byte 31) axis))
  (if (zerop axis)
      (apply #'concatenate-axis-0 out arrays)      
      (let ((out-storage-array (1d-storage-array out))
            (out-size (array-total-size out))
            (out-offset-stride (/ (array-total-size out)
                                  (apply #'* (subseq (array-dimensions out) 0 axis)))))
        (declare (type (signed-byte 31) out-size out-offset-stride)
                 (type (simple-array single-float) out-storage-array))
        (flet ((copy-along-axis (initial-idx-offset array) ; there must be a better name!
                 (declare (optimize (speed 3))
                          (type (signed-byte 31) initial-idx-offset)                      
                          (type (array single-float) array))
                 (let* ((storage-array (1d-storage-array array))
                        (size (array-total-size array))
                        (stride (elt (strides array) axis))
                        (offset-stride (/ (array-total-size array)
                                          (apply #'* (subseq (array-dimensions array) 0 axis))))
                        (offset-stride-1 (1- offset-stride))
                        ;; -1 to avoid the (1+ i) calculations below
                        (offset-stride-diff (- out-offset-stride offset-stride)))
                   (declare (type (signed-byte 31) stride size offset-stride-1
                                  offset-stride offset-stride-diff)
                            (type (simple-array single-float) storage-array))
                   ;; (print (vector initial-idx-offset offset-stride))
                   ;; Given an row-major-index i into the out array, we need to determine
                   ;; 1. when to skip indices
                   ;; 2. how much to subtract to get the correct position into the array
                   (loop for oi fixnum from initial-idx-offset below out-size
                      for ai fixnum below size
                      do ;; (print (list oi idx-offset))
                        (setf (aref out-storage-array oi)
                              (aref storage-array ai))
                        (when ;; ((- (1+ oi) idx-offset))
                            (= offset-stride-1 (rem ai offset-stride))
                          (incf oi offset-stride-diff)))
                   offset-stride)))
          ;; (print (list out-axis-size out-stride))
          ;; (handler-case (copy-along-axis 0 (first arrays))
          ;;   (error (c) (format t "~%~D" c)))
          ;; (handler-case (copy-along-axis 6 (second arrays))
          ;;   (error (c) (format t "~%~D" c)))      
          (let ((offset 0))
            (declare (type (signed-byte 31) offset))
            (loop for array in arrays
               do (incf offset (copy-along-axis offset array))))
          out))))

;; TODO: Checking if this and other functions work well for displaced arrays, or if it should
;; be restricted to simple arrays.
(defun concatenate-axis-0 (out &rest arrays)
  (declare (optimize (speed 3))
           (type (array single-float) out))
  (let ((start 0)
        (out-storage-array (1d-storage-array out)))
    (declare (type (simple-array single-float) out-storage-array)
             (type (signed-byte 31) start))
    (loop for array in arrays
       do (multiple-value-bind (storage-array offset)
              (1d-storage-array array)
            (declare (type (simple-array single-float) storage-array))            
            (unless offset (setq offset 0))
            (let* ((array-simd-bound (* +simd-single-1d-aref-stride+
                                        (floor (array-total-size array)
                                               +simd-single-1d-aref-stride+)))
                   (array-bound (+ offset (array-total-size array)))
                   (out-simd-bound (+ start array-simd-bound))
                   (out-bound (+ start (array-total-size array))))
              (declare (type (signed-byte 31) out-bound out-simd-bound
                             array-simd-bound array-bound)
                       (optimize (speed 3)))
              (loop :for idx fixnum :from start
                 :below out-simd-bound :by +simd-single-1d-aref-stride+
                 :for i fixnum :from offset
                 :below array-simd-bound :by +simd-single-1d-aref-stride+
                 :do ;; (print (list i idx 'simd))
                   (setf (simd-single-1d-aref out-storage-array idx)
                         (simd-single-1d-aref storage-array i))
                 finally
                   (loop :for idx :from out-simd-bound :below out-bound
                      :for i :from (max offset array-simd-bound)
                      :below array-bound
                      :do ;; (print (list i idx))
                        (setf (aref out-storage-array idx)
                              (aref storage-array i))))
              (setq start out-bound))))
    out))

;; (defun concatenate-axis-1 (out &rest arrays)
;;   (declare (optimize (speed 3))
;;            (type (array single-float) out))
;;   (let ((start 0)
;;         (out-storage-array (1d-storage-array out)))
;;     (declare (type (simple-array single-float) out-storage-array)
;;              (type (signed-byte 31) start))
;;     (loop for array in arrays
;;        do (let* ((storage-array (1d-storage-array array))
;;                  (length-simd-bound (* +simd-single-1d-aref-stride+
;;                                        (floor (length storage-array)
;;                                               +simd-single-1d-aref-stride+)))
;;                  (end-simd-bound (+ start length-simd-bound))
;;                  (end (+ start (length storage-array))))
;;             (declare (type (simple-array single-float) storage-array)
;;                      (type (signed-byte 31) end)
;;                      (optimize (speed 3)))
;;             (loop for idx fixnum from start
;;                below end-simd-bound by +simd-single-1d-aref-stride+
;;                for i fixnum below length-simd-bound by +simd-single-1d-aref-stride+
;;                do ;; (print (list i idx))
;;                  (setf (simd-single-1d-aref out-storage-array idx)
;;                        (simd-single-1d-aref storage-array i))
;;                finally
;;                  (loop for idx from (+ start length-simd-bound) below end
;;                     for i from length-simd-bound below (length storage-array)
;;                     do ;; (print (list i idx))
;;                       (setf (aref out-storage-array idx)
;;                             (aref storage-array i))))
;;             (setq start end)))
;;     out))

;; (defparameter a (numcl:zeros '(1024 512 2) :type 'single-float))
;; (defparameter b (numcl:zeros '(1024 512 2) :type 'single-float))
;; (defparameter c (numcl:zeros '(2048 512 2) :type 'single-float))
;; (defparameter d (numcl:zeros '(1024 512 4) :type 'single-float))

;; (defparameter a (nu:zeros 1024 512 2))
;; (defparameter b (nu:zeros 1024 512 2))
;; (defparameter c (nu:zeros 2048 512))
