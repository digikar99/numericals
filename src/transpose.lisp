(cl:in-package :numericals.impl)
(5am:in-suite nu::array)

;; Numpy uses an array wrapper, so that transposing is merely defining a new wrapper.

;; The same effect has been achieved in "dense-arrays-plus-lite". Use that system for purposes of speed - for larger arrays, it happens to be orders of magnitudes faster, since we are doing zero (almost rather; see the source code NUMERICALS.ARRAY:TRANSPOSE) copying.

;; The below is just about 2x slower than alexandria:copy-array!

;; TODO: Add ability to transpose about "any" axis. (Hint: https://numpy.org/doc/stable/reference/generated/numpy.transpose.html suggests this should just be a matter of permuting the dimensions and strides.)

(defun nu:transpose (array &key (out nil out-supplied-p) axes)
  ;; Quite a bit slower than TORCH, also thanks to multithreading
  (declare (type cl:array array))
  (let ((ndim (array-rank array)))
    (declare (type uint32 ndim))
    (multiple-value-bind (rstrides rdim rdim-list)
        (let* ((strides    (strides (array-dimensions array)))
               (dimensions (array-dimensions array)))
          (loop :for axis :in (or axes (iota ndim :start (1- ndim) :step -1))
                :for new-stride := (nth axis strides)
                :for new-dim := (nth axis dimensions)
                :collect new-stride :into rstrides
                :collect new-dim :into rdim
                :finally (return (values (coerce rstrides 'vector)
                                         (coerce rdim 'vector)
                                         rdim))))
      (if out-supplied-p
          (progn
            (unless (cl:arrayp out) (error "Expected OUT to be a CL:ARRAY but is ~D" out))
            (unless (equalp rdim-list (array-dimensions out))
              (error "Expected OUT to be of dimensions ~D but has dimensions ~D"
                     rdim-list (array-dimensions out))))
          (setq out (nu:zeros rdim-list :type (array-element-type array))))
      (let* ((storage-array  (array-storage array))
             (rstorage-array (array-storage out))
             (dim-idx 0)
             (i  (cl-array-offset array))
             (ri 0))
        (declare (special i dim-idx)
                 (cl:type uint32 i ri dim-idx)
                 (optimize speed))
        (let ((ndim-1 (1- ndim)))
          (declare (type uint32 ndim-1))
          (labels ((%fill ()
                     (if (= (the uint32 dim-idx)
                            (the uint32 ndim))
                         (progn
                           (setf (cl:aref rstorage-array ri)
                                 (cl:aref storage-array i))
                           (incf ri)
                           (incf i
                               (the uint32 (cl:aref rstrides (the uint32
                                                                  (1- (the uint32 dim-idx)))))))
                         (progn
                           (let ((dim-idx-1 dim-idx)
                                 (dim-idx (1+ (the uint32 dim-idx))))
                             (declare (special dim-idx i)
                                      (type uint32 dim-idx dim-idx-1))
                             (let ((i i))
                               (declare (special i))
                               (loop :for idx :of-type uint32
                                     :from 0 :below (cl:aref rdim dim-idx-1)
                                     :do (list i ri)
                                         (%fill)))
                             (unless (zerop dim-idx-1)
                               (incf i
                                   (the uint32
                                        (cl:aref rstrides
                                                 (the uint32
                                                      (1- (the uint32 dim-idx-1))))))))))))
            (%fill)))
        out))))

(5am:def-test nu:transpose ())
