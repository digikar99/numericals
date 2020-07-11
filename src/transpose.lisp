(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

;; Numpy uses an array wrapper, so that transposing is merely defining a new wrapper.

;; The same effect has been achieved in "numericals+array" system that uses "numericals/array" under the hood. Use that system for purposes of speed - for larger arrays, it happens to be orders of magnitudes faster, since we are doing zero (almost rather; see the source code NUMERICALS.ARRAY:TRANSPOSE) copying.

;; The below is just about 2x slower than alexandria:copy-array!

;; TODO: Add ability to transpose about "any" axis. (Hint: https://numpy.org/doc/stable/reference/generated/numpy.transpose.html suggests this should just be a matter of permuting the dimensions and strides.)

(defun nu:transpose (array &key (out nil out-supplied-p))
  (declare (optimize speed)
           (type cl:array array))
  (let ((dimensions (nreverse (array-dimensions array))))
    (declare (type list dimensions))
    (if out-supplied-p
        (progn
          (unless (cl:arrayp out) (error "Expected OUT to be a CL:ARRAY but is ~D" out))
          (unless (equalp dimensions (array-dimensions out))
            (error "Expected OUT to be of dimensions ~D but has dimensions ~D"
                   dimensions (array-dimensions out))))
        (setq out (nu:zeros dimensions)))
    (let ((ndim (length (array-dimensions array)))
          (rstrides (coerce (nreverse (strides array))
                            'vector))
          (i (nth-value 1 (1d-storage-array array)))
          (ri 0)
          (storage-array (nth-value 0 (1d-storage-array array)))
          (rstorage-array (1d-storage-array out))
          (rdim (coerce (nreverse (array-dimensions array))
                        'vector))
          (dim-idx 0))
      (declare (special i dim-idx)
               (type uint32 i ri dim-idx ndim))
      (let ((ndim-1 (1- ndim)))
        (declare (type uint32 ndim-1))
        (labels ((%fill ()
                   (if (= (the uint32 dim-idx)
                          (the uint32 ndim))
                       (progn
                         (setf (cl:aref rstorage-array ri)
                               (cl:aref storage-array i))
                         (incf ri)
                         (incf (the uint32 i)
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
                             (incf (the uint32 i)
                                   (the uint32
                                        (cl:aref rstrides
                                                 (the uint32
                                                      (1- (the uint32 dim-idx-1))))))))))))
          (%fill)))
      out)))
