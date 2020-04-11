(in-package :numericals.sbcl)

(defun 1d-storage-array (array)
  (declare (optimize (speed 3))
           (type array array))
  (if (typep array 'simple-array)
      (array-storage-vector array)
      (array-displacement array)))
