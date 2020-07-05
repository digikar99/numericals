(in-package :numericals.sbcl)

(defun 1d-storage-array (array)
  (declare (optimize (speed 3))
           (type array array))
  (if (typep array 'simple-array)
      (values (array-storage-vector array) 0)
      (array-displacement array)))
