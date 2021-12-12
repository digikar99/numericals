(in-package :dense-numericals.impl)

(defun nu:shape (array &optional axis)
  (let ((shape (dense-arrays-plus-lite::dimensions array)))
    (if axis
        (nth axis shape)
        shape)))

