(in-package :dense-numericals.impl)

(defun dn:shape (array &optional axis)
  (let ((shape (dense-arrays-plus-lite::dimensions array)))
    (if axis
        (nth axis shape)
        shape)))

