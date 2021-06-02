(in-package :dense-numericals.impl)

(defun dn:shape (array &optional axis)
  (let ((shape (dense-arrays-plus-lite::dimensions array)))
    (if axis
        (nth axis shape)
        shape)))

;; arbitrary arrays
(defpolymorph (one-arg-fn :inline t) ((name symbol) (x array) &key ((out array))) array
  ;; FIXME: We are assuming OUT is either single-float or double-float
  (if (type= (array-element-type x)
             (array-element-type out))
      (one-arg-fn name x :out out)
      (one-arg-fn name (dn:copy x :out out) :out out))
  out)

