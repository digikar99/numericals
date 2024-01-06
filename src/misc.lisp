(in-package :dense-numericals.impl)

(defpolymorph nu:shape (array &optional ((axis null)))
    list
  (dense-arrays-plus-lite::dimensions array))

(defpolymorph nu:shape (array &optional ((axis integer)))
    integer
  (nth axis (dense-arrays-plus-lite::dimensions array)))

