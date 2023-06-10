(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function la:norm2 (array-like) :overwrite t
  :documentation "Calculate the L2 norm of vectors or the frobenius norm of 2D matrix.")

(defpolymorph la:norm2 ((a (simple-array <type> 2)))
    number
  (let* ((layout (ecase (array-layout a)
                   (:row-major 82)
                   (:column-major 67)))
         (m (nu:array-dimension a 0))
         (n (nu:array-dimension a 1))
         (c-name (c-name <type> 'la::norm2/matrix)))
    (pflet ((sv (array-storage a)))
      (declare (type (cl:simple-array <type> 1) sv))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name m n ptr layout)))))

(defpolymorph la:norm2 ((a (simple-array <type> 1)))
    number
  (let* ((m (nu:array-dimension a 0))
         (c-name (c-name <type> 'la::norm2/vector)))
    (pflet ((sv (array-storage a)))
      (declare (type (cl:simple-array <type> 1) sv))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name m ptr)))))

(defpolymorph (la:norm2 :inline t) ((a list))
    number
  (la:norm2 (nu:asarray a :type nu:*default-float-format*)))
