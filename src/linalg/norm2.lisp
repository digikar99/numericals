(in-package :numericals/linalg)

(5am:in-suite :numericals)

(define-polymorphic-function norm2 (array-like) :overwrite t
  :documentation "Calculate the L2 norm of vectors or the frobenius norm of 2D matrix.")

(defpolymorph norm2 ((a (simple-array <type> 2)))
    number
  (let* ((layout (ecase (array-layout a)
                   (:row-major 82)
                   (:column-major 67)))
         (m (array-dimension a 0))
         (n (array-dimension a 1))
         (c-name (c-name <type> 'norm2/matrix)))
    (pflet ((sv (array-storage a)))
      (declare (type (cl:simple-array <type> 1) sv))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name m n ptr layout)))))

(defpolymorph norm2 ((a (simple-array <type> 1)))
    number
  (let* ((m (array-dimension a 0))
         (c-name (c-name <type> 'norm2/vector)))
    (pflet ((sv (array-storage a)))
      (declare (type (cl:simple-array <type> 1) sv))
      (with-pointers-to-vectors-data ((ptr sv))
        (inline-or-funcall c-name m ptr)))))

(defpolymorph (norm2 :inline t) ((a list))
    number
  (norm2 (asarray a :type *default-float-format*)))
