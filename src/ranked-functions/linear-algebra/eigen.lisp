(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(defun float-close-p (x y)
  (or (= x y)
      (progn
        ;; (print (list x y))
        (< (/ (abs (- x y))
              (+ (abs x) (abs y)))
           0.01))))

(declaim (inline eigen-array-layout))
(defun eigen-array-layout (array)
  (declare (type simple-array array))
  (ecase (array-layout array)
    (:row-major 82)
    (:column-major 67)))
