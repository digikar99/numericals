(in-package :numericals.internals)

(define-polymorphic-function two-arg-fn/non-broadcast
    (name x y &key out)
  :overwrite t)

(deftype comparison-operator ()
  `(member cl:< cl:= cl:<= cl:/= cl:>= cl:>
           nu:< nu:= nu:<= nu:/= nu:>= nu:>
           nu:two-arg-<  nu:two-arg-<= nu:two-arg-=
           nu:two-arg-/= nu:two-arg->= nu:two-arg->))
(deftype non-comparison-operator ()
  `(and symbol (not comparison-operator)))

;; single-float - 2+2+2 polymorphs

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array single-float)) (y number)
     &key ((out (array single-float))
           (nu:zeros (array-dimensions x) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svx svo))
      (cffi:with-foreign-pointer (ptr-y 4)
        (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (the-size (* 4 (cl-array-offset x))))
            (cffi:incf-pointer ptr-o (the-size (* 4 (cl-array-offset out))))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array single-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svx)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-y 4)
        (setf (cffi:mem-ref ptr-y :float) (coerce y 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (the-size (* 4 (cl-array-offset x))))
            (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

;;; There are 2x2 of these (above and below) because functions may not be commutative

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x number) (y (array single-float))
     &key ((out (array single-float))
           (nu:zeros (array-dimensions y) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svy)
               (type (cl:simple-array single-float 1) svo))
      (cffi:with-foreign-pointer (ptr-x 4)
        (setf (cffi:mem-ref ptr-x :float) (coerce x 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (the-size (* 4 (cl-array-offset y))))
            (cffi:incf-pointer ptr-o (the-size (* 4 (cl-array-offset out))))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x number) (y (array single-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1)      svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-x 4)
        (setf (cffi:mem-ref ptr-x :float) (coerce x 'single-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (the-size (* 4 (cl-array-offset y))))
            (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
            (funcall single-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)


(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array single-float))
           (nu:zeros (array-dimensions y) :type 'single-float)))
    (array single-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (and (equalp (array-dimensions x)
                               (array-dimensions y))
                       (equalp (array-dimensions y)
                               (array-dimensions out)))
                  (x y out)
                  "Expected X, Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svx svy)
               (type (cl:simple-array single-float 1) svo))
      (with-thresholded-multithreading (array-total-size out)
          (x y out)          
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-y svy)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 4 (cl-array-offset x))))
          (cffi:incf-pointer ptr-y (the-size (* 4 (cl-array-offset y))))
          (cffi:incf-pointer ptr-o (the-size (* 4 (cl-array-offset out))))
          (funcall single-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array single-float)) (y (array single-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (and (equalp (array-dimensions x)
                               (array-dimensions y))
                       (equalp (array-dimensions y)
                               (array-dimensions out)))
                  (x y out)
                  "Expected X, Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((single-float-c-name (single-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array single-float 1) svx svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (with-thresholded-multithreading (array-total-size out)
          (x y out)          
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-y svy)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 4 (cl-array-offset x))))
          (cffi:incf-pointer ptr-y (the-size (* 4 (cl-array-offset y))))
          (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
          (funcall single-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)

;;; double-float - 2+2+2 polymorphs

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array double-float)) (y number)
     &key ((out (array double-float))
           (nu:zeros (array-dimensions x) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svx svo))
      (cffi:with-foreign-pointer (ptr-y 8)
        (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (the-size (* 8 (cl-array-offset x))))
            (cffi:incf-pointer ptr-o (the-size (* 8 (cl-array-offset out))))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array double-float)) (y number)
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions x)
                          (array-dimensions out))
                  (x out)
                  "Expected X and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svx)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-y 8)
        (setf (cffi:mem-ref ptr-y :double) (coerce y 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (x out)          
          (with-pointers-to-vectors-data ((ptr-x svx)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-x (the-size (* 8 (cl-array-offset x))))
            (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 1
                     ptr-y 0
                     ptr-o 1))))))
  out)

;;; There are 2x2 of these (above and below) because functions may not be commutative

(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x number) (y (array double-float))
     &key ((out (array double-float))
           (nu:zeros (array-dimensions y) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svy)
               (type (cl:simple-array double-float 1) svo))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :double) (coerce x 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (the-size (* 8 (cl-array-offset y))))
            (cffi:incf-pointer ptr-o (the-size (* 8 (cl-array-offset out))))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x number) (y (array double-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (array-dimensions y)
                          (array-dimensions out))
                  (y out)
                  "Expected Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1)      svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (cffi:with-foreign-pointer (ptr-x 8)
        (setf (cffi:mem-ref ptr-x :double) (coerce x 'double-float))
        (with-thresholded-multithreading (array-total-size out)
            (y out)          
          (with-pointers-to-vectors-data ((ptr-y svy)
                                          (ptr-o svo))
            (cffi:incf-pointer ptr-y (the-size (* 8 (cl-array-offset y))))
            (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
            (funcall double-float-c-name
                     (array-total-size out)
                     ptr-x 0
                     ptr-y 1
                     ptr-o 1))))))
  out)


(defpolymorph two-arg-fn/non-broadcast
    ((name non-comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array double-float))
           (nu:zeros (array-dimensions y) :type 'double-float)))
    (array double-float)
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (and (equalp (array-dimensions x)
                               (array-dimensions y))
                       (equalp (array-dimensions y)
                               (array-dimensions out)))
                  (x y out)
                  "Expected X, Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svx svy)
               (type (cl:simple-array double-float 1) svo))
      (with-thresholded-multithreading (array-total-size out)
          (x y out)          
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-y svy)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 8 (cl-array-offset x))))
          (cffi:incf-pointer ptr-y (the-size (* 8 (cl-array-offset y))))
          (cffi:incf-pointer ptr-o (the-size (* 8 (cl-array-offset out))))
          (funcall double-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)

(defpolymorph two-arg-fn/non-broadcast
    ((name comparison-operator) (x (array double-float)) (y (array double-float))
     &key ((out (array (unsigned-byte 8)))
           (nu:zeros (array-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (policy-cond:with-expectations (= safety 0)
      ((assertion (and (equalp (array-dimensions x)
                               (array-dimensions y))
                       (equalp (array-dimensions y)
                               (array-dimensions out)))
                  (x y out)
                  "Expected X, Y and OUT to have same dimensions but they are~%  ~S  ~S"
                  (array-dimensions x)
                  (array-dimensions y)
                  (array-dimensions out)))
    (let ((double-float-c-name (double-float-c-name name))
          (svx (array-storage x))
          (svy (array-storage y))
          (svo (array-storage out)))
      (declare (type (cl:simple-array double-float 1) svx svy)
               (type (cl:simple-array (unsigned-byte 8) 1) svo))
      (with-thresholded-multithreading (array-total-size out)
          (x y out)          
        (with-pointers-to-vectors-data ((ptr-x svx)
                                        (ptr-y svy)
                                        (ptr-o svo))
          (cffi:incf-pointer ptr-x (the-size (* 8 (cl-array-offset x))))
          (cffi:incf-pointer ptr-y (the-size (* 8 (cl-array-offset y))))
          (cffi:incf-pointer ptr-o (the-size (* 1 (cl-array-offset out))))
          (funcall double-float-c-name
                   (array-total-size out)
                   ptr-x 1
                   ptr-y 1
                   ptr-o 1)))))
  out)
