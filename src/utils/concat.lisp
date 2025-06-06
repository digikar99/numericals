(in-package :numericals/utils/impl)

(5am:in-suite :numericals)

(define-polymorphic-function concat (array-likes &key out (axis 0)) :overwrite t)

(defun out-shape-for-concat (axis array-likes)
  (let* ((out-rank
           (loop :for array-like :in array-likes
                 :with rank :of-type (or null (integer 0 #.array-rank-limit)) := nil
                 :do (let ((array-rank (array-rank array-like)))
                       (if (null rank)
                           (setq rank array-rank)
                           (when (cl:/= array-rank rank)
                             (error "Arrays of different ranks cannot be concatenated"))))
                 :finally (return rank)))
         (out-shape (make-list out-rank :initial-element nil)))
    (unless (cl:< axis out-rank)
      (error "Axis ~A out of range for arrays of rank ~A"
             axis out-rank))
    (dolist (array-like array-likes)
      (loop :for array-axis :below out-rank
            :for dim := (array-dimension array-like array-axis)
            :do (let ((out-dim (nth array-axis out-shape)))
                  (cond ((null out-dim)
                         (setf (nth array-axis out-shape) dim))
                        ((cl:= axis array-axis)
                         (setf (nth array-axis out-shape)
                               (+ dim out-dim)))
                        (t
                         (when (cl:/= out-dim dim)
                           (error "Arrays differ on an axis ~A different from concatenation axis ~A" array-axis axis)))))))
    out-shape))

(defpolymorph (concat :inline t) (array-likes
                                  &key ((out (simple-array <type>)))
                                  ((axis (integer 0 #.array-rank-limit))))
    (simple-array <type>)
  (declare (optimize speed))
  ;; TODO: error recover restart for out of range AXIS
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (let ((out-shape (out-shape-for-concat axis array-likes)))
                    (equal out-shape (narray-dimensions out)))
                  (out)))
    (let* ((array-rank (array-rank out))
           (op-rank    (- array-rank axis))
           (c-size     (c-size <type>)))
      (declare (type size c-size))
      (cl:let ((out-offset   0)
               (last-in-size 0))
        (declare (cl:type size out-offset last-in-size))
        (flet ((concat (in-size in-ptr in-dims out-ptr out-dims)
                 (declare (ignorable in-dims out-dims)
                          (type cffi:foreign-pointer in-ptr out-ptr)
                          (type size in-size))
                 ;; (print (list in-size in-ptr in-dims out-ptr out-dims))
                 (setq last-in-size in-size)
                 (cffi:foreign-funcall "memcpy"
                                       :pointer (cffi:inc-pointer out-ptr out-offset)
                                       :pointer in-ptr
                                       :uint (* c-size in-size))))
          (dolist (array-like array-likes)
            (pflet ((array-like array-like))
              (declare (type (simple-array <type>) array-like))
              (with-simple-array-broadcast (concat op-rank op-rank)
                (array-like c-size) (out c-size))
              (setq out-offset (+ out-offset (the-size (* c-size last-in-size)))))))))
    out))

(defpolymorph (concat :inline t) (array-likes &key ((out null)) ((axis integer)))
    t
  (declare (ignore out))
  (let ((array-likes (loop :for array :in array-likes
                           :collect (if (typep array 'array)
                                        array
                                        (asarray array)))))
    ;; TODO: error recover restart for out of range AXIS
    (concat array-likes
            :out (zeros (out-shape-for-concat axis array-likes)
                        :type (array-element-type (first array-likes)))
            :axis axis)))

(5am:def-test concat ()
  (dolist (*array-element-type* '(single-float
                                  double-float
                                  (signed-byte 64)
                                  (signed-byte 32)
                                  (signed-byte 16)
                                  (signed-byte 8)
                                  (unsigned-byte 64)
                                  (unsigned-byte 32)
                                  (unsigned-byte 16)
                                  (unsigned-byte 8)
                                  fixnum))
    (5am:is (array= (asarray '(1 2 3 4 5 6))
                    (concat (list '(1 2 3) '(4 5 6))
                            :axis 0)))
    (5am:is (array= (asarray '((1 2 3)
                               (4 5 6)))
                    (concat (list '((1 2 3))
                                  '((4 5 6)))
                            :axis 0)))
    (5am:is (array= (asarray '((1 2 3 4 5 6)))
                    (concat (list '((1 2 3))
                                  '((4 5 6)))
                            :axis 1)))
    (5am:is (array= (asarray '(((1 2 3) (4 5 6))))
                    (concat (list '(((1 2 3)))
                                  '(((4 5 6))))
                            :axis 1)))))
