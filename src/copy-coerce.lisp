(in-package :numericals.internals)

;;; We are not implementing ASTYPE, because dispatching on the TYPE
;;; requires TYPE= checks; instead we will provide a TRIVIAL-COERCE:COERCE wrapper!

(define-polymorphic-function nu:copy (x &key out))

;;; Should we allow broadcasted copying?
;;; Yeah, because a[:] = b does broadcast
;;; But, how does one broadcast 1x1000 against 1000x1000 parallely without strided arrays?
;;; We avoid lparallel in this case :(

(defpolymorph nu:copy ((x (array single-float)) &key ((out (array single-float))))
    (array single-float)
  (one-arg-fn 'nu:copy x :out out)
  out)

(defpolymorph nu:copy ((x (array double-float)) &key ((out (array double-float))))
    (array double-float)
  (one-arg-fn 'nu:copy x :out out)
  out)

(defpolymorph nu:copy ((x (array single-float)) &key ((out (array double-float))))
    (array double-float)
  (let ((dim-x (array-dimensions x))
        (dim-o (array-dimensions out)))
    (if (equalp dim-x dim-o)
        (let ((svx (array-storage x))
              (svo (array-storage out)))
          (declare (type (cl:simple-array single-float 1) svx)
                   (type (cl:simple-array double-float 1) svo))
          (with-thresholded-multithreading (array-total-size out)
              (x out)          
            (with-pointers-to-vectors-data ((ptr-x svx)
                                            (ptr-o svo))
              (cffi:incf-pointer ptr-x (* 4 (cl-array-offset x)))
              (cffi:incf-pointer ptr-o (* 8 (cl-array-offset out)))
              (bmas:cast-sd (array-total-size out)
                            ptr-x 1
                            ptr-o 1))))
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (%broadcast-compatible-p dim-x dim-o)
          (assert broadcast-compatible-p (x out)
                  'incompatible-broadcast-dimensions
                  :dimensions (list dim-x dim-o)
                  :array-likes (list x out))
          (ptr-iterate-but-inner broadcast-dimensions n
            ((ptr-x 4 ix x)
             (ptr-o 8 io out))
            (bmas:cast-sd n
                          ptr-x ix
                          ptr-o io)))))
  out)

(defpolymorph nu:copy ((x (array double-float)) &key ((out (array single-float))))
    (array single-float)
  (let ((dim-x (array-dimensions x))
        (dim-o (array-dimensions out)))
    (if (equalp dim-x dim-o)
        (let ((svx (array-storage x))
              (svo (array-storage out)))
          (declare (type (cl:simple-array double-float 1) svx)
                   (type (cl:simple-array single-float 1) svo))
          (with-thresholded-multithreading (array-total-size out)
              (x out)          
            (with-pointers-to-vectors-data ((ptr-x svx)
                                            (ptr-o svo))
              (cffi:incf-pointer ptr-x (* 8 (cl-array-offset x)))
              (cffi:incf-pointer ptr-o (* 4 (cl-array-offset out)))
              (bmas:cast-ds (array-total-size out)
                            ptr-x 1
                            ptr-o 1))))
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (%broadcast-compatible-p dim-x dim-o)
          (assert broadcast-compatible-p (x out)
                  'incompatible-broadcast-dimensions
                  :dimensions (list dim-x dim-o)
                  :array-likes (list x out))
          (ptr-iterate-but-inner broadcast-dimensions n
            ((ptr-x 8 ix x)
             (ptr-o 4 io out))
            (bmas:cast-ds n
                          ptr-x ix
                          ptr-o io)))))
  out)

(5am:def-test nu:copy ()
  (5am:is (nu:array= (nu:asarray '(1 2 3) :type 'single-float)
                     (nu:copy (nu:asarray '(1 2 3) :type 'single-float)
                              :out (nu:zeros 3 :type 'double-float))))  
  (5am:is (nu:array= (nu:asarray '(1 2 3) :type 'double-float)
                     (nu:copy (nu:asarray '(1 2 3) :type 'double-float)
                              :out (nu:zeros 3 :type 'single-float))))
  
  (let ((rand (cl:make-array '(500 500)
                             :element-type 'single-float
                             :displaced-to
                             (nu:rand 1000 1000 :type 'single-float)
                             :displaced-index-offset 200000)))
    (5am:is (nu:array= rand
                       (nu:copy rand
                                :out (nu:zeros 500 500 :type 'double-float)))))
  (let ((rand (cl:make-array '(500 500)
                             :element-type 'double-float
                             :displaced-to
                             (nu:rand 1000 1000 :type 'double-float)
                             :displaced-index-offset 200000)))
    (5am:is (nu:array= rand
                       (nu:copy rand
                                :out (nu:zeros 500 500 :type 'single-float))
                       :test (lambda (x y)
                               (< (abs (- x y))
                                  single-float-epsilon))))))

(trivial-coerce:define-coercion (a :from (array single-float) :to (simple-array double-float))
  (nu:copy a :out (the (simple-array double-float)
                       (nu:zeros (array-dimensions a) :type 'double-float))))

(trivial-coerce:define-coercion (a :from (array double-float) :to (simple-array single-float))
  (nu:copy a :out (the (simple-array single-float)
                       (nu:zeros (array-dimensions a) :type 'single-float))))
