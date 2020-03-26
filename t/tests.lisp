(defpackage :sbcl-numericals.test
  (:use :sbcl-numericals :cl :fiveam :alexandria))
(in-package :sbcl-numericals.test)

(def-suite :sbcl-numericals)

(in-suite :sbcl-numericals)

(defun map-array (function arr-a arr-b arr-c)
  (unless (and (equalp (array-dimensions arr-a) (array-dimensions arr-b))
               (equalp (array-dimensions arr-a) (array-dimensions arr-c)))
    (error "ARR-A, ARR-B and ARR-C must have same dimensions!"))
  (let ((vec-a (sb-ext:array-storage-vector arr-a))
        (vec-b (sb-ext:array-storage-vector arr-b))
        (vec-c (sb-ext:array-storage-vector arr-c)))
    (loop for i below (array-total-size arr-a)
       do (setf (aref vec-c i)
                (funcall function
                         (aref vec-a i)
                         (aref vec-b i)))
       finally (return arr-c))))

(defun array-equal (arr-a arr-b)
  (let ((vec-a (sb-ext:array-storage-vector arr-a))
        (vec-b (sb-ext:array-storage-vector arr-b)))
    (not (when-let (diff (loop for i below (length vec-a)
                            if (/= (aref vec-a i) (aref vec-b i))
                            collect i))
           (let ((*print-pretty* nil))
             (format t "ARR-A and ARR-B differ at position(s) ~D~%" diff))
           t))))

(defmacro with-all-offsets (n element-type
                            arr-a-initial-contents-generator
                            arr-b-initial-contents-generator
                            &body body)
  (let ((base-length 16)
        (axis-0 3))
    `(let (,@(loop for i below n
                for arr-a-sym = (intern (concatenate 'string "ARR-A" (write-to-string i)))
                for arr-b-sym = (intern (concatenate 'string "ARR-B" (write-to-string i)))
                for arr-c-sym = (intern (concatenate 'string "ARR-C" (write-to-string i)))
                for arr-r-sym = (intern (concatenate 'string "ARR-R" (write-to-string i)))
                for len = (+ base-length i)
                collect `(,arr-a-sym
                          (make-array '(,axis-0 ,len)  :element-type ,element-type
                                      :initial-contents
                                      (loop for i below ,axis-0
                                         collect (map 'vector
                                                      ,arr-a-initial-contents-generator
                                                      (iota ,len)))))
                collect `(,arr-b-sym
                          (make-array '(,axis-0 ,len) :element-type ,element-type
                                      :initial-contents
                                      (loop for i below ,axis-0
                                         collect (map 'vector
                                                      ,arr-b-initial-contents-generator
                                                      (iota ,len)))))
                collect `(,arr-c-sym
                          (make-array '(,axis-0 ,len) :element-type ,element-type
                                      :initial-contents
                                      (loop for i below ,axis-0
                                         collect (map 'vector (lambda (i)
                                                                (declare (ignore i))
                                                                (coerce 0 ,element-type))
                                                      (iota ,len)))))
                collect `(,arr-r-sym
                          (make-array '(,axis-0 ,len) :element-type ,element-type
                                      :initial-contents
                                      (loop for i below ,axis-0
                                         collect (map 'vector (lambda (i)
                                                                (declare (ignore i))
                                                                (coerce 0 ,element-type))
                                                      (iota ,len)))))))
       ,@body)))


(defmacro define-sse-double-tests (&body op-double-op-pairs)
  `(progn
     ,@(loop for (op double-op) in op-double-op-pairs
          collect `(test ,double-op
                     (with-all-offsets 2 'double-float
                         (lambda (i) (+ 0.1d0 i)) (lambda (i) (+ 0.2d0 i))
                       (is (array-equal (,double-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,double-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1))))))))
(defmacro define-sse-single-tests (&body op-single-op-pairs)
  `(progn
     ,@(loop for (op single-op) in op-single-op-pairs
          collect `(test ,single-op
                     (with-all-offsets 4 'single-float
                         (lambda (i) (+ 0.1 i)) (lambda (i) (+ 0.2 i))
                       (is (array-equal (,single-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,single-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1)))
                       (is (array-equal (,single-op arr-a2 arr-b2 arr-c2)
                                        (map-array ',op arr-a2 arr-b2 arr-r2)))
                       (is (array-equal (,single-op arr-a3 arr-b3 arr-c3)
                                        (map-array ',op arr-a3 arr-b3 arr-r3))))))))

(define-sse-double-tests
    (+ d2+)
    (- d2-)
  (* d2*)
  (/ d2/))

(define-sse-single-tests
    (+ s2+)
    (- s2-)
  (* s2*)
  (/ s2/))

(defmacro define-avx2-double-tests (&body op-double-op-pairs)
  `(progn
     ,@(loop for (op double-op) in op-double-op-pairs
          collect `(test ,double-op
                     (with-all-offsets 4 'double-float
                         (lambda (i) (+ 0.1d0 i)) (lambda (i) (+ 0.2d0 i))
                       (is (array-equal (,double-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,double-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1)))
                       (is (array-equal (,double-op arr-a2 arr-b2 arr-c2)
                                        (map-array ',op arr-a2 arr-b2 arr-r2)))
                       (is (array-equal (,double-op arr-a3 arr-b3 arr-c3)
                                        (map-array ',op arr-a3 arr-b3 arr-r3))))))))

(define-avx2-double-tests
    (+ d+)
    (- d-)
  (* d*)
  (/ d/))

(defmacro define-avx2-single-tests (&body op-single-op-pairs)
  `(progn
     ,@(loop for (op single-op) in op-single-op-pairs
          collect `(test ,single-op
                     (with-all-offsets 8 'single-float
                         (lambda (i) (+ 0.1 i)) (lambda (i) (+ 0.2 i))
                       (is (array-equal (,single-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,single-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1)))
                       (is (array-equal (,single-op arr-a2 arr-b2 arr-c2)
                                        (map-array ',op arr-a2 arr-b2 arr-r2)))
                       (is (array-equal (,single-op arr-a3 arr-b3 arr-c3)
                                        (map-array ',op arr-a3 arr-b3 arr-r3)))
                       (is (array-equal (,single-op arr-a4 arr-b4 arr-c4)
                                        (map-array ',op arr-a4 arr-b4 arr-r4)))
                       (is (array-equal (,single-op arr-a5 arr-b5 arr-c5)
                                        (map-array ',op arr-a5 arr-b5 arr-r5)))
                       (is (array-equal (,single-op arr-a6 arr-b6 arr-c6)
                                        (map-array ',op arr-a6 arr-b6 arr-r6)))
                       (is (array-equal (,single-op arr-a7 arr-b7 arr-c7)
                                        (map-array ',op arr-a7 arr-b7 arr-r7))))))))

(define-avx2-single-tests
    (+ s+)
    (- s-)
  (* s*)
  (/ s/))

