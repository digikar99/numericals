(in-package :numericals.internals)

;; (let ((a (asarray '((1 2 3))))
;;             (b (asarray '((1) (2) (3))))
;;             (c (zeros 1 1)))
;;         (nu:two-arg-matmul a b :out c))

(define-polymorphic-function nu:two-arg-matmul (a b &key out) :overwrite t)

(macrolet ((def (element-type c-fn)
             `(defpolymorph nu:two-arg-matmul ((a (array ,element-type 2))
                                               (b (array ,element-type 2))
                                               &key ((out (array ,element-type 2))
                                                     (nu:zeros (array-dimension a 0)
                                                               (array-dimension b 1)
                                                               :type ',element-type)))
                  (simple-array ,element-type 2)
                ;; TODO: Generalize this to more dimensions
                (flet ((matmul-compatible-arrays (a b out)
                         (let ((a0 (array-dimension a 0))
                               (a1 (array-dimension a 1))
                               (b0 (array-dimension b 0))
                               (b1 (array-dimension b 1))
                               (o0 (array-dimension out 0))
                               (o1 (array-dimension out 1)))
                           (and (= a0 o0) (= a1 b0) (= b1 o1)))))
                  (policy-cond:with-expectations (= 0 safety)
                      ((assertion (matmul-compatible-arrays a b out) (a b out)))
                    ;; We do C^T = (B^T A^T) - since we are unable
                    ;; to obtain the result with cblas:+row-major+ :/
                    (with-pointers-to-vectors-data ((ptr-b (array-storage b))
                                                    (ptr-a (array-storage a))
                                                    (ptr-o (array-storage out)))
                      ,@(let ((elt-size (ecase element-type
                                          (single-float 4)
                                          (double-float 8))))
                          `((cffi:incf-pointer ptr-b (* ,elt-size (cl-array-offset b)))
                            (cffi:incf-pointer ptr-a (* ,elt-size (cl-array-offset a)))
                            (cffi:incf-pointer ptr-o (* ,elt-size (cl-array-offset out)))))
                      (let ((m (array-dimension b 1))
                            (k (array-dimension b 0))
                            (n (array-dimension a 0)))
                        (,c-fn cblas:+col-major+
                               cblas:+no-trans+
                               cblas:+no-trans+
                               m n k
                               (coerce 1 ',element-type)
                               ptr-b m
                               ptr-a k
                               (coerce 0 ',element-type)
                               ptr-o m)))))
                out)))

  (def single-float cblas:sgemm)
  (def double-float cblas:dgemm))

(5am:def-test nu:two-arg-matmul ()
  (loop :for nu:*array-element-type* :in '(single-float double-float)
        :do
           (5am:is (nu:array= (nu:asarray '((2)))
                              (nu:two-arg-matmul (nu:asarray '((1)))
                                                 (nu:asarray '((2)))
                                                 :out (nu:zeros 1 1))))
           (5am:is (nu:array= (nu:asarray '((8)))
                              (nu:two-arg-matmul (nu:asarray '((1 2)))
                                                 (nu:asarray '((2) (3)))
                                                 :out (nu:zeros 1 1))))
           (5am:is (nu:array= (nu:asarray '((2 4)
                                            (3 6)))
                              (nu:two-arg-matmul (nu:asarray '((2) (3)))
                                                 (nu:asarray '((1 2)))
                                                 :out (nu:zeros 2 2))))
           (5am:is (nu:array= (nu:asarray '((2 4)
                                            (3 6)
                                            (1 2)))
                              (nu:two-arg-matmul (nu:asarray '((2) (3) (1)))
                                                 (nu:asarray '((1 2)))
                                                 :out (nu:zeros 3 2))))
           (5am:is (nu:array= (nu:asarray '((2 4)
                                            (3 6)
                                            (1 2)))
                              (nu:two-arg-matmul (nu:asarray '((2) (3) (1)))
                                                 (nu:asarray '((1 2))))))
           (5am:signals error (nu:two-arg-matmul (nu:asarray '((2) (3) (1)))
                                                 (nu:asarray '((1 2 3)))
                                                 :out (nu:zeros 3 2)))))


(define-polymorphic-function nu:dot (a b &key out) :overwrite t)
(defpolymorph nu:dot ((a (array single-float 1))
                      (b (array single-float 1))
                      &key out)
    single-float
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (with-pointers-to-vectors-data ((ptr-a (array-storage a))
                                  (ptr-b (array-storage b)))
    (cffi:incf-pointer ptr-a (* 4 (cl-array-offset a)))
    (cffi:incf-pointer ptr-b (* 4 (cl-array-offset b)))
    (cblas:sdot (array-total-size a)
                      ptr-a 1
                      ptr-b 1)))

(defpolymorph nu:dot ((a (array double-float 1))
                      (b (array double-float 1))
                      &key out)
    double-float
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (with-pointers-to-vectors-data ((ptr-a (array-storage a))
                                  (ptr-b (array-storage b)))
    (cffi:incf-pointer ptr-a (* 8 (cl-array-offset a)))
    (cffi:incf-pointer ptr-b (* 8 (cl-array-offset b)))
    (cblas:ddot (array-total-size a)
                ptr-a 1
                ptr-b 1)))
