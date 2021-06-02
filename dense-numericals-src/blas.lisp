(in-package :dense-numericals.impl)

(5am:in-suite array)

(define-polymorphic-function dn:copy (x &key out))

(defpolymorph dn:copy
    ((x (array single-float))  &key
     ((out (array single-float)) (zeros-like x)))
    (array single-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                              (ptr-o 4 iout out))
      (cblas:scopy n ptr-x ix ptr-o iout)))
  out)

(defpolymorph dn:copy
    ((x (array double-float))  &key
     ((out (array double-float)) (zeros-like x)))
    (array double-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                              (ptr-o 8 iout out))
      (cblas:dcopy n ptr-x ix ptr-o iout)))
  out)

;; (let ((a (asarray '((1 2 3))))
;;             (b (asarray '((1) (2) (3))))
;;             (c (zeros 1 1)))
;;         (dn:two-arg-matmul a b :out c))

(define-polymorphic-function dn:two-arg-matmul (a b &key out) :overwrite t)

(macrolet ((def (element-type c-fn)
             `(defpolymorph dn:two-arg-matmul ((a (simple-array ,element-type 2))
                                               (b (simple-array ,element-type 2))
                                               &key ((out (simple-array ,element-type 2))
                                                     (zeros (array-dimension a 0)
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

(5am:def-test dn:two-arg-matmul ()
  (loop :for *array-element-type* :in '(single-float double-float)
        :do
           (5am:is (array= (asarray '((2)))
                           (dn:two-arg-matmul (asarray '((1)))
                                              (asarray '((2)))
                                              :out (zeros 1 1))))
           (5am:is (array= (asarray '((8)))
                           (dn:two-arg-matmul (asarray '((1 2)))
                                              (asarray '((2) (3)))
                                              :out (zeros 1 1))))
           (5am:is (array= (asarray '((2 4)
                                      (3 6)))
                           (dn:two-arg-matmul (asarray '((2) (3)))
                                              (asarray '((1 2)))
                                              :out (zeros 2 2))))
           (5am:is (array= (asarray '((2 4)
                                      (3 6)
                                      (1 2)))
                           (dn:two-arg-matmul (asarray '((2) (3) (1)))
                                              (asarray '((1 2)))
                                              :out (zeros 3 2))))
           (5am:is (array= (asarray '((2 4)
                                      (3 6)
                                      (1 2)))
                           (dn:two-arg-matmul (asarray '((2) (3) (1)))
                                              (asarray '((1 2))))))
           (5am:signals error (dn:two-arg-matmul (asarray '((2) (3) (1)))
                                                 (asarray '((1 2 3)))
                                                 :out (zeros 3 2)))))


(define-polymorphic-function dn:dot (a b &key out) :overwrite t)
(defpolymorph dn:dot ((a (array single-float 1))
                      (b (array single-float 1))
                      &key out)
    single-float
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (let ((sum 0.0f0))
    (ptr-iterate-but-inner n ((ptr-a 4 inc-a a)
                              (ptr-b 4 inc-b b))
      (incf sum (cblas:sdot n
                                  ptr-a inc-a
                                  ptr-b inc-b)))
    sum))

(defpolymorph dn:dot ((a (simple-array single-float 1))
                      (b (simple-array single-float 1))
                      &key out)
    t
  (declare (ignore out))
  ;; TODO: Generalize this to more dimensions
  (with-pointers-to-vectors-data ((ptr-a (array-storage a))
                                  (ptr-b (array-storage b)))
    (cblas:sdot (array-total-size a)
                      ptr-a 1
                      ptr-b 1)))
