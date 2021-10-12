(in-package :dense-numericals.impl)

(5am:in-suite array)

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


(define-polymorphic-function dn:vdot (a b) :overwrite t
  :documentation "Treat the two input arrays as 1D vectors and calculate their dot product.")

;;; CBLAS is faster than BMAS (obviously)

(macrolet ((def (type c-fn type-size)

             `(defpolymorph dn:vdot ((x (array ,type)) (y (array ,type)))
                  (values ,type &optional)
                (let ((sum ,(coerce 0 type)))
                  (declare (type real sum))
                  (ptr-iterate-but-inner n ((ptr-x ,type-size inc-x x)
                                            (ptr-y ,type-size inc-y y))
                    (incf sum (,c-fn n ptr-x inc-x ptr-y inc-y)))
                  (the ,type (trivial-coerce:coerce
                              (trivial-coerce:coerce sum 'integer)
                              ',type))))))

  ;; For verification purposes
  ;; (def single-float     bmas:sdot 4)
  ;; (def double-float     bmas:ddot 8)

  ;; These are faster
  (def single-float     cblas:sdot 4)
  (def double-float     cblas:ddot 8)

  (def (signed-byte 64) bmas:i64dot 8)
  (def (signed-byte 32) bmas:i32dot 4)
  (def (signed-byte 16) bmas:i16dot 2)
  (def (signed-byte 08) bmas:i8dot  1)

  ;; FIXME: Why does this work?
  (def (unsigned-byte 64) bmas:i64dot 8)
  (def (unsigned-byte 32) bmas:i32dot 4)
  (def (unsigned-byte 16) bmas:i16dot 2)
  (def (unsigned-byte 08) bmas:i8dot  1))

(5am:def-test dn:vdot ()
  (flet ((float-close-p (x y)
           (or (= x y)
               (progn
                 ;; (print (list x y))
                 (< (/ (abs (- x y))
                       (+ (abs x) (abs y)))
                    0.1)))))
    (loop :for *array-element-type* :in `(single-float
                                          double-float

                                          (signed-byte 64)
                                          (signed-byte 32)
                                          (signed-byte 16)
                                          (signed-byte 08)

                                          (unsigned-byte 64)
                                          (unsigned-byte 32)
                                          (unsigned-byte 16)
                                          (unsigned-byte 08))
          :do
             (5am:is (= 14 (dn:vdot (asarray '(1 2 3))
                                    (asarray '(1 2 3)))))
             (5am:is (= 91 (dn:vdot (asarray '((1 2 3)
                                               (4 5 6)))
                                    (asarray '((1 2 3)
                                               (4 5 6))))))
             (let ((rand-1 (dn:rand 100))
                   (rand-2 (dn:rand 100)))
               (5am:is (float-close-p (dn:vdot rand-1 rand-2)
                                      (let ((sum 0))
                                        (do-arrays ((x rand-1)
                                                    (y rand-2))
                                          (incf sum (* x y)))
                                        sum))))
             (let ((rand-1 (aref (dn:rand 100 1000
                                          :max (if (listp *array-element-type*)
                                                   (if (eq 'signed-byte
                                                           (first *array-element-type*))
                                                       (1- (expt 2 (1- (second *array-element-type*))))
                                                       (expt 2 (1- (second *array-element-type*))))
                                                   1)
                                          :min (if (listp *array-element-type*)
                                                   (if (eq 'signed-byte
                                                           (first *array-element-type*))
                                                       (- (expt 2 (1- (second *array-element-type*))))
                                                       0)
                                                   0))
                                 '(10 :step 2)
                                 '(100 :step 20)))
                   (rand-2 (aref (dn:rand 200 2000)
                                 '(88 :step -2)
                                 '(200 :step 40))))
               (5am:is (float-close-p (dn:vdot rand-1 rand-2)
                                      (let ((sum 0))
                                        (do-arrays ((x rand-1)
                                                    (y rand-2))
                                          (incf sum (* x y)))
                                        sum)))))))
