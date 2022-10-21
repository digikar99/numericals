(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;; (let ((a (nu:asarray '((1 2 3))))
;;             (b (nu:asarray '((1) (2) (3))))
;;             (c (zeros 1 1)))
;;         (dn:two-arg-matmul a b :out c))

(define-polymorphic-function nu:two-arg-matmul (a b &key out) :overwrite t)

(macrolet ((def (element-type c-fn ctype)
             `(defpolymorph nu:two-arg-matmul ((a (simple-array ,element-type 2))
                                               (b (simple-array ,element-type 2))
                                               &key ((out (simple-array ,element-type 2))
                                                     (nu:zeros (array-dimension a 0)
                                                               (array-dimension b 1)
                                                               :type ',element-type
                                                               :layout (array-layout a))))
                  (simple-array ,element-type 2)
                ;; TODO: Generalize this to more dimensions
                (declare (inline ,c-fn))
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
                    (with-pointers-to-vectors-data ((ptr-b (array-storage b))
                                                    (ptr-a (array-storage a))
                                                    (ptr-o (array-storage out)))
                      (let ((m (array-dimension a 0))
                            (k (array-dimension a 1))
                            (n (array-dimension b 1))
                            (col-major-out (eq :column-major (array-layout out))))
                        (multiple-value-bind (m k n a b ptr-a ptr-b)
                            (if col-major-out
                                (values m k n a b ptr-a ptr-b)
                                (values n k m b a ptr-b ptr-a))
                          (with-foreign-objects ((m :int m)
                                                 (k :int k)
                                                 (n :int n)
                                                 (alpha ,ctype (coerce 1 ',element-type))
                                                 (beta  ,ctype (coerce 0 ',element-type)))
                            (,c-fn (blas-trans a (not col-major-out))
                                   (blas-trans b (not col-major-out))
                                   m n k
                                   alpha
                                   ptr-a m
                                   ptr-b k
                                   beta
                                   ptr-o m)))))))
                out)))

  (def single-float magicl.blas-cffi::%%sgemm :float)
  (def double-float magicl.blas-cffi::%%dgemm :double))

(5am:def-test nu:two-arg-matmul ()
  (let ((cl-array-p (string= "NUMERICALS" (package-name (find-package :nu)))))
    (loop :for *array-element-type* :in '(single-float double-float)
          :do (loop :for *array-layout* :in (if cl-array-p
                                                '(:row-major)
                                                '(:row-major :column-major))
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
                       (5am:is (nu:array= (nu:asarray '((-3 0) (-5 2)))
                                          (nu:two-arg-matmul (nu:asarray '((1 2) (3 4)))
                                                             (nu:asarray '((1 2) (-2 -1))))))
                       (5am:signals error (nu:two-arg-matmul (nu:asarray '((2) (3) (1)))
                                                             (nu:asarray '((1 2 3)))
                                                             :out (nu:zeros 3 2))))
              ;; Mixed layout
              (unless cl-array-p
                (5am:is (nu:array= (nu:asarray '((-3 0) (-5 2)) :layout :row-major)
                                   (nu:two-arg-matmul (nu:asarray '((1 2) (3 4))
                                                                  :layout :row-major)
                                                      (nu:asarray '((1 2) (-2 -1))
                                                                  :layout :column-major))))
                (5am:is (nu:array= (nu:asarray '((-3 0) (-5 2)) :layout :column-major)
                                   (nu:two-arg-matmul (nu:asarray '((1 2) (3 4)) :layout :column-major)
                                                      (nu:asarray '((1 2) (-2 -1)) :layout :row-major))))
                (5am:is (nu:array= (nu:asarray '((-3 0) (-5 2)) :layout :column-major)
                                   (nu:two-arg-matmul (nu:asarray '((1 2) (3 4)) :layout :column-major)
                                                      (nu:asarray '((1 2) (-2 -1)) :layout :row-major)
                                                      :out (nu:zeros 2 2 :layout :row-major))))))))


(define-polymorphic-function nu:vdot (a b) :overwrite t
  :documentation "Treat the two input arrays as 1D vectors and calculate their dot product.")

;;; CBLAS is faster than BMAS (obviously)

(macrolet ((def (type c-fn type-size)

             `(defpolymorph nu:vdot ((x (array ,type)) (y (array ,type)))
                  ,type
                (let ((sum ,(coerce 0 type)))
                  (declare (type real sum))
                  (ptr-iterate-but-inner (narray-dimensions x) n
                    ((ptr-x ,type-size inc-x x)
                     (ptr-y ,type-size inc-y y))
                    ,(if (starts-with-subseq "%%" (symbol-name c-fn))
                         `(with-foreign-objects ((n :int n)
                                                 (inc-x :int inc-x)
                                                 (inc-y :int inc-y))
                            (incf sum (,c-fn n ptr-x inc-x ptr-y inc-y)))
                         `(incf sum (,c-fn n ptr-x inc-x ptr-y inc-y))))
                  (nth-value 0 (trivial-coerce:coerce
                                (trivial-coerce:coerce (the real sum) 'integer)
                                ',type))))))

  ;; For verification purposes
  ;; (def single-float     bmas:sdot 4)
  ;; (def double-float     bmas:ddot 8)

  ;; These are faster
  (def single-float     magicl.blas-cffi::%%sdot 4)
  (def double-float     magicl.blas-cffi::%%ddot 8)

  (def (signed-byte 64) bmas:i64dot 8)
  (def (signed-byte 32) bmas:i32dot 4)
  (def (signed-byte 16) bmas:i16dot 2)
  (def (signed-byte 08) bmas:i8dot  1)

  ;; FIXME: Why does this work?
  (def (unsigned-byte 64) bmas:i64dot 8)
  (def (unsigned-byte 32) bmas:i32dot 4)
  (def (unsigned-byte 16) bmas:i16dot 2)
  (def (unsigned-byte 08) bmas:i8dot  1)

  (def fixnum fixnum-dot 8))

(5am:def-test nu:vdot ()
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
                                          (unsigned-byte 08)

                                          fixnum)
          :do
             (5am:is (= 14 (nu:vdot (nu:asarray '(1 2 3))
                                    (nu:asarray '(1 2 3)))))
             (5am:is (= 91 (nu:vdot (nu:asarray '((1 2 3)
                                                  (4 5 6)))
                                    (nu:asarray '((1 2 3)
                                                  (4 5 6))))))
             (let ((rand-1 (nu:rand 100))
                   (rand-2 (nu:rand 100)))
               (5am:is (float-close-p (nu:vdot rand-1 rand-2)
                                      (let ((sum 0))
                                        (nu:do-arrays ((x rand-1)
                                                       (y rand-2))
                                          (incf sum (* x y)))
                                        sum))))
             (let ((rand-1 (nu:aref* (nu:rand 100 1000 :max 100)
                                     '(10 :step 2)
                                     '(100 :step 20)))
                   (rand-2 (nu:aref* (nu:rand 200 2000 :max 2)
                                     '(88 :step -2)
                                     '(200 :step 40))))
               (5am:is (float-close-p (nu:vdot rand-1 rand-2)
                                      (trivial-coerce:coerce
                                       (let ((sum 0))
                                         (nu:do-arrays ((x rand-1)
                                                        (y rand-2))
                                           (incf sum (* x y)))
                                         sum)
                                       *array-element-type*)))))))
