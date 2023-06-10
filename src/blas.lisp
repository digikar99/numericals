(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;; (let ((a (nu:asarray '((1 2 3))))
;;             (b (nu:asarray '((1) (2) (3))))
;;             (c (zeros 1 1)))
;;         (dn:two-arg-matmul a b :out c))

(define-polymorphic-function nu:vdot (a b) :overwrite t
  :documentation "Treat the two input arrays as 1D vectors and calculate their dot product.")

;;; CBLAS is faster than BMAS (obviously)

(defpolymorph nu:vdot ((x (array <type>)) (y (array <type>))) t
  (let ((sum (coerce 0 <type>))
        (c-size (c-size <type>))
        (c-name (c-name <type> 'nu:vdot)))
    (declare (type real sum))
    (ptr-iterate-but-inner (narray-dimensions x) n
      ((ptr-x c-size inc-x x)
       (ptr-y c-size inc-y y))
      (incf sum (funcall c-name n ptr-x inc-x ptr-y inc-y)))
    (nth-value 0
               (nu:coerce (the real sum) <type>))))

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
                                      (nu:coerce
                                       (let ((sum 0))
                                         (nu:do-arrays ((x rand-1)
                                                        (y rand-2))
                                           (incf sum (* x y)))
                                         sum)
                                       *array-element-type*)))))))
