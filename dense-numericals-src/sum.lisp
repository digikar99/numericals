(in-package :dense-numericals.impl)
(5am:in-suite array)

;;; numpy is still about a factor 2 fast for large arrays, perhaps because
;;; it might not be using dot under the hood
;;; TODO: Check out Bela Pecsek's simd version
;;; TODO: Let DN:SUM take AXES as a LIST
(define-polymorphic-function dn:sum (x &key axes out) :overwrite t)

(defpolymorph (dn:sum :inline t) ((x list) &key axes out) t
  (dn:sum (asarray x) :axes axes :out out))


(macrolet ((def (type c-fn type-size)

             `(progn
                (defpolymorph dn:sum ((x (array ,type))
                                      &key ((axes null)) ((out null)))
                    (values ,type &optional)
                  (declare (ignore axes out))
                  (let ((sum ,(coerce 0 type)))
                    (declare (type real sum))
                    (ptr-iterate-but-inner n ((ptr-x ,type-size inc-x x))
                      (incf sum (,c-fn n ptr-x inc-x)))
                    (the ,type (trivial-coerce:coerce
                                (trivial-coerce:coerce sum 'integer)
                                ',type))))
                (defpolymorph dn:sum ((x (array ,type 1))
                                      &key ((axes (eql 0))) ((out null)))
                    (values ,type &optional)
                  (declare (ignore axes out))
                  (let ((sum
                          (with-pointers-to-vectors-data ((ptr-x (array-storage x)))
                            (let ((inc-x (array-stride x 0)))
                              (,c-fn (array-total-size x)
                                     ptr-x inc-x)))))
                    (trivial-coerce:coerce sum ',type)))
                (defpolymorph dn:sum ((x (array ,type))
                                      &key ((axes integer))
                                      ((out (array ,type))
                                       (zeros (let ((dim (narray-dimensions x)))
                                                (append (subseq dim 0 axes)
                                                        (nthcdr (+ 1 axes) dim)))
                                              :type ',type)))
                    (array ,type)
                  ;; We obtain a slice of X into COPY-X
                  ;; This slice has a rank one less than X
                  ;; We reuse the same COPY-X by increasing the initial offset
                  ;; And collect this sum into OUT
                  (let ((copy-x (apply #'aref x (loop :for i :below (array-rank x)
                                                      :if (= i axes)
                                                        :collect 0
                                                      :else
                                                        :collect nil)))
                        (stride (array-stride x axes)))
                    (declare (type (array ,type) copy-x))
                    (loop :for i :below (array-dimension x axes)
                          :do (dn:two-arg-+ out copy-x :out out)
                              (incf (first (array-offsets copy-x))
                                    stride)))
                  out))))

  (def single-float     bmas:ssum 4)
  (def double-float     bmas:dsum 8)
  (def (signed-byte 64) bmas:i64sum 8)
  (def (signed-byte 32) bmas:i32sum 4)
  (def (signed-byte 16) bmas:i16sum 2)
  (def (signed-byte 08) bmas:i8sum  1)
  (def (unsigned-byte 64) bmas:i64sum 8)
  (def (unsigned-byte 32) bmas:i32sum 4)
  (def (unsigned-byte 16) bmas:i16sum 2)
  (def (unsigned-byte 08) bmas:i8sum  1))

(5am:def-test dn:sum ()
  (flet ((float-close-p (x y)
           (or (= x y)
               (progn
                 ;; (print (list x y))
                 (< (/ (abs (- x y))
                       (+ (abs x) (abs y)))
                    0.01)))))
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
             (5am:is (= 06 (dn:sum (asarray '(1 2 3)))))
             (5am:is (= 21 (dn:sum (asarray '((1 2 3)
                                              (4 5 6))))))
             (5am:is (array= (asarray '(5 7 9))
                             (dn:sum (asarray '((1 2 3)
                                                (4 5 6)))
                                     :axes 0
                                     :out (zeros 3))))
             (5am:is (array= (asarray '(6 15))
                             (dn:sum (asarray '((1 2 3)
                                                (4 5 6)))
                                     :axes 1
                                     :out (zeros 2))))
             (5am:is (array= (asarray '((5 7 9)
                                        (7 9 11)))
                             (dn:sum (asarray '(((1 2 3)
                                                 (4 5 6))
                                                ((7 8 9)
                                                 (0 1 2))))
                                     :axes 1
                                     :out (zeros 2 3))))
             (5am:is (array= (asarray '((6 15)
                                        (24 3)))
                             (dn:sum (asarray '(((1 2 3)
                                                 (4 5 6))
                                                ((7 8 9)
                                                 (0 1 2))))
                                     :axes 2
                                     :out (zeros 2 2))))

             (let ((array (dn:rand 100)))
               (5am:is (float-close-p (dn:sum array)
                                      (let ((sum 0))
                                        (do-arrays ((x array))
                                          (incf sum x))
                                        sum)))))))

;; ;;; The perf diff between simple and non-simple versions is less than 5%
;; ;;; for (RAND 1000 1000); may be we could delete this (?)
;; (defpolymorph dn:sum ((x (simple-array single-float))
;;                       &key ((axes null)) ((out null)))
;;     single-float
;;   (declare (ignore axes out))
;;   (cffi:with-foreign-pointer (ones 4)
;;     (setf (cffi:mem-aref ones :float) 1.0f0)
;;     (cblas:sdot (array-total-size x)
;;                          (ptr x) 1
;;                          ones 0)))

