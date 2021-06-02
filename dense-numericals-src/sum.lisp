(in-package :dense-numericals.impl)
(5am:in-suite array)

;;; numpy is still about a factor 2 fast for large arrays, perhaps because
;;; it might not be using dot under the hood
;;; TODO: Check out Bela Pecsek's simd version
;;; TODO: Let DN:SUM take AXES as a LIST
(define-polymorphic-function dn:sum (x &key axes out) :overwrite t)

(defpolymorph (dn:sum :inline t) ((x list) &key axes out) t
  (dn:sum (asarray x) :axes axes :out out))


(macrolet ((def (type c-fn type-size ctype)

             `(progn
                (defpolymorph dn:sum ((x (array ,type))
                                      &key ((axes null)) ((out null)))
                    ,type
                  (declare (ignore axes out))
                  (let ((sum 0.0f0))
                    (cffi:with-foreign-pointer (ones ,type-size)
                      (setf (cffi:mem-aref ones ,ctype)
                            (coerce 1 ',type))
                      (ptr-iterate-but-inner n ((ptr-x ,type-size inc-x x))
                        (incf sum
                              (,c-fn n
                                     ptr-x 1
                                     ones 0))))
                    sum))

                (defpolymorph dn:sum ((x (array ,type 1))
                                      &key ((axes (eql 0))) ((out null)))
                    ,type
                  (declare (ignore axes out))
                  (cffi:with-foreign-pointer (ones ,type-size)
                    (setf (cffi:mem-aref ones ,ctype)
                          (coerce 1 ',type))
                    (cffi:with-pointer-to-vector-data (ptr-x (array-storage x))
                      (,c-fn (array-total-size x)
                             ptr-x 1
                             ones 0))))

                (defpolymorph dn:sum ((x (array ,type))
                                      &key ((axes integer)) ((out (array ,type))))
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

  (def single-float cblas:sdot 4 :float)
  (def double-float cblas:ddot 8 :double))



(5am:def-test dn:sum ()
  (loop :for *array-element-type* :in '(single-float double-float)
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
                                   :out (zeros 2 2))))))

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

