(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;;; TODO: Let NU:SUM take AXES as a LIST
(define-polymorphic-function one-arg-reduce-fn (name initial-value-name x &key axes out) :overwrite t)

(defpolymorph (one-arg-reduce-fn :inline t) ((name symbol) (initial-value-name symbol)
                                             (x list) &key axes ((out (not null))))
    t
  (one-arg-reduce-fn name initial-value-name (print (nu:asarray x)) :axes axes :out out))

(defpolymorph (one-arg-reduce-fn :inline t) ((name symbol) (initial-value-name symbol)
                                             (x list) &key axes ((out null)))
    t
  (declare (ignore out))
  (one-arg-reduce-fn name initial-value-name (print (nu:asarray x)) :axes axes))

(macrolet ((def (type c-fn-retriever type-size)

             `(progn
                (defpolymorph one-arg-reduce-fn ((name symbol)
                                                 (initial-value-name symbol)
                                                 (x (array ,type))
                                                 &key ((axes null)) ((out null)))
                    (values ,type &optional)
                  (declare (ignore axes out)
                           (ignorable name))
                  (let ((acc             (funcall initial-value-name ',type))
                        (cl-name         (cl-name name))
                        (,c-fn-retriever (,c-fn-retriever name)))
                    (declare (type real acc))
                    (ptr-iterate-but-inner (narray-dimensions x) n
                      ((ptr-x ,type-size inc-x x))
                      (setq acc (funcall cl-name
                                         acc
                                         (funcall ,c-fn-retriever n ptr-x inc-x))))
                    (if (typep acc ',type)
                        acc
                        (trivial-coerce:coerce acc ',type))))
                (defpolymorph one-arg-reduce-fn ((name symbol)
                                                 (initial-value-name symbol)
                                                 (x (array ,type 1))
                                                 &key ((axes (eql 0))) ((out null)))
                    (values ,type &optional)
                  (declare (ignore axes out initial-value-name)
                           (ignorable name))
                  (let ((acc
                          (with-pointers-to-vectors-data ((ptr-x (array-storage x)))
                            (let ((inc-x (array-stride x 0)))
                              (funcall (,c-fn-retriever name) (array-total-size x)
                                       ptr-x inc-x)))))
                    (if (typep acc ',type)
                        acc
                        (trivial-coerce:coerce acc ',type))))
                ;; (defpolymorph one-arg-reduce-fn ((x (array ,type))
                ;;                       &key ((axes integer))
                ;;                       ((out (array ,type))
                ;;                        (nu:zeros (let ((dim (narray-dimensions x)))
                ;;                                 (append (subseq dim 0 axes)
                ;;                                         (nthcdr (+ 1 axes) dim)))
                ;;                               :type ',type)))
                ;;     (array ,type)
                ;;   ;; We obtain a slice of X into COPY-X
                ;;   ;; This slice has a rank one less than X
                ;;   ;; We reuse the same COPY-X by increasing the initial offset
                ;;   ;; And collect this sum into OUT
                ;;   (let ((copy-x (apply #'aref x (loop :for i :below (array-rank x)
                ;;                                       :if (= i axes)
                ;;                                         :collect 0
                ;;                                       :else
                ;;                                         :collect nil)))
                ;;         (stride (array-stride x axes)))
                ;;     (declare (type (array ,type) copy-x))
                ;;     (loop :for i :below (array-dimension x axes)
                ;;           :do (nu:two-arg-+ out copy-x :out out)
                ;;               (incf (first (array-offsets copy-x))
                ;;                     stride)))
                ;;   out)
                )))

  (def single-float     single-float-c-name 4)
  (def double-float     double-float-c-name 8)
  (def (signed-byte 64) int64-c-name 8)
  (def (signed-byte 32) int32-c-name 4)
  (def (signed-byte 16) int16-c-name 2)
  (def (signed-byte 08) int8-c-name  1)
  (def (unsigned-byte 64) uint64-c-name 8)
  (def (unsigned-byte 32) uint32-c-name 4)
  (def (unsigned-byte 16) uint16-c-name 2)
  (def (unsigned-byte 08) uint8-c-name  1)
  (def fixnum           fixnum-c-name 8))

(macrolet ((def (name initial-value-name)
             `(progn
                (define-polymorphic-function ,name (x &key axes out) :overwrite t)
                (defpolymorph ,name (x &key axes ((out (not null)))) t
                  (one-arg-reduce-fn ',name ',initial-value-name x :axes axes :out out))
                (defpolymorph ,name (x &key axes ((out null))) t
                  (declare (ignore out))
                  (one-arg-reduce-fn ',name ',initial-value-name x :axes axes)))))
  (def nu:sum type-zero)
  (def nu:max type-min)
  (def nu:min type-max))

(5am:def-test nu:sum ()
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
                                          (unsigned-byte 08)
                                          fixnum)
          :do
             (5am:is (= 06 (nu:sum (nu:asarray '(1 2 3)))))
             (5am:is (= 21 (nu:sum (nu:asarray '((1 2 3)
                                                 (4 5 6))))))
             ;; (5am:is (array= (nu:asarray '(5 7 9))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 0
             ;;                         :out (nu:zeros 3))))
             ;; (5am:is (array= (nu:asarray '(6 15))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2))))
             ;; (5am:is (array= (nu:asarray '((5 7 9)
             ;;                            (7 9 11)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2 3))))
             ;; (5am:is (array= (nu:asarray '((6 15)
             ;;                            (24 3)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 2
             ;;                         :out (nu:zeros 2 2))))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (nu:sum array)
                                      (let ((sum 0))
                                        (nu:do-arrays ((x array))
                                          (incf sum x))
                                        sum)))))))

(5am:def-test nu:max ()
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
                                          (unsigned-byte 08)
                                          fixnum)
          :do
             (5am:is (= 3 (nu:max (nu:asarray '(1 2 3)))))
             (5am:is (= 9 (nu:max (nu:asarray '((1 2 3)
                                                (4 5 6)
                                                (7 8 9))))))
             ;; (5am:is (array= (nu:asarray '(5 7 9))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 0
             ;;                         :out (nu:zeros 3))))
             ;; (5am:is (array= (nu:asarray '(6 15))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2))))
             ;; (5am:is (array= (nu:asarray '((5 7 9)
             ;;                            (7 9 11)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2 3))))
             ;; (5am:is (array= (nu:asarray '((6 15)
             ;;                            (24 3)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 2
             ;;                         :out (nu:zeros 2 2))))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (nu:max array)
                                      (let ((max most-negative-double-float))
                                        (nu:do-arrays ((x array))
                                          (if (> x max) (setq max x)))
                                        max)))))))

(5am:def-test nu:min ()
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
                                          (unsigned-byte 08)
                                          fixnum)
          :do
             (5am:is (= 1 (nu:min (nu:asarray '(1 2 3)))))
             (5am:is (= 1 (nu:min (nu:asarray '((1 2 3)
                                                (4 5 6))))))
             ;; (5am:is (array= (nu:asarray '(5 7 9))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 0
             ;;                         :out (nu:zeros 3))))
             ;; (5am:is (array= (nu:asarray '(6 15))
             ;;                 (nu:sum (nu:asarray '((1 2 3)
             ;;                                    (4 5 6)))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2))))
             ;; (5am:is (array= (nu:asarray '((5 7 9)
             ;;                            (7 9 11)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 1
             ;;                         :out (nu:zeros 2 3))))
             ;; (5am:is (array= (nu:asarray '((6 15)
             ;;                            (24 3)))
             ;;                 (nu:sum (nu:asarray '(((1 2 3)
             ;;                                     (4 5 6))
             ;;                                    ((7 8 9)
             ;;                                     (0 1 2))))
             ;;                         :axes 2
             ;;                         :out (nu:zeros 2 2))))

             (let ((array (nu:rand 100)))
               (5am:is (float-close-p (nu:min array)
                                      (let ((min most-positive-double-float))
                                        (nu:do-arrays ((x array))
                                          (if (< x min) (setq min x)))
                                        min)))))))

;; ;;; The perf diff between simple and non-simple versions is less than 5%
;; ;;; for (RAND 1000 1000); may be we could delete this (?)
;; (defpolymorph nu:sum ((x (simple-array single-float))
;;                       &key ((axes null)) ((out null)))
;;     single-float
;;   (declare (ignore axes out))
;;   (cffi:with-foreign-pointer (ones 4)
;;     (setf (cffi:mem-aref ones :float) 1.0f0)
;;     (cblas:sdot (array-total-size x)
;;                          (ptr x) 1
;;                          ones 0)))

