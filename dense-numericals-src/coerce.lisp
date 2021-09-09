(in-package :dense-numericals.impl)
(5am:in-suite array)

;;; We are not implementing ASTYPE, because dispatching on the TYPE
;;; requires TYPE= checks; instead we will provide a TRIVIAL-COERCE:COERCE wrapper!

(define-polymorphic-function dn:copy (x &key out))

(defpolymorph dn:copy ((x (array single-float)) &key ((out (array double-float))))
    (array double-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (ptr-iterate-but-inner n ((ptr-x 4 ix x)
                              (ptr-o 8 iout out))
                           (bmas:cast-sd n ptr-x ix ptr-o iout)))
  out)

(defpolymorph dn:copy ((x (array double-float)) &key ((out (array single-float))))
    (array single-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (equalp (narray-dimensions x)
                          (narray-dimensions out))))
    (ptr-iterate-but-inner n ((ptr-x 8 ix x)
                              (ptr-o 4 iout out))
                           (bmas:cast-ds n ptr-x ix ptr-o iout)))
  out)

(5am:def-test dn:copy ()
  (5am:is (array= (asarray '(1 2 3) :type 'single-float)
                  (dn:copy (asarray '(1 2 3) :type 'single-float)
                           :out (zeros 3 :type 'double-float))))
  (5am:is (array= (asarray '(1 2 3) :type 'double-float)
                  (dn:copy (asarray '(1 2 3) :type 'double-float)
                           :out (zeros 3 :type 'single-float))))

  (let ((rand (aref (rand 1000 1000 :type 'single-float)
                    '(0 :step 2) '(0 :step 2))))
    (5am:is (array= rand
                    (dn:copy rand
                             :out (zeros 500 500 :type 'double-float)))))
  (let ((rand (aref (rand 1000 1000 :type 'double-float)
                    '(0 :step 2) '(0 :step 2))))
    (5am:is (array= rand
                    (dn:copy rand
                             :out (zeros 500 500 :type 'single-float))
                    :test (lambda (x y)
                            (< (abs (- x y))
                               single-float-epsilon))))))


(trivial-coerce:define-coercion (a :from (array single-float) :to (array double-float))
  (let ((out (zeros (narray-dimensions a) :type 'double-float)))
    (dn:copy a :out (the (array double-float) out))
    out))

(trivial-coerce:define-coercion (a :from (array double-float) :to (array single-float))
  (let ((out (zeros (narray-dimensions a) :type 'single-float)))
    (dn:copy a :out (the (array single-float) out))
    out))

