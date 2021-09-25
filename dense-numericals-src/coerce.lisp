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

(macrolet ((def (bytes single-signed-cast single-unsigned-cast double-signed-cast double-unsigned-cast)
             `(progn
                (defpolymorph dn:copy ((x (array (signed-byte ,(* 8 bytes))))
                                       &key ((out (array single-float))))
                    (array single-float)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (ptr-iterate-but-inner n ((ptr-x ,bytes ix x)
                                              (ptr-o 4 iout out))
                      (,single-signed-cast n ptr-x ix ptr-o iout)))
                  out)
                (defpolymorph dn:copy ((x (array (unsigned-byte ,(* 8 bytes))))
                                       &key ((out (array single-float))))
                    (array single-float)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (ptr-iterate-but-inner n ((ptr-x ,bytes ix x)
                                              (ptr-o 4 iout out))
                      (,single-unsigned-cast n ptr-x ix ptr-o iout)))
                  out)
                (defpolymorph dn:copy ((x (array (signed-byte ,(* 8 bytes))))
                                       &key ((out (array double-float))))
                    (array double-float)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (ptr-iterate-but-inner n ((ptr-x ,bytes ix x)
                                              (ptr-o 8 iout out))
                      (,double-signed-cast n ptr-x ix ptr-o iout)))
                  out)
                (defpolymorph dn:copy ((x (array (unsigned-byte ,(* 8 bytes))))
                                       &key ((out (array double-float))))
                    (array double-float)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (ptr-iterate-but-inner n ((ptr-x ,bytes ix x)
                                              (ptr-o 8 iout out))
                      (,double-unsigned-cast n ptr-x ix ptr-o iout)))
                  out))))
  (def 1 bmas:cast-i8s  bmas:cast-u8s  bmas:cast-i8d  bmas:cast-u8d)
  (def 2 bmas:cast-i16s bmas:cast-u16s bmas:cast-i16d bmas:cast-u16d)
  (def 4 bmas:cast-i32s bmas:cast-u32s bmas:cast-i32d bmas:cast-u32d)
  (def 8 bmas:cast-i64s bmas:cast-u64s bmas:cast-i64d bmas:cast-u64d))

(macrolet ((def (size type copy-fn)
             `(progn
                (defpolymorph dn:copy
                    ((x (array ,type))  &key
                     ((out (array ,type)) (zeros-like x)))
                    (array ,type)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (ptr-iterate-but-inner n ((ptr-x ,size ix x)
                                              (ptr-o ,size iout out))
                      (,copy-fn n ptr-x ix ptr-o iout)))
                  out)
                (defpolymorph dn:copy
                    ((x (simple-array ,type))  &key
                     ((out (simple-array ,type)) (zeros-like x)))
                    (simple-array ,type)
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                                    (ptr-o (array-storage out)))
                      (,copy-fn (array-total-size x)
                                ptr-x 1
                                ptr-o 1)))
                  out))))

  (def 4 single-float bmas:scopy)
  (def 8 double-float bmas:dcopy)

  (def 1 (signed-byte 8)  bmas:i8copy)
  (def 2 (signed-byte 16) bmas:i16copy)
  (def 4 (signed-byte 32) bmas:i32copy)
  (def 8 (signed-byte 64) bmas:i64copy)

  (def 1 (unsigned-byte 8)  bmas:i8copy)
  (def 2 (unsigned-byte 16) bmas:i16copy)
  (def 4 (unsigned-byte 32) bmas:i32copy)
  (def 8 (unsigned-byte 64) bmas:i64copy))

(trivial-coerce:define-coercion (a :from (array single-float) :to (array double-float))
  (let ((out (zeros (narray-dimensions a) :type 'double-float)))
    (dn:copy a :out (the (array double-float) out))
    out))

(trivial-coerce:define-coercion (a :from (array double-float) :to (array single-float))
  (let ((out (zeros (narray-dimensions a) :type 'single-float)))
    (dn:copy a :out (the (array single-float) out))
    out))

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

(5am:def-suite dn:copy :in array)

(defun test-copy (to-type from-type min max)
  (5am:is (array= (asarray '(1 2 3) :type from-type)
                  (dn:copy (asarray '(1 2 3) :type from-type)
                           :out (zeros 3 :type to-type))))
  (5am:is (array= (asarray '(1 2 3) :type from-type)
                  (dn:copy (asarray '(1 2 3) :type from-type)
                           :out (zeros 3 :type to-type))))

  (let ((rand (aref (rand 1000 1000 :type from-type :min min :max max)
                    '(0 :step 2) '(0 :step 2))))
    (5am:is (array= rand
                    (dn:copy rand
                             :out (zeros 500 500 :type to-type))
                    :test (lambda (x y)
                            (or (= x y)
                                (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                   single-float-epsilon))))))
  (let ((rand (aref (rand 1000 1000 :type from-type :min min :max max)
                    '(0 :step 2) '(0 :step 2))))
    (5am:is (array= rand
                    (dn:copy rand
                             :out (zeros 500 500 :type to-type))
                    :test (lambda (x y)
                            (or (= x y)
                                (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                   single-float-epsilon)))))))

(5am:def-test dn::copy/single-float (:suite dn:copy)
  (test-copy 'single-float 'single-float
             (/ most-negative-single-float 10) (/ most-positive-single-float 10))
  (test-copy 'single-float 'double-float
             (/ most-negative-single-float 10) (/ most-positive-single-float 10))

  (test-copy 'single-float '(signed-byte  8) (- (expt 2 7))  (1- (expt 2 7)))
  (test-copy 'single-float '(signed-byte 16) (- (expt 2 15)) (1- (expt 2 15)))
  (test-copy 'single-float '(signed-byte 32) (- (expt 2 31)) (1- (expt 2 31)))
  (test-copy 'single-float '(signed-byte 64) (- (expt 2 63)) (1- (expt 2 63)))

  (test-copy 'single-float '(unsigned-byte  8) 0 (1- (expt 2 8)))
  (test-copy 'single-float '(unsigned-byte 16) 0 (1- (expt 2 16)))
  (test-copy 'single-float '(unsigned-byte 32) 0 (1- (expt 2 32)))
  (test-copy 'single-float '(unsigned-byte 64) 0 (1- (expt 2 64))))

(5am:def-test dn::copy/double-float (:suite dn:copy)
  (test-copy 'double-float 'single-float
             (/ most-negative-single-float 10) (/ most-positive-single-float 10))
  (test-copy 'double-float 'double-float
             (/ most-negative-single-float 10) (/ most-positive-single-float 10))

  (test-copy 'double-float '(signed-byte  8) (- (expt 2 7))  (1- (expt 2 7)))
  (test-copy 'double-float '(signed-byte 16) (- (expt 2 15)) (1- (expt 2 15)))
  (test-copy 'double-float '(signed-byte 32) (- (expt 2 31)) (1- (expt 2 31)))
  (test-copy 'double-float '(signed-byte 64) (- (expt 2 63)) (1- (expt 2 63)))

  (test-copy 'double-float '(unsigned-byte  8) 0 (1- (expt 2 8)))
  (test-copy 'double-float '(unsigned-byte 16) 0 (1- (expt 2 16)))
  (test-copy 'double-float '(unsigned-byte 32) 0 (1- (expt 2 32)))
  (test-copy 'double-float '(unsigned-byte 64) 0 (1- (expt 2 64))))

(5am:def-test dn::copy/identical (:suite dn:copy)
  (test-copy 'single-float 'single-float
             (/ most-negative-single-float 10) (/ most-positive-single-float 10))
  (test-copy 'double-float 'double-float
             (/ most-negative-double-float 10) (/ most-positive-double-float 10))

  (test-copy '(signed-byte  8) '(signed-byte  8) (- (expt 2 7))  (1- (expt 2 7)))
  (test-copy '(signed-byte 16) '(signed-byte 16) (- (expt 2 15)) (1- (expt 2 15)))
  (test-copy '(signed-byte 32) '(signed-byte 32) (- (expt 2 31)) (1- (expt 2 31)))
  (test-copy '(signed-byte 64) '(signed-byte 64) (- (expt 2 63)) (1- (expt 2 63)))

  (test-copy '(unsigned-byte  8) '(unsigned-byte  8) 0 (1- (expt 2 8)))
  (test-copy '(unsigned-byte 16) '(unsigned-byte 16) 0 (1- (expt 2 16)))
  (test-copy '(unsigned-byte 32) '(unsigned-byte 32) 0 (1- (expt 2 32)))
  (test-copy '(unsigned-byte 64) '(unsigned-byte 64) 0 (1- (expt 2 64))))



