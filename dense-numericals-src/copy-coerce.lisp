(in-package :dense-numericals.impl)

(5am:def-suite array :in :dense-numericals)
(5am:in-suite array)

;;; We are not implementing ASTYPE, because dispatching on the TYPE
;;; requires TYPE= checks; instead we will provide a TRIVIAL-COERCE:COERCE wrapper!

(define-polymorphic-function dn:copy (x &key out) :overwrite t)

;;; We cannot write this as a one-arg-fn function with multiple arguments, because
;;; such a function would then take in arrays with incompatible element-types as
;;; arguments; the entry to one-arg-fn is the primary location where type-checking
;;; takes place; allowing arbitrary element-types would mean bypassing type-safety.
(defmacro define-one-arg-broadcast-polymorph (name c-name (x-size x-type) (o-size o-type))
  `(defpolymorph ,name ((x (array ,x-type)) &key ((out (array ,o-type))))
       (array ,o-type)
     (unless (equalp (narray-dimensions x)
                     (narray-dimensions out))
       (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
           (broadcast-compatible-p x out)
         (assert broadcast-compatible-p (x out)
                 'incompatible-broadcast-dimensions
                 :dimensions (mapcar #'narray-dimensions (list x out))
                 :array-likes (list x out))
         (setq x (broadcast-array x broadcast-dimensions))))
     (with-thresholded-multithreading (array-total-size out)
         (x out)
       (ptr-iterate-but-inner n ((ptr-x   ,x-size ix   x)
                                 (ptr-out ,o-size iout out))
         (,c-name n ptr-x ix ptr-out iout)))
     out))

(define-one-arg-broadcast-polymorph dn:copy bmas:cast-sd (4 single-float) (8 double-float))
(define-one-arg-broadcast-polymorph dn:copy bmas:cast-ds (8 double-float) (4 single-float))

(macrolet ((def (bytes single-signed-cast single-unsigned-cast double-signed-cast double-unsigned-cast)
             `(progn
                (define-one-arg-broadcast-polymorph dn:copy ,single-signed-cast
                    (,bytes (signed-byte ,(* 8 bytes)))
                    (4 single-float))
                (define-one-arg-broadcast-polymorph dn:copy ,single-unsigned-cast
                    (,bytes (unsigned-byte ,(* 8 bytes)))
                    (4 single-float))
                (define-one-arg-broadcast-polymorph dn:copy ,double-signed-cast
                    (,bytes (signed-byte ,(* 8 bytes)))
                    (8 double-float))
                (define-one-arg-broadcast-polymorph dn:copy ,double-unsigned-cast
                    (,bytes (unsigned-byte ,(* 8 bytes)))
                    (8 double-float)))))
  (def 1 bmas:cast-i8s  bmas:cast-u8s  bmas:cast-i8d  bmas:cast-u8d)
  (def 2 bmas:cast-i16s bmas:cast-u16s bmas:cast-i16d bmas:cast-u16d)
  (def 4 bmas:cast-i32s bmas:cast-u32s bmas:cast-i32d bmas:cast-u32d)
  (def 8 bmas:cast-i64s bmas:cast-u64s bmas:cast-i64d bmas:cast-u64d))

(macrolet ((def (size type copy-fn)
             `(define-one-arg-broadcast-polymorph dn:copy ,copy-fn (,size ,type) (,size ,type))))

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

;; Doesn't make sense to broadcast OUT to dimensions of X?
(defpolymorph (dn:copy) ((x (array t)) &key ((out (array single-float))))
    (array single-float)
  (do-arrays ((x-elt (the (array t)
                          (if (equalp (narray-dimensions out)
                                      (narray-dimensions x))
                              x
                              (broadcast-array x (array-dimensions out)))))
              (o-elt out))
    (setf o-elt (trivial-coerce:coerce (the real x-elt) 'single-float)))
  out)

(defpolymorph dn:copy ((x (array t)) &key ((out (array double-float))))
    (array double-float)
  (do-arrays ((x-elt (the (array t)
                          (if (equalp (narray-dimensions out)
                                      (narray-dimensions x))
                              x
                              (broadcast-array x (array-dimensions out)))))
              (o-elt out))
    (setf o-elt (trivial-coerce:coerce (the real x-elt) 'double-float)))
  out)

(defpolymorph dn:copy ((x (array t)) &key ((out (array t)) (zeros-like x)))
    (array t)
  (do-arrays ((x-elt (the (array t)
                          (if (equalp (narray-dimensions out)
                                      (narray-dimensions x))
                              x
                              (broadcast-array x (array-dimensions out)))))
              (o-elt out))
    ;; TODO: Would it be possible to use SIMD to shallow-copy here?
    (setf o-elt x-elt))
  out)

(macrolet ((def (from to)
             `(trivial-coerce:define-coercion (a :from (array ,from) :to (simple-array ,to))
                (let ((out (zeros (narray-dimensions a) :type ',to)))
                  (dn:copy a :out (the (array ,to) out))
                  out))))

  (def single-float double-float)
  (def double-float single-float)

  (def (unsigned-byte 08) single-float)
  (def (unsigned-byte 16) single-float)
  (def (unsigned-byte 32) single-float)
  (def (unsigned-byte 64) single-float)
  (def (signed-byte 08) single-float)
  (def (signed-byte 16) single-float)
  (def (signed-byte 32) single-float)
  (def (signed-byte 64) single-float)

  (def (unsigned-byte 08) double-float)
  (def (unsigned-byte 16) double-float)
  (def (unsigned-byte 32) double-float)
  (def (unsigned-byte 64) double-float)
  (def (signed-byte 08) double-float)
  (def (signed-byte 16) double-float)
  (def (signed-byte 32) double-float)
  (def (signed-byte 64) double-float))

(5am:def-suite dn:copy :in array)

;; TODO: Test broadcasting
(defun test-copy (to-type from-type min max)
  (let ((dn:*multithreaded-threshold* 1000000))
    (5am:is (array= (asarray '(1 2 3) :type from-type)
                    (dn:copy (asarray '(1 2 3) :type from-type)
                             :out (zeros 3 :type to-type)))
            "Simplest case")
    (5am:is (array= (broadcast-array (asarray '(1 2 3) :type from-type) '(3 3))
                    (dn:copy (asarray '(1 2 3) :type from-type)
                             :out (zeros 3 3 :type to-type)))
            "Simplest broadcast")

    (let ((rand (aref (rand 100 100 :type from-type :min min :max max)
                      '(10 :step 2) '(10 :step 2))))
      (5am:is (array= rand
                      (dn:copy rand
                               :out (zeros 45 45 :type to-type))
                      :test (lambda (x y)
                              (or (= x y)
                                  (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                     single-float-epsilon))))
              "Non-simple"))
    (let ((rand (aref (rand 100 100 :type from-type :min min :max max)
                      '(10 :step 2) '(10 :step 2))))
      (5am:is (array= (broadcast-array rand '(10 45 45))
                      (dn:copy rand
                               :out (zeros 10 45 45 :type to-type))
                      :test (lambda (x y)
                              (or (= x y)
                                  (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                     single-float-epsilon))))
              "Non-simple broadcast"))
    (let ((dn:*multithreaded-threshold* 100)
          (rand (aref (rand 100 100 :type from-type :min min :max max)
                      '(10 :step 2) '(10 :step 2))))
      (5am:is (array= (broadcast-array rand '(10 45 45))
                      (dn:copy rand
                               :out (zeros 10 45 45 :type to-type))
                      :test (lambda (x y)
                              (or (= x y)
                                  (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                     single-float-epsilon))))
              "Non-simple multithreaded broadcast"))))

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



