(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;;; TODO: Implement methods of copying from CL:ARRAY to DENSE-ARRAYS:ARRAY and vice-versa
;;; TODO: Provide a fast (setf aref)

(define-polymorphic-function nu:copy (x &key out broadcast) :overwrite t)

;;; We are not implementing ASTYPE directly, because dispatching on the TYPE
;;; requires TYPE= checks; instead we will provide a TRIVIAL-COERCE:COERCE wrapper!

(define-polymorphic-function nu:astype (array type) :overwrite t)
(defpolymorph nu:astype ((array array) type) (values array &optional)
  (nu:copy array :out (nu:zeros (array-dimensions array) :type type)))
(defpolymorph-compiler-macro nu:astype (array t) (&whole form array type &environment env)
  (if (constantp type env)
      (with-gensyms (array-sym)
        (let ((type (constant-form-value type env)))
          `(let ((,array-sym ,array))
             (declare (type ,(cl-form-types:nth-form-type array env 0) ,array-sym))
             (nu:copy ,array-sym :out (the ,type (nu:zeros (array-dimensions ,array-sym) :type ',type))))))
      form))

;;; We cannot write this as a one-arg-fn function with multiple arguments, because
;;; such a function would then take in arrays with incompatible element-types as
;;; arguments; the entry to one-arg-fn is the primary location where type-checking
;;; takes place; allowing arbitrary element-types would mean bypassing type-safety.
(defmacro define-one-arg-broadcast-polymorph (name c-name (x-size x-type) (o-size o-type))
  `(progn
     (defpolymorph ,name ((x (array ,x-type))
                          &key ((out (array ,o-type)) ,@(when (type= x-type o-type)
                                                          `((nu:zeros-like x))))
                          (broadcast nu:*broadcast-automatically*))
         (array ,o-type)
       (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
           (broadcast-compatible-p x out)
         (assert broadcast-compatible-p (x out)
                 'incompatible-broadcast-dimensions
                 :dimensions (mapcar #'narray-dimensions (list x out))
                 :array-likes (list x out))
         (policy-cond:with-expectations (= safety 0)
             ((assertion (or broadcast
                             (equalp (narray-dimensions x)
                                     (narray-dimensions out)))))
           (ptr-iterate-but-inner broadcast-dimensions n
             ((ptr-x   ,x-size ix   x)
              (ptr-out ,o-size iout out))
             (,c-name n ptr-x ix ptr-out iout))))
       out)
     ,(when (type= x-type o-type)
        `(defpolymorph ,name ((x real)
                              &key ((out (array ,o-type)))
                              ((broadcast (not null)) nu:*broadcast-automatically*))
             (array ,o-type)
           (declare (ignore broadcast))
           (cffi:with-foreign-pointer (tmp ,o-size)
             (setf (cffi:mem-ref tmp ,(eswitch (o-type :test #'type=)
                                        ('single-float :float)
                                        ('double-float :double)
                                        ('(signed-byte 64) :long)
                                        ('(signed-byte 32) :int)
                                        ('(signed-byte 16) :short)
                                        ('(signed-byte 08) :char)
                                        ('(unsigned-byte 64) :long)
                                        ('(unsigned-byte 32) :int)
                                        ('(unsigned-byte 16) :short)
                                        ('(unsigned-byte 08) :char)))
                   (trivial-coerce:coerce x ',o-type))
             (ptr-iterate-but-inner (narray-dimensions out) n
               ((ptr-out ,o-size iout out))
               (,c-name n tmp 0 ptr-out iout)))
           out))))

(define-one-arg-broadcast-polymorph nu:copy bmas:cast-sd (4 single-float) (8 double-float))
(define-one-arg-broadcast-polymorph nu:copy bmas:cast-ds (8 double-float) (4 single-float))

(macrolet ((def (bytes single-signed-cast single-unsigned-cast double-signed-cast double-unsigned-cast)
             `(progn
                (define-one-arg-broadcast-polymorph nu:copy ,single-signed-cast
                    (,bytes (signed-byte ,(* 8 bytes)))
                    (4 single-float))
                (define-one-arg-broadcast-polymorph nu:copy ,single-unsigned-cast
                    (,bytes (unsigned-byte ,(* 8 bytes)))
                    (4 single-float))
                (define-one-arg-broadcast-polymorph nu:copy ,double-signed-cast
                    (,bytes (signed-byte ,(* 8 bytes)))
                    (8 double-float))
                (define-one-arg-broadcast-polymorph nu:copy ,double-unsigned-cast
                    (,bytes (unsigned-byte ,(* 8 bytes)))
                    (8 double-float)))))
  (def 1 bmas:cast-i8s  bmas:cast-u8s  bmas:cast-i8d  bmas:cast-u8d)
  (def 2 bmas:cast-i16s bmas:cast-u16s bmas:cast-i16d bmas:cast-u16d)
  (def 4 bmas:cast-i32s bmas:cast-u32s bmas:cast-i32d bmas:cast-u32d)
  (def 8 bmas:cast-i64s bmas:cast-u64s bmas:cast-i64d bmas:cast-u64d))

(macrolet ((def (size type copy-fn)
             `(define-one-arg-broadcast-polymorph nu:copy ,copy-fn (,size ,type) (,size ,type))))

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
(defpolymorph (nu:copy) ((x (array t))
                         &key ((out (array single-float)))
                         (broadcast nu:*broadcast-automatically*))
    (array single-float)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    ;; FIXME: Error signalling when BROADCAST is NIL could be better
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    ;; FIXME? Why depend on (= SAFETY 0)
    (policy-cond:with-expectations (= safety 0)
        ((assertion (or broadcast
                        (equalp (narray-dimensions x)
                                (narray-dimensions out)))))
      (do-with-broadcasting broadcast-dimensions ((x-elt x)
                                                  (o-elt out))
        (setf o-elt (trivial-coerce:coerce (the real x-elt) 'single-float)))))
  out)

(defpolymorph nu:copy ((x (array t))
                       &key ((out (array double-float)))
                       (broadcast nu:*broadcast-automatically*))
    (array double-float)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    ;; FIXME: Error signalling when BROADCAST is NIL could be better
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    ;; FIXME? Why depend on (= SAFETY 0)
    (policy-cond:with-expectations (= safety 0)
        ((assertion (or broadcast
                        (equalp (narray-dimensions x)
                                (narray-dimensions out)))))
      (do-with-broadcasting broadcast-dimensions ((x-elt x)
                                                  (o-elt out))
        (setf o-elt (trivial-coerce:coerce (the real x-elt) 'double-float)))))
  out)

(defpolymorph nu:copy ((x (array t))
                       &key ((out (array t)) (nu:zeros-like x))
                       (broadcast nu:*broadcast-automatically*))
    (array t)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    ;; FIXME: Error signalling when BROADCAST is NIL could be better
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    ;; FIXME? Why depend on (= SAFETY 0)
    (policy-cond:with-expectations (= safety 0)
        ((assertion (or broadcast
                        (equalp (narray-dimensions x)
                                (narray-dimensions out)))))
      (do-with-broadcasting broadcast-dimensions ((x-elt x)
                                                  (o-elt out))
        (setf o-elt x-elt))))
  out)

(macrolet ((def (from to)
             `(trivial-coerce:define-coercion (a :from (array ,from) :to (simple-array ,to))
                (let ((out (nu:zeros (narray-dimensions a) :type ',to)))
                  (nu:copy a :out (the (array ,to) out))
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

(5am:def-suite nu:copy :in nu::array)

(defun test-copy (to-type from-type min max)
  (let ((nu:*multithreaded-threshold* 1000000))
    (5am:is (nu:array= (nu:asarray '(1 2 3) :type from-type)
                       (nu:copy (nu:asarray '(1 2 3) :type from-type)
                                :out (nu:zeros 3 :type to-type)))
            "Simplest case")
    (5am:is (nu:array= (nu:broadcast-array (nu:asarray '(1 2 3) :type from-type) '(3 3))
                       (nu:copy (nu:asarray '(1 2 3) :type from-type)
                                :out (nu:zeros 3 3 :type to-type)))
            "Simplest broadcast")

    (let ((rand (nu:aref (nu:rand 100 100 :type from-type :min min :max max)
                         '(10 :step 2) '(10 :step 2))))
      (5am:is (nu:array= rand
                         (nu:copy rand
                                  :out (nu:zeros 45 45 :type to-type))
                         :test (lambda (x y)
                                 (or (= x y)
                                     (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                        single-float-epsilon))))
              "Non-simple"))
    (let ((rand (nu:aref (nu:rand 100 100 :type from-type :min min :max max)
                         '(10 :step 2) '(10 :step 2))))
      (5am:is (nu:array= (nu:broadcast-array rand '(10 45 45))
                         (nu:copy rand
                                  :out (nu:zeros 10 45 45 :type to-type))
                         :test (lambda (x y)
                                 (or (= x y)
                                     (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                        single-float-epsilon))))
              "Non-simple broadcast"))
    (let ((nu:*multithreaded-threshold* 100)
          (rand (nu:aref (nu:rand 100 100 :type from-type :min min :max max)
                         '(10 :step 2) '(10 :step 2))))
      (5am:is (nu:array= (nu:broadcast-array rand '(10 45 45))
                         (nu:copy rand
                                  :out (nu:zeros 10 45 45 :type to-type))
                         :test (lambda (x y)
                                 (or (= x y)
                                     (< (/ (abs (- x y)) (+ (abs x) (abs y)))
                                        single-float-epsilon))))
              "Non-simple multithreaded broadcast"))))

(5am:def-test nu::copy/single-float (:suite nu:copy)
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

(5am:def-test nu::copy/double-float (:suite nu:copy)
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

(5am:def-test nu::copy/identical (:suite nu:copy)
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



