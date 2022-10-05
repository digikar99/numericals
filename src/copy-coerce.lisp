(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

;;; TODO: Implement methods of copying from CL:ARRAY to DENSE-ARRAYS:ARRAY and vice-versa
;;; TODO: Provide a fast (setf aref)

(define-polymorphic-function nu:copy (x &key out (broadcast nu:*broadcast-automatically*))
  :overwrite t)

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

;; 4 polymorphs: for same types
(defpolymorph (nu:copy :more-optimal-type-list
                       ((simple-array <type>)
                        &key (:out (simple-array <type>))
                        (:broadcast null)))
    ((x (array <type>))
     &key ((out (array <type>)))
     (broadcast nu:*broadcast-automatically*))
    (array <type>)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    (let ((c-name (c-name <type> 'nu:copy))
          (c-size (c-size <type>)))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions out)))))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-x   c-size ix   x)
           (ptr-out c-size iout out))
          (funcall c-name n ptr-x ix ptr-out iout))))
    out))

(defpolymorph (nu:copy :suboptimal-note runtime-array-allocation
                       :more-optimal-type-list
                       ((simple-array <type>)
                        &key (:broadcast null)
                        (:out (simple-array <type>))))
    ((x (array <type>))
     &key ((out null))
     (broadcast nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignore out broadcast))
  (pflet ((out (nu:zeros-like x))
          (c-name (c-name <type> 'nu:copy))
          (c-size (c-size <type>)))
    (declare (type (simple-array <type>) out))
    (ptr-iterate-but-inner (narray-dimensions x) n
      ((ptr-x   c-size ix   x)
       (ptr-out c-size iout out))
      (funcall c-name n ptr-x ix ptr-out iout))
    out))

(defpolymorph (nu:copy :suboptimal-note runtime-array-allocation)
    ((x (simple-array <type>))
     &key ((out null))
     ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (declare (ignore out broadcast))
  (pflet ((out (nu:zeros-like x))
          (c-name (c-name <type> 'nu:copy))
          (n (array-total-size x)))
    (declare (type (simple-array <type>) out))
    (pflet ((svx (array-storage x))
            (svo (array-storage out)))
      (declare (type (cl:simple-array <type> 1) svx svo))
      (with-pointers-to-vectors-data ((ptr-x   svx)
                                      (ptr-out svo))
        (funcall c-name n ptr-x 1 ptr-out 1)))
    out))

(defpolymorph nu:copy ((x (simple-array <type>))
                       &key ((out (simple-array <type>)))
                       ((broadcast null) nu:*broadcast-automatically*))
    (simple-array <type>)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions out)))))
    (pflet ((c-name (c-name <type> 'nu:copy))
            (n (array-total-size x)))
      (pflet ((svx (array-storage x))
              (svo (array-storage out)))
        (declare (type (cl:simple-array <type> 1) svx svo))
        (with-pointers-to-vectors-data ((ptr-x   svx)
                                        (ptr-out svo))
          (funcall c-name n ptr-x 1 ptr-out 1)))
      out)))

;; 2 polymorphs: to single-float
(defpolymorph (nu:copy :more-optimal-type-list
                       ((simple-array <type>)
                        &key (:out (simple-array single-float))
                        (:broadcast null)))
    ((x (array <type>))
     &key ((out (array single-float)))
     (broadcast nu:*broadcast-automatically*))
    (array single-float)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    (let ((c-name (c-name <type> 'nu::to-single-float))
          (c-size (c-size <type>)))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions out)))))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-x   c-size ix   x)
           (ptr-out    4   iout out))
          (funcall c-name n ptr-x ix ptr-out iout))))
    out))

(defpolymorph nu:copy ((x (simple-array <type>))
                       &key ((out (simple-array single-float)))
                       ((broadcast null) nu:*broadcast-automatically*))
    (simple-array single-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions out)))))
    (pflet ((c-name (c-name <type> 'nu::to-single-float))
            (n (array-total-size x)))
      (pflet ((svx (array-storage x))
              (svo (array-storage out)))
        (declare (type (cl:simple-array <type> 1) svx)
                 (type (cl:simple-array single-float 1) svo))
        (with-pointers-to-vectors-data ((ptr-x   svx)
                                        (ptr-out svo))
          (funcall c-name n ptr-x 1 ptr-out 1)))
      out)))

;; 2 polymorphs: to double-float
(defpolymorph (nu:copy :more-optimal-type-list
                       ((simple-array <type>)
                        &key (:out (simple-array double-float))
                        (:broadcast null)))
    ((x (array <type>))
     &key ((out (array double-float)))
     (broadcast nu:*broadcast-automatically*))
    (array double-float)
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p x out)
    (assert broadcast-compatible-p (x out)
            'incompatible-broadcast-dimensions
            :dimensions (mapcar #'narray-dimensions (list x out))
            :array-likes (list x out))
    (let ((c-name (c-name <type> 'nu::to-double-float))
          (c-size (c-size <type>)))
      (policy-cond:with-expectations (= safety 0)
          ((assertion (or broadcast
                          (equalp (narray-dimensions x)
                                  (narray-dimensions out)))))
        (ptr-iterate-but-inner broadcast-dimensions n
          ((ptr-x   c-size ix   x)
           (ptr-out    8   iout out))
          (funcall c-name n ptr-x ix ptr-out iout))))
    out))

(defpolymorph nu:copy ((x (simple-array <type>))
                       &key ((out (simple-array double-float)))
                       ((broadcast null) nu:*broadcast-automatically*))
    (simple-array double-float)
  (policy-cond:with-expectations (= safety 0)
      ((assertion (or broadcast
                      (equalp (narray-dimensions x)
                              (narray-dimensions out)))))
    (pflet ((c-name (c-name <type> 'nu::to-double-float))
            (n (array-total-size x)))
      (pflet ((svx (array-storage x))
              (svo (array-storage out)))
        (declare (type (cl:simple-array <type> 1) svx)
                 (type (cl:simple-array double-float 1) svo))
        (with-pointers-to-vectors-data ((ptr-x   svx)
                                        (ptr-out svo))
          (funcall c-name n ptr-x 1 ptr-out 1)))
      out)))

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



