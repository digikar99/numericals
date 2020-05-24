(in-package :sbcl-numericals.internals)

;; We must provide facilities so that sn:+ not only accepts any number of
;; arguments, like cl:+; but also can be used without overhead with apply and funcall.
(defun foo (a b c)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum a b c))
  (the fixnum (+ a b c)))
(defun bar (a b c)
  (declare (optimize (speed 3) (safety 0))
           (type fixnum a b c))
  (the fixnum (funcall '+ a b c)))


;; deftransform: https://github.com/sbcl/sbcl/blob/master/src/compiler/macros.lisp
;; https://github.com/sbcl/sbcl/search?p=2&q=deftransform&unscoped_q=deftransform
;; https://github.com/sbcl/sbcl/blob/9d2ee5b235daf62b608aa98350a6ebfc4f8af67a/src/compiler/integer-tran.lisp
(defknown foo (t) t
    (unwind any)
  :overwrite-fndb-silently t)

(deftransform foo
    ((x) (*) *
     :policy (> speed space))
  ""
  '(print x))

(declaim (inline foo))
(defun foo (x)
  (print x))

(defun bar (x)
  (declare (optimize (space 3)))
  (foo x))

(defun baz (x)
  (declare (optimize (speed 3)))
  (funcall 'foo x))

(define-compiler-macro foo (&whole whole x)
  (if (eq 'funcall (car whole))
      `(print ,x)      
      whole))

(defun broadcast-compatible-p (array-a array-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two arrays can be broadcasted.
  The second value is the dimension of the array resulting from the broadcast."
  (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1))
        (for dim-a initially (nreverse (array-dimensions array-a))
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (nreverse (array-dimensions array-b))
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b)
                         (= a 1)
                         (= b 1))
                     (max a b)
                     (return nil))
          into broadcast-dimensions-reversed)
        (finally (return (values t
                                 (nreverse broadcast-dimensions-reversed))))))

(defun ensure-length (length pad-element list)
  (append (make-list (- length (length list)) :initial-element pad-element)
          list))

(defmacro fixnum-+ (&rest args)
  (cond ((null args) 0)
        ((null (cdr args)) `(the fixnum ,(car args)))
        (t `(the fixnum (+ (the fixnum ,(car args))
                           ,(macroexpand `(fixnum-+ ,@(cdr args))))))))

(defmacro double-float-+ (&rest args)
  (cond ((null args) 0.0d0)
        ((null (cdr args)) `(the double-float ,(car args)))
        (t `(the double-float (+ (the double-float ,(car args))
                                 ,(macroexpand `(double-float-+ ,@(cdr args))))))))

(defparameter *max-array-dimensions* 2)

(defmacro with-broadcast (broadcast-fn-name array (&rest required-dimensions) &body body)
  (let ((index-code `(fixnum-+ (* s0 i0)
                               (* s1 i1))))
    `(let ()
       (declare (optimize (speed 0) (compilation-speed 3)))
       (let* ((reversed-actual-dimensions (reverse (array-dimensions ,array)))
              (reversed-required-dimensions (reverse ,required-dimensions))
              ;; sanity check
              (reversed-strides
               (iter (for act = (if reversed-actual-dimensions
                                    (first reversed-actual-dimensions)
                                    1))
                     (setq reversed-actual-dimensions
                           (cdr reversed-actual-dimensions))
                     (for exp in reversed-required-dimensions)
                     ;; (print (list act exp))
                     (for full-stride initially 1
                          then (* full-stride act))
                     (for stride
                          = (cond 
                              ((= act 1) 0)
                              ((= act exp) full-stride)
                              (t
                               (error "Cannot broadcast array ~D of shape ~D to shape ~D"
                                      ,array (array-dimensions ,array) ,required-dimensions))))
                     (collect stride into strides)
                     (finally (return strides))))
              (strides (ensure-length *max-array-dimensions* 0 (reverse reversed-strides)))
              (vector (array-storage-vector ,array)))
         ;; (print strides)
         (destructuring-bind (s0 s1) strides
           (flet ((,broadcast-fn-name (simd-mode i0 i1)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum i0 i1 s0 s1))
                    (if simd-mode
                        (if (zerop s1)
                            (d4b-ref vector ,index-code)
                            (d4-ref vector ,index-code))
                        (row-major-aref vector ,index-code)))
                  ((setf ,broadcast-fn-name) (new-value simd-mode i0 i1)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum i0 i1 s0 s1))
                    (if simd-mode
                        (setf (d4-ref vector ,index-code)
                              new-value)
                        (setf (row-major-aref vector ,index-code)
                              new-value))))
             (declare (inline ,broadcast-fn-name (setf ,broadcast-fn-name))
                      (ignorable (function ,broadcast-fn-name)
                                 (function (setf ,broadcast-fn-name)))
                      (optimize (speed 3)))
             ,@body))))))

(defmacro with-broadcast (broadcast-fn-name array (&rest required-dimensions) &body body)
  (let ((index-code `(fixnum-+ (* s0 i0)
                               (* s1 i1)
                               (* s2 i2)
                               (* s3 i3))))
    `(let ()
       (declare (optimize (speed 0) (compilation-speed 3)))
       (let* ((reversed-actual-dimensions (reverse (array-dimensions ,array)))
              (reversed-required-dimensions (reverse ,required-dimensions))
              ;; sanity check
              (reversed-strides
               (iter (for act = (if reversed-actual-dimensions
                                    (first reversed-actual-dimensions)
                                    1))
                     (setq reversed-actual-dimensions
                           (cdr reversed-actual-dimensions))
                     (for exp in reversed-required-dimensions)
                     ;; (print (list act exp))
                     (for full-stride initially 1
                          then (* full-stride act))
                     (for stride
                          = (cond 
                              ((= act 1) 0)
                              ((= act exp) full-stride)
                              (t
                               (error "Cannot broadcast array ~D of shape ~D to shape ~D"
                                      ,array (array-dimensions ,array) ,required-dimensions))))
                     (collect stride into strides)
                     (finally (return strides))))
              (strides (ensure-length *max-array-dimensions* 0 (reverse reversed-strides)))
              (vector (array-storage-vector ,array)))
         ;; (print strides)
         (destructuring-bind (s0 s1 s2 s3) strides
           (flet ((,broadcast-fn-name (simd-mode i0 i1 i2 i3)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum i0 i1 i2 i3 s0 s1 s2 s3))
                    (if simd-mode
                        (if (zerop s3)
                            (d4b-ref vector ,index-code)
                            (d4-ref vector ,index-code))
                        (row-major-aref vector ,index-code)))
                  ((setf ,broadcast-fn-name) (new-value simd-mode i0 i1 i2 i3)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum i0 i1 i2 i3 s0 s1 s2 s3))
                    (if simd-mode
                        (setf (d4-ref vector ,index-code)
                              new-value)
                        (setf (row-major-aref vector ,index-code)
                              new-value))))
             (declare (inline ,broadcast-fn-name (setf ,broadcast-fn-name))
                      (ignorable (function ,broadcast-fn-name)
                                 (function (setf ,broadcast-fn-name)))
                      (optimize (speed 3)))
             ,@body))))))

;;; The Entry Point ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter sn:*automatically-upgrade-array-element-type* t
  "When T, upgrades single-float to double-float when required.")
(defparameter sn:*array-element-type* 'double-float)
(defparameter sn:*automatically-convert-lists-to-arrays* t)
(setf (macro-function 'lm) (macro-function 'lambda))


;; May be convert this to a type
(defun sn:array-like-p (object) (or (listp object) (arrayp object)))

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) (:a 2 :b 3))"
  (if args
      (if (keywordp (car args))
          (list () args)
          (destructuring-bind (non-keyword-args keyword-args)
              (split-at-keywords (cdr args))
            (list (cons (car args) non-keyword-args)
                  keyword-args)))
      '(() ())))

;; The plan:
;; + is the entry point: it calls array-+ which broadcasts two arrays of
;; same element-type

(defun sn:+ (&rest args)
  "ARGS must be a lambda list with the initial elements comprising of the list 
of operands, and the final optional elements comprise of :OUT and :DTYPE 
keyword args. For example
  (+ a b :out c :dtype 'single-float)
  (+ a b c :out d)
 (a b &optional (result nil result-supplied-p))"
  ;; Do the checks to provide helpful error messages.
  (destructuring-bind (args (&key (out nil out-supplied-p)
                                  (dtype nil dtype-supplied-p)))
      (split-at-keywords args)
    (when out-supplied-p
      (assert (some 'sn:array-like-p args) nil
              "Cannot supply result in ~D when no argument is array-like."
              out)
      (assert (arrayp out) nil
              "Cannot supply result in non-array type ~D" out)
      (assert (equalp (array-dimensions out)
                      (broadcast-compatible-p )))))

  (cond ((and (not (arrayp a)) (not (arrayp b)))
         (+ a b))        
        ((not (arrayp a))
         (%+ (let ((type (array-element-type b)))
               (make-array 1 :initial-element (coerce a type) :element-type type))
             b
             result))
        ((not (arrayp b))
         (%+ a
             (let ((type (array-element-type a)))
               (make-array 1 :initial-element (coerce b type) :element-type type))
             result))
        (t
         (%+ a b result))))

(defun copy-array-single-float (contents-array)
  (let* ((array (make-array (array-dimensions contents-array)
                            :element-type 'single-float))
         (from-storage-vector (if (typep contents-array 'simple-array)
                                  (array-storage-vector contents-array)
                                  (array-displacement contents-array)))
         (to-storage-vector (array-storage-vector array)))
    (loop for i below (length to-storage-vector)
       do (setf (elt to-storage-vector i)
                (coerce (elt from-storage-vector i) 'single-float)))))

(defun copy-array-double-float (contents-array)
  (let* ((array (make-array (array-dimensions contents-array)
                            :element-type 'double-float))
         (from-storage-vector (if (typep contents-array 'simple-array)
                                  (array-storage-vector contents-array)
                                  (array-displacement contents-array)))
         (to-storage-vector (array-storage-vector array)))
    (loop for i below (length to-storage-vector)
       do (setf (elt to-storage-vector i)
                (coerce (elt from-storage-vector i) 'double-float)))))

(defun sbcl-numericals:asarray (contents &key (type sn:*array-element-type*))
  (ecase type
    (single-float (copy-array-single-float contents))
    (double-float (copy-array-double-float contents))))

(defun ensure-type (a b &key (type sn:*array-element-type*))
  (declare (type array a b))
  (let ((type-a (array-element-type a))
        (type-b (array-element-type b)))
    (eswitch ((list type-a type-b) :test 'equalp)
      ('(single-float single-float) (list a b))
      ('(double-float double-float) (list a b))
      ('(single-float double-float)
        (list (sn:asarray a :type 'double-float)
              b))
      ('(double-float single-float)
        (list a
              (make-array (array-dimensions b) :element-type 'double-float
                          :initial-contents b))))))

(defparameter sa (make-array '(1 4) :element-type 'single-float
                             :initial-contents (list (iota 4))))

(defun %+ (a b r)
  (declare (type array a b))
  (when *automatically-upgrade-array-element-type*
    (dsetq (a b) (ensure-type-correct a b)))
  )



(defun sbcl-numericals:broadcast-d+ (a b c)
  (declare (optimize (speed 3))
           (type (simple-array double-float) a b c))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p a b)
    (unless broadcast-compatible-p
      (error "Arrays ~D and ~D are not compatible for broadcasting" a b))
    (let ((extended-broadcast-dimensions (ensure-length *max-array-dimensions* 1 broadcast-dimensions)))
      (destructuring-bind (b0 b1) extended-broadcast-dimensions
        (declare (type fixnum b0 b1))
        (let ((b1-4 (- b1 4)))
          (declare (type fixnum b1-4))
          (with-broadcast a-ref a broadcast-dimensions
                          (with-broadcast b-ref b broadcast-dimensions
                                          (with-broadcast c-ref c broadcast-dimensions
                                                          (declare (optimize (speed 3)))
                                                          (loop for i0 fixnum below b0
                                                             do (loop for i1 fixnum below b1-4 by 4
                                                                   do (setf (c-ref t i0 i1)
                                                                            (d4+ (a-ref t i0 i1)
                                                                                 (b-ref t i0 i1)))
                                                                   finally
                                                                     (loop for i fixnum from i1 below b1
                                                                        do (setf (c-ref nil i0 i)
                                                                                 (double-float-+ (a-ref nil i0 i)
                                                                                                 (b-ref nil i0 i)))))))))))))
  c)

(defun sbcl-numericals:broadcast-d+ (a b c)
  (declare (optimize (speed 3))
           (type (simple-array double-float) a b c))
  (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
      (broadcast-compatible-p a b)
    (unless broadcast-compatible-p
      (error "Arrays ~D and ~D are not compatible for broadcasting" a b))
    (let ((extended-broadcast-dimensions (ensure-length *max-array-dimensions* 1 broadcast-dimensions)))
      (destructuring-bind (b0 b1 b2 b3) extended-broadcast-dimensions
        (declare (type fixnum b0 b1 b2 b3))
        (let ((b3-4 (- b3 4)))
          (declare (type fixnum b3-4))
          (with-broadcast a-ref a broadcast-dimensions
                          (with-broadcast b-ref b broadcast-dimensions
                                          (with-broadcast c-ref c broadcast-dimensions
                                                          (declare (optimize (speed 3)))
                                                          (loop for i0 fixnum below b0
                                                             do (loop for i1 fixnum below b1
                                                                   do (loop for i2 fixnum below b2
                                                                         do (loop for i3 fixnum below b3-4 by 4
                                                                               do (setf (c-ref t i0 i1 i2 i3)
                                                                                        (d4+ (a-ref t i0 i1 i2 i3)
                                                                                             (b-ref t i0 i1 i2 i3)))
                                                                               finally
                                                                                 (loop for i fixnum from i3 below b3
                                                                                    do (setf (c-ref nil i0 i1 i2 i)
                                                                                             (double-float-+ (a-ref nil i0 i1 i2 i)
                                                                                                             (b-ref nil i0 i1 i2 i)))))))))))))))
  c)
