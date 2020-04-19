(in-package :numericals.internals)

(defmacro fixnum-+ (&rest args)
  (cond ((null args) 0)
        ((null (cdr args)) `(the fixnum ,(car args)))
        (t `(the fixnum (+ (the fixnum ,(car args))
                           ,(macroexpand `(fixnum-+ ,@(cdr args))))))))

(defun-c symbols (prefix n)
  (loop for dim below n
     for i = (intern (concatenate 'string prefix (write-to-string dim))
                     :numericals.internals)
     collect i))

(defun-c index-calculation-code (num-dimensions)
  (loop for s in (symbols "S" num-dimensions)
     for i in (symbols "I" num-dimensions)
     collect (list '* s i)))

(defun-c strides (n factor reversed-actual-dimension-symbols
                    reversed-required-dimension-symbols
                    reversed-stride-symbols)
  (when (> n 0)
    (with-gensyms ()
      (let ((first (car reversed-actual-dimension-symbols)))
        `(progn
           ;; assume sanity checking is done by callers
           (setq ,(car reversed-stride-symbols) (if (= 1 ,first) 0 ,factor))
           ,(strides (1- n)
                     (if (and (numberp factor) (= 1 factor))
                         first
                         `(the (signed-byte 31) (* ,factor ,first)))
                     (cdr reversed-actual-dimension-symbols)
                     (cdr reversed-required-dimension-symbols)
                     (cdr reversed-stride-symbols)))))))

(defmacro with-broadcast (type num-dimensions broadcast-fn-name array
                          (&rest required-dimensions) &body body)
  (let ((index-code `(fixnum-+ ,@(index-calculation-code num-dimensions)))
        (reversed-stride-symbols (nreverse (symbols "S" num-dimensions)))
        (index-symbols (symbols "I" num-dimensions))
        (reversed-required-dimension-symbols
         (nreverse (symbols "R" num-dimensions)))
        (reversed-actual-dimension-symbols
         (nreverse (symbols "A" num-dimensions)))
        (simd-aref (ecase type
                     (single-float 'simd-single-1d-aref)
                     (double-float 'simd-double-1d-aref)))
        (simd-broadcast-aref (ecase type
                               (single-float 'simd-single-broadcast-1d-aref)
                               (double-float 'simd-double-broadcast-1d-aref)))
        (aref 'aref)
        (broadcast-fn-name-simd (intern (concatenate 'string
                                                     (symbol-name broadcast-fn-name)
                                                     "-SIMD"))))
    (with-gensyms (vector)
      `(destructuring-bind (,reversed-required-dimension-symbols
                            (&optional ,@(loop for s in reversed-actual-dimension-symbols
                                            collect `(,s 1))))
           (list ,required-dimensions (array-dimensions ,array))
         (declare (ignorable ,@reversed-required-dimension-symbols)
                  (optimize (speed 3) (safety 0)))
         (let ((,vector (1d-storage-array ,array))
               ,@(loop for s in reversed-stride-symbols collect `(,s 0)))
           (declare (type (simple-array ,type) ,vector)
                    (type (signed-byte 31) ,@reversed-stride-symbols
                          ,@reversed-actual-dimension-symbols)
                    (optimize (speed 3)))
           ,(strides num-dimensions 1
                     reversed-actual-dimension-symbols
                     reversed-required-dimension-symbols
                     reversed-stride-symbols)
           ;; (unless (= ,num-dimensions (length ,required-dimensions))
           ;;   (error "Length of ~D is supposed to be ~D" ,required-dimensions ,num-dimensions)))
           
           (flet ((,broadcast-fn-name (,@index-symbols)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum ,@index-symbols ,@reversed-stride-symbols))
                    (,aref ,vector ,index-code))
                  ((setf ,broadcast-fn-name) (new-value ,@index-symbols)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum ,@index-symbols ,@reversed-stride-symbols))
                    (setf (,aref ,vector ,index-code) new-value))
                  ((setf ,broadcast-fn-name-simd) (new-value ,@index-symbols)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum ,@index-symbols ,@reversed-stride-symbols))
                    (setf (,simd-aref ,vector ,index-code) new-value))
                  (,broadcast-fn-name-simd (,@index-symbols)
                    (declare (optimize (speed 3) (safety 0))
                             (type fixnum ,@index-symbols ,@reversed-stride-symbols))
                    (if (zerop ,(car reversed-stride-symbols))
                        ;; Doing this is more performant than keeping separate branches
                        (,simd-broadcast-aref ,vector ,index-code)
                        (,simd-aref ,vector ,index-code))))
             (declare (inline ,broadcast-fn-name (setf ,broadcast-fn-name)
                              (setf ,broadcast-fn-name-simd)
                              ,broadcast-fn-name-simd)
                      (ignorable (function ,broadcast-fn-name)
                                 (function ,broadcast-fn-name-simd)
                                 (function (setf ,broadcast-fn-name))
                                 (function (setf ,broadcast-fn-name-simd)))
                      (optimize (speed 3) (safety 0))
                      (type fixnum ,@reversed-stride-symbols))
             ,@body))))))

(defmacro nested-for (n loop-vars bound-vars &body body)
  (if (= n 0)
      `(progn ,@body)
      `(loop for ,(car loop-vars) fixnum below ,(car bound-vars)
          do ,(macroexpand-1 `(nested-for ,(1- n)
                                  ,(cdr loop-vars)
                                  ,(cdr bound-vars)
                                ,@body)))))

(defmacro define-nd-broadcast-operation (name n type simd-op base-op)
  ;; assume these are quoted; so remove quote; just that quoted looks good
  (setq type (second type)
        simd-op (second simd-op)
        base-op (second base-op))
  (let* ((bound-symbols (symbols "B" n))
         ;; N and (1- N) is not a bug. This relates to how nested-for and the last
         ;; nested loop is supposed to handle the "remainder" elements of the array.
         (loop-symbols (symbols "I" (1- n)))
         (stride (ecase type
                   (single-float +simd-single-1d-aref-stride+)
                   (double-float +simd-double-1d-aref-stride+))))
    (progn
      `(declaim (notinline ,name))
      `(defun ,name (result a b)
         (declare (optimize (speed 3))
                  (type (simple-array single-float)
                        a b result))
         (let ((broadcast-dimensions (array-dimensions result)))
           (destructuring-bind ,bound-symbols broadcast-dimensions
             (declare (type fixnum ,@bound-symbols))
             (let ((bn (- ,@(last bound-symbols)
                          ,stride)))
               (declare (type fixnum bn))
               (with-broadcast ,type ,n r-ref result broadcast-dimensions
                 (with-broadcast ,type ,n a-ref a broadcast-dimensions
                   (with-broadcast ,type ,n b-ref b broadcast-dimensions
                     (nested-for ,(1- n) ,loop-symbols ,bound-symbols
                       (loop for in fixnum below bn by ,stride
                          do (setf (r-ref-simd ,@loop-symbols in)
                                   (,simd-op (a-ref-simd ,@loop-symbols in)
                                             (b-ref-simd ,@loop-symbols in)))
                          finally
                            (loop for i fixnum from in below ,@(last bound-symbols)
                               do (setf (r-ref ,@loop-symbols i)
                                        (,base-op (a-ref ,@loop-symbols i)
                                                  (b-ref ,@loop-symbols i))))))))))))
         result))))

(defun-c specialized-operation (operation type num-dimensions)
  (intern (concatenate 'string
                       (ecase type
                         (single-float "SINGLE")
                         (double-float "DOUBLE")
                         (fixnum "FIXNUM"))
                       "-" (write-to-string num-dimensions) "D-" (symbol-name operation))
          :numericals.internals))

(defmacro define-nd-broadcast-operations (type simd-op base-op)
  `(progn
     ,@(loop for i from 1 to *max-broadcast-dimensions*
          ;; assume quoted!
          for specialized-op-name = (specialized-operation (second base-op)
                                                           (second type)
                                                           i)
          collect `(define-nd-broadcast-operation
                       ,specialized-op-name
                       ,i
                     ,type
                     ,simd-op
                     ,base-op))))

(define-nd-broadcast-operations 'single-float 'simd-single-+ '+)
(define-nd-broadcast-operations 'single-float 'simd-single-- '-)
(define-nd-broadcast-operations 'single-float 'simd-single-* '*)
(define-nd-broadcast-operations 'single-float 'simd-single-/ '/)

;; (let ((size 128))
;;   (defparameter a (nu:asarray (make-list size :initial-element 0.1)))
;;   (defparameter b (nu:asarray (make-list size :initial-element 0.2)))
;;   (defparameter c (nu:zeros size)))

;; (let ((size 1024))
;;   (defparameter a (nu:asarray (list (make-list size :initial-element 0.1))))
;;   (defparameter b (nu:asarray (make-list size :initial-element '(0.2))))
;;   (defparameter c (nu:zeros size size)))

;; (let ((size 64)
;;       (size-2 32))
;;   (let ((a (nu:zeros 1 size-2 size))
;;         (b (nu:zeros size size-2 1))
;;         (c (nu:zeros size size-2 size)))
;;     (time (loop repeat 1000
;;              do (nu:+ a b :out c)))))

;; (let ((size 64)
;;       (size-2 32))
;;   (defparameter a (nu:zeros 1 size-2 size))
;;   (defparameter b (nu:zeros size size-2 1))
;;   (defparameter c (nu:zeros size size-2 size)))

;; (let ((size 64))
;;   (defparameter a (nu:zeros size size size))
;;   (defparameter b (nu:zeros size size size))
;;   (defparameter c (nu:zeros size size size)))


;; (let ((size 16))
;;   (defparameter a (nu:zeros 1 size size size))
;;   (defparameter b (nu:zeros size size size 1))
;;   (defparameter c (nu:zeros size size size size)))
