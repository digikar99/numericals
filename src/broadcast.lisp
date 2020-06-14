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
     collect `(the fixnum (* ,s ,i))))

(defun-c broadcast-strides (n factor reversed-actual-dimension-symbols
                              reversed-required-dimension-symbols
                              reversed-stride-symbols)
  (when (> n 0)
    (with-gensyms ()
      (let ((first (car reversed-actual-dimension-symbols)))
        `(progn
           ;; assume sanity checking is done by callers
           (setq ,(car reversed-stride-symbols) (if (= 1 ,first) 0 ,factor))
           ,(broadcast-strides (1- n)
                               (if (and (numberp factor) (= 1 factor))
                                   first
                                   `(the (signed-byte 31) (* ,factor ,first)))
                               (cdr reversed-actual-dimension-symbols)
                               (cdr reversed-required-dimension-symbols)
                               (cdr reversed-stride-symbols)))))))

;; Examples for final codes generated from the broadcast are given near the bottom
;; of this file in the form of comments.
;; !!! UPDATE THE EXAMPLE AT THE BOTTOM WHENEVER UPDATES TO THE CODE BELOW !!!
(defmacro with-broadcast (type num-dimensions stride-symbols
                          broadcast-fn-name array
                          (&rest required-dimensions) &body body)
  (let* ((vector (gensym))
         (offset (gensym))
         (broadcasted-array (gensym))
         (index-symbols (symbols "I" num-dimensions))
         (index-code `(the fixnum (+ ,offset ,@index-symbols)))
         (reversed-stride-symbols (reverse stride-symbols))
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
    (once-only (array required-dimensions)
      `(let (,vector
             ,offset
             ,@(loop for s in stride-symbols collect `(,s 0))
             (,broadcasted-array (if (equalp ,required-dimensions (array-dimensions ,array))
                                     ,array
                                     (broadcast-array ,array ,required-dimensions))))
         (declare (type (or null (simple-array ,type)) ,vector)
                  (type (or null (signed-byte 31)) ,offset)
                  (type (signed-byte 31) ,@stride-symbols)
                  (optimize (speed 3)))
         ;; (setq ,vector (1d-storage-array ,array))
         (multiple-value-setq (,vector ,offset) (1d-storage-array ,array))
         ;; https://ipython-books.github.io/46-using-stride-tricks-with-numpy/
         (dsetq ,stride-symbols (array-strides ,broadcasted-array))
         ;; The most obvious way to calculate the "true" index, as stated 
         ;; in the link few lines above, is to take the "dot" product of
         ;; index-symbols with stride-symbols.
         ;; However, this is expensive. Instead, the multiplying step is offloaded
         ;; to the loop variables in define-nd-broadcast-operation below. Only addition
         ;; is performed by index-code .
         (flet ((,broadcast-fn-name (,@index-symbols)
                  (declare (optimize (speed 3))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (,aref ,vector ,index-code))
                ((setf ,broadcast-fn-name) (new-value ,@index-symbols)
                  (declare (optimize (speed 3))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (setf (,aref ,vector ,index-code) new-value))
                ((setf ,broadcast-fn-name-simd) (new-value ,@index-symbols)
                  (declare (optimize (speed 3) (safety 0))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (setf (,simd-aref ,vector ,index-code) new-value))
                (,broadcast-fn-name-simd (,@index-symbols)
                  (declare (optimize (speed 3) (safety 0))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
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
                    (type (signed-byte 31) ,@reversed-stride-symbols))
           ,@body)))))

(defmacro nested-for (n bound-vars (loop-vars-r stride-vars-r)
                      (loop-vars-a stride-vars-a)
                      (loop-vars-b stride-vars-b)
                      &body body)
  (if (= n 0)
      `(progn ,@body)
      `(let ((,(car loop-vars-a) 0)
             (,(car loop-vars-b) 0))
         (declare (type (signed-byte 31) ,(car loop-vars-a) ,(car loop-vars-b)))
         (loop :for ,(car loop-vars-r) fixnum :by ,(car stride-vars-r)
            :repeat ,(car bound-vars)
            :do ,(macroexpand-1 `(nested-for ,(1- n)
                                     ,(cdr bound-vars)
                                     (,(cdr loop-vars-r) ,(cdr stride-vars-r))
                                     (,(cdr loop-vars-a) ,(cdr stride-vars-a))
                                     (,(cdr loop-vars-b) ,(cdr stride-vars-b))
                                   ,@body))
              (incf ,(car loop-vars-a) ,(car stride-vars-a))
              (incf ,(car loop-vars-b) ,(car stride-vars-b))))))

(defmacro define-nd-broadcast-operation (name n type simd-op base-op)
  ;; assume these are quoted; so remove quote; just that quoted looks good
  (setq type (second type)
        simd-op (second simd-op)
        base-op (second base-op))
  (let* ((bound-symbols (symbols "BOUND-" n))
         (loop-symbols-r (symbols "IR" n))
         (loop-symbols-a (symbols "IA" n))
         (loop-symbols-b (symbols "IB" n))
         (stride-symbols-r (symbols "R" n))
         (stride-symbols-a (symbols "A" n))
         (stride-symbols-b (symbols "B" n))
         ;; Should the above be replaced with just gensyms?
         (stride (ecase type
                   (single-float +simd-single-1d-aref-stride+)
                   (double-float +simd-double-1d-aref-stride+))))
    (progn
      `(declaim (notinline ,name)) ;; Should this be inlined?
      `(defun ,name (result a b)
         (declare (optimize (speed 3))
                  (type (array single-float)
                        a b result))
         (let* ((broadcast-dimensions (array-dimensions result)))
           (destructuring-bind ,bound-symbols broadcast-dimensions
             (declare (type (signed-byte 31) ,@bound-symbols))
             (with-broadcast ,type ,n ,stride-symbols-r r-ref result broadcast-dimensions
               (with-broadcast ,type ,n ,stride-symbols-a a-ref a broadcast-dimensions
                 (with-broadcast ,type ,n ,stride-symbols-b b-ref b broadcast-dimensions
                   (let ((an-simd-stride (* ,stride ,@(last stride-symbols-a)))
                         (bn-simd-stride (* ,stride ,@(last stride-symbols-b)))
                         (bound-n-simd (* ,stride
                                          (floor ,@(last bound-symbols)
                                                 ,stride))))
                     ;; an-simd-stride and bn-simd-stride can be 0 or ,stride depending
                     ;; on that last value
                     ;; By contrast, the equivalent rn-simd-stride would always be non-zero.
                     (declare (type (signed-byte 31) bound-n-simd
                                    an-simd-stride bn-simd-stride)
                              (optimize (speed 3)))
                     ;; (print (list (list ,@stride-symbols-a)
                     ;;              (list ,@stride-symbols-b)
                     ;;              (list ,@stride-symbols-r)))
                     (nested-for ,(1- n) ,bound-symbols
                         (,loop-symbols-r ,stride-symbols-r)
                         (,loop-symbols-a ,stride-symbols-a)
                         (,loop-symbols-b ,stride-symbols-b)
                       (let ((,@(last loop-symbols-a) 0)
                             (,@(last loop-symbols-b) 0))
                         (declare (type (signed-byte 31)
                                        ,@(last loop-symbols-a)
                                        ,@(last loop-symbols-b)))
                         (loop :for ,@(last loop-symbols-r) fixnum
                            :below bound-n-simd by ,stride
                            :do ;; (print (list (list ,@loop-symbols-a)
                            ;;          (list ,@loop-symbols-b)
                            ;;          (list ,@loop-symbols-r)))
                              (setf (r-ref-simd ,@loop-symbols-r)
                                    (,simd-op (a-ref-simd ,@loop-symbols-a)
                                              (b-ref-simd ,@loop-symbols-b)))
                              (incf ,@(last loop-symbols-a) an-simd-stride)
                              (incf ,@(last loop-symbols-b) bn-simd-stride)
                            :finally
                              (let ((,@(last loop-symbols-a) ,@(last loop-symbols-a))
                                    (,@(last loop-symbols-b) ,@(last loop-symbols-b)))
                                (declare (type (signed-byte 31) ,@(last loop-symbols-a)
                                               ,@(last loop-symbols-b)))
                                ;; While it is more natural to incorporate the above
                                ;; variables and the incf statements below into
                                ;; another loop variable; the stride can be 0,
                                ;; and therefore, it cannot be the "by" part of the
                                ;; resulting for loop variable.
                                (loop :for ,@(last loop-symbols-r) fixnum
                                   :from ,@(last loop-symbols-r)
                                   :below ,@(last bound-symbols)
                                   :do
                                   ;; (print (list (list ,@loop-symbols-a)
                                   ;;              (list ,@loop-symbols-b)
                                   ;;              (list ,@loop-symbols-r)))
                                     (setf (r-ref ,@loop-symbols-r)
                                           (,base-op (a-ref ,@loop-symbols-a)
                                                     (b-ref ,@loop-symbols-b)))
                                     (incf ,@(last loop-symbols-a)
                                           ,@(last stride-symbols-a))
                                     (incf ,@(last loop-symbols-b)
                                           ,@(last stride-symbols-b)))))))))))))
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

;; (let ((size 1048576))
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

;; (let ((size 32))
;;   (defparameter a (nu:zeros 1 size size size))
;;   (defparameter b (nu:zeros size size size 1))
;;   (defparameter c (nu:zeros size size size size)))


;; Study this code in conjunction with the comments inside the macros
;; with-broadcast and define-nd-broadcast-operation
;; THE EXAMPLE
;; (defun single-1d-+ (result a b)
;;    (declare (optimize (speed 3))
;;             (type (array single-float) a b result))
;;    (let ((broadcast-dimensions (array-dimensions result)))
;;      (destructuring-bind
;;          (bound-0)
;;          broadcast-dimensions
;;        (declare (type (signed-byte 31) bound-0))
;;        (with-broadcast single-float 1 (r0) r-ref result broadcast-dimensions
;;          (with-broadcast single-float 1 (a0) a-ref a broadcast-dimensions
;;            (with-broadcast single-float 1 (b0) b-ref b broadcast-dimensions
;;              (let ((an-simd-stride (* 8 a0))
;;                    (bn-simd-stride (* 8 b0))
;;                    (bound-n-simd (* 8 (floor bound-0 8))))
;;                (declare
;;                 (type (signed-byte 31) bound-n-simd an-simd-stride
;;                  bn-simd-stride)
;;                 (optimize (speed 3)))
;;                (nested-for 0
;;                    (bound-0)
;;                    ((ir0) (r0))
;;                    ((ia0) (a0))
;;                    ((ib0) (b0))
;;                  (let ((ia0 0) (ib0 0))
;;                    (declare (type (signed-byte 31) ia0 ib0))
;;                    (loop :for ir0 fixnum :below bound-n-simd by 8
;;                       :do (setf (r-ref-simd ir0)
;;                                 (simd-single-+ (a-ref-simd ia0)
;;                                                (b-ref-simd ib0)))
;;                         (incf ia0 an-simd-stride)
;;                         (incf ib0 bn-simd-stride)
;;                       :finally (let ((ia0 ia0) (ib0 ib0))
;;                                  (declare (type (signed-byte 31) ia0 ib0))
;;                                  (loop :for ir0 fixnum :from ir0 :below bound-0
;;                                     :do (setf (r-ref ir0)
;;                                               (+ (a-ref ia0)
;;                                                  (b-ref ib0)))
;;                                       (incf ia0 a0)
;;                                       (incf ib0 b0))))))))))))
;;    result)
