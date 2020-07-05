(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

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
                  (type (array ,type)
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
          numericals.helper:*numericals-internals-package*))

(macrolet ((define-nd-broadcast-operations (type simd-op base-op)
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
                                ,base-op)))))

  (define-nd-broadcast-operations 'single-float 'simd-single-+ '+)
  (define-nd-broadcast-operations 'single-float 'simd-single-- '-)
  (define-nd-broadcast-operations 'single-float 'simd-single-* '*)
  (define-nd-broadcast-operations 'single-float 'simd-single-/ '/)

  (define-nd-broadcast-operations 'double-float 'simd-double-+ '+)
  (define-nd-broadcast-operations 'double-float 'simd-double-- '-)
  (define-nd-broadcast-operations 'double-float 'simd-double-* '*)
  (define-nd-broadcast-operations 'double-float 'simd-double-/ '/))

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
