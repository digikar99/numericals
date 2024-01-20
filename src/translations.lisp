(numericals.common:compiler-in-package numericals.common:*compiler-package*)

;;; This table does not contain all the instructions. Excluded instructions include:
;;; - NU:COPY

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +fixnum-shift-pointer+ (progn
                                   (when (boundp '+fixnum-shift-pointer+)
                                     ;; Should we be doing this?
                                     (cffi:foreign-free +fixnum-shift-pointer+))
                                   (cffi:foreign-alloc :long :initial-element 1))))

#-(or ccl sbcl)
(warn "FIXNUM operations are untested on non-SBCL/CCL platforms")

(declaim (inline fixnum-mul))
(defun fixnum-mul (n x incx y incy out inc-out)
  (bmas:i64mul n x incx y incy out inc-out)
  #+sbcl
  (bmas:i64sra n out inc-out +fixnum-shift-pointer+ 0 out inc-out))

(declaim (inline fixnum-sum))
(defun fixnum-sum (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64sum n x incx) 2))
  #-sbcl
  (bmas:i64sum n x incx))

(declaim (inline fixnum-hmax))
(defun fixnum-hmax (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64hmax n x incx) 2))
  #-sbcl
  (bmas:i64hmax n x incx))

(declaim (inline fixnum-hmin))
(defun fixnum-hmin (n x incx)
  #+sbcl
  (nth-value 0 (floor (bmas:i64hmin n x incx) 2))
  #-sbcl
  (bmas:i64hmax n x incx))

(declaim (inline fixnum-dot))
(defun fixnum-dot (n x incx y incy)
  #+sbcl
  (nth-value 0 (floor (bmas:i64dot n x incx y incy) 4))
  #-sbcl
  (bmas:i64dot n x incx y incy))

(declaim (type hash-table *translation-table*))
(defparameter *translation-table*
  (alist-hash-table
   '((nu:sin bmas:ssin bmas:dsin cl:sin)
     (nu:cos bmas:scos bmas:dcos cl:cos)
     (nu:tan bmas:stan bmas:dtan cl:tan)

     (nu:asin bmas:sasin bmas:dasin cl:asin)
     (nu:acos bmas:sacos bmas:dacos cl:acos)
     (nu:atan bmas:satan bmas:datan cl:atan)

     (nu:sinh bmas:ssinh bmas:dsinh cl:sinh)
     (nu:cosh bmas:scosh bmas:dcosh cl:cosh)
     (nu:tanh bmas:stanh bmas:dtanh cl:tanh)

     (nu:asinh bmas:sasinh bmas:dasinh cl:asinh)
     (nu:acosh bmas:sacosh bmas:dacosh cl:acosh)
     (nu:atanh bmas:satanh bmas:datanh cl:atanh)

     (nu:exp bmas:sexp bmas:dexp cl:exp)
     (nu:abs bmas:sfabs bmas:dfabs cl:abs bmas:i64abs bmas:i32abs bmas:i16abs bmas:i8abs
                                          bmas:i64abs bmas:i32abs bmas:i16abs bmas:i8abs
                                          bmas:i64abs)

     (nu:log       bmas:slog   bmas:dlog   cl:log)
     (nu:fround    bmas:sround bmas:dround cl:fround)
     (nu:ftruncate bmas:strunc bmas:dtrunc cl:ftruncate)
     (nu:ffloor    bmas:sfloor bmas:dfloor cl:ffloor)
     (nu:fceiling  bmas:sceil  bmas:dceil  cl:fceiling)

     (nu::atan2 bmas:satan2 bmas::datan2 cl:atan)
     (nu:expt   bmas:spow   bmas:dpow    cl:expt)
     (nu:sqrt   bmas:ssqrt  bmas:dsqrt   cl:sqrt)

     (nu:copy bmas:scopy bmas:dcopy cl:identity
                                    bmas:i64copy bmas:i32copy bmas:i16copy bmas:i8copy
                                    bmas:i64copy bmas:i32copy bmas:i16copy bmas:i8copy)
     (nu::to-single-float bmas:scopy bmas:cast-ds nil
                          bmas:cast-i64s bmas:cast-i32s bmas:cast-i16s bmas:cast-i8s
                          bmas:cast-u64s bmas:cast-u32s bmas:cast-u16s bmas:cast-u8s)
     (nu::to-double-float bmas:cast-sd bmas:dcopy nil
                          bmas:cast-i64d bmas:cast-i32d bmas:cast-i16d bmas:cast-i8d
                          bmas:cast-u64d bmas:cast-u32d bmas:cast-u16d bmas:cast-u8d)

     (nu:add      bmas:sadd bmas:dadd cl:+ bmas:i64add bmas:i32add bmas:i16add bmas:i8add
      bmas:i64add bmas:i32add bmas:i16add bmas:i8add
      bmas:i64add)
     (nu:subtract bmas:ssub bmas:dsub cl:- bmas:i64sub bmas:i32sub bmas:i16sub bmas:i8sub
                                           bmas:i64sub bmas:i32sub bmas:i16sub bmas:i8sub
                                           bmas:i64sub)
     (nu:multiply bmas:smul bmas:dmul cl:* bmas:i64mul bmas:i32mul bmas:i16mul bmas:i8mul
                                           bmas:u64mul bmas:u32mul bmas:u16mul bmas:u8mul
                                           fixnum-mul)
     (nu:divide   bmas:sdiv bmas:ddiv cl:/)

     (nu:two-arg-max bmas:smax bmas:dmax cl:max bmas:i64max bmas:i32max bmas:i16max bmas:i8max
                                                bmas:u64max bmas:u32max bmas:u16max bmas:u8max
                                                fixnum-max)
     (nu:two-arg-min bmas:smin bmas:dmin cl:min bmas:i64min bmas:i32min bmas:i16min bmas:i8min
                                                bmas:u64min bmas:u32min bmas:u16min bmas:u8min
                                                fixnum-min)

     (nu:sum bmas:ssum bmas:dsum cl:+ bmas:i64sum bmas:i32sum bmas:i16sum bmas:i8sum
                                      bmas:i64sum bmas:i32sum bmas:i16sum bmas:i8sum
                                      fixnum-sum)
     (nu:maximum bmas:shmax bmas:dhmax cl:max bmas:i64hmax bmas:i32hmax bmas:i16hmax bmas:i8hmax
                                              bmas:u64hmax bmas:u32hmax bmas:u16hmax bmas:u8hmax
                                              fixnum-hmax)
     (nu:minimum bmas:shmin bmas:dhmin cl:min bmas:i64hmin bmas:i32hmin bmas:i16hmin bmas:i8hmin
                                              bmas:u64hmin bmas:u32hmin bmas:u16hmin bmas:u8hmin
                                              fixnum-hmin)

     (nu:arg-maximum bmas:shimax bmas:dhimax cl:nil bmas:i64himax bmas:i32himax bmas:i16himax bmas:i8himax
                                             bmas:u64himax bmas:u32himax bmas:u16himax bmas:u8himax
                                             bmas:i64himax)
     (nu:arg-minimum bmas:shimin bmas:dhimin cl:nil bmas:i64himin bmas:i32himin bmas:i16himin bmas:i8himin
                                             bmas:u64himin bmas:u32himin bmas:u16himin bmas:u8himin
                                             bmas:i64himin)

     (nu:two-arg-logand nil nil cl:logand bmas:i8and bmas:i8and bmas:i8and bmas:i8and)
     (nu:two-arg-logior nil nil cl:logior bmas:i8or  bmas:i8or  bmas:i8or  bmas:i8or)
     (nu:two-arg-logxor nil nil cl:logxor bmas:i8xor bmas:i8xor bmas:i8xor bmas:i8xor)
     (nu:lognot         nil nil cl:lognot bmas:i8not bmas:i8not bmas:i8not bmas:i8not)
     (nu:logandc1 nil nil cl:logandc1 bmas:i8andnot bmas:i8andnot bmas:i8andnot bmas:i8andnot)

     (nu:two-arg-<  bmas:slt  bmas:dlt  cl:<  bmas:i64lt  bmas:i32lt  bmas:i16lt  bmas:i8lt
                                              bmas:u64lt  bmas:u32lt  bmas:u16lt  bmas:u8lt
                                              bmas:i64lt)
     (nu:two-arg-<= bmas:sle  bmas:dle  cl:<= bmas:i64le  bmas:i32le  bmas:i16le  bmas:i8le
                                              bmas:u64le  bmas:u32le  bmas:u16le  bmas:u8le
                                              bmas:i64le)
     (nu:two-arg-=  bmas:seq  bmas:deq  cl:=  bmas:i64eq  bmas:i32eq  bmas:i16eq  bmas:i8eq
                                              bmas:u64eq  bmas:u32eq  bmas:u16eq  bmas:u8eq
                                              bmas:i64eq)
     (nu:two-arg-/= bmas:sneq bmas:dneq cl:/= bmas:i64neq bmas:i32neq bmas:i16neq bmas:i8neq
                                              bmas:u64neq bmas:u32neq bmas:u16neq bmas:u8neq
                                              bmas:i64neq)
     (nu:two-arg->= bmas:sge  bmas:dge  cl:>= bmas:i64ge  bmas:i32ge  bmas:i16ge  bmas:i8ge
                                              bmas:u64ge  bmas:u32ge  bmas:u16ge  bmas:u8ge
                                              bmas:i64ge)
     (nu:two-arg->  bmas:sgt  bmas:dgt  cl:>  bmas:i64gt  bmas:i32gt  bmas:i16gt  bmas:i8gt
                                              bmas:u64gt  bmas:u32gt  bmas:u16gt  bmas:u8gt
                                              bmas:i64gt)

     ;; Use bmas for consistency
     (nu:vdot bmas:sdot bmas:ddot ;; magicl.blas-cffi:%sdot magicl.blas-cffi:%ddot
              cl:*
              bmas:i64dot bmas:i32dot bmas:i16dot bmas:i8dot
              bmas:i64dot bmas:i32dot bmas:i16dot bmas:i8dot
              fixnum-dot)

     (nu:matmul ceigen-lite:smatmul ceigen-lite:dmatmul)
     ;; FIXME: partial-piv-lu is only valid for square matrices
     (la:solve ceigen-lite:shouseholder-qr
               ceigen-lite:dhouseholder-qr)
     (la::solve/square ceigen-lite:spartial-piv-lu
                       ceigen-lite:dpartial-piv-lu)

     (la::norm2/vector ceigen-lite:snorm2v
                       ceigen-lite:dnorm2v)
     (la::norm2/matrix ceigen-lite:snorm2m
                       ceigen-lite:dnorm2m)
     (la:rank ceigen-lite:srank ceigen-lite:drank)
     (la:det ceigen-lite:sdeterminant
             ceigen-lite:ddeterminant)
     (la:inv ceigen-lite:sinverse
             ceigen-lite:dinverse)
     (la:pinv ceigen-lite:spinverse
              ceigen-lite:dpinverse)
     (la:cholesky ceigen-lite:scholesky
                  ceigen-lite:dcholesky)

     (la:qr ceigen-lite:sqr ceigen-lite:dqr)
     (la:lu ceigen-lite:slu ceigen-lite:dlu)
     (la:svd ceigen-lite:ssvd ceigen-lite:dsvd)

     (rand:gaussian  ceigen-lite:snormal ceigen-lite:dnormal)
     (rand:beta      ceigen-lite:sbeta   ceigen-lite:dbeta)
     (rand:chisquare ceigen-lite:schi-squared   ceigen-lite:dchi-squared)

     ;; (nu:ash-right nil nil nil bmas:i64sra bmas:i32sra bmas:i16sra bmas:i8sra
     ;;                           bmas:u64srl bmas:u32srl bmas:u16srl bmas:u8srl)
     )))

;; TODO: Replace C-NAME with C-FN
(macrolet ((def (fn-name index)
             `(progn
                (declaim (inline ,fn-name))
                (defun ,fn-name (name)
                  (declare (optimize speed)
                           (type symbol name))
                  (fdefinition
                   (or (nth ,index (gethash name *translation-table*))
                       (error "No CFUNCTION known for ~S for ~S" name ',fn-name))))
                (define-compiler-macro ,fn-name (&whole form name &environment env)
                  (let ((form-type (cl-form-types:form-type name env)))
                    ;; TODO: Use CTYPE to normalize types?
                    (if (and (listp form-type)
                             (null (cddr form-type))
                             (or (eq 'eql (first form-type))
                                 (eq 'member (first form-type))))
                        (list 'function
                              (or (nth ,index (gethash (second form-type) *translation-table*))
                                  (error "No CFUNCTION known for ~S for ~S" name ',fn-name)))
                        form))))))
  (def single-float-c-name 0)
  (def double-float-c-name 1)
  (def cl-name 2)

  (def int64-c-name 3)
  (def int32-c-name 4)
  (def int16-c-name 5)
  (def int8-c-name  6)

  (def uint64-c-name 7)
  (def uint32-c-name 8)
  (def uint16-c-name 9)
  (def uint8-c-name  10)

  (def fixnum-c-name   11))

(defun c-name (type name)
  (declare (optimize speed))
  (let ((idx (switch (type :test #'alexandria:type=)
               ('single-float 0)
               ('double-float 1)
               ('(signed-byte 64) 3)
               ('(signed-byte 32) 4)
               ('(signed-byte 16) 5)
               ('(signed-byte 08) 6)
               ('(unsigned-byte 64) 7)
               ('(unsigned-byte 32) 8)
               ('(unsigned-byte 16) 9)
               ('(unsigned-byte 08) 10)
               ('fixnum 11)
               (t 2))))
    (or (nth idx (gethash name *translation-table*))
        (error "No CFUNCTION known for ~S for ~S" name type))))
(define-compiler-macro c-name (&whole form type name &environment env)
  (let* ((type-form-type (cl-form-types:form-type type env))
         (name-form-type (cl-form-types:form-type name env)))
    (if (and (optima:match type-form-type
               ((or (list 'eql _)
                    (list 'member _)
                    (list 'equal _))
                t)
               (_
                nil))
             (optima:match name-form-type
               ((or (list 'eql _)
                    (list 'member _)
                    (list 'equal _))
                t)
               (_
                nil)))
        (let ((idx (switch ((second type-form-type)
                            :test (cl:lambda (x y)
                                    (and (cl-type-specifier-p x)
                                         (cl-type-specifier-p y)
                                         (alexandria:type= x y))))
                     ('single-float 0)
                     ('double-float 1)
                     ('(signed-byte 64) 3)
                     ('(signed-byte 32) 4)
                     ('(signed-byte 16) 5)
                     ('(signed-byte 08) 6)
                     ('(unsigned-byte 64) 7)
                     ('(unsigned-byte 32) 8)
                     ('(unsigned-byte 16) 9)
                     ('(unsigned-byte 08) 10)
                     ('fixnum 11)
                     (t 2))))
          (list 'function
                (or (nth idx (gethash (second name-form-type) *translation-table*))
                    (error "No CFUNCTION known for ~S for ~S"
                           (second name-form-type) (second type-form-type)))))
        form)))

(declaim (ftype (function ((or list symbol)) (unsigned-byte 4)) c-size))
(defun c-size (type)
  (declare (optimize speed))
  (eswitch (type :test #'alexandria:type=)
    ('single-float 4)
    ('double-float 8)
    ('fixnum 8)
    ('(signed-byte 64) 8)
    ('(signed-byte 32) 4)
    ('(signed-byte 16) 2)
    ('(signed-byte 08) 1)
    ('(unsigned-byte 64) 8)
    ('(unsigned-byte 32) 4)
    ('(unsigned-byte 16) 2)
    ('(unsigned-byte 08) 1)))
(define-compiler-macro c-size (&whole form type-form &environment env)
  (declare (optimize debug))
  (let ((form-type (cl-form-types:form-type type-form env)))
    ;; TODO: Use CTYPE to normalize types?
    (if (and (listp form-type)
             (null (cddr form-type))
             (or (member (first form-type)
                         '(eql member equal))))
        (switch ((second form-type) :test #'alexandria:type=)
          ('single-float 4)
          ('double-float 8)
          ('fixnum 8)
          ('(signed-byte 64) 8)
          ('(signed-byte 32) 4)
          ('(signed-byte 16) 2)
          ('(signed-byte 08) 1)
          ('(unsigned-byte 64) 8)
          ('(unsigned-byte 32) 4)
          ('(unsigned-byte 16) 2)
          ('(unsigned-byte 08) 1)
          (t form))
        form)))

(declaim (ftype (function ((or list symbol)) keyword) c-type))
(defun c-type (type)
  (declare (optimize speed))
  (eswitch (type :test #'alexandria:type=)
    ('single-float :float)
    ('double-float :double)
    ('(signed-byte 64) :int64)
    ('(signed-byte 32) :int32)
    ('(signed-byte 16) :int16)
    ('(signed-byte 08)  :int8)
    ('(unsigned-byte 64) :uint64)
    ('(unsigned-byte 32) :uint32)
    ('(unsigned-byte 16) :uint16)
    ('(unsigned-byte 08) :uint8)))
(define-compiler-macro c-type (&whole form type-form &environment env)
  (let ((form-type (cl-form-types:form-type type-form env)))
    ;; TODO: Use CTYPE to normalize types?
    (if (and (listp form-type)
             (null (cddr form-type))
             (or (member (first form-type)
                         '(eql member equal))))
        (switch ((second form-type) :test #'alexandria:type=)
          ('single-float :float)
          ('double-float :double)
          ('(signed-byte 64) :int64)
          ('(signed-byte 32) :int32)
          ('(signed-byte 16) :int16)
          ('(signed-byte 08)  :int8)
          ('(unsigned-byte 64) :uint64)
          ('(unsigned-byte 32) :uint32)
          ('(unsigned-byte 16) :uint16)
          ('(unsigned-byte 08) :uint8)
          (t form))
        form)))
