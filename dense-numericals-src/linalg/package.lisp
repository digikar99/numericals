(peltadot/utils:defpackage :dense-numericals/linalg
  (:use :peltadot :dense-numericals/utils/impl)
  (:shadowing-import-exported-symbols :dense-numericals/utils)
  (:export #:vdot

           #:cholesky
           #:svd
           #:lu
           #:qr

           #:eigvals
           #:eigvecs

           #:norm2
           #:det
           #:rank
           ;; #:slogdet ; unimplemented by eigen

           #:solve
           ;; #:tensorsolve
           ;; #:lstsq ; This is subsumed by #:SOLVE
           #:inv
           #:pinv
           ;; #:tensorinv
           ))

(numericals/common:export-all-external-symbols :dense-numericals/linalg :dense-numericals)

(in-package :dense-numericals/linalg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (notinline array-dimension)))

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)

(pushnew-c-translations
 '((vdot bmas:sdot bmas:ddot
    cl:*
    bmas:i64dot bmas:i32dot bmas:i16dot bmas:i8dot
    bmas:i64dot bmas:i32dot bmas:i16dot bmas:i8dot
    fixnum-dot)

   (matmul ceigen-lite:smatmul ceigen-lite:dmatmul)
   ;; FIXME: partial-piv-lu is only valid for square matrices
   (solve ceigen-lite:shouseholder-qr
    ceigen-lite:dhouseholder-qr)
   (solve/square ceigen-lite:spartial-piv-lu
    ceigen-lite:dpartial-piv-lu)

   (norm2/vector ceigen-lite:snorm2v
    ceigen-lite:dnorm2v)
   (norm2/matrix ceigen-lite:snorm2m
    ceigen-lite:dnorm2m)
   (rank ceigen-lite:srank ceigen-lite:drank)
   (det ceigen-lite:sdeterminant
    ceigen-lite:ddeterminant)
   (inv ceigen-lite:sinverse
    ceigen-lite:dinverse)
   (pinv ceigen-lite:spinverse
    ceigen-lite:dpinverse)
   (cholesky ceigen-lite:scholesky
    ceigen-lite:dcholesky)

   (qr ceigen-lite:sqr ceigen-lite:dqr)
   (lu ceigen-lite:slu ceigen-lite:dlu)
   (svd ceigen-lite:ssvd ceigen-lite:dsvd)))
