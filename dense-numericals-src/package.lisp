(cl:in-package :cl)

(uiop:define-package :dense-numericals-lite
  (:documentation "Functionality in this package is available with pure lisp.")
  (:use :dense-arrays-plus-lite)
  (:reexport :dense-arrays-plus-lite)
  (:export #:empty
           #:empty-like)
  (:shadow #:full
           #:full-like
           #:zeros
           #:zeros-like
           #:ones
           #:ones-like
           #:rand
           #:rand-like
           #:empty
           #:empty-like))

(uiop:define-package :dense-numericals
  (:use)
  (:documentation "Depends on foreign-functions")
  (:mix :dense-numericals-lite)
  (:import-from :extensible-optimizing-coerce
                #:coerce)
  (:reexport :dense-numericals-lite)
  (:export #:astype
           #:fill
           
           #:*multithreaded-threshold*
           #:+optimized-types+
           #:*default-float-format*
           #:*inline-with-multithreading*
           #:*broadcast-automatically*

           #:sin
           #:cos
           #:tan
           #:asin
           #:acos
           #:atan
           #:sinh
           #:cosh
           #:tanh
           #:asinh
           #:acosh
           #:atanh

           #:sin!
           #:cos!
           #:tan!
           #:asin!
           #:acos!
           #:atan!
           #:sinh!
           #:cosh!
           #:tanh!
           #:asinh!
           #:acosh!
           #:atanh!

           #:exp
           #:log
           #:expt

           #:exp!
           #:log!
           #:expt!

           #:sqrt

           #:ffloor
           #:fceiling
           #:fround
           #:abs
           #:ftruncate

           #:ffloor!
           #:fceiling!
           #:fround!
           #:abs!
           #:ftruncate!

           #:copy
           #:coerce
           #:concat
           #:matmul
           #:two-arg-matmul
           #:max
           #:two-arg-max
           #:min
           #:two-arg-min

           #:vdot
           #:sum
           #:mean
           #:variance
           #:std
           #:maximum
           #:minimum

           #:shape

           #:+
           #:add
           #:add!
           #:*
           #:multiply
           #:multiply!
           #:-
           ;; #:one-arg--
           #:subtract
           #:subtract!
           #:/
           ;; #:one-arg-/
           #:divide
           #:divide!

           #:logand
           #:two-arg-logand
           #:logior
           #:two-arg-logior
           #:logxor
           #:two-arg-logxor

           #:lognot
           #:logandc1
           #:logandc2
           #:lognand
           #:lognor
           #:logorc1
           #:logorc2
           ;; #:logtest
           ;; #:logbitp
           ;; #:logcount

           #:<
           #:two-arg-<
           #:<=
           #:two-arg-<=
           #:=
           #:two-arg-=
           #:/=
           #:two-arg-/=
           #:>
           #:two-arg->
           #:>=
           #:two-arg->=))

(uiop:define-package :dense-numericals.impl
  #+extensible-compound-types
  (:mix :dense-arrays-plus-lite :extensible-compound-types-cl :cl :alexandria :iterate)
  #-extensible-compound-types
  (:mix :dense-arrays-plus-lite :cl :alexandria :iterate)
  (:use :numericals.common :abstract-arrays)
  (:import-from :polymorphic-functions
                :define-polymorphic-function
                :undefine-polymorphic-function
                :defpolymorph
                :suboptimal-polymorph-note
                :env
                :optim-speed
                :optim-debug
                :defpolymorph-compiler-macro
                #:pflet
                #:pflet*
                #:traverse-tree
                #:cl-type-specifier-p)
  (:import-from :cl-form-types
                #:constant-form-value)
  (:import-from :dense-arrays
                #:dimensions->strides
                #:lm
                #:array-strides
                #:array-offsets
                #:size
                #:the-size
                #:int-index
                #:the-int-index
                #:broadcast-arrays
                #:%broadcast-compatible-p
                #:broadcast-compatible-p
                #:default-element-type)
  (:import-from :dense-arrays-plus-lite
                #:max-type
                #:split-at-keywords
                #:define-splice-list-fn
                #:dimensions
                #:element-type))

;; (defpackage :dense-numericals.linalg
;;   (:export #:multidot
;;            #:matrix-power

;;            #:cholesky
;;            #:qr
;;            #:svd

;;            #:eig
;;            #:eigh
;;            #:eigvals
;;            #:eigvalsh

;;            #:norm
;;            #:cond
;;            #:det
;;            #:matrix-rank
;;            #:slogdet

;;            #:solve
;;            #:tensorsolve
;;            #:lstsq
;;            #:inv
;;            #:pinv
;;            #:tensorinv

;;            #:axpy))

(in-package :dense-numericals.impl)

;; FIXME: Avoid TPLN
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :nu :dense-numericals)
  ;; (trivial-package-local-nicknames:add-package-local-nickname :la :dense-numericals.linalg)
  )

(5am:def-suite :dense-numericals)

(pushnew (cons (find-package :dense-numericals.impl) 'single-float)
         *array-element-type-alist*
         :test #'equal)

(defun type-parameter-p (symbol) (member symbol '(<type>)))
(pushnew 'type-parameter-p polymorphic-functions:*parametric-type-symbol-predicates*)

(setq numericals.common:*compiler-package* :dense-numericals.impl
      numericals.common:*suite-name* :dense-numericals)

(5am:def-suite nu::array :in :dense-numericals)
