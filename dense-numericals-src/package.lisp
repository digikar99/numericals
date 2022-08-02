(cl:in-package :cl)

(defpackage :dense-numericals-lite
  (:documentation "Functionality in this package is available with pure lisp.")
  (:use :dense-arrays-plus-lite)
  (:export #:array
           #:array=
           #:simple-array

           #:aref
           #:row-major-aref
           #:do-arrays
           #:broadcast-array
           #:macro-map-array

           #:array-dimension
           #:array-dimensions

           #:array-element-type

           #:asarray
           #:zeros
           #:ones
           #:eye
           #:rand
           #:full
           #:zeros-like
           #:ones-like
           #:rand-like
           #:full-like
           #:reshape
           #:transpose

           #:*dense-array-class*
           #:*array-element-type*
           #:*array-element-type-alist*

           ;; TODO
           ))

(uiop:define-package :dense-numericals
  (:use)
  (:documentation "Depends on foreign-functions")
  (:mix :dense-numericals-lite)
  (:import-from :trivial-coerce
                #:coerce)
  (:reexport :dense-numericals-lite)
  (:export #:astype
           
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
                :defpolymorph
                :suboptimal-polymorph-note
                :env
                :optim-speed
                :optim-debug
                :defpolymorph-compiler-macro
                #:pflet
                #:pflet*)
  (:import-from :cl-form-types
                #:constant-form-value)
  (:import-from :dense-arrays
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

(defpackage :dense-numericals.linalg
  (:export #:multidot
           #:matrix-power

           #:cholesky
           #:qr
           #:svd

           #:eig
           #:eigh
           #:eigvals
           #:eigvalsh

           #:norm
           #:cond
           #:det
           #:matrix-rank
           #:slogdet

           #:solve
           #:tensorsolve
           #:lstsq
           #:inv
           #:pinv
           #:tensorinv

           #:axpy))

(in-package :dense-numericals.impl)

;; FIXME: Avoid TPLN
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :nu :dense-numericals)
  (trivial-package-local-nicknames:add-package-local-nickname :la :dense-numericals.linalg))

(5am:def-suite :dense-numericals)

(pushnew (cons (find-package :dense-numericals.impl) 'single-float)
         *array-element-type-alist*
         :test #'equal)

(defun type-parameter-p (symbol) (member symbol '(<type>)))
(pushnew 'type-parameter-p polymorphic-functions:*parametric-type-symbol-predicates*)

(setq numericals.common:*compiler-package* :dense-numericals.impl
      numericals.common:*suite-name* :dense-numericals)

(5am:def-suite nu::array :in :dense-numericals)
