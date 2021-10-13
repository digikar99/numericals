(cl:in-package :cl)

(defpackage :dense-numericals-lite
  (:documentation "Functionality in this package is available with pure lisp.")
  (:use :dense-arrays-plus-lite)
  (:export #:array
           #:simple-array

           #:aref
           #:row-major-aref

           #:array-dimension
           #:array-dimensions

           #:array-element-type

           #:asarray
           #:zeros
           #:ones
           #:rand
           #:zeros-like
           #:ones-like
           #:rand-like

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
  (:export #:*multithreaded-threshold*
           #:+optimized-types+
           #:*default-float-format*
           #:*inline-with-multithreading*

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

           #:exp
           #:log
           #:expt

           #:sqrt

           #:ffloor
           #:fceiling
           #:fround
           #:abs
           #:ftruncate

           #:copy
           #:coerce
           #:concat
           #:matmul
           #:two-arg-matmul
           #:max
           #:min
           #:vdot
           #:sum
           #:shape

           #:+
           #:two-arg-+
           #:*
           #:two-arg-*
           #:-
           #:one-arg--
           #:two-arg--
           #:/
           #:one-arg-/
           #:two-arg-/

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
  (:mix :dense-arrays-plus-lite :cl :alexandria :iterate)
  (:import-from :polymorphic-functions
                :define-polymorphic-function
                :defpolymorph
                :env
                :optim-speed
                :optim-debug
                :defpolymorph-compiler-macro)
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
  (:export #:axpy))

(in-package :dense-numericals.impl)

(loop :for (nick package) :in '((:dn    :dense-numericals))
      :do (trivial-package-local-nicknames:add-package-local-nickname nick package))

(defvar *src-dir* (asdf:component-pathname (asdf:find-system "dense-numericals")))

(5am:def-suite :dense-numericals)

(push (cons (find-package :dense-numericals.impl) 'single-float)
      *array-element-type-alist*)
