(cl:in-package :cl)

(defpackage :dense-numericals-lite
  (:documentation "Functionality in this package is available with pure lisp.")
  (:use)
  (:export))

(uiop:define-package :dense-numericals
  (:use)
  (:documentation "Depends on foreign-functions")
  (:mix :dense-numericals-lite)
  (:import-from :trivial-coerce
                #:coerce)
  (:reexport :dense-numericals-lite)
  (:export #:*multithreaded-threshold*
           #:+optimized-types+
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
           #:dot
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
                #:broadcast-compatible-p
                #:default-element-type)
  (:import-from :dense-arrays-plus-lite
                #:split-at-keywords
                #:define-splice-list-fn
                #:dimensions))

(defpackage :dense-numericals.linalg
  (:export #:axpy))

(in-package :dense-numericals.impl)

(loop :for (nick package) :in '((:dn    :dense-numericals))
      :do (trivial-package-local-nicknames:add-package-local-nickname nick package))

(defvar *src-dir* (asdf:component-pathname (asdf:find-system "dense-numericals")))

(5am:def-suite :dense-numericals)

(push (cons (find-package :dense-numericals.impl) 'single-float)
      *array-element-type-alist*)
