(cl:in-package :cl)

(defpackage :numericals
  (:use)
  (:documentation "Vectorized basic math functionality for CL:ARRAY")
  (:import-from :peltadot #:coerce)
  (:import-from :cl
                #:aref
                #:array-rank
                #:array
                #:simple-array
                #:array-total-size
                #:array-element-type
                #:array-dimensions
                #:array-dimension)
  (:export

   #:*multithreaded-threshold*
   #:+optimized-types+
   #:*default-float-format*
   #:*inline-with-multithreading*
   #:*broadcast-automatically*

   #:*array-element-type*
   #:*array-element-type-alist*
   #:*array-layout*

   #:broadcast-array

   #:macro-map-array
   #:do-arrays

   :with-broadcast
   :with-elementwise-operations
   :weop          ; convenient wrapper for with-elementwise-operations
   :with-inline
   :with-array
   :with-arrays*
   :with-constant
   :with-constants
   :maybe-form-not-constant-error
   :def-array
   :array-like-p
   :numericals-array-element-type
   :map-outer
   
   #:astype
   #:asarray

   #:aref
   #:array-rank
   #:array-total-size
   #:array-element-type
   #:array-dimensions
   #:array-dimension
   #:array
   #:simple-array
   #:aref*

   #:concatenate
   #:array=
   #:empty
   #:empty-like
   #:zeros
   #:zeros-like
   #:ones
   #:ones-like
   #:rand
   #:rand-like
   #:full
   #:full-like
   #:fill
   #:shape
   #:eye
   :transpose

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

   :sqrt

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
   #:dot
   #:max
   #:two-arg-max
   #:min
   #:two-arg-min
   #:sum
   #:maximum
   #:minimum
   #:arg-maximum
   #:arg-minimum
   #:vdot
   #:reshape
   #:mean
   #:variance
   #:std
   #:stan

   #:shape

   #:+
   #:add
   #:add!
   #:*
   #:multiply
   #:multiply!
   #:-
   #:subtract
   #:subtract!
   #:/
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
   #:two-arg->=

   ))

(defpackage :numericals.random
  (:use)
  (:documentation "Contains functions that provide arrays of random numbers sampled from various distributions")
  (:export ;; real / float types
   #:gaussian
   #:normal
   #:beta
   #:chisquare
   #:exponential
   #:fisher-f
   #:gamma
   #:log-normal
   #:student-t
   #:uniform
   #:weibull

   ;; integer types
   #:bernoulli
   #:binomial
   #:discrete
   #:geometric
   #:poisson))

(defpackage :numericals.linalg
  (:use)
  (:import-from :numericals
                #:matmul
                #:vdot)
  (:export #:matmul
           #:vdot
           #:inner
           #:outer
           #:copy-matrix
           #:lower
           #:upper
           #:rotate-2d
           #:minimize-lls

           #:multidot
           #:matrix-power

           #:cholesky
           #:svd
           #:lu
           #:qr

           #:eigvals
           #:eigvecs


           #:norm2
           #:cond
           #:det
           #:rank
           ;; #:slogdet ; unimplemented by eigen

           #:solve
           ;; #:tensorsolve
           ;; #:lstsq ; This is subsumed by #:SOLVE
           #:inv
           #:pinv
           ;; #:tensorinv

           #:axpy))

(uiop:define-package :numericals.impl
  (:mix :iterate :peltadot :alexandria :introspect-environment)
  (:use)
  (:import-from :numericals
                #:maybe-form-not-constant-error
                #:*type*
                #:*lookup-type-at-compile-time*
                #:*array-element-type*)
  (:import-from :numericals.common
                #:type-zero
                #:type-min
                #:type-max
                #:inline-or-funcall
                #:fref))


(cl:in-package :numericals.impl)

#+sbcl
`(declaim (sb-ext:maybe-inline
           ,@(iter (for s in-package :numericals external-only t)
                   (collect s))))

;; FIXME: Avoid TPLN
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname
   :polymorphic-functions :peltadot/polymorphic-functions)
  (trivial-package-local-nicknames:add-package-local-nickname
   :cl-form-types :peltadot/form-types)
  (trivial-package-local-nicknames:add-package-local-nickname
   :traits :peltadot-traits-library)
  (trivial-package-local-nicknames:add-package-local-nickname :nu :numericals)
  (trivial-package-local-nicknames:add-package-local-nickname :rand :numericals.random)
  (trivial-package-local-nicknames:add-package-local-nickname :la :numericals.linalg))

(5am:def-suite :numericals)

(defun type-parameter-p (symbol) (member symbol '(<type> <m> <n> <k>)))
(pushnew 'type-parameter-p *parametric-type-symbol-predicates*)

(setq numericals.common:*compiler-package* :numericals.impl
      numericals.common:*suite-name* :numericals)

(5am:def-suite nu::array :in :numericals)
