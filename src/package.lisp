(cl:in-package :cl)

(defpackage :numericals
  (:import-from :trivial-coerce #:coerce)
  (:export

   #:*multithreaded-threshold*
   #:+optimized-types+
   #:*default-float-format*
   #:*inline-with-multithreading*
   #:*broadcast-automatically*

   #:*array-element-type*
   #:*array-element-type-alist*

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
   :aref
   :concatenate
   #:array=
   #:zeros
   #:zeros-like
   #:ones
   #:ones-like
   #:rand
   #:rand-like
   #:shape
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

   #:exp
   #:log
   #:expt

   :sqrt

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
   #:max
   #:min
   #:vdot
   #:reshape
   
   #:shape

   #:+
   #:two-arg-+
   #:*
   #:two-arg-*
   #:-
   #:two-arg--
   #:/
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
   #:two-arg->=

   ))

(uiop:define-package :numericals.internals
    (:mix :cl :alexandria :iterate :introspect-environment
          :polymorphic-functions)
  (:import-from :numericals
                #:maybe-form-not-constant-error
                #:*type*
                #:*lookup-type-at-compile-time*
                #:*array-element-type*)
  (:import-from :numericals.common
                #:type-zero
                #:type-min
                #:type-max)
  (:import-from :polymorphic-functions
                #:optim-speed
                #:env))


(cl:in-package :numericals.internals)

#+sbcl
`(declaim (sb-ext:maybe-inline
           ,@(iter (for s in-package :numericals external-only t)
                   (collect s))))

;; FIXME: Avoid TPLN
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :nu :numericals))

(5am:def-suite :numericals)

(setq numericals.common:*compiler-package* :numericals.internals
      numericals.common:*suite-name* :numericals)

(5am:def-suite nu::array :in :numericals)

(pushnew (cons (cons `(and (eql :auto)
                           (polymorphic-functions.extended-types:subtypep real))
                     nil)
               t)
         polymorphic-functions.extended-types:*subtypep-alist*
         :test #'equal)

(pushnew (cons (cons `(polymorphic-functions.extended-types:subtypep real)
                     `(eql :auto))
               nil)
         polymorphic-functions.extended-types:*subtypep-alist*
         :test #'equal)

(pushnew (cons (cons `(eql :auto)
                     `(polymorphic-functions.extended-types:subtypep real))
               nil)
         polymorphic-functions.extended-types:*subtypep-alist*
         :test #'equal)
