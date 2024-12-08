(peltadot/utils:defpackage :numericals/transcendental
  (:shadowing-import-exported-symbols :numericals/utils)
  (:reexport :numericals/utils)
  (:use)
  (:export

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

   #:sqrt))

(numericals/common:export-all-external-symbols :numericals/transcendental :numericals)

(peltadot/utils:defpackage :numericals/transcendental/impl
  (:use #:peltadot
        #:numericals/utils/impl
        #:numericals/basic-math/impl)
  (:import-from #:alexandria
                #:lastcar
                #:with-gensyms
                #:make-gensym-list)
  (:shadowing-import-from #:numericals/basic-math #:copy)
  (:shadowing-import-exported-symbols #:numericals/utils)
  (:local-nicknames (:nu :numericals/transcendental)
                    (:form-types :peltadot/form-types)
                    (:polymorphic-functions :peltadot/polymorphic-functions)))

(in-package :numericals/transcendental/impl)

(pushnew-c-translations
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
   (nu:log bmas:slog bmas:dlog cl:log)

   (nu::atan2 bmas:satan2 bmas::datan2 cl:atan)
   (nu:expt   bmas:spow   bmas:dpow    cl:expt)
   (nu:sqrt   bmas:ssqrt  bmas:dsqrt   cl:sqrt)))
