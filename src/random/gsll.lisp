(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(cffi:defcvar (+gsl-rng-default+ "gsl_rng_default") :pointer)
(defvar +gsl-rng+ (cffi:foreign-funcall "gsl_rng_alloc" :pointer +gsl-rng-default+ :pointer))
