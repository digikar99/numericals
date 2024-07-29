(peltadot/utils:defpackage :dense-numericals/random
  (:use :peltadot)
  (:shadowing-import-exported-symbols :dense-numericals/more-utils)
  (:documentation "Contains functions that provide arrays of random numbers sampled from various distributions")
  (:export ;; real / float types
   #:seed

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

(numericals/common:export-all-external-symbols :dense-numericals/random :dense-numericals)

(in-package :dense-numericals/random)

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)

(pushnew-c-translations
 '((gaussian  ceigen-lite:snormal ceigen-lite:dnormal)
   (beta      ceigen-lite:sbeta   ceigen-lite:dbeta)
   (chisquare ceigen-lite:schi-squared   ceigen-lite:dchi-squared)))
