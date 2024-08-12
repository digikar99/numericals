(peltadot/utils:defpackage :dense-numericals/statistics
  (:use :peltadot :dense-numericals/utils/impl)
  (:shadowing-import-exported-symbols :dense-numericals/utils)
  (:export #:mean
           #:variance
           #:std)
  (:local-nicknames (:nu :dense-numericals)))

(numericals/common:export-all-external-symbols :dense-numericals/statistics :dense-numericals)

(in-package :dense-numericals/statistics)

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)
