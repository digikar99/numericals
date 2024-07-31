(peltadot/utils:defpackage :numericals/statistics
  (:use :peltadot)
  (:shadowing-import-exported-symbols :numericals/utils)
  (:export #:mean
           #:variance
           #:std)
  (:local-nicknames (:nu :numericals)))

(numericals/common:export-all-external-symbols :numericals/statistics :numericals)

(in-package :numericals/statistics)

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)
