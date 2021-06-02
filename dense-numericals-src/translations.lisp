(in-package :dense-numericals.impl)


(declaim (type hash-table *translation-table*))
(defparameter *translation-table*
  (alist-hash-table '((dn:sin bmas:ssin bmas:dsin cl:sin)
                      (dn:cos bmas:scos bmas:dcos cl:cos)
                      (dn:tan bmas:stan bmas:dtan cl:tan)

                      (dn:asin bmas:sasin bmas:dasin cl:asin)
                      (dn:acos bmas:sacos bmas:dacos cl:acos)
                      (dn:atan bmas:satan bmas:datan cl:atan)

                      (dn:sinh bmas:ssinh bmas:dsinh cl:sinh)
                      (dn:cosh bmas:scosh bmas:dcosh cl:cosh)
                      (dn:tanh bmas:stanh bmas:dtanh cl:tanh)

                      (dn:asinh bmas:sasinh bmas:dasinh cl:asinh)
                      (dn:acosh bmas:sacosh bmas:dacosh cl:acosh)
                      (dn:atanh bmas:satanh bmas:datanh cl:atanh)

                      (dn:exp bmas:sexp bmas:dexp cl:exp)
                      (dn:abs bmas:sfabs bmas:dfabs cl:abs)

                      (dn:log       bmas:slog   bmas:dlog   cl:log)
                      (dn:fround    bmas:sround bmas:dround cl:fround)
                      (dn:ftruncate bmas:strunc bmas:dtrunc cl:ftruncate)
                      (dn:ffloor    bmas:sfloor bmas:dfloor cl:ffloor)
                      (dn:fceiling  bmas:sceil  bmas:dceil  cl:fceiling)

                      (dn::atan2 bmas:satan2 bmas::datan2 cl:atan)
                      (dn:expt   bmas:spow   bmas:dpow    cl:expt)

                      (dn:two-arg-+ bmas:sadd bmas:dadd cl:+)
                      (dn:two-arg-- bmas:ssub bmas:dsub cl:-)
                      (dn:two-arg-* bmas:smul bmas:dmul cl:*)
                      (dn:two-arg-/ bmas:sdiv bmas:ddiv cl:/)

                      (dn:two-arg-<  bmas:slt  bmas:dlt  cl:<)
                      (dn:two-arg-<= bmas:sle  bmas:dle  cl:<=)
                      (dn:two-arg-=  bmas:seq  bmas:deq  cl:=)
                      (dn:two-arg-/= bmas:sneq bmas:dneq cl:/=)
                      (dn:two-arg->= bmas:sge  bmas:dge  cl:>=)
                      (dn:two-arg->  bmas:sgt  bmas:dgt  cl:>))))

(macrolet ((def (fn-name index)
             `(progn
                (declaim (inline ,fn-name))
                (defun ,fn-name (name)
                  (declare (optimize speed)
                           (type symbol name))
                  (nth ,index (gethash name *translation-table*)))
                (define-compiler-macro ,fn-name (&whole form name &environment env)
                  (let ((form-type (cl-form-types:form-type name env)))
                    ;; TODO: Use CTYPE to normalize types?
                    (if (and (listp form-type)
                             (null (cddr form-type))
                             (or (eq 'eql (first form-type))
                                 (eq 'member (first form-type))))
                        (list 'function
                              (nth ,index (gethash (second form-type) *translation-table*)))
                        form))))))
  (def single-float-c-name 0)
  (def double-float-c-name 1)
  (def cl-name 2))

