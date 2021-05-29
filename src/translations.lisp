(in-package :numericals.internals)


(declaim (type hash-table *translation-table*))
(defparameter *translation-table*
  (alist-hash-table '((nu:sin bmas:ssin bmas:dsin cl:sin)
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
                      (nu:abs bmas:sfabs bmas:dfabs cl:abs)

                      (nu:log       bmas:slog   bmas:dlog   cl:log)
                      (nu:fround    bmas:sround bmas:dround cl:fround)
                      (nu:ftruncate bmas:strunc bmas:dtrunc cl:ftruncate)
                      (nu:ffloor    bmas:sfloor bmas:dfloor cl:ffloor)
                      (nu:fceiling  bmas:sceil  bmas:dceil  cl:fceiling)

                      (nu::atan2 bmas:satan2 bmas::datan2 cl:atan)
                      (nu:expt   bmas:spow   bmas:dpow    cl:expt)

                      (nu:two-arg-+ bmas:sadd bmas:dadd cl:+)
                      (nu:two-arg-- bmas:ssub bmas:dsub cl:-)
                      (nu:two-arg-* bmas:smul bmas:dmul cl:*)
                      (nu:two-arg-/ bmas:sdiv bmas:ddiv cl:/)

                      (nu:two-arg-<  bmas:slt  bmas:dlt  cl:<)
                      (nu:two-arg-<= bmas:sle  bmas:dle  cl:<=)
                      (nu:two-arg-=  bmas:seq  bmas:deq  cl:=)
                      (nu:two-arg-/= bmas:sneq bmas:dneq cl:/=)
                      (nu:two-arg->= bmas:sge  bmas:dge  cl:>=)
                      (nu:two-arg->  bmas:sgt  bmas:dgt  cl:>)

                      (nu:copy cblas:scopy cblas:dcopy))))

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

