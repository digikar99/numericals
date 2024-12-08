(peltadot/utils:defpackage :numericals/basic-math
  (:use)
  (:shadowing-import-exported-symbols :numericals/utils)
  (:reexport :numericals/utils)
  (:documentation "Vectorized basic math functionality for CL:ARRAY")
  (:export

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
   #:astype
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
   #:reshape

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

(numericals/common:export-all-external-symbols :numericals/basic-math :numericals)

(peltadot/utils:defpackage :numericals/basic-math/impl
  (:use :peltadot :numericals/utils/impl)
  (:shadowing-import-exported-symbols :numericals/utils)
  (:import-from #:alexandria
                #:define-constant
                #:with-gensyms
                #:make-gensym-list)
  (:import-from #:numericals/utils/impl
                #:split-at-keywords)
  (:import-from #:introspect-environment
                #:constant-form-value)
  (:local-nicknames (:nu :numericals/basic-math)
                    (:traits :peltadot-traits-library))
  (:export #:one-arg-fn/float
           #:+one-arg-fn-float-doc+
           #:define-numericals-one-arg-test
           #:define-numericals-two-arg-test))

(in-package :numericals/basic-math/impl)

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)

(pushnew-c-translations
 '((nu:abs bmas:sfabs bmas:dfabs cl:abs bmas:i64abs bmas:i32abs bmas:i16abs bmas:i8abs
                                        bmas:i64abs bmas:i32abs bmas:i16abs bmas:i8abs
                                        bmas:i64abs)

   (nu:fround    bmas:sround bmas:dround cl:fround)
   (nu:ftruncate bmas:strunc bmas:dtrunc cl:ftruncate)
   (nu:ffloor    bmas:sfloor bmas:dfloor cl:ffloor)
   (nu:fceiling  bmas:sceil  bmas:dceil  cl:fceiling)

   (nu:copy bmas:scopy bmas:dcopy cl:identity
            bmas:i64copy bmas:i32copy bmas:i16copy bmas:i8copy
            bmas:i64copy bmas:i32copy bmas:i16copy bmas:i8copy)
   (nu::to-single-float bmas:scopy bmas:cast-ds nil
                        bmas:cast-i64s bmas:cast-i32s bmas:cast-i16s bmas:cast-i8s
                        bmas:cast-u64s bmas:cast-u32s bmas:cast-u16s bmas:cast-u8s)
   (nu::to-double-float bmas:cast-sd bmas:dcopy nil
                        bmas:cast-i64d bmas:cast-i32d bmas:cast-i16d bmas:cast-i8d
                        bmas:cast-u64d bmas:cast-u32d bmas:cast-u16d bmas:cast-u8d)

   (nu:add      bmas:sadd bmas:dadd cl:+ bmas:i64add bmas:i32add bmas:i16add bmas:i8add
                                         bmas:i64add bmas:i32add bmas:i16add bmas:i8add
                                         bmas:i64add)
   (nu:subtract bmas:ssub bmas:dsub cl:- bmas:i64sub bmas:i32sub bmas:i16sub bmas:i8sub
                                         bmas:i64sub bmas:i32sub bmas:i16sub bmas:i8sub
                                         bmas:i64sub)
   (nu:multiply bmas:smul bmas:dmul cl:* bmas:i64mul bmas:i32mul bmas:i16mul bmas:i8mul
                                         bmas:u64mul bmas:u32mul bmas:u16mul bmas:u8mul
                                         fixnum-mul)
   (nu:divide   bmas:sdiv bmas:ddiv cl:/)

   (nu:two-arg-max bmas:smax bmas:dmax cl:max bmas:i64max bmas:i32max bmas:i16max bmas:i8max
                                       bmas:u64max bmas:u32max bmas:u16max bmas:u8max
                                       bmas:i64max)
   (nu:two-arg-min bmas:smin bmas:dmin cl:min bmas:i64min bmas:i32min bmas:i16min bmas:i8min
                                       bmas:u64min bmas:u32min bmas:u16min bmas:u8min
                                       bmas:i64min)

   (nu:sum bmas:ssum bmas:dsum cl:+ bmas:i64sum bmas:i32sum bmas:i16sum bmas:i8sum
                                    bmas:i64sum bmas:i32sum bmas:i16sum bmas:i8sum
                                    fixnum-sum)
   (nu:maximum bmas:shmax bmas:dhmax cl:max bmas:i64hmax bmas:i32hmax bmas:i16hmax bmas:i8hmax
                                            bmas:u64hmax bmas:u32hmax bmas:u16hmax bmas:u8hmax
                                            fixnum-hmax)
   (nu:minimum bmas:shmin bmas:dhmin cl:min bmas:i64hmin bmas:i32hmin bmas:i16hmin bmas:i8hmin
                                            bmas:u64hmin bmas:u32hmin bmas:u16hmin bmas:u8hmin
                                            fixnum-hmin)

   (nu:arg-maximum bmas:shimax bmas:dhimax cl:nil bmas:i64himax bmas:i32himax bmas:i16himax bmas:i8himax
                                                  bmas:u64himax bmas:u32himax bmas:u16himax bmas:u8himax
                                                  bmas:i64himax)
   (nu:arg-minimum bmas:shimin bmas:dhimin cl:nil bmas:i64himin bmas:i32himin bmas:i16himin bmas:i8himin
                                                  bmas:u64himin bmas:u32himin bmas:u16himin bmas:u8himin
                                                  bmas:i64himin)

   (nu:two-arg-logand nil nil cl:logand bmas:i64and bmas:i32and bmas:i16and bmas:i8and
                                        bmas:u64and bmas:u32and bmas:u16and bmas:u8and
                                        bmas:i64and)
   (nu:two-arg-logior nil nil cl:logior bmas:i64or  bmas:i32or  bmas:i16or  bmas:i8or
                                        bmas:u64or  bmas:u32or  bmas:u16or  bmas:u8or
                                        bmas:i64or)
   (nu:two-arg-logxor nil nil cl:logxor bmas:i64xor bmas:i32xor bmas:i16xor bmas:i8xor
                                        bmas:u64xor bmas:u32xor bmas:u16xor bmas:u8xor
                                        bmas:i64xor)
   (nu:lognot         nil nil cl:lognot bmas:i64not bmas:i32not bmas:i16not bmas:i8not
                                        bmas:u64not bmas:u32not bmas:u16not bmas:u8not
                                        bmas:i64not)
   (nu:logandc1 nil nil cl:logandc1 bmas:i64andnot bmas:i32andnot bmas:i16andnot bmas:i8andnot
                                    bmas:u64andnot bmas:u32andnot bmas:u16andnot bmas:u8andnot
                                    bmas:i64andnot)

   (nu:two-arg-<  bmas:slt  bmas:dlt  cl:<  bmas:i64lt  bmas:i32lt  bmas:i16lt  bmas:i8lt
                                            bmas:u64lt  bmas:u32lt  bmas:u16lt  bmas:u8lt
                                            bmas:i64lt)
   (nu:two-arg-<= bmas:sle  bmas:dle  cl:<= bmas:i64le  bmas:i32le  bmas:i16le  bmas:i8le
                                            bmas:u64le  bmas:u32le  bmas:u16le  bmas:u8le
                                            bmas:i64le)
   (nu:two-arg-=  bmas:seq  bmas:deq  cl:=  bmas:i64eq  bmas:i32eq  bmas:i16eq  bmas:i8eq
                                            bmas:u64eq  bmas:u32eq  bmas:u16eq  bmas:u8eq
                                            bmas:i64eq)
   (nu:two-arg-/= bmas:sneq bmas:dneq cl:/= bmas:i64neq bmas:i32neq bmas:i16neq bmas:i8neq
                                            bmas:u64neq bmas:u32neq bmas:u16neq bmas:u8neq
                                            bmas:i64neq)
   (nu:two-arg->= bmas:sge  bmas:dge  cl:>= bmas:i64ge  bmas:i32ge  bmas:i16ge  bmas:i8ge
                                            bmas:u64ge  bmas:u32ge  bmas:u16ge  bmas:u8ge
                                            bmas:i64ge)
   (nu:two-arg->  bmas:sgt  bmas:dgt  cl:>  bmas:i64gt  bmas:i32gt  bmas:i16gt  bmas:i8gt
                                            bmas:u64gt  bmas:u32gt  bmas:u16gt  bmas:u8gt
                                            bmas:i64gt)

   ;; (nu:ash-right nil nil nil bmas:i64sra bmas:i32sra bmas:i16sra bmas:i8sra
   ;;                           bmas:u64srl bmas:u32srl bmas:u16srl bmas:u8srl)
   ))
