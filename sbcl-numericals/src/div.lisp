(in-package :sbcl-numericals.internals)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown (%sdiv14)
      (fixnum (simd-pack single-float))
      (simd-pack single-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%sfloor4)
    (:translate %sfloor4)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::any-reg))
           (y :scs (sb-vm::single-sse-reg)))
    (:arg-types sb-vm::simd-pack-single)
    (:results (r :scs (sb-vm::single-sse-reg)))
    (:result-types sb-vm::simd-pack-single)
    (:generator 1 
                (sb-vm::inst roundps r x 1))))
