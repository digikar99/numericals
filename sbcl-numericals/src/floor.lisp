(in-package :sbcl-numericals.internals)

;;; https://hjlebbink.github.io/x86doc/html/ROUNDPS.html
;;; I was unable to understand the flags there; the following helped
;;; https://stackoverflow.com/questions/37091422/avx-sse-round-floats-down-and-return-vector-of-ints

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown (%sfloor4)
      ((simd-pack single-float))
      (simd-pack single-float)
      (movable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (%sfloor4)
    (:translate %sfloor4)
    (:policy :fast-safe)
    (:args (x :scs (sb-vm::single-sse-reg)))
    (:arg-types sb-vm::simd-pack-single)
    (:results (r :scs (sb-vm::single-sse-reg)))
    (:result-types sb-vm::simd-pack-single)
    (:generator 1 
                (sb-vm::inst roundps r x 1))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defknown (%sfloor8)
;;       ((simd-pack-256 single-float))
;;       (simd-pack-256 single-float)
;;       (movable flushable always-translatable)
;;     :overwrite-fndb-silently t)
;;   (define-vop (%sfloor8)
;;     (:translate %sfloor8)
;;     (:policy :fast-safe)
;;     (:args (x :scs (sb-vm::single-avx2-reg)))
;;     (:arg-types sb-vm::simd-pack-256-single)
;;     (:results (r :scs (sb-vm::single-avx2-reg)))
;;     (:result-types sb-vm::simd-pack-256-single)
;;     (:generator 1 
;;                 (sb-vm::inst vroundps r x 1))))


(defun sfloor4 (x)
  (%sfloor4 x))

;; (defun sfloor8 (x)
;;   (%sfloor8 x))

(defparameter a (%make-simd-pack-single 1.2 1.5 1.7 2.1))
