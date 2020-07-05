(in-package :numericals.sbcl)

(macro-when (member :avx2 sb-impl:+internal-features+)

  (eval-when (:compile-toplevel :load-toplevel :execute)

    (defknown (%single=)
        ((simd-pack single-float) (simd-pack single-float))
        (simd-pack-256 double-float)
        (movable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (%single=)
      (:translate %single=)
      (:policy :fast-safe)
      (:args (x :scs (single-sse-reg))
             (y :scs (single-sse-reg)))
      (:arg-types simd-pack-single
                  simd-pack-single)
      (:temporary (:scs (double-avx2-reg)) a b)
      (:results (r :scs (double-avx2-reg)))
      (:result-types simd-pack-256-double)
      (:generator 1
                  (inst vcvtps2pd a x)
                  (inst vcvtps2pd b y)
                  (inst vaddpd r a b))))

  (defun simd-single-= (a b)
    (declare (optimize (speed 3)))
    (%single= a b)))

(defparameter a (%make-simd-pack-single 1.0 2.0 3.0 4.0))
(defparameter b (%make-simd-pack-single 1.1 2.0 3.0 4.0))

(defparameter bool-array (make-array 16 :initial-element t))
(simd-single-1d-aref bool-array 0)

(defparameter x (%make-simd-pack-256-single 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
(defparameter y (%make-simd-pack-256-single 1.1 2.0 3.0 4.0 5.0 6.0 7.0 8.0))

(macro-when (member :avx2 sb-impl:+internal-features+)

  (eval-when (:compile-toplevel :load-toplevel :execute)

    (defknown (%s2d)
        ((simd-pack single-float))
        (simd-pack-256 double-float)
        (movable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (%s2d)
      (:translate %s2d)
      (:policy :fast-safe)
      (:args (x :scs (single-sse-reg)))
      (:arg-types simd-pack-single)
      ;; A bug in pre SBCL-2.0.5 doesn't pick up on the type of r
      ;; allocating a xmm register to r instead of the ymm.
      (:results (r :scs (sb-vm::double-avx2-reg)))
      (:result-types sb-vm::simd-pack-256-double)
      (:generator 1
                  (inst vcvtps2pd r x))))

  (defun simd-single-to-double (a)
    (declare (optimize (speed 3)))
    (%s2d a)))



;; Convert SIMD-PACK to SIMD-PACK-256; then compare SIMD-PACK-256s and
;; convert to appropriate lisp boolean values t and nil. Even more
;; particularly, it's the 0 that needs to be converted to nil. We are fine
;; with other truthy values.
