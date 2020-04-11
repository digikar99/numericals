(in-package :numericals.sbcl)

;; How do we check for the presence of AVX2 support given that it's not a part of +features+
;; > In this way.
(macro-when (member :avx2 sb-impl:+internal-features+)

  (eval-when (:compile-toplevel :load-toplevel :execute)    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; d4-ref | d4-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defknown (d4-ref) ((simple-array double-float (*))
                        (integer 0 #.most-positive-fixnum))
        (simd-pack-256 double-float)
        (movable foldable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (d4-ref)
      (:translate d4-ref)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg)))
      (:arg-types simple-array-double-float
                  tagged-num)
      (:results (dest :scs (double-avx2-reg)))
      (:result-types simd-pack-256-double)
      (:policy :fast-safe)
      (:generator 1
                  (inst vmovups
                        dest
                        (float-ref-ea v i 0 0
                                      :scale (ash 8 (- n-fixnum-tag-bits))))))

    (defknown (d4-set) ((simple-array double-float (*))
                        (integer 0 #.most-positive-fixnum)
                        (simd-pack-256 double-float))
        (simd-pack-256 double-float)
        (always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (d4-set)
      (:translate d4-set)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg))
             (x :scs (double-avx2-reg)))
      (:arg-types simple-array-double-float
                  tagged-num
                  simd-pack-256-double)
      (:policy :fast-safe)
      (:generator 1
                  (inst vmovups
                        (float-ref-ea v i 0 0
                                      :scale (ash 8 (- n-fixnum-tag-bits)))
                        x)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;; s8-ref | s8-set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (defknown (s8-ref) ((simple-array single-float (*))
                        (integer 0 #.most-positive-fixnum))
        (simd-pack-256 single-float)
        (movable foldable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (s8-ref)
      (:translate s8-ref)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg)))
      (:arg-types simple-array-single-float
                  tagged-num)
      (:results (dest :scs (single-avx2-reg)))
      (:result-types simd-pack-256-single)
      (:policy :fast-safe)
      (:generator 1
                  (inst vmovups
                        dest
                        (float-ref-ea v i 0 0
                                      :scale (ash 4 (- n-fixnum-tag-bits))))))
    (defknown (s8-set) ((simple-array single-float (*))
                        (integer 0 #.most-positive-fixnum)
                        (simd-pack-256 single-float))
        (simd-pack-256 single-float)
        (always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (s8-set)
      (:translate s8-set)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg))
             (x :scs (single-avx2-reg)))
      (:arg-types simple-array-single-float
                  tagged-num
                  simd-pack-256-single)
      (:results (result :scs (single-avx2-reg)))
      (:result-types simd-pack-256-single)
      (:policy :fast-safe)
      (:generator 1
                  (inst vmovups
                        (float-ref-ea v i 0 0
                                      :scale (ash 4 (- n-fixnum-tag-bits)))
                        x)
                  (move result x))))

  (defconstant +simd-double-1d-aref-stride+ 4)
  (defconstant +simd-single-1d-aref-stride+ 8)

  (defun simd-single-1d-aref (simple-1d-array index)
    "Access I, I+1, ..., I+7 elements of VEC in the form of (SB-EXT:SIMD-PACK-256 SINGLE-FLOAT)."
    (declare (optimize (speed 3))
             (type (simple-array single-float (*)) simple-1d-array)
             (type fixnum index))
    (s8-ref simple-1d-array index))

  (defun (setf simd-single-1d-aref) (new-value simple-1d-array index)
    (declare (optimize (speed 3))
             (type (simple-array single-float (*)) simple-1d-array)
             (type fixnum index))
    (s8-set simple-1d-array index new-value))

  (defun simd-double-1d-aref (simple-1d-array index)
    "Access I, I+1, I+2, I+3 elements of VEC in the form of (SB-EXT:SIMD-PACK-256 DOUBLE-FLOAT)."
    (declare (optimize (speed 3))
             (type (simple-array double-float (*)) simple-1d-array)
             (type fixnum index))
    (d4-ref simple-1d-array index))

  (defun (setf simd-double-1d-aref) (new-value simple-1d-array index)
    (declare (optimize (speed 3))
             (type (simple-array double-float (*)) simple-1d-array)
             (type fixnum index))
    (d4-set simple-1d-array index new-value)))
