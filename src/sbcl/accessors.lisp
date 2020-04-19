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
                  (move result x)))

    (defknown (s8b-ref) ((simple-array single-float (*))
                         (integer 0 #.most-positive-fixnum))
        (simd-pack-256 single-float)
        (movable foldable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (s8b-ref)
      (:translate s8b-ref)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg)))
      (:arg-types simple-array-single-float
                  tagged-num)
      (:results (dest :scs (single-avx2-reg)))
      (:result-types simd-pack-256-single)
      (:policy :fast-safe)
      (:generator 1
                  (inst vbroadcastss
                        dest
                        (float-ref-ea v i 0 0
                                      :scale (ash 4 (- n-fixnum-tag-bits))))))

    (defknown (d4b-ref) ((simple-array double-float (*))
                         (integer 0 #.most-positive-fixnum))
        (simd-pack-256 double-float)
        (movable foldable flushable always-translatable)
      :overwrite-fndb-silently t)
    (define-vop (d4b-ref)
      (:translate d4b-ref)
      (:args (v :scs (descriptor-reg))
             (i :scs (any-reg)))
      (:arg-types simple-array-double-float
                  tagged-num)
      (:results (dest :scs (double-avx2-reg)))
      (:result-types simd-pack-256-double)
      (:policy :fast-safe)
      (:generator 1
                  (inst vbroadcastsd
                        dest
                        (float-ref-ea v i 0 0
                                      :scale (ash 8 (- n-fixnum-tag-bits)))))))

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
    (d4-set simple-1d-array index new-value))

  (defun simd-single-broadcast-1d-aref (simple-1d-array index)
    "Access Ith elements of VEC in the form of (SB-EXT:SIMD-PACK-256 SINGLE-FLOAT).
The element is copied 8 times in the SIMD-PACK-256."
    (declare (optimize (speed 3))
             (type (simple-array single-float (*)) simple-1d-array)
             (type fixnum index))
    (s8b-ref simple-1d-array index))
  
  (defun simd-double-broadcast-1d-aref (simple-1d-array index)
    "Access Ith element of VEC in the form of (SB-EXT:SIMD-PACK-256 DOUBLE-FLOAT).
The element is copied 4 times in the SIMD-PACK-256."
    (declare (optimize (speed 3))
             (type (simple-array double-float (*)) simple-1d-array)
             (type fixnum index))
    (d4b-ref simple-1d-array index)))


;;; Does not depend on AVX2
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defknown (d-ref) ((simple-array double-float (*))
                     (integer 0 #.most-positive-fixnum))
      double-float
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d-ref)
    (:translate d-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-double-float
                tagged-num)
    (:results (dest :scs (double-reg)))
    (:result-types double-float)
    (:policy :fast-safe)
    ;; EA below stands for effective address. 
    ;; To understand the role of each arg, see the source code for float-ref-ea at
    ;; https://github.com/sbcl/sbcl/blob/master/src/compiler/x86/array.lisp
    ;; and consider the (EA = base + index * scale + offset * element_size) model.
    (:generator 1
                (inst movq
                      dest
                      (float-ref-ea v i 0 0
                                    :scale (ash 8  (- n-fixnum-tag-bits))))))

  (defknown (d-set) ((simple-array double-float (*))
                     (integer 0 #.most-positive-fixnum)
                     double-float)
      double-float
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (d-set)
    (:translate d-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (double-reg)))
    (:arg-types simple-array-double-float
                tagged-num
                double-float)
    (:policy :fast-safe)
    (:generator 1
                (inst movq
                      (float-ref-ea v i 0 0
                                    :scale (ash 8 (- n-fixnum-tag-bits)))
                      x)))

  (defknown (s-ref) ((simple-array single-float (*))
                     (integer 0 #.most-positive-fixnum))
      single-float
      (movable foldable flushable always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s-ref)
    (:translate s-ref)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg)))
    (:arg-types simple-array-single-float
                tagged-num)
    (:results (dest :scs (single-reg)))
    (:result-types single-float)
    (:policy :fast-safe)
    (:generator 1
                (inst movq
                      dest
                      (float-ref-ea v i 0 0
                                    :scale (ash 4  (- n-fixnum-tag-bits))))))

  (defknown (s-set) ((simple-array single-float (*))
                     (integer 0 #.most-positive-fixnum)
                     single-float)
      single-float
      (always-translatable)
    :overwrite-fndb-silently t)
  (define-vop (s-set)
    (:translate s-set)
    (:args (v :scs (descriptor-reg))
           (i :scs (any-reg))
           (x :scs (single-reg)))
    (:arg-types simple-array-single-float
                tagged-num
                single-float)
    (:policy :fast-safe)
    (:generator 1
                (inst movq
                      (float-ref-ea v i 0 0
                                    :scale (ash 4 (- n-fixnum-tag-bits)))
                      x))))

(defun double-1d-aref (v i)
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) v)
           (type fixnum i))
  (d-ref v i))

(defun (setf double-1d-aref) (new-value v i)
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) v)
           (type fixnum i)
           (type double-float new-value))
  (d-set v i new-value))

(defun single-1d-aref (v i)
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) v)
           (type fixnum i))
  (s-ref v i))

(defun (setf single-1d-aref) (new-value v i)
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) v)
           (type fixnum i)
           (type single-float new-value))
  (s-set v i new-value))
