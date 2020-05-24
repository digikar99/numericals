(in-package :numericals.sbcl)

;; How do we check for the presence of AVX2 support given that it's not a part of +features+
;; > In this way.

(macro-when (member :avx2 sb-impl:+internal-features+)

  (defmacro define-simd-aref (simd-operation-name vop-ref-name vop-set-name
                              scale arg-type vop-arg-type result-type inst)
    (destructuring-bind (simd-pack-type simd-reg simd-pack-type-vop)
        (ecase result-type
          (:double '((simd-pack-256 double-float) double-avx2-reg simd-pack-256-double))
          (:single '((simd-pack-256 single-float) single-avx2-reg simd-pack-256-single))
          (:integer '((simd-pack-256 integer) int-avx2-reg simd-pack-256-int)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defknown (,vop-ref-name) (,arg-type
                                      (integer 0 #.most-positive-fixnum))
               ,simd-pack-type
               (movable foldable flushable always-translatable)
             :overwrite-fndb-silently t)
           (define-vop (,vop-ref-name)
             (:translate ,vop-ref-name)
             (:args (v :scs (descriptor-reg))
                    (i :scs (any-reg)))
             (:arg-types ,vop-arg-type
                         tagged-num)
             (:results (dest :scs (,simd-reg)))
             (:result-types ,simd-pack-type-vop)
             (:policy :fast-safe)
             (:generator 1
                         (inst ,inst
                               dest
                               ;; Originally scale used to be: (ash 8 (- n-fixnum-tag-bits))
                               ;; but I don't see the point of this.
                               ;; Does it depend on the architecture?
                               (float-ref-ea v i 0 0 :scale ,scale)))))

         
         (defun ,simd-operation-name (v i)
           (declare (optimize (speed 3)))
           (,vop-ref-name v i))
         
         ,(when vop-set-name
            `(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defknown (,vop-set-name) (,arg-type
                                            (integer 0 #.most-positive-fixnum)
                                            ,simd-pack-type)
                     ,simd-pack-type
                     (always-translatable)
                   :overwrite-fndb-silently t)
                 (define-vop (,vop-set-name)
                   (:translate ,vop-set-name)
                   (:args (v :scs (descriptor-reg))
                          (i :scs (any-reg))
                          (x :scs (,simd-reg)))
                   (:arg-types ,vop-arg-type
                               tagged-num
                               ,simd-pack-type-vop)
                   (:policy :fast-safe)
                   (:generator 1
                               (inst ,inst
                                     (float-ref-ea v i 0 0
                                                   :scale ,scale)
                                     x))))
               (defun (setf ,simd-operation-name) (new-value v i)
                 (declare (optimize (speed 3)))
                 (,vop-set-name v i new-value)))))))

  (define-simd-aref simd-double-1d-aref d4-ref d4-set 4
                    (simple-array double-float (*))
                    simple-array-double-float
                    :double vmovups)

  (define-simd-aref simd-double-broadcast-1d-aref d4b-ref nil 4
                    (simple-array double-float (*))
                    simple-array-double-float
                    :double vbroadcastsd)

  (define-simd-aref simd-single-1d-aref s8-ref s8-set 2
                    (simple-array single-float (*))
                    simple-array-single-float
                    :single vmovups)

  (define-simd-aref simd-single-broadcast-1d-aref s8b-ref nil 2
                    (simple-array single-float (*))
                    simple-array-single-float
                    :single vbroadcastss)

  (define-simd-aref simd-fixnum-1d-aref f4-ref f4-set 4
                    (simple-array fixnum (*))
                    simple-array-fixnum
                    :single vmovups)

  ;; (define-simd-aref simd-fixnum-broadcast-1d-aref f4b-ref f4b-set 4
  ;;                   (simple-array fixnum (*))
  ;;                   simple-array-fixnum
  ;;                   :single vpbroadcast)

  (define-simd-aref simd-svref sv4-ref sv4-set 4
                    simple-vector
                    simple-vector
                    :integer vmovups)

  (defconstant +simd-double-1d-aref-stride+ 4)
  (defconstant +simd-single-1d-aref-stride+ 8)
  (defconstant +simd-fixnum-1d-aref-stride+ 4)
  (defconstant +simd-svref-stride+ 4))

;;; Does not depend on AVX2
(defun double-1d-aref (v i)
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) v)
           (type fixnum i))
  (sb-kernel:hairy-data-vector-ref v i))

(defun (setf double-1d-aref) (new-value v i)
  (declare (optimize (speed 3))
           (type (simple-array double-float (*)) v)
           (type fixnum i)
           (type double-float new-value))
  (sb-kernel:hairy-data-vector-set v i new-value))

(defun single-1d-aref (v i)
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) v)
           (type fixnum i))
  (sb-kernel:hairy-data-vector-ref v i))

(defun (setf single-1d-aref) (new-value v i)
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) v)
           (type fixnum i)
           (type single-float new-value))
  (sb-kernel:hairy-data-vector-set v i new-value))

(defun fixnum-1d-aref (v i)
  (declare (optimize (speed 3))
           (type (simple-array fixnum (*)) v)
           (type fixnum i))
  (sb-kernel:hairy-data-vector-ref v i))

(defun (setf fixnum-1d-aref) (new-value v i)
  (declare (optimize (speed 3))
           (type (simple-array fixnum (*)) v)
           (type fixnum i)
           (type fixnum new-value))
  (sb-kernel:hairy-data-vector-set v i new-value))
