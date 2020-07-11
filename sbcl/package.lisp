(cl:in-package :cl)

(defpackage :numericals.sbcl
  (:use :cl :sb-c :sb-ext :sb-vm :alexandria :iterate :numericals.helper)
  (:import-from :sb-vm
                :descriptor-reg
                :any-reg
                :tagged-num
                
                :simple-array-double-float
                :simple-array-single-float
                :simple-array-fixnum
                
                :double-avx2-reg
                :single-avx2-reg
                :int-avx2-reg
                
                :simd-pack-256-double
                :simd-pack-256-single
                :simd-pack-256-int

                :float-ref-ea
                :inst)
  (:export :simd-function-spec                 

           ;; In SBCL, the natural inputs to such functions are 
           ;; 1d simple-arrays. I'd like to hear a better name :).
           :single-1d-aref
           :double-1d-aref
           :fixnum-1d-aref
           :svref
           
           :simd-single-1d-aref
           :simd-double-1d-aref
           :simd-fixnum-1d-aref
           :simd-svref
           
           :simd-single-broadcast-1d-aref
           :simd-double-broadcast-1d-aref
           :simd-fixnum-broadcast-1d-aref
           
           :+simd-single-1d-aref-stride+
           :+simd-double-1d-aref-stride+
           :+simd-fixnum-1d-aref-stride+
           :+simd-svref-stride+
           
           :simd-single-+
           :simd-single--
           :simd-single-/
           :simd-single-*

           :simd-double-+
           :simd-double--
           :simd-double-/
           :simd-double-*

           :simd-single-sqrt
           :simd-double-sqrt

           :simd-and
           :simd-or
           :simd-not
           :simd-xor
           
           ;; While it may be possible to merge aref, stride and storage-array,
           ;; to me, an efficient non-redundant operative way isn't clear.
           :1d-storage-array))

;;; DO NOT INLINE CODE UNLESS NECESSARY.
;;; This makes it harder for the user to debug.
;;; Instead, use the with-inline macro provided
(in-package :numericals.sbcl)
(declaim (inline single-1d-aref
                 (setf single-1d-aref)
                 double-1d-aref
                 (setf double-1d-aref)
                 simd-single-1d-aref
                 simd-double-1d-aref
                 (setf simd-single-1d-aref)
                 (setf simd-double-1d-aref)
                 simd-single-broadcast-1d-aref
                 simd-double-broadcast-1d-aref
                 1d-storage-array
                 simd-single-+
                 simd-single--
                 simd-single-/
                 simd-single-*
                 simd-single-sqrt
                 simd-double-+
                 simd-double--
                 simd-double-/
                 simd-double-*
                 simd-double-sqrt))
