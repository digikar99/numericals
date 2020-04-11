#+sbcl
(defpackage :numericals.sbcl
  (:use :cl :sb-c :sb-ext :sb-vm :alexandria :iterate)
  (:import-from :sb-vm
                :descriptor-reg
                :any-reg
                :simple-array-double-float
                :simple-array-single-float
                :tagged-num
                :double-avx2-reg
                :single-avx2-reg
                :simd-pack-256-double
                :simd-pack-256-single
                
                :float-ref-ea
                :inst)
  (:export :simd-function-spec                 
           
           :simd-single-1d-aref
           :simd-double-1d-aref
           :+simd-single-1d-aref-stride+
           :+simd-double-1d-aref-stride+
           
           :simd-single-+
           :simd-double-+

           :simd-single--
           :simd-single-/
           :simd-single-*

           :1d-storage-array))

;; How do we check for the presence of AVX2 support given that it's not a part of +features+

(in-package #+sbcl :numericals.sbcl)
(defmacro macro-when (condition &body body)
  (when condition
    `(progn
       ,@body)))

(defpackage :numericals.internals
  (:use :cl :alexandria)

  (:export :with-simd-operations)

  ;;; The package below is supposed to define the following functions
  (:import-from  #+sbcl :numericals.sbcl
                 
                 :simd-function-spec                 

                 ;; In SBCL, the natural inputs to such functions are neither
                 ;; 1d simple-arrays. I'd like to hear a better name :).
                 :simd-single-1d-aref
                 :simd-double-1d-aref                 
                 :+simd-single-1d-aref-stride+
                 :+simd-double-1d-aref-stride+
                 
                 :simd-single-+
                 :simd-double-+

                 :simd-single--
                 :simd-single-*
                 :simd-single-/

                 :1d-storage-array

                 ;; While it may be possible to merge aref, stride and storage-array,
                 ;; to me, an efficient non-redundant operative way isn't clear.
                 ))

(in-package :numericals.internals)

(declaim (inline simd-single-1d-aref simd-double-1d-aref
                 (setf simd-single-1d-aref)
                 (setf simd-double-1d-aref)))



(defpackage :numericals
  (:use :numericals.internals)
  (:export

   :with-broadcast
   :with-simd-operations

   :+))

