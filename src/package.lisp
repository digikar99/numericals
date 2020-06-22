(cl:in-package :cl)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(eval-always ; TODO: Avoid using the package CL
 (alexandria:define-constant +cl-array-symbols+
     '(:array :arrayp
       :array-dimensions
       :array-dimension
       :array-element-type
       :array-total-size
       :array-displacement
       :1d-storage-array
       :aref
       :cl-aref
       :cl-array
       :row-major-aref
       :array-rank
       :make-array)
   :test 'equalp)

 (alexandria:define-constant +numericals-array-symbols+
     '(:array-stride
       :array-strides
       :array-displaced-to
       :array-displaced-index-offset
       :array-dim
       :array-cl-array
       :broadcast-array
       :array-contiguous-p
       :numericals-array
       :make-numericals-array
       :cl-array-array)
   :test 'equalp)

 (alexandria:define-constant +numericals-array-slots+
     '(:displaced-index-offset
       :displaced-to
       :strides
       :dim
       :element-type)
   :test 'equalp))

(defpackage :numericals
  #.(cons :export +cl-array-symbols+)
  (:export

   :with-broadcast
   :with-elementwise-operations
   :weop ; convenient wrapper for with-elementwise-operations
   :with-inline
   :with-array
   :with-arrays*
   :with-constant
   :with-constants
   :maybe-form-not-constant-error
   :def-array
   :make-array
   :array-like-p
   :numericals-array-element-type
   :map-outer
   
   :*type*
   :*lookup-type-at-compile-time*
   :astype
   :asarray
   :concatenate
   :zeros
   :ones
   :empty
   :shape

   :+
   :-
   :/
   :*

   :sqrt))

#+sbcl
(defpackage :numericals.sbcl
  (:use :cl :sb-c :sb-ext :sb-vm :alexandria :iterate)
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


;;; Ideally, we'd like to keep only one interface package - however, there are certain
;;; array- functions we might not want to be public, but want them to be available for
;;; internal use.
(defpackage :numericals.array
  (:use)
  #.`(:intern ,@+numericals-array-slots+)
  #.`(:import-from :numericals
                   :*type*
                   :*lookup-type-at-compile-time*
                   :numericals-array-element-type
                   ,@+cl-array-symbols+)
  #.`(:export
      ,@+cl-array-symbols+)
  #.`(:export
      ,@+numericals-array-symbols+))

;; This package implements a multidimensional displaced array. This is required to implement ;; faster aref-ing. Without this, aref can be 50 times slower than numpy - since all numpy
;; does while arefing is provides a "view", without actually copying over the data.
(defpackage :numericals.array.internals
  (:use :cl :alexandria :iterate :introspect-environment)

  (:shadowing-import-from #+sbcl :numericals.sbcl
                          :1d-storage-array)

  (:import-from :trivial-types :function-designator)
  (:import-from :numericals
                :*type*
                :*lookup-type-at-compile-time*
                :numericals-array-element-type)
  
  #.`(:shadowing-import-from :numericals.array
                             :make-numericals-array
                             ,@+numericals-array-symbols+
                             ,@+numericals-array-slots+)
  (:local-nicknames (:na :numericals.array)))

;; How do we check for the presence of AVX2 support given that it's not a part of +features+ ?

(in-package #+sbcl :numericals.sbcl)
(defmacro macro-when (condition &body body)
  (when condition
    `(progn
       ,@body)))

#.(when (member :sbcl *features*)
    `(declaim (sb-ext:maybe-inline ,@(iter (for s in-package :numericals external-only t)
                                           (collect s)))))

(defpackage :numericals.internals
  (:use :cl :alexandria :iterate :introspect-environment
        #+sbcl :numericals.sbcl)
  (:local-nicknames (:nu :numericals))
  (:import-from :numericals
                :maybe-form-not-constant-error
                :*type*
                :*lookup-type-at-compile-time*)
  #.`(:shadowing-import-from :numericals.array
                             ,@cl::+cl-array-symbols+
                             ,@cl::+numericals-array-symbols+))

(in-package :numericals.internals)

;;; DO NOT INLINE CODE UNLESS NECESSARY.
;;; This makes it harder for the user to debug.
;;; Instead, use the with-inline macro provided
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

