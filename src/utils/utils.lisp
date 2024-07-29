(defpackage :numericals/utils
  (:use :cl :alexandria)
  (:export #:size
           #:the-size
           #:int-index
           #:the-int-index
           #:lm

           #:*default-float-format*
           #:*broadcast-automatically*
           #:*array-layout*
           #:*array-element-type*
           #:*array-element-type-alist*
           #:default-element-type
           #:*multithreaded-threshold*
           #:*inline-with-multithreading*

           #:array
           #:simple-array
           #:array-rank
           #:array-dimensions
           #:narray-dimensions
           #:array-element-type
           #:array-total-size
           #:array-stride
           #:array-layout
           #:cl-array-offset
           #:array-storage
           #:array-type-element-type))

(in-package :numericals/utils)

(deftype size () `(unsigned-byte 62))
(defmacro the-size (form)
  #+sbcl `(sb-ext:truly-the size ,form)
  #-sbcl `(the size ,form))

(deftype int-index () `(signed-byte 62))
(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    int-index ,form))

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(defvar *default-float-format* 'single-float
  "Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.")
(declaim (type (member single-float double-float) *default-float-format*))

(defvar *broadcast-automatically* t
  "If non-NIL, operations automatically perform broadcasting as necessary.
If NIL, broadcasting is expected to be performed by the user. Such strictness
can be helpful to locate bugs.")


(defvar *array-element-type*)

(setf (documentation '*array-element-type* 'variable)
      "If BOUND, this is the default value of the ELEMENT-TYPE or TYPE argument.
Overrides *ARRAY-ELEMENT-TYPE-ALIST*.
Is overriden by explicitly passing an ELEMENT-TYPE or TYPE argument.")

(defvar *array-element-type-alist* nil
  "An ALIST mapping package to the default element-type used in that package.
(Inspired from SWANK:*READTABLE-ALIST*)
Overrides none.
Is overriden by *ARRAY-ELEMENT-TYPE* when bound, or by explicitly passing an
  ELEMENT-TYPE or TYPE argument.")

(define-symbol-macro package-local-element-type
    (cdr (assoc *package* *array-element-type-alist*)))

(define-symbol-macro default-element-type
    (or (when (boundp '*array-element-type*)
          *array-element-type*)
        package-local-element-type
        t))

(defvar *array-layout* :row-major
  "Dummy variable provided so that code written for NUMERICALS may be easily
upgradeable to DENSE-NUMERICALS")

(defparameter *inline-with-multithreading* nil
  "Inlining is usually necessary for smaller arrays; for such arrays multithreading
becomes unnecessary. If this parameter is non-NIL, code using multithreading
would be emitted; otherwise, the code would be skipped.

This is only relevant for transcendental functions which uses lparallel for multithreading.")

(defparameter *multithreaded-threshold* 80000)
(declaim (type fixnum *multithreaded-threshold*))


(declaim (inline narray-dimensions))
(declaim (cl:ftype (cl:function (cl:array) list) narray-dimensions))
(defun narray-dimensions (array)
  (declare (type cl:array array)
           (optimize speed))
  (array-dimensions array))

(declaim (inline array-stride))
(defun array-stride (array axis)
  (declare (optimize speed)
           (type size axis))
  (loop :for d :of-type size :in (subseq (narray-dimensions array) (1+ axis))
        :with stride :of-type size := 1
        :do (setq stride (* d stride))
        :finally (return stride)))

(declaim (inline array-layout))
(defun array-layout (array)
  (declare (type array array)
           (ignore array))
  :row-major)

(declaim (inline cl-array-offset))
(declaim (ftype (function (cl:array) size) cl-array-offset))
(defun cl-array-offset (array)
  (declare (optimize speed)
           (type cl:array array))
  (loop :with total-offset :of-type (signed-byte 61) := 0
        :if (typep array 'cl:simple-array)
          :do (return total-offset)
        :else
          :do (multiple-value-bind (displaced-to offset)
                  (cl:array-displacement array)
                (declare (type (signed-byte 61) offset))
                (incf total-offset offset)
                (setq array displaced-to))))

(declaim (inline array-storage)
         (ftype (function (cl:array) (cl:simple-array * 1))))
(defun array-storage (array)
  (declare (ignorable array)
           (optimize speed))
  (loop :with array := array
        :do (locally (declare #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
              (typecase array
                ((cl:simple-array * (*)) (return array))
                (cl:simple-array (return #+sbcl (sb-ext:array-storage-vector array)
                                         #+ccl (ccl::%array-header-data-and-offset array)
                                         #-(or sbcl ccl)
                                         (error "Don't know how to obtain ARRAY-STORAGE on ~S"
                                                (lisp-implementation-type))))
                (t (setq array (cl:array-displacement array)))))))


(defun array-type-element-type (array-type &optional env)
  (loop :for type :in '(t
                        single-float
                        double-float
                        fixnum
                        (unsigned-byte 64)
                        (unsigned-byte 32)
                        (unsigned-byte 16)
                        (unsigned-byte 08)
                        (signed-byte 64)
                        (signed-byte 32)
                        (signed-byte 16)
                        (signed-byte 08))
        :if (cl:subtypep array-type `(cl:array ,type) env)
          :do (return type)))
