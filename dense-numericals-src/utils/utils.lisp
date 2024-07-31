(in-package :dense-numericals/utils)

(deftype uint32 () `(unsigned-byte 32))
(deftype int64 () '(signed-byte 64))
(deftype int16 () '(signed-byte 16))
(deftype int8  () '(signed-byte 08))

(defvar *default-float-format* 'single-float
  "Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.")
(declaim (type (member single-float double-float) *default-float-format*))

(defvar *broadcast-automatically* t
  "If non-NIL, operations automatically perform broadcasting as necessary.
If NIL, broadcasting is expected to be performed by the user. Such strictness
can be helpful to locate bugs.")

(defparameter *inline-with-multithreading* nil
  "Inlining is usually necessary for smaller arrays; for such arrays multithreading
becomes unnecessary. If this parameter is non-NIL, code using multithreading
would be emitted; otherwise, the code would be skipped.

This is only relevant for transcendental functions which uses lparallel for multithreading.")

(defparameter *multithreaded-threshold* 80000)
(declaim (type fixnum *multithreaded-threshold*))

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

(define-condition runtime-array-allocation (suboptimal-polymorph-note)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unable to avoid array allocation at run time. Consider supplying
the OUT argument, and/or ensuring all the appropriate arguments are
arrays of appropriate types."))))

(defmacro defun* (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,lambda-list ,@body)))

(defmacro ensure-row-major-layout ()
  `(assert (eq layout :row-major)
           (layout)
           "CL:ARRAY can only have :ROW-MAJOR layout. See DENSE-ARRAYS:ARRAY
or MAGICL:TENSOR for arrays with :COLUMN-MAJOR or other layouts."))

(defun shape (array-like &optional (axis nil axis-p))
  ;; This is a potentially domain specific functionality; since
  ;; there exists the ambiguity of what should one do with strings
  (let ((dimensions (typecase array-like
                      (sequence (cons (length array-like)
                                      (shape (elt array-like 0))))
                      (array (array-dimensions array-like))
                      (t nil))))
    (if axis-p
        (elt dimensions axis)
        dimensions)))

(define-polymorphic-function out-shape-compatible-p (function-name &rest args)
  :overwrite t)

(define-polymorphic-function out-shape (function-name &rest args)
  :overwrite t)
