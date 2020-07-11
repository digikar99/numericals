(cl:in-package :cl)

(defpackage :numericals.helper
  (:use :cl :alexandria :iterate)
  (:export +cl-array-symbols+
           +numericals-array-symbols+
           +numericals-array-slots+
           eval-always
           macro-when
           *numericals-internals-package*
           *numericals-tests-package*))

(in-package :numericals.helper)

(defvar *numericals-internals-package*)
(defvar *numericals-tests-package*)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro macro-when (condition &body body)
  (when condition
    `(progn
       ,@body)))

(eval-always 
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
		:transpose
        :cl-array-array)
    :test 'equalp)

  (alexandria:define-constant +numericals-array-slots+
      '(:displaced-index-offset
        :displaced-to
        :strides
        :dim
        :element-type
		:cl-array
		:contiguous-p
		:total-size)
    :test 'equalp))
