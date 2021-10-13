(in-package :dense-numericals.impl)

(defun ensure-appropriate-dense-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (asarray (ensure-list array-like) :type default-element-type)))

(defvar dn:*default-float-format* 'single-float
  "Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.")
(declaim (type (member single-float double-float) dn:*default-float-format*))

(defmacro with-pointers-to-vectors-data (bindings &body body)
  "Each entry of BINDINGS is of the form (POINTER-VAR VECTOR)."
  (if bindings
      `(cffi:with-pointer-to-vector-data ,(first bindings)
         (locally (declare (type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

;; FIXME: This should be in DENSE-ARRAYS itself?
(define-condition incompatible-broadcast-dimensions (error)
  ((dimensions :initarg :dimensions :reader condition-dimensions)
   (array-likes :initarg :array-likes :reader condition-array-likes))
  (:report (lambda (c s)
             (pprint-logical-block (s nil)
               (format s "The following array-likes with dimensions~{~%  ~S~}~%cannot be broadcast together:~%" (condition-dimensions c))
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (condition-array-likes c)))))))


(declaim (inline type-min))
(defun type-min (type)
  "Returns the minimum value that is of type TYPE"
  (eswitch (type :test #'type=)
    ('single-float most-negative-single-float)
    ('double-float most-negative-double-float)
    ('fixnum       most-negative-fixnum)
    ('(signed-byte 64) (- (expt 2 63)))
    ('(signed-byte 32) (- (expt 2 31)))
    ('(signed-byte 16) (- (expt 2 15)))
    ('(signed-byte 08) (- (expt 2 07)))
    ('(unsigned-byte 64) 0)
    ('(unsigned-byte 32) 0)
    ('(unsigned-byte 16) 0)
    ('(unsigned-byte 08) 0)))

(declaim (inline type-zero))
(defun type-zero (type)
  "Returns the 0 of type TYPE"
  (eswitch (type :test #'type=)
    ('single-float 0.0f0)
    ('double-float 0.0d0)
    ('fixnum       0)
    ('(signed-byte 64) 0)
    ('(signed-byte 32) 0)
    ('(signed-byte 16) 0)
    ('(signed-byte 08) 0)
    ('(unsigned-byte 64) 0)
    ('(unsigned-byte 32) 0)
    ('(unsigned-byte 16) 0)
    ('(unsigned-byte 08) 0)))

(declaim (inline type-max))
(defun type-max (type)
  "Returns the minimum value that is of type TYPE"
  (eswitch (type :test #'type=)
    ('single-float most-positive-single-float)
    ('double-float most-positive-double-float)
    ('fixnum       most-positive-fixnum)
    ('(signed-byte 64) (1- (expt 2 63)))
    ('(signed-byte 32) (1- (expt 2 31)))
    ('(signed-byte 16) (1- (expt 2 15)))
    ('(signed-byte 08) (1- (expt 2 07)))
    ('(unsigned-byte 64) (1- (expt 2 64)))
    ('(unsigned-byte 32) (1- (expt 2 32)))
    ('(unsigned-byte 16) (1- (expt 2 16)))
    ('(unsigned-byte 08) (1- (expt 2 08)))))


