(in-package :dense-numericals.impl)
;; (numericals.common:compiler-in-package numericals.common:*compiler-package*)

(defun ensure-appropriate-dense-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (asarray (ensure-list array-like) :type default-element-type)))

(defvar nu:*default-float-format* 'single-float
  "Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.")
(declaim (type (member single-float double-float) nu:*default-float-format*))

(defvar nu:*broadcast-automatically* t
  "If non-NIL, operations automatically perform broadcasting as necessary.
If NIL, broadcasting is expected to be performed by the user. Such strictness
can be helpful to locate bugs.")

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
