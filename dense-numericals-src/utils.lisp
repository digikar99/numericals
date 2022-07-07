(in-package :dense-numericals.impl)
;; (numericals.common:compiler-in-package numericals.common:*compiler-package*)

(define-condition runtime-array-allocation (suboptimal-polymorph-note)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unable to avoid array allocation at run time. Consider supplying
the OUT argument, and/or ensuring all the appropriate arguments are
arrays of appropriate types."))))

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
         (locally (declare (cl:type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

(defmacro with-foreign-object ((var type &optional (value nil valuep)) &body body)
  "A wrapper around CFFI:WITH-FOREIGN-OBJECT with an additional VALUE option."
  (once-only (value)
    `(cffi:with-foreign-object (,var ,type)
       ,(when valuep
          `(setf (cffi:mem-ref ,var ,type) ,value))
       ,@body)))

(defmacro with-foreign-objects (bindings &body body)
  "A wrapper around CFFI:WITH-FOREIGN-OBJECTS with an additional VALUE option."
  (if bindings
      `(with-foreign-object ,(first bindings)
         (with-foreign-objects ,(rest bindings)
           ,@body))
      `(locally ,@body)))

(declaim (inline blas-trans))
(defun blas-trans (array &optional invertedp)
  (declare (optimize speed)
           (type dense-arrays::dense-array array))
  (ecase (array-layout array)
    (:row-major (if invertedp "N" "T"))
    (:column-major (if invertedp "T" "N"))
    ('nil (error "Expected array to have a known layout"))))

;; FIXME: This should be in DENSE-ARRAYS itself?
(define-condition incompatible-broadcast-dimensions (error)
  ((dimensions :initarg :dimensions :reader condition-dimensions)
   (array-likes :initarg :array-likes :reader condition-array-likes))
  (:report (lambda (c s)
             (pprint-logical-block (s nil)
               (format s "The following array-likes with dimensions~{~%  ~S~}~%cannot be broadcast together:~%" (condition-dimensions c))
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (condition-array-likes c)))))))
