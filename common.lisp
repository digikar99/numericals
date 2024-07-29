(defpackage :numericals/common
  (:use :peltadot)
  (:import-from #:alexandria
                #:switch
                #:eswitch
                #:once-only
                #:define-constant)
  (:export #:compiler-in-package
           #:*compiler-package*

           #:compiler-in-suite
           #:*suite-name*

           #:type-min
           #:type-max
           #:type-zero

           #:inline-or-funcall
           #:fref

           #:with-pointers-to-vectors-data
           #:with-foreign-object
           #:with-foreign-objects
           #:blas-trans
           #:ccall
           #:max-type
           #:upgraded-c-array-element-type

           #:export-all-external-symbols))

(uiop:define-package :dense-numericals/common
  (:use :numericals/common)
  (:reexport :numericals/common))

(in-package :numericals/common)

(defmacro compiler-in-package (package-variable)
  `(cl:in-package ,(package-name (find-package (symbol-value package-variable)))))

(defvar *compiler-package*)

(defmacro compiler-in-suite (suite-variable)
  `(5am:in-suite ,(symbol-value suite-variable)))
(defvar *suite-name*)

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
(define-compiler-macro type-zero (&whole form type-form &environment env)
  (let ((type-form-type (peltadot/form-types:nth-form-type type-form env 0 t t)))
    ;; (print (cons type-form type-form-type))
    (if (and (listp type-form-type)
             (null (cddr type-form-type))
             (member (first type-form-type)
                     '(eql member equal)))
        (switch ((second type-form-type) :test #'type=)
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
          ('(unsigned-byte 08) 0)
          (t form))
        form)))

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
(define-compiler-macro type-max (&whole form type-form &environment env)
  (let ((type-form-type (peltadot/form-types:nth-form-type type-form env 0 t t)))
    (if (and (listp type-form-type)
             (null (cddr type-form-type))
             (member (first type-form-type)
                     '(eql member equal)))
        (switch ((second type-form-type) :test #'type=)
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
          ('(unsigned-byte 08) (1- (expt 2 08)))
          (t form))
        form)))

(defun inline-or-funcall (function-designator &rest arguments)
  (apply #'funcall function-designator arguments))
(define-compiler-macro inline-or-funcall
    (function-designator-form
     &rest arguments
     &environment env)
  (let ((form-type (peltadot/form-types:nth-form-type function-designator-form env 0 t t)))
    (if (subtypep form-type 'symbol env)
        (if (and (listp form-type)
                 (null (cddr form-type))
                 (member (first form-type)
                         '(eql member equal)))
            `(,(second form-type) ,@arguments)
            `(funcall (fdefinition ,function-designator-form) ,@arguments))
        `(funcall ,function-designator-form ,@arguments))))

(define-polymorphic-function fref (ptr type)
  :documentation "A type-inferencing alternative to CFFI:MEM-REF"
  :overwrite t)

(macrolet ((def (lisp-type ctype)
             `(defpolymorph fref (ptr (type (eql ,ctype))) ,lisp-type
                (declare (ignore type))
                (cffi:mem-ref ptr ,ctype))))
  (def single-float :float)
  (def double-float :double)
  (def (signed-byte 64) :int64)
  (def (signed-byte 32) :int32)
  (def (signed-byte 16) :int16)
  (def (signed-byte 08) :int8)
  (def (unsigned-byte 64) :uint64)
  (def (unsigned-byte 32) :uint32)
  (def (unsigned-byte 16) :uint16)
  (def (unsigned-byte 08) :uint8))

(defpolymorph fref (ptr (type (eql :fixnum))) fixnum
  (declare (ignore type))
  #+sbcl
  (nth-value 0 (floor (cffi:mem-ref ptr :int64) 2))
  #+ccl
  (cffi:mem-ref ptr :int64)
  #-(or sbcl ccl)
  (error "FREF does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(define-polymorphic-function (setf fref) (value ptr type)
  :documentation "A type-inferencing alternative to CFFI:MEM-REF"
  :overwrite t)

(macrolet ((def (lisp-type ctype)
             `(defpolymorph (setf fref) (value ptr (type (eql ,ctype))) ,lisp-type
                (declare (ignore type))
                (setf (cffi:mem-ref ptr ,ctype) value))))
  (def single-float :float)
  (def double-float :double)
  (def (signed-byte 64) :int64)
  (def (signed-byte 32) :int32)
  (def (signed-byte 16) :int16)
  (def (signed-byte 08) :int8)
  (def (unsigned-byte 64) :uint64)
  (def (unsigned-byte 32) :uint32)
  (def (unsigned-byte 16) :uint16)
  (def (unsigned-byte 08) :uint8))

(defpolymorph (setf fref) (value ptr (type (eql :fixnum))) fixnum
  (declare (ignore type))
  #+sbcl
  (setf (cffi:mem-ref ptr :int64) (* value 2))
  #+ccl
  (setf (cffi:mem-ref ptr :int64) value)
  #-(or sbcl ccl)
  (error "FREF does not know how to handle fixnums on ~A"
         (lisp-implementation-type)))

(defmacro with-pointers-to-vectors-data (bindings &body body)
  (if bindings
      `(cffi:with-pointer-to-vector-data ,(first bindings)
         (locally (declare (cl:type cffi-sys:foreign-pointer ,(caar bindings)))
           (with-pointers-to-vectors-data ,(rest bindings) ,@body)))
      `(progn ,@body)))

(defmacro with-foreign-object ((var type &optional (value nil valuep)) &body body)
  "A wrapper around CFFI:WITH-FOREIGN-OBJECT with an additional VALUE option."
  (if valuep
      (once-only (value)
        `(cffi:with-foreign-object (,var ,type)
           (setf (cffi:mem-ref ,var ,type) ,value)
           ,@body))
      `(cffi:with-foreign-object (,var ,type)
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
           (type cl:array array)
           (ignore array))
  (if invertedp "N" "T"))

(defmacro ccall (name &rest args)
  "Calls CFFI:FOREIGN-FUNCALL on NAME with appropriate type declarations on args."
  `(cffi:foreign-funcall
    ,@(loop :for arg :in args
            :appending `(once-only (arg)
                          ((etypecase ,arg
                             (cffi:foreign-pointer :pointer)
                             (single-float :float)
                             (double-float :double))
                           ,arg)))))


;;; Below functions are taken from DENSE-ARRAYS-PLUS-LITE

(defun max-type (type-1 type-2)
  (cond ((subtypep type-1 type-2)
         type-2)
        ((subtypep type-2 type-1)
         type-1)
        ((or (alexandria:type= type-1 'double-float)
             (alexandria:type= type-2 'double-float))
         'double-float)
        ((or (alexandria:type= type-1 'single-float)
             (alexandria:type= type-2 'single-float))
         'single-float)
        ;; At this point, none of the types are floats
        ;; FIXME: Operate better on impl with other float types
        ((and (subtypep type-1 '(unsigned-byte *))
              (subtypep type-2 '(signed-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-1 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        ((and (subtypep type-1 '(signed-byte *))
              (subtypep type-2 '(unsigned-byte *)))
         (loop :for num-bits :in '(8 16 32 64)
               :if (subtypep type-2 `(signed-byte ,num-bits))
                 :do (return-from max-type `(signed-byte ,num-bits))
               :finally (return-from max-type 'single-float)))
        (t
         (error "Don't know how to find MAX-TYPE of ~S and ~S" type-1 type-2))))

(define-constant +c-array-element-types+
    '(single-float
      double-float
      (complex single-float)
      (complex double-float)
      (unsigned-byte 64)
      (unsigned-byte 32)
      (unsigned-byte 16)
      (unsigned-byte 08)
      (signed-byte 64)
      (signed-byte 32)
      (signed-byte 16)
      (signed-byte 08))
  :test #'equal)

(defun upgraded-c-array-element-type (type)
  (loop :for ctype :in +c-array-element-types+
        :when (type= type ctype)
          :do (return-from upgraded-c-array-element-type type))
  (loop :for supertype :in +c-array-element-types+
        :when (subtypep type supertype)
        :do (return-from upgraded-c-array-element-type supertype)))

(defun export-all-external-symbols (from to &optional exceptions)
  "FROM - Package designator with existing external symbols
TO - Package designator to which the existing symbols should be exported"
  (use-package from to)
  (let ((external-symbols nil))
    (do-external-symbols (s from)
      (unless (member s exceptions :test #'string=)
        (push s external-symbols)))
    (export external-symbols to)))
