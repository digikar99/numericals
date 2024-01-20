(defpackage :numericals.common
  (:use :peltadot)
  (:import-from #:alexandria
                #:switch
                #:eswitch)
  (:export #:compiler-in-package
           #:*compiler-package*

           #:compiler-in-suite
           #:*suite-name*

           #:type-min
           #:type-max
           #:type-zero

           #:inline-or-funcall
           #:fref))

(in-package :numericals.common)

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
  (let ((type-form-type (cl-form-types:nth-form-type type-form env 0 t t)))
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
  (let ((type-form-type (cl-form-types:nth-form-type type-form env 0 t t)))
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
  (let ((form-type (cl-form-types:nth-form-type function-designator-form env 0 t t)))
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
