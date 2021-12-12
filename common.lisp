
(defpackage :numericals.common
  (:use :cl :alexandria)
  (:export #:compiler-in-package
           #:*compiler-package*

           #:compiler-in-suite
           #:*suite-name*

           #:type-min
           #:type-max
           #:type-zero))

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


