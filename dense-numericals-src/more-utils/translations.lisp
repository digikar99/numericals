(in-package :dense-numericals/more-utils)

(declaim (type hash-table *translation-table*))
(defvar *translation-table* (make-hash-table))

(defmacro define-c-translation
    (&whole form
     lisp-name &optional single-float-name double-float-name scalar-name
                 sb64-name sb32-name sb16-name sb8-name
                 ub64-name ub32-name ub16-name ub8-name
                 fixnum-name)
  "Defines translation from lisp-name to the corresponding C functions.
These can be accessed by the C-NAME function.

SB : signed-byte
UB: unsigned-byte"
  (declare (ignorable single-float-name double-float-name scalar-name
                      sb64-name sb32-name sb16-name sb8-name
                      ub64-name ub32-name ub16-name ub8-name
                      fixnum-name))
  `(setf (gethash ',lisp-name *translation-table*) ',(nthcdr 2 form)))

(defun pushnew-c-translations (translation-alists)
  "Each element of the ALIST should be
  (LISP-NAME &REST C-NAMES)
The order of C names should be the same as for DEFINE-C-TRANSLATION"
  (loop :for (lisp-name . c-names) :in translation-alists
        :do (setf (gethash lisp-name *translation-table*) c-names)))

(macrolet ((def (fn-name index)
             `(progn
                (declaim (inline ,fn-name))
                (defun ,fn-name (name)
                  (declare (optimize speed)
                           (type symbol name))
                  (fdefinition
                   (or (nth ,index (gethash name *translation-table*))
                       (error "No CFUNCTION known for ~S for ~S" name ',fn-name))))
                (define-compiler-macro ,fn-name (&whole form name &environment env)
                  (let ((form-type (form-type name env)))
                    ;; TODO: Use CTYPE to normalize types?
                    (if (and (listp form-type)
                             (null (cddr form-type))
                             (or (eq 'eql (first form-type))
                                 (eq 'member (first form-type))))
                        (list 'function
                              (or (nth ,index (gethash (second form-type) *translation-table*))
                                  (error "No CFUNCTION known for ~S for ~S" name ',fn-name)))
                        form))))))
  (def single-float-c-name 0)
  (def double-float-c-name 1)
  (def cl-name 2)

  (def int64-c-name 3)
  (def int32-c-name 4)
  (def int16-c-name 5)
  (def int8-c-name  6)

  (def uint64-c-name 7)
  (def uint32-c-name 8)
  (def uint16-c-name 9)
  (def uint8-c-name  10)

  (def fixnum-c-name   11))

(defun c-name (type name)
  (declare (optimize speed))
  (let ((idx (switch (type :test #'alexandria:type=)
               ('single-float 0)
               ('double-float 1)
               ('(signed-byte 64) 3)
               ('(signed-byte 32) 4)
               ('(signed-byte 16) 5)
               ('(signed-byte 08) 6)
               ('(unsigned-byte 64) 7)
               ('(unsigned-byte 32) 8)
               ('(unsigned-byte 16) 9)
               ('(unsigned-byte 08) 10)
               ('fixnum 11)
               (t 2))))
    (or (nth idx (gethash name *translation-table*))
        (error "No CFUNCTION known for ~S for ~S" name type))))
(define-compiler-macro c-name (&whole form type name &environment env)
  (let* ((type-form-type (form-type type env))
         (name-form-type (form-type name env)))
    (if (and (optima:match type-form-type
               ((or (list 'eql _)
                    (list 'member _)
                    (list 'equal _))
                t)
               (_
                nil))
             (optima:match name-form-type
               ((or (list 'eql _)
                    (list 'member _)
                    (list 'equal _))
                t)
               (_
                nil)))
        (let ((idx (switch ((second type-form-type)
                            :test (cl:lambda (x y)
                                    (and (cl-type-specifier-p x)
                                         (cl-type-specifier-p y)
                                         (alexandria:type= x y))))
                     ('single-float 0)
                     ('double-float 1)
                     ('(signed-byte 64) 3)
                     ('(signed-byte 32) 4)
                     ('(signed-byte 16) 5)
                     ('(signed-byte 08) 6)
                     ('(unsigned-byte 64) 7)
                     ('(unsigned-byte 32) 8)
                     ('(unsigned-byte 16) 9)
                     ('(unsigned-byte 08) 10)
                     ('fixnum 11)
                     (t 2))))
          (list 'function
                (or (nth idx (gethash (second name-form-type) *translation-table*))
                    (error "No CFUNCTION known for ~S for ~S"
                           (second name-form-type) (second type-form-type)))))
        form)))

(declaim (ftype (function ((or list symbol)) (unsigned-byte 4)) c-size))
(defun c-size (type)
  (declare (optimize speed))
  (eswitch (type :test #'alexandria:type=)
    ('single-float 4)
    ('double-float 8)
    ('fixnum 8)
    ('(signed-byte 64) 8)
    ('(signed-byte 32) 4)
    ('(signed-byte 16) 2)
    ('(signed-byte 08) 1)
    ('(unsigned-byte 64) 8)
    ('(unsigned-byte 32) 4)
    ('(unsigned-byte 16) 2)
    ('(unsigned-byte 08) 1)))
(define-compiler-macro c-size (&whole form type-form &environment env)
  (declare (optimize debug))
  (let ((form-type (form-type type-form env)))
    ;; TODO: Use CTYPE to normalize types?
    (if (and (listp form-type)
             (null (cddr form-type))
             (or (member (first form-type)
                         '(eql member equal))))
        (switch ((second form-type) :test #'alexandria:type=)
          ('single-float 4)
          ('double-float 8)
          ('fixnum 8)
          ('(signed-byte 64) 8)
          ('(signed-byte 32) 4)
          ('(signed-byte 16) 2)
          ('(signed-byte 08) 1)
          ('(unsigned-byte 64) 8)
          ('(unsigned-byte 32) 4)
          ('(unsigned-byte 16) 2)
          ('(unsigned-byte 08) 1)
          (t form))
        form)))

(declaim (ftype (function ((or list symbol)) keyword) c-type))
(defun c-type (type)
  (declare (optimize speed))
  (eswitch (type :test #'alexandria:type=)
    ('single-float :float)
    ('double-float :double)
    ('(signed-byte 64) :int64)
    ('(signed-byte 32) :int32)
    ('(signed-byte 16) :int16)
    ('(signed-byte 08)  :int8)
    ('(unsigned-byte 64) :uint64)
    ('(unsigned-byte 32) :uint32)
    ('(unsigned-byte 16) :uint16)
    ('(unsigned-byte 08) :uint8)
    #+(or sbcl ccl)
    ('fixnum :fixnum)))
(define-compiler-macro c-type (&whole form type-form &environment env)
  (let ((form-type (form-type type-form env)))
    ;; TODO: Use CTYPE to normalize types?
    (if (and (listp form-type)
             (null (cddr form-type))
             (or (member (first form-type)
                         '(eql member equal))))
        (switch ((second form-type) :test #'alexandria:type=)
          ('single-float :float)
          ('double-float :double)
          ('(signed-byte 64) :int64)
          ('(signed-byte 32) :int32)
          ('(signed-byte 16) :int16)
          ('(signed-byte 08)  :int8)
          ('(unsigned-byte 64) :uint64)
          ('(unsigned-byte 32) :uint32)
          ('(unsigned-byte 16) :uint16)
          ('(unsigned-byte 08) :uint8)
          #+(or sbcl ccl)
          ('fixnum :fixnum)
          (t form))
        form)))
