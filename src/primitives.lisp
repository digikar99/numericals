(in-package :numericals.internals)

;;; DO NOT INLINE CODE UNLESS NECESSARY

(defparameter *type* 'single-float
  ;; better way to restrict the value?
  "Can only be one of FIXNUM, SINGLE-FLOAT, or DOUBLE-FLOAT. Depending on the compile-time
value of *LOOKUP-TYPE-AT-COMPILE-TIME*, this variable may be looked up at compile-time
or run-time.")

(defparameter *lookup-type-at-compile-time* t
  "If the compile-time value is T, looks up the value of *TYPE* at compile-time to aid 
compiler-macros to generate efficient code.")

(defparameter *max-broadcast-dimensions* 4)
;; several macros need to be re-expanded for change in this to be reflected.

(deftype nu:numericals-array-element-type ()
  `(member single-float double-float fixnum))

(defmacro defun-c (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,lambda-list ,@body)))

(defun-c valid-numericals-type-p (type) (member type '(fixnum single-float double-float)))

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) (:a 2 :b 3))"
  (if args
      (if (keywordp (car args))
          (list () args)
          (destructuring-bind (non-keyword-args keyword-args)
              (split-at-keywords (cdr args))
            (list (cons (car args) non-keyword-args)
                  keyword-args)))
      '(() ())))

(defun array-initial-element (type)
  (ecase type
    (single-float 0.0)
    (double-float 0.0d0)
    (fixnum 0)))

;;; Should error handling be improved?
(defun %cast (to-type object)
  (if (eq to-type 'fixnum)
      (nth-value 0 (floor object))
      (coerce object to-type)))

(defun map-tree (function tree)
  (when tree
    (if (listp tree)
        (loop for tr in tree
           collect (map-tree function tr))
        (funcall function tree))))

(defun cast (to-type object)
  (if (arrayp object)
      (nu:astype object to-type)
      (map-tree (curry '%cast to-type) object)))

(define-compiler-macro cast (&whole whole to-type object &environment env)
  (if (= 3 (policy-quality 'speed env))
      (if (or (numberp object)
              (and (symbolp object) (subtypep (variable-type object env) 'number))
              (and (listp object)
                   (let ((return-type (elt (function-type (car object)) 2)))
                     (cond ((symbolp return-type)
                            (subtypep return-type 'number))
                           ((eq (first return-type) 'values)
                            (subtypep (second return-type) 'number))
                           (t (format t "; note: Return type ~S of function ~S not understood"
                                      return-type (car object)))))))
          (if (eq to-type 'fixnum)
              `(floor ,object)
              `(coerce ,object ,to-type))
          whole)
      whole))

;;; Possibly do this using SIMD
(defun nu:astype (array type)
  (let* ((storage-vector (1d-storage-array array))
         (return-array (make-array (array-dimensions array)
                                   :element-type type
                                   :initial-element (array-initial-element type)))
         (ra-storage-vector (1d-storage-array return-array)))
    (loop for i fixnum from 0 below (length storage-vector)
       do (setf (aref ra-storage-vector i)
                (cast type (aref storage-vector i)))
       finally (return return-array))))

(defun nu:shape (array-like)
  (cond ((arrayp array-like)
         (array-dimensions array-like))
        ((listp array-like)
         (cons (length array-like)
               (nu:shape (car array-like))))
        (t nil)))

(defun nu:zeros (&rest args)
  "ARGS: (&rest array-dimensions &key (type *type*))
Examples: 
  (zeros 2 3)
  (zeros 3 3 :type 'fixnum)"
  (declare (optimize (speed 3)))
  (destructuring-bind (dimensions (&key (type *type*))) (split-at-keywords args)
    (when (and (listp (car dimensions)) (null (cdr dimensions)))
      (setq dimensions (car dimensions)))
    (make-array dimensions :element-type type
                :initial-element (%cast type 0))))

(macrolet
    ((define-allocation-function (name initial-element)
       `(progn
          
          (defun ,name (&rest args)
            ,(format nil "ARGS: (&rest array-dimensions &key (type *type*))
Examples: 
  (~D 2 3)
  (~D '(5 5))
  (~D 3 3 :type 'fixnum)"
                     (string-downcase name)
                     (string-downcase name)
                     (string-downcase name))
            (declare (optimize (speed 3)))
            (destructuring-bind (dimensions (&key (type *type*))) (split-at-keywords args)
              (when (and (listp (car dimensions)) (null (cdr dimensions)))
                (setq dimensions (car dimensions)))
              (make-array dimensions :element-type type
                          :initial-element (%cast type ,initial-element))))
          ;; TODO: portable way of signalling compiler notes
          (define-compiler-macro ,name (&whole whole &rest args &environment env)
            (let ((optimizable-p (= 3 (policy-quality 'speed env))))
              (if (member (car whole) (list ',name 'funcall)) ; ignoring the case of funcall or apply
                  (destructuring-bind (dimensions (&key (type '*type* type-p))) (split-at-keywords args)
                    (when optimizable-p
                      (setq type
                            (cond ((and (not type-p) *lookup-type-at-compile-time*) (list 'quote *type*))
                                  ((and (listp type)
                                        (eq (car type) 'quote)
                                        (null (cddr type))
                                        (valid-numericals-type-p (cadr type)))
                                   type)
                                  (t type))))
                    ;; dimensions is a list
                    (when optimizable-p
                      (setq dimensions
                            (if (null (cdr dimensions))
                                (if (or (typep (car dimensions) '(or list fixnum))
                                        (subtypep (variable-type (car dimensions) env)
                                                  '(or list fixnum)))
                                    (car dimensions)
                                    (progn
                                      (setq optimizable-p nil)
                                      (format
                                       t "~&; note: ~D"
                                       (format
                                        nil
                                        ,(format nil "Unable to optimize ~S without knowing type of ~~D at compile-time." name)
                                        (car dimensions)))
                                      dimensions))
                                dimensions)))
                    (if optimizable-p
                        ;; At this point, if OPTIMIZABLE-P is T, then type is either of SINGLE-FLOAT,
                        ;; DOUBLE-FLOAT, FIXNUM. And DIMENSIONS is either a LIST or a FIXNUM
                        ;; - precisely the type of arguments accepted by MAKE-ARRAY.
                        `(make-array ,dimensions :element-type ,type
                                     :initial-element (%cast ,type ,,initial-element))
                        whole))
                  (progn
                    (when (= 3 (policy-quality 'speed env))
                      (format t "~& note: Optimization for call to ~S is not implemented for ~S" ',name whole)
                      whole))))))))
  (define-allocation-function nu:zeros 0)
  (define-allocation-function nu:empty 0)
  (define-allocation-function nu:ones 1))

(defun foo ()
  (declare (optimize (speed 3)))
  (let ((type 'double-float))    
    (nu:zeros 100 :type type)))

(defun nu:asarray (array-like &key (type *type*))
  (if (arrayp array-like)
      (nu:astype array-like type)
      (make-array (nu:shape array-like)
                  :element-type type
                  :initial-contents (cast type array-like))))

(defmacro nu:with-inline (&body body)
  `(let ()
     (declare (inline ,@(iter (for symbol in-package :numericals external-only t)
                              (when (and (fboundp symbol)
                                         (not (macro-function symbol)))
                                (collect symbol)))))
     ,@body))

(defmacro nu:def-array
    (var dimensions &rest args
     &key (type *type*) initial-element initial-contents
       adjustable fill-pointer displaced-to displaced-index-offset
       (proclaim-type t) doc)
  ;; TODO: use array as function
  (declare (ignorable initial-element initial-contents adjustable fill-pointer
                      displaced-to displaced-index-offset))
  (remf args :type)
  (remf args :proclaim-type)
  (remf args :doc)
  (when (typep type 'nu:numericals-array-element-type)
    (setq type (list 'quote type)))
  (once-only (dimensions type)
    `(progn
       ,(when proclaim-type `(proclaim (list 'type
                                             (list ',(if displaced-to 'array 'simple-array)
                                                   ,type
                                                   ,dimensions)
                                             ',var)))
       (defparameter ,var (make-array ,dimensions :element-type ,type
                                      ,@args))
       ,(when doc `(setf (documentation ',var 'variable) ,doc))))  )

(defmacro nu:with-array ((var dimensions &rest args
                              &key (type *type* type-p) initial-element initial-contents
                              adjustable fill-pointer displaced-to displaced-index-offset
                              (declare-type t)) &body body &environment env)
  ;; Compile time value of *TYPE* is used!!
  (declare (ignorable initial-element initial-contents adjustable fill-pointer
                      displaced-to displaced-index-offset))
  (remf args :type)
  (remf args :declare-type)

  ;; Parse type
  (setq type (cond ((not type-p) type)
                   ((constantp type env)
                    (constant-form-value type env))
                   ((eq type '*type*) *type*)
                   (t (error 'maybe-form-not-constant-error :form type))))
  
  ;; Parse dimensions
  (when (constantp dimensions env)
    (setq dimensions (constant-form-value dimensions env)))
  
  `(let ((,var (make-array (list ,@dimensions) :element-type ',type
                           ,@args)))
     ,(when declare-type
        `(declare (type (,(if displaced-to 'array 'simple-array)
                          ,type
                          ,dimensions)
                        ,var)))
     ,@body))

(defmacro nu:with-arrays* (bindings &body body)
  (if (cdr bindings)
      `(nu:with-array ,(car bindings)
         ,(macroexpand-1 `(nu:with-arrays* ,(cdr bindings) ,@body)))
      `(nu:with-array ,(car bindings) ,@body)))

;; TODO: constantp with symbol-macros is implementation dependent
;; SBCL says symbol-macros are constant, CCL says no;
;; Smoothen out this behaviour
(defun-c ensure-get-constant-value (form env)
  (if (constantp form env)
      (constant-form-value form env)
      (error 'maybe-form-not-constant :form form)))

(define-condition maybe-form-not-constant-error (error)
  ((form :reader form :initform (error "FORM not supplied") :initarg :form))
  (:report (lambda (condition stream)
             (format stream
                     "~&The following form could not be determined to be a constant~%~%~D~%"
                     (form condition)))))

;; A failed implementation of nu:with-constant that wanted to consider
;; dynamic variables.

;; (defmacro nu:with-constant ((var value) &body body &environment env)
;;   (unless (constantp value env)
;;     (error 'maybe-form-not-constant-error :form value))
;;   (if (eq :special (variable-information var))
;;       (if (eq var '*type*)
;;           (let ((*type* (ensure-get-constant-value value env)))
;;             (print `(let ((*type* ',(ensure-get-constant-value value env)))
;;                       ,@(mapcar (rcurry #'macroexpand env) body))))
;;           (error "This case has not been considered!"))
;;       `(symbol-macrolet ((,var ,value))
;;          ,@body)))

;; (defmacro nu:with-constants (bindings &body body)
;;   (if (cdr bindings)
;;       `(nu:with-constant ,(car bindings)
;;          (nu:with-constants ,(cdr bindings) ,@body))
;;       `(nu:with-constant ,(car bindings) ,@body)))

;; (nu:with-constants ((*type* 'double-float)
;;                     (shape '(10 10)))
;;   (nu:with-array (f shape) f))

(defmacro nu:with-constant ((var value) &body body &environment env)
  (unless (constantp value env)
    (error 'maybe-form-not-constant-error :form value))
  (if (eq :special (variable-information var))
      (error "~D should ne not be special" var)
      `(symbol-macrolet ((,var ,value))
         ,@body)))

(defmacro nu:with-constants (bindings &body body &environment env)
  `(symbol-macrolet ,(loop :for (var value) :in bindings
                        :do (unless (constantp value env)
                              (error 'maybe-form-not-constant-error :form value))
                          (when (specialp var)
                            (error "~D should not be special" var))
                        :finally (return bindings))
     ,@body))
