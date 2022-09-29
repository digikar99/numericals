(in-package :numericals.impl)
;; (numericals.common:compiler-in-package numericals.common:*compiler-package*)

;;; DO NOT INLINE CODE UNLESS NECESSARY

(deftype uint32 () '(unsigned-byte 32))

(defvar nu:*array-element-type*)

(setf (documentation 'nu:*array-element-type* 'variable)
      "If BOUND, this is the default value of the ELEMENT-TYPE or TYPE argument.
Overrides *ARRAY-ELEMENT-TYPE-ALIST*.
Is overriden by explicitly passing an ELEMENT-TYPE or TYPE argument.")

(defvar nu:*array-element-type-alist* nil
  "An ALIST mapping package to the default element-type used in that package.
(Inspired from SWANK:*READTABLE-ALIST*)
Overrides none.
Is overriden by *ARRAY-ELEMENT-TYPE* when bound, or by explicitly passing an
  ELEMENT-TYPE or TYPE argument.")

(define-symbol-macro package-local-element-type
    (cdr (assoc *package* nu:*array-element-type-alist*)))

(define-symbol-macro default-element-type
    (or (when (boundp 'nu:*array-element-type*)
          nu:*array-element-type*)
        package-local-element-type
        t))

(defvar nu:*array-layout* :row-major
  "Dummy variable provided so that code written for NUMERICALS may be easily
upgradeable to DENSE-NUMERICALS")

(defun ensure-appropriate-array (array-like)
  (if (typep array-like `(array ,default-element-type))
      array-like
      (nu:asarray (ensure-list array-like) :type default-element-type)))

(deftype nu:numericals-array-element-type ()
  `(member single-float double-float fixnum))

(defmacro defun-c (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defun ,name ,lambda-list ,@body)))

(defun split-at-keywords (args)
  "Example: (1 2 3 :a 2 :b 3) => ((1 2 3) :a 2 :b 3)"
  (if args
      (if (keywordp (car args))
          (cons () args)
          (destructuring-bind (non-keyword-args &rest keyword-args)
              (split-at-keywords (cdr args))
            (append (list (cons (car args) non-keyword-args))
                    keyword-args)))
      '(nil)))

(defmacro ensure-row-major-layout ()
  `(assert (eq layout :row-major)
           (layout)
           "CL:ARRAY can only have :ROW-MAJOR layout. See DENSE-ARRAYS:ARRAY
or MAGICL:TENSOR for arrays with :COLUMN-MAJOR or other layouts."))


(macrolet
    ((def (name initial-value)
       `(progn
          (declaim (inline ,name))
          (defun ,name (&rest args)
            ,(format nil "ARGS: (&rest array-dimensions &key (type default-element-type))
Examples: 
  (~D 2 3)
  (~D '(5 5))
  (~D 3 3 :type 'fixnum)"
                     (string-downcase name)
                     (string-downcase name)
                     (string-downcase name))
            (destructuring-bind (shape &key (type default-element-type) (layout :row-major))
                (split-at-keywords args)
              (when (listp (first shape))
                (assert (null (rest shape)) (shape)
                        "Expected (REST SHAPE) to be NULL but is ~S" (rest shape))
                (setq shape (first shape)))
              (ensure-row-major-layout)
              (make-array shape :element-type type
                                :initial-element (coerce ,initial-value type))))

          (define-compiler-macro ,name (&whole form &rest args &environment env)
            (let ((arg-types
                    (mapcar (lm arg (cl-form-types:nth-form-type arg env 0 t t))
                            args)))
              (let* ((first-type  (first arg-types))
                     (second-type (second arg-types))
                     (args-length (length args))
                     (second-last-type (when (>= args-length 2)
                                         (nth (- args-length 2) arg-types))))
                (cond ((or (subtypep first-type 'list)
                           (and (subtypep first-type 'size)
                                (or (null (rest args))
                                    (and (subtypep second-type '(eql :type))
                                         (= 3 args-length)))))
                       (return-from ,name
                         `((cl:lambda (shape &key (type default-element-type))
                             (make-array shape :element-type type))
                           ,@args)))
                      ((every (lm type (subtypep type 'size)) arg-types)
                       (return-from ,name
                         `((cl:lambda (shape &key (type default-element-type))
                             (make-array shape :element-type type))
                           (list ,@args))))
                      ((and (>= args-length 2)
                            (every (lm type (subtypep type 'size))
                                   (subseq arg-types 0 (- args-length 2)))
                            (subtypep second-last-type '(eql :type)))
                       (return-from ,name
                         `((cl:lambda (shape &key (type default-element-type))
                             (make-array shape :element-type type))
                           (list ,@(subseq args 0 (- args-length 2)))
                           :type ,(lastcar args))))
                      (t
                       form))))))))
  (def nu:zeros 0)
  (def nu:ones 1))

(defun nu:rand (&rest args)
  "Lambda List: (shape &key (type default-element-type) (min 0) (max 1))"
  (destructuring-bind
      (shape &key (type default-element-type) (min (coerce 0 type)) (max (coerce 1 type))
               (layout :row-major))
      (split-at-keywords args)
    (when (listp (first shape))
      (assert (null (rest shape)))
      (setq shape (first shape)))
    (ensure-row-major-layout)
    (let* ((a (nu:zeros (the list shape) :type type))
           (asv (array-storage a))
           (range (- max min))
           (min (coerce min type)))
      (declare (type (cl:simple-array * 1) asv))
      ;; A good use of specialized function :)
      (specialized-function:specializing (asv min range) ()
        (dotimes (index (array-total-size a))
          (funcall #'(setf row-major-aref) (+ min (random range)) asv index)))
      a)))

(defun nu:full (&rest args)
  "Lambda List: (shape &key value (type default-element-type))"
  (destructuring-bind (shape &key value (type default-element-type) (layout :row-major))
      (split-at-keywords args)
    (when (listp (first shape))
      (assert (null (rest shape)) (shape)
              "expected (rest shape) to be null but is ~s" (rest shape))
      (setq shape (first shape)))
    (ensure-row-major-layout)
    (make-array shape :element-type type :initial-element (coerce value type))))

(declaim (inline nu:zeros-like nu:ones-like nu:rand-like nu:full-like))
(defun nu:zeros-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (nu:zeros (array-dimensions array) :type (array-element-type array)))
(defun nu:ones-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (nu:ones (array-dimensions array) :type (array-element-type array)))
(defun nu:rand-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (nu:rand (array-dimensions array) :type (array-element-type array)))
(defun nu:full-like (array value)
  (declare (type cl:array array))
  (nu:full (array-dimensions array) :value value
           :type (array-element-type array)))

(defun nu:shape (array-like &optional (axis nil axis-p))
  ;; This is a potentially domain specific functionality; since
  ;; there exists the ambiguity of what should one do with strings
  (let ((dimensions (typecase array-like
                      (sequence (cons (length array-like)
                                      (nu:shape (elt array-like 0))))
                      (array (array-dimensions array-like))
                      (t nil))))
    (if axis-p
        (elt dimensions axis)
        dimensions)))

(declaim (inline nu:reshape))
(defun nu:reshape (array new-dimensions)
  (make-array new-dimensions
              :element-type (array-element-type array)
              :displaced-to (array-storage array)
              :displaced-index-offset (cl-array-offset array)))

;;; TODO: Change &KEY TYPE to &KEY (TYPE DEFAULT-ELEMENT-TYPE)
;;; Requires changes to polymorphic-functions
(define-polymorphic-function nu:asarray (array-like &key type layout) :overwrite t
  :documentation "ARRAY-LIKE can be a nested sequence of sequences, or an array.")
(declaim (notinline nu:asarray))
(defpolymorph (nu:asarray :inline t) (array-like &key ((type (eql :auto))) (layout :row-major))
    (values cl:array &optional)
  (declare (ignore type))
  (ensure-row-major-layout)
  (nu:asarray array-like :type (element-type array-like)))

(define-polymorphic-function nu:copy (x &key out broadcast))
(defpolymorph nu:asarray (array-like &key (#-extensible-compound-types
                                           (type (polymorphic-functions.extended-types:subtypep real))
                                           #+extensible-compound-types
                                           (type (extensible-compound-types:subtypep real))
                                           default-element-type)
                                     (layout :row-major))
    (values cl:array &optional)
  ;; TODO: Define the predicate array-like-p.
  (ensure-row-major-layout)
  (etypecase array-like
    (array
     (nu:copy array-like :out (nu:zeros (narray-dimensions array-like)
                                        :type type)))
    (sequence
     (let* ((result-array (nu:zeros (nu:shape array-like) :type type))
            (index 0)
            (rsv (array-storage result-array)))
       (labels ((%asarray (array-like)
                  (etypecase array-like
                    (sequence
                     (map nil #'%asarray array-like))
                    (array
                     (let ((asv (array-storage array-like))
                           (offset (cl-array-offset array-like)))
                       (loop :for i :from offset
                               :below (+ offset (array-total-size array-like))
                             :do (setf (cl:aref rsv index)
                                       ;; We will leave the optimization for this
                                       ;; to cl-form-types
                                       (trivial-coerce:coerce
                                        (cl:aref asv i)
                                        type))
                                 (incf index))))
                    (real
                     (setf (cl:aref rsv index)
                           (trivial-coerce:coerce (the real array-like) type))
                     (incf index)))))
         (%asarray array-like))
       result-array))
    (atom (make-array 1 :element-type type
                        :initial-element (trivial-coerce:coerce array-like type)))))

(defmacro nu:macro-map-array (result-array function &rest arrays)
  (alexandria:with-gensyms (result i result-type)
    (let ((array-syms (alexandria:make-gensym-list (length arrays) "ARRAY"))
          (function   (cond ((eq 'quote (first function)) (second function))
                            ((eq 'function (first function)) (second function))
                            (t (error "Unexpected")))))
      `(let (,@(loop :for sym :in array-syms
                     :for array-expr :in arrays
                     :collect `(,sym ,array-expr)))
         (declare (type array ,@array-syms))
         ;; TODO: Optimize this
         (let* ((,result (or ,result-array (nu:zeros-like ,(first array-syms))))
                (,result-type (array-element-type ,result)))
           (dotimes (,i (array-total-size ,(first array-syms)))
             (funcall #'(setf row-major-aref)
                      (trivial-coerce:coerce
                       (,function ,@(mapcar (lm array-sym `(row-major-aref ,array-sym ,i))
                                            array-syms))
                       ,result-type)
                      ,result ,i))
           ,result)))))

(defmacro nu:do-arrays (bindings &body body)
  "Each element of BINDINGS is of the form (VAR ARRAY-EXPR &OPTIONAL ELEMENT-TYPE)"
  (let* ((variables   (mapcar #'first bindings))
         (array-exprs (mapcar #'second bindings))
         (num-vars    (length variables))
         (array-vars  (make-gensym-list num-vars "ARRAY"))
         (storages    (make-gensym-list num-vars "ARRAY-STORAGE"))
         (indices     (make-gensym-list num-vars "INDEX")))
    `(let (,@(mapcar (lm v e `(,v ,e))
                     array-vars array-exprs))
       ,@(if (< num-vars 2)
             ()
             (mapcar (lm v `(assert (equalp (narray-dimensions ,v)
                                            (narray-dimensions ,(first array-vars)))
                                    (,v ,(first array-vars))
                                    "DO-ARRAYS expects equal dimensions, but are ~S and ~S"
                                    (narray-dimensions ,v)
                                    (narray-dimensions ,(first array-vars))))
                     array-vars))
       (let (,@(mapcar (lm s v `(,s (array-storage ,v)))
                       storages array-vars)
             ,@(mapcar (lm i v `(,i (cl-array-offset ,v)))
                       indices array-vars))
         (symbol-macrolet (,@(mapcar (lm v s i `(,v (cl:aref ,s ,i)))
                                     variables storages indices))
           ,(when array-vars
              `(loop :repeat (array-total-size ,(first array-vars))
                     :do (locally ,@body)
                     ,@(mapcar (lm i `(incf ,i)) indices))))))))

;; (defmacro nu:with-inline (&body body)
;;   `(let ()
;;      (declare (inline ,@(iter (for symbol in-package :numericals external-only t)
;;                               (when (and (fboundp symbol)
;;                                          (not (macro-function symbol)))
;;                                 (collect symbol)))))
;;      ,@body))

;; (defmacro nu:def-array
;;     (var dimensions &rest args
;;      &key (type default-element-type) initial-element initial-contents
;;        adjustable fill-pointer displaced-to displaced-index-offset
;;        (proclaim-type t) doc)
;;   ;; TODO: use array as function
;;   (declare (ignorable initial-element initial-contents adjustable fill-pointer
;;                       displaced-to displaced-index-offset))
;;   (remf args :type)
;;   (remf args :proclaim-type)
;;   (remf args :doc)
;;   (when (typep type 'nu:numericals-array-element-type)
;;     (setq type (list 'quote type)))
;;   (once-only (dimensions type)
;;     `(progn
;;        ,(when proclaim-type `(proclaim (list 'type
;;                                              (list ',(if displaced-to 'array 'simple-array)
;;                                                    ,type
;;                                                    ,dimensions)
;;                                              ',var)))
;;        (defparameter ,var (make-array ,dimensions :element-type ,type
;;                                       ,@args))
;;        ,(when doc `(setf (documentation ',var 'variable) ,doc))))  )

;; (defmacro nu:with-array ((var dimensions &rest args
;;                               &key (type default-element-type type-p) initial-element initial-contents
;;                               adjustable fill-pointer displaced-to displaced-index-offset
;;                               (declare-type t)) &body body &environment env)
;;   ;; Compile time value of DEFAULT-ELEMENT-TYPE is used!!
;;   (declare (ignorable initial-element initial-contents adjustable fill-pointer
;;                       displaced-to displaced-index-offset))
;;   (remf args :type)
;;   (remf args :declare-type)

;;   ;; Parse type
;;   (setq type (cond ((not type-p) type)
;;                    ((constantp type env)
;;                     (constant-form-value type env))
;;                    ((eq type 'default-element-type) default-element-type)
;;                    (t (error 'maybe-form-not-constant-error :form type))))
  
;;   ;; Parse dimensions
;;   (when (constantp dimensions env)
;;     (setq dimensions (constant-form-value dimensions env)))
  
;;   `(let ((,var (make-array (list ,@dimensions) :element-type ',type
;;                            ,@args)))
;;      ,(when declare-type
;;         `(declare (type (,(if displaced-to 'array 'simple-array)
;;                           ,type
;;                           ,dimensions)
;;                         ,var)))
;;      ,@body))

;; (defmacro nu:with-arrays* (bindings &body body)
;;   (if (cdr bindings)
;;       `(nu:with-array ,(car bindings)
;;          ,(macroexpand-1 `(nu:with-arrays* ,(cdr bindings) ,@body)))
;;       `(nu:with-array ,(car bindings) ,@body)))

;; ;; TODO: constantp with symbol-macros is implementation dependent
;; ;; SBCL says symbol-macros are constant, CCL says no;
;; ;; Smoothen out this behaviour
;; (defun-c ensure-get-constant-value (form env)
;;   (if (constantp form env)
;;       (constant-form-value form env)
;;       (error 'maybe-form-not-constant :form form)))

;; (define-condition maybe-form-not-constant-error (error)
;;   ((form :reader form :initform (error "FORM not supplied") :initarg :form))
;;   (:report (lambda (condition stream)
;;              (format stream
;;                      "~&The following form could not be determined to be a constant~%~%~D~%"
;;                      (form condition)))))

;; ;; A failed implementation of nu:with-constant that wanted to consider
;; ;; dynamic variables.

;; ;; (defmacro nu:with-constant ((var value) &body body &environment env)
;; ;;   (unless (constantp value env)
;; ;;     (error 'maybe-form-not-constant-error :form value))
;; ;;   (if (eq :special (variable-information var))
;; ;;       (if (eq var 'default-element-type)
;; ;;           (let ((*type* (ensure-get-constant-value value env)))
;; ;;             (print `(let ((*type* ',(ensure-get-constant-value value env)))
;; ;;                       ,@(mapcar (rcurry #'macroexpand env) body))))
;; ;;           (error "This case has not been considered!"))
;; ;;       `(symbol-macrolet ((,var ,value))
;; ;;          ,@body)))

;; ;; (defmacro nu:with-constants (bindings &body body)
;; ;;   (if (cdr bindings)
;; ;;       `(nu:with-constant ,(car bindings)
;; ;;          (nu:with-constants ,(cdr bindings) ,@body))
;; ;;       `(nu:with-constant ,(car bindings) ,@body)))

;; ;; (nu:with-constants ((*type* 'double-float)
;; ;;                     (shape '(10 10)))
;; ;;   (nu:with-array (f shape) f))

;; (defmacro nu:with-constant ((var value) &body body &environment env)
;;   (unless (constantp value env)
;;     (error 'maybe-form-not-constant-error :form value))
;;   (if (eq :special (variable-information var))
;;       (error "~D should ne not be special" var)
;;       `(symbol-macrolet ((,var ,value))
;;          ,@body)))

;; (defmacro nu:with-constants (bindings &body body &environment env)
;;   `(symbol-macrolet ,(loop :for (var value) :in bindings
;;                         :do (unless (constantp value env)
;;                               (error 'maybe-form-not-constant-error :form value))
;;                           (when (specialp var)
;;                             (error "~D should not be special" var))
;;                         :finally (return bindings))
;;      ,@body))
