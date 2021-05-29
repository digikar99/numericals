(in-package :numericals.internals)
;;; The value is set in package / package+array file.

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
            (destructuring-bind (shape &key (type default-element-type))
                (split-at-keywords args)
              (when (listp (first shape))
                (assert (null (rest shape)) (shape)
                        "Expected (REST SHAPE) to be NULL but is ~S" (rest shape))
                (setq shape (first shape)))
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
                         `((lambda (shape &key (type default-element-type))
                             (make-array shape :element-type type))
                           ,@args)))
                      ((every (lm type (subtypep type 'size)) arg-types)
                       (return-from ,name
                         `((lambda (shape &key (type default-element-type))
                             (make-array shape :element-type type))
                           (list ,@args))))
                      ((and (>= args-length 2)
                            (every (lm type (subtypep type 'size))
                                   (subseq arg-types 0 (- args-length 2)))
                            (subtypep second-last-type '(eql :type)))
                       (return-from ,name
                         `((lambda (shape &key (type default-element-type))
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
      (shape &key (type default-element-type) (min 0) (max 1))
      (split-at-keywords args)
    (when (listp (first shape))
      (assert (null (rest shape)))
      (setq shape (first shape)))
    (let* ((a (nu:zeros (the list shape) :type type))
           (asv (array-storage a))
           (range (coerce (- max min) type))
           (min (coerce min type)))
      (declare (type (cl:simple-array * 1) asv))
      ;; A good use of specialized function :)
      (specialized-function:specializing (asv min range) ()
        (dotimes (index (array-total-size a))
          (funcall #'(setf row-major-aref) (+ min (random range)) asv index)))
      a)))

(declaim (inline nu:zeros-like nu:ones-like nu:rand-like))
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



(defun cast (to-type object)
  (etypecase object
    (sequence (map 'list (curry #'cast to-type) object))
    (array (nu:astype object to-type))
    (t (case to-type
         (fixnum (nth-value 0 (floor object)))
         (t (coerce object to-type))))))

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
;;; TODO: Rewrite
(defun nu:astype (array type)
  (let* ((storage-vector (1d-storage-array array))
         (return-array (make-array (array-dimensions array)
                                   :element-type type
                                   :initial-element (array-initial-element type)))
         (ra-storage-vector (1d-storage-array return-array)))
    (loop for i fixnum from 0 below (length storage-vector)
          do (setf (cl:aref ra-storage-vector i)
                   (cast type (cl:aref storage-vector i)))
          finally (return return-array))))

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

(defun single-float-type-p (object)
  (type= object 'single-float))
(deftype our-type= (type)
  (eswitch (type :test #'type=)
    ('single-float `(satisfies single-float-type-p))))

(define-polymorphic-function nu:asarray (array-like &key type) :overwrite t
  :documentation "ARRAY-LIKE can be a nested sequence of sequences, or an array.")
(defpolymorph nu:asarray (array-like &key (type default-element-type))
    (values cl:array &optional)
  ;; TODO: Define the predicate array-like-p.
  (etypecase array-like
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
                     (incf index))
                    (t
                     (setf (cl:aref rsv index)
                           (trivial-coerce:coerce array-like type))
                     (incf index)))))
         (%asarray array-like))
       result-array))
    (array
     (nu:astype array-like type))))

(defmacro nu:with-inline (&body body)
  `(let ()
     (declare (inline ,@(iter (for symbol in-package :numericals external-only t)
                              (when (and (fboundp symbol)
                                         (not (macro-function symbol)))
                                (collect symbol)))))
     ,@body))

(defmacro nu:def-array
    (var dimensions &rest args
     &key (type default-element-type) initial-element initial-contents
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
                              &key (type default-element-type type-p) initial-element initial-contents
                              adjustable fill-pointer displaced-to displaced-index-offset
                              (declare-type t)) &body body &environment env)
  ;; Compile time value of DEFAULT-ELEMENT-TYPE is used!!
  (declare (ignorable initial-element initial-contents adjustable fill-pointer
                      displaced-to displaced-index-offset))
  (remf args :type)
  (remf args :declare-type)

  ;; Parse type
  (setq type (cond ((not type-p) type)
                   ((constantp type env)
                    (constant-form-value type env))
                   ((eq type 'default-element-type) default-element-type)
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
;;       (if (eq var 'default-element-type)
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
