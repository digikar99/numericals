(in-package :dense-numericals.impl)


(define-condition out-unknown-at-compile-time
    (compiler-macro-notes:optimization-failure-note)
  ((args :initarg :args :reader condition-args))
  (:report (lambda (c s)
             (format s "Unable to infer :OUT parameter from argument list~%  ~S~%not derived to be numbers"
                     (condition-args c)))))

(define-condition bad-position-for-out
    (compiler-macro-notes:optimization-failure-note)
  ((args :initarg :args :reader condition-args))
  (:report (lambda (c s)
             (format s "Failed to infer :OUT argument to be the last in the argument list~%  ~S"
                     (condition-args c)))))

(define-condition arg-out-type-mismatch
    (compiler-macro-notes:optimization-failure-note)
  ((arg :initarg :arg :reader condition-arg)
   (arg-type :initarg :arg-type :reader condition-arg-type)
   (out :initarg :out :reader condition-out)
   (out-type :initarg :out-type :reader condition-out-type))
  (:report (lambda (c s)
             (format s "Argument~%  ~S~%is derived to be of type~%  ~S~%which is different from type~%  ~S~%of OUT~%~S"
                     (condition-arg c)
                     (condition-arg-type c)
                     (condition-out-type c)
                     (condition-out c)))))

(define-condition arg-is-not-array
    (compiler-macro-notes:optimization-failure-note)
  ((arg :initarg :arg :reader condition-arg)
   (arg-type :initarg :arg-type :reader condition-arg-type))
  (:report (lambda (c s)
             (format s "Expected argument~%  ~S~%to be an array but it was derived to be a~%  ~S"
                     (condition-arg c)
                     (condition-arg-type c)))))

(define-condition array-type-is-unoptimized
    (compiler-macro-notes:optimization-failure-note)
  ((type :initarg :type :reader condition-type))
  (:report (lambda (c s)
             (format s "DENSE-NUMERICALS is only optimized for arrays with element types~%  ~S~%and not: ~S"
                     dn:+optimized-types+
                     (condition-type c)))))


(macrolet ((def (name reduce-fn initial-value &optional returns-identity)

`(define-compiler-macro ,name (&whole form &rest args &environment env)
   (compiler-macro-notes:with-notes
       (form :optimization-note-condition optim-speed)
     ;; 0. Abort optimization if not asked to do so; to help debug better
     (unless optim-speed (return-from ,name form))
     (let* ((arg-types (loop :for arg :in args
                             :collect (cl-form-types:nth-form-type arg env 0 t t)))
            (out-pos (loop :for arg :in args
                           :for arg-type :in arg-types
                           :for i :from 0
                           :if (subtypep arg-type '(eql :out))
                             :do (return (1+ i))
                                 ;; 1. Optimization is insignificant if OUT is unsupplied
                           :finally
                              (cond ((every (lm type (subtypep type 'number)) arg-types)
                                     (return-from ,name `(,',(cl-name reduce-fn) ,@args)))
                                    ,@(if returns-identity
                                          `(((= 1 (length arg-types))
                                             (return-from ,name (first args)))))
                                    ((= 2 (length arg-types))
                                     (return-from ,name `(,',reduce-fn ,@args)))
                                    (t
                                     (signal 'out-unknown-at-compile-time
                                             :args args))))))
       ;; 2. We don't want to not detect bad cases
       (unless (null (nthcdr (1+ out-pos) args))
         (signal 'bad-position-for-out :args args))
       (let ((out-arg     (nth out-pos args))
             (out-type    (nth out-pos arg-types))
             (array-likes (subseq args 0 (1- out-pos)))
             (array-types (subseq arg-types 0 (1- out-pos))))
         ;; 3. OUT is supplied and is at the right place, but the array-likes are
         ;; either not arrays or not of types TYPE= to OUT
         (unless (subtypep out-type 'array)
           (signal 'arg-is-not-array :arg out-arg :arg-type out-type))
         (mapc (lambda (array-like array-type)
                 (unless (subtypep array-type 'array)
                   (signal 'arg-is-not-array :arg array-like :arg-type array-type))
                 (unless (type= array-type out-type)
                   (signal 'arg-out-type-mismatch
                           :arg array-like
                           :arg-type array-type
                           :out out-arg
                           :out-type out-type)))
               array-likes
               array-types)
         ;; 4. Everything else is good; now we only need to worry about broadcasting
         (with-gensyms (out-sym broadcast-compatible-p dimensions)
           (let* ((array-like-syms (make-gensym-list (length array-likes) "ARRAY-LIKE"))
                  (element-type    (abstract-arrays:array-type-element-type out-type))
                  (main-code       (cond ((null array-like-syms)
                                          `(,',reduce-fn (the ,element-type
                                                              (coerce ,,initial-value ',element-type))
                                                         ,out-sym
                                                         :out ,out-sym))
                                         ((null (rest array-like-syms))
                                          `(,',reduce-fn (the ,element-type
                                                              (coerce ,,initial-value ',element-type))
                                                         ,(first array-like-syms)
                                                         :out ,out-sym))
                                         (t
                                          `(progn
                                             (,',reduce-fn ,(first array-like-syms)
                                                           ,(second array-like-syms)
                                                           :out ,out-sym)
                                             ,@(mapcar (lm sym `(,',reduce-fn ,out-sym ,sym
                                                                              :out ,out-sym))
                                                       (cddr array-like-syms)))))))
             (unless (member element-type dn:+optimized-types+
                             :test (lambda (a b)
                                     (and (trivial-types:type-specifier-p a)
                                          (trivial-types:type-specifier-p b)
                                          (not (eq a 'cl:*))
                                          (not (eq b 'cl:*))
                                          (type= a b))))
               (signal 'array-type-is-unoptimized :type element-type))
             ;; FIXME: Mysterious consing
             `(the ,out-type
                   (let* (,@(mapcar (lm sym array-like `(,sym ,array-like))
                                    array-like-syms array-likes)
                          (,out-sym ,out-arg)
                          (,dimensions (narray-dimensions ,out-sym)))
                     (declare (type (,(if (and (subtypep out-type 'simple-array)
                                               (every (lm type (subtypep type 'simple-array))
                                                      array-types))
                                          'simple-array
                                          'array)
                                     ,element-type)
                                    ,@array-like-syms ,out-sym))
                     (if (and ,@(mapcar (lm sym `(equal ,dimensions
                                                        (narray-dimensions ,sym)))
                                        array-like-syms))
                         (locally (declare (optimize (safety 0)))
                           ,main-code)
                         (multiple-value-bind (,broadcast-compatible-p ,dimensions)
                             (broadcast-compatible-p ,out-sym ,@array-like-syms)
                           (if ,broadcast-compatible-p
                               (let (,@(mapcar (lm sym `(,sym (broadcast-array ,sym ,dimensions)))
                                               array-like-syms))
                                 (declare (type (array ,element-type) ,@array-like-syms ,out-sym)
                                          (optimize (safety 0)))
                                 ,main-code)
                               (error 'incompatible-broadcast-dimensions
                                      :array-likes (list ,@array-like-syms ,out-sym)
                                      :dimensions (mapcar #'narray-dimensions
                                                          (list ,@array-like-syms))))))
                     ,out-sym))))))))))

  (def dn:+ dn:two-arg-+ 0 t)
  (def dn:- dn:two-arg-- 0)
  (def dn:* dn:two-arg-* 1 t)
  (def dn:/ dn:two-arg-/ 1))
