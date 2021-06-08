(in-package :numericals.internals)

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
                     (introspect-environment:typexpand-1 'nu:numericals-array-element-type)
                     (condition-type c)))))


(defun n-arg-fn-compiler-macro (form &optional env)
  (compiler-macro-notes:with-notes (form env :optimization-note-condition
                                    optim-speed)
    ;; 0. Abort optimization if not asked to do so; to help debug better                  
    (unless optim-speed (return-from n-arg-fn-compiler-macro form))
    (let* ((fn-name (if (eq 'funcall (first form))
                        (second form)
                        (first form)))
           (args    (if (eq 'funcall (first form))
                        (cddr form)
                        (cdr form)))
           (arg-types
             (loop :for arg :in args
                   :collect (cl-form-types:nth-form-type arg env 0 t t)))
           
           (cl-fn  (find-symbol (symbol-name fn-name) :cl))
           (two-arg-fn (find-symbol (concatenate 'string "TWO-ARG-" (symbol-name fn-name))
                                    :numericals))
           (out-pos
             (loop :for arg :in args
                   :for arg-type :in arg-types
                   :for i :from 0
                   :if (subtypep arg-type '(eql :out))
                     :do (return (1+ i))
                   ;; 1. Optimization is insignificant if OUT is unsupplied
                   :finally (cond
                              ((every (lm type (subtypep type 'number))
                                      arg-types)
                               (return-from n-arg-fn-compiler-macro `(,cl-fn ,@args)))
                              ((= 1 (length arg-types))
                               (return-from n-arg-fn-compiler-macro
                                 (cond ((member fn-name '(nu:+ nu:*))
                                        (first args))
                                       ((eq fn-name 'nu:-)
                                        `(,two-arg-fn 0 ,@args))
                                       ((member fn-name '(nu:/ nu:< nu:<= nu:= nu:/= nu:>= nu:>))
                                        `(,two-arg-fn 1 ,@args))
                                       (t (error "Unexpected!")))))
                              ((= 2 (length arg-types))
                               (return-from n-arg-fn-compiler-macro `(,two-arg-fn ,@args)))
                              ;; If it's more than two, then we need to first allocate an OUT
                              (t
                               (signal 'out-unknown-at-compile-time :args
                                       args))))))
      
      ;; At this point, we know that OUT exists, but the number of arguments can be arbitrary
      ;; 2. We don't want to not detect bad cases                    
      (unless (null (nthcdr (1+ out-pos) args))
        (signal 'bad-position-for-out :args args))
      (let ((out-arg (nth out-pos args))
            (out-type (nth out-pos arg-types))
            (array-likes (subseq args 0 (1- out-pos)))
            (array-types (subseq arg-types 0 (1- out-pos))))
        ;; 3. OUT is supplied and is at the right place, but the array-likes are
        ;; either not arrays or not of types TYPE= to OUT                      
        (unless (subtypep out-type 'array)
          (signal 'arg-is-not-array :arg out-arg :arg-type out-type))
        (mapc
         (lambda (array-like array-type)
           (unless (subtypep array-type 'array)
             (signal 'arg-is-not-array :arg array-like :arg-type array-type))
           (unless (type= array-type out-type)
             (signal 'arg-out-type-mismatch :arg array-like :arg-type
                     array-type :out out-arg :out-type out-type)))
         array-likes array-types)
        (with-gensyms (out-sym)
          (let* ((array-like-syms
                   (make-gensym-list (length array-likes) "array-like"))
                 (element-type
                   (sandalphon.compiler-macro:array-type-element-type out-type))
                 (main-code
                   (cond
                     ((null array-like-syms)
                      `(,two-arg-fn
                        (the ,element-type (coerce ,(case fn-name
                                                      (nu:+ 0)
                                                      (nu:* 1)
                                                      (t (return-from
                                                          n-arg-fn-compiler-macro
                                                           form)))
                                                   ',element-type))
                        ,out-sym
                        :out ,out-sym))
                     ((null (rest array-like-syms))
                      `(,two-arg-fn
                        (the ,element-type (coerce ,0 ',element-type))
                        ,(first array-like-syms) :out ,out-sym))
                     (t
                      `(progn
                         (,two-arg-fn ,(first array-like-syms)
                                      ,(second array-like-syms) :out ,out-sym)
                         ,@(mapcar
                            (lm sym `(,two-arg-fn ,out-sym ,sym :out ,out-sym))
                            (cddr array-like-syms)))))))
            (unless (typep element-type 'nu:numericals-array-element-type)
              (signal 'array-type-is-unoptimized :type element-type))
            `(the ,out-type
                  (let* (,@(mapcar (lm sym array-like `(,sym ,array-like))
                                   array-like-syms array-likes)
                         (,out-sym ,out-arg))
                    (declare
                     ,@(loop :for sym :in array-like-syms
                             :for array-like :in array-likes
                             :collect `(type ,(cl-form-types:nth-form-type array-like env 0 t t)
                                             ,sym))
                     (type ,(cl-form-types:nth-form-type out-arg env 0 t t) ,out-sym))
                    ,main-code
                    ,out-sym))))))))

(macrolet ((def (name)
             `(setf (compiler-macro-function ',name)
                    #'n-arg-fn-compiler-macro)))
  (def nu:+)
  (def nu:-)
  (def nu:*)
  (def nu:/)

  (def nu:<)
  (def nu:<=)
  (def nu:=)
  (def nu:/=)
  (def nu:>=)
  (def nu:>))
