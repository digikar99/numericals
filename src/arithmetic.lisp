(in-package :numericals.internals)

(defun %broadcast-compatible-p (dimensions-a dimensions-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two dimensions are broadcast compatible.
  The second value is the dimensions of the array resulting from the broadcast."
  (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1)) 
        ;; We do not use a "for in" clause because we want to terminate at the
        ;; maximum of the two lists rather than the minimum
        (for dim-a initially (reverse dimensions-a)
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (reverse dimensions-b)
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b) (= a 1) (= b 1))
                     (max a b)
                     (return nil))
          into broadcast-dimensions-reversed)
        (finally (return (values t
                                 (nreverse broadcast-dimensions-reversed))))))

(defun ensure-array-dimensions (array-like)
  (etypecase array-like
    (array (array-dimensions array-like))
    ;; In the future, may be make efforts to handle nested lists
    (list (list (length array-like)))))

(defun broadcast-compatible-p (&rest arrays-or-dimensions)
  "Returns two values:
  The first value is a generalized boolean indicating whether the arrays can be broadcasted.
  The second value is the dimension of the array resulting from the broadcast."
  (case (length arrays-or-dimensions)
    (0 t)
    (1 (values t (ensure-array-dimensions (first arrays-or-dimensions))))
    (2 (%broadcast-compatible-p (ensure-array-dimensions (first arrays-or-dimensions))
                                (ensure-array-dimensions (second arrays-or-dimensions))))
    (t (multiple-value-bind (compatible-p broadcast-dimensions)
           (%broadcast-compatible-p
            (ensure-array-dimensions (first arrays-or-dimensions))
            (ensure-array-dimensions (second arrays-or-dimensions)))
         ;; Can this be simplified?
         (when compatible-p
           (multiple-value-bind (compatible-p-rest broadcast-dimensions-rest)
               (apply 'broadcast-compatible-p (cddr arrays-or-dimensions))
             (when compatible-p-rest
               (%broadcast-compatible-p broadcast-dimensions
                                        broadcast-dimensions-rest)))))))  )

(defun array-like-p (object) (or (listp object) (arrayp object)))
;; support only 1d lists currently

;; What do we do when this variable is modified?
(defun ensure-array (type object)
  (etypecase object
    (list (make-array (nu:shape object) :element-type type
                      :initial-contents (cast type object)))
    (array (if (eq type (array-element-type object))
               object
               (nu:astype object type)))
    (cl:array (nu:astype (cl-array-array object) type))
    (number (make-array '(1) :element-type type
                        :initial-element (cast type object)))))

(macrolet
    ((define-broadcast-wrapper (name broadcast-operation base-operation identity)

       ;; Assume quoted; because it looks good
       (setq broadcast-operation (cadr broadcast-operation)
             base-operation (print (cadr (print base-operation))))
       ;; Do the checks to provide helpful error messages.
       `(progn

          ;; TODO: Separate docstrings into separate files
          (defun ,name (&rest args)
            "ARGS must be a lambda list with the initial elements comprising of the list
of operands, and the final optional elements comprise of :OUT and :TYPE 
keyword args. For example
  (* a b :out c :type 'single-float)
  (+ a b c :out d)"
            (declare (optimize (speed 3)))
            (destructuring-bind (args (&key (out nil out-supplied-p)
                                            (type *type*)))
                (split-at-keywords args)
              (let ((broadcast-dimensions nil)
                    (orig-args args)
                    (some-array-p (some #'array-like-p args)))
                (declare (type list broadcast-dimensions))
                (if some-array-p
                    (progn
                      (setq args (mapcar (curry #'ensure-array type) args))
                      (setq broadcast-dimensions
                            (nth-value 1 (apply #'broadcast-compatible-p args)))
                      (unless broadcast-dimensions
                        (error "Cannot broadcast ~D together" orig-args))
                      (when out-supplied-p
                        (assert (arrayp out) nil
                                "Cannot supply result in non-array type ~D" out)
                        (assert (equalp (array-dimensions out) broadcast-dimensions)))
                      (when (not out-supplied-p)
                        (setq out
                              ,(if (= identity 1)
                                   `(nu:ones broadcast-dimensions :type type)
                                   `(nu:zeros broadcast-dimensions :type type))))

                      (apply ',broadcast-operation type broadcast-dimensions out args))
                    (progn
                      (assert (not out-supplied-p) nil
                              "Cannot supply result in ~D when no argument is array-like."
                              out)
                      (coerce (apply ',base-operation args) type))))))

          (define-compiler-macro ,name (&whole whole &rest orig-args &environment env)
            (let ((optimizable-p (= 3 (policy-quality 'speed env))))
              (if (member (car whole) (list ',name 'funcall)) ; ignoring the case of funcall or apply
                  (destructuring-bind (args (&key (out nil out-supplied-p)
                                                  (type *type* type-p)))
                      (split-at-keywords orig-args)
                    (declare (ignorable args out type type-p out-supplied-p))
                    (when optimizable-p
                      (setq type
                            (cond ((and (not type-p)
                                        *lookup-type-at-compile-time*)
                                   (list 'quote *type*))
                                  ;; TODO: These patterns of quoting abound in compiler macros the way I'm doing things. Abstract them out, or find (and abstract?) a better pattern.
                                  ((and (listp type)
                                        (eq (car type) 'quote)
                                        (null (cddr type))
                                        (valid-numericals-type-p (cadr type)))
                                   type)
                                  (t type))))
                    (when optimizable-p
                      (when (and (every #'(lambda (arg) (or (numberp arg)
                                                            (typep arg 'number env)))
                                        args)
                                 (not out-supplied-p))
                        (return-from ,name (list 'cast type (cons ',base-operation args)))))

                    ;; TODO: These notes should not be printed if call cannot be optimized.
                    (when (and optimizable-p out-supplied-p)
                      (let ((out-type (variable-type out env)))
                        (unless (and (listp out-type) (= 3 (length out-type)))
                          (format t
                                  "~&; note: Unable to determine optimizability of call to ~S because type of ~S ~S is not exact"
                                  ',name
                                  out out-type))
                        (when (and (listp out-type) (= 3 (length out-type))
                                   (subtypep out-type
                                             '(simple-array single-float *))
                                   (every
                                    (lambda (x)
                                      (let ((x-type (variable-type x env)))
                                        (unless (= 3 (length x-type))
                                          (format t
                                                  "~&; note: Unable to determine optimizability of call to ~S because type of ~S ~S is not exact"
                                                  ',name x x-type))
                                        (equalp x-type out-type)))
                                    args))
                          (return-from ,name
                            (let ((base-operation ',base-operation))
                              `(with-elementwise-operations
                                   'single-float ,out (,base-operation ,@args)))))))
                    
                    ;; args is a list ; TODO: What were we trying to do here?
                    ;; (when optimizable-p
                    ;;   (setq dimensions
                    ;;         (if (null (cdr dimensions))
                    ;;             (if (or (typep (car dimensions) '(or list fixnum))
                    ;;                     (subtypep (variable-type (car dimensions) env)
                    ;;                               '(or list fixnum)))
                    ;;                 (car dimensions)
                    ;;                 (progn
                    ;;                   (setq optimizable-p nil)
                    ;;                   (format
                    ;;                    t "~&; note: ~D"
                    ;;                    (format
                    ;;                     nil
                    ;;                     ,(format nil "Unable to optimize ~S without knowing type of ~~D at compile-time." name)
                    ;;                     (car dimensions)))
                    ;;                   dimensions))
                    ;;             dimensions)))
                    whole)
                  (progn
                    (when (= 3 (policy-quality 'speed env))
                      (format t "~& note: Optimization for call to ~S is not implemented for ~S" ',name whole)
                      whole))))))))

  (define-broadcast-wrapper nu:+ '%+ '+ 0)
  (define-broadcast-wrapper nu:- '%- '- 0)
  (define-broadcast-wrapper nu:* '%* '* 1)
  (define-broadcast-wrapper nu:/ '%/ '/ 1))

;; To be able to handle *max-broadcast-dimensions*, we need to generate %+ using a macro
;; TODO: See if static-dispatch can help simplify this branching
(macrolet
    ((define-broadcast-operation (name operation)
       `(defun ,name (type broadcast-dimensions result &rest args)
          (declare (optimize (speed 3)))
          (ecase type
            ,@(loop for type in '(single-float)
                 collect
                   `(,type
                     (ecase (length broadcast-dimensions)
                       ,@(loop for i from 1 to *max-broadcast-dimensions*
                            collect
                              `(,i ,(let ((specialized-op
                                           (specialized-operation operation type i))
                                          (non-broadcast-op
                                           (non-broadcast-operation operation type)))
                                      `(loop while (cddr args)
                                          for a = (car args)
                                          do
                                            (if (equalp (array-dimensions result)
                                                        (array-dimensions a))
                                                (,non-broadcast-op result result a)
                                                (,specialized-op result result a))
                                            (setq args (cdr args))
                                          finally
                                            (if (cdr args) ; given (cddr args) is null
                                                (if (and (equalp
                                                          (array-dimensions result)
                                                          (array-dimensions (car args)))
                                                         (equalp
                                                          (array-dimensions result)
                                                          (array-dimensions (cadr args))))
                                                    (,non-broadcast-op result
                                                                       (car args)
                                                                       (cadr args))
                                                    (,specialized-op result
                                                                     (car args)
                                                                     (cadr args)))
                                                (if (equalp (array-dimensions result)
                                                            (array-dimensions (car args)))
                                                    (,non-broadcast-op result
                                                                       result
                                                                       (car args))
                                                    (,specialized-op result
                                                                     result
                                                                     (car args))))
                                            (return result))))))))))))

  (define-broadcast-operation %+ +)
  (define-broadcast-operation %- -)
  (define-broadcast-operation %/ /)
  (define-broadcast-operation %* *))

(macrolet
    ((define-unary-wrapper (name base-operation)
       
       ;; Assume quoted; because it looks good
       (setq base-operation (print (cadr (print base-operation))))
       ;; Do the checks to provide helpful error messages.
       `(progn

          ;; TODO: Separate docstrings into separate files
          (defun ,name (arg &key (out nil out-supplied-p) (type *type*))
            "ARGS must be a lambda list with the initial elements comprising of the list
of operands, and the final optional elements comprise of :OUT and :TYPE
keyword args. For example
  (sqrt a :out c :type 'single-float)
  (sqrt a :out d)"
            (declare (optimize (speed 3)))
            (if (array-like-p arg)
                (let ((arg-dimensions (progn
                                        (setq arg (ensure-array type arg))
                                        (array-dimensions arg))))
                  (when out-supplied-p
                    (assert (arrayp out) nil
                            "Cannot supply result in non-array type ~D" out)
                    (assert (equalp (array-dimensions out) arg-dimensions)))
                  (when (not out-supplied-p)
                    (setq out (nu:zeros (array-dimensions arg) :type type)))
                  (ecase type
                    ,@(loop :for type :in '(single-float)
                         :for non-broadcast-op := (non-broadcast-operation base-operation type)
                         :collect `(,type (,non-broadcast-op out arg))))
                  out)
                (progn
                  (assert (not out-supplied-p) nil
                          "Cannot supply result in ~D when argument is not array-like."
                          out)
                  (coerce (,base-operation arg) type))))

          (define-compiler-macro ,name (&whole whole arg
                                               &key (out nil out-supplied-p)
                                               (type *type* type-p)
                                               &environment env)
            (declare (ignorable arg out type type-p out-supplied-p))
            (let ((optimizable-p (= 3 (policy-quality 'speed env))))
              (if (member (car whole) (list ',name 'funcall)) ; ignoring the case of funcall or apply
                  (progn
                    (when optimizable-p
                      (setq type
                            (cond ((and (not type-p)
                                        *lookup-type-at-compile-time*)
                                   (list 'quote *type*))
                                  ((and (listp type)
                                        (eq (car type) 'quote)
                                        (null (cddr type))
                                        (valid-numericals-type-p (cadr type)))
                                   type)
                                  (t type))))

                    ;; Takes care of the numberp branch
                    (when optimizable-p
                      (when (or (numberp arg) (typep arg 'number env))
                        (if out-supplied-p
                            (setq optimizable-p nil)
                            (return-from ,name (list 'cast type
                                                     (cons ',base-operation arg))))))

                    ;; TODO: These notes should not be printed if call cannot be optimized.
                    (when optimizable-p
                      (if out-supplied-p
                          (let ((out-type (variable-type out env)))
                            (unless (and (listp out-type) (= 3 (length out-type)))
                              (format t
                                      "~&; note: Unable to determine optimizability of call to ~S because type of ~S ~S is not exact"
                                      ',name
                                      out out-type))
                            (when (and (and (listp out-type) (= 3 (length out-type)))
                                       (subtypep out-type
                                                 '(simple-array single-float *))
                                       (let ((arg-type (variable-type arg env)))
                                         (unless (= 3 (length arg-type))
                                           (format t
                                                   "~&; note: Unable to determine optimizability of call to ~S because type of ~S ~S is not exact"
                                                   ',name arg arg-type))
                                         (equalp arg-type out-type)))
                              (return-from ,name
                                (let ((base-operation ',base-operation))
                                  `(with-elementwise-operations
                                       'single-float ,out (,base-operation ,arg))))))))
                    whole)
                  (progn
                    (when (= 3 (policy-quality 'speed env))
                      (format t "~& note: Optimization for call to ~S is not implemented for ~S" ',name whole)
                      whole))))))
       
       ;; TODO: Add compiler-macro
       ))

  (define-unary-wrapper nu:sqrt 'sqrt))
