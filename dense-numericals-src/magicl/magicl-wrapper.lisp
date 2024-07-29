(uiop:define-package :dense-numericals/magicl
  (:documentation "A DENSE-NUMERICALS wrapper around MAGICL.")
  (:use)
  (:import-from :magicl
                #:*default-allocator*
                #:*default-tensor-type*

                ;; If CAST should behave intuitively, then the classes
                ;; exported by this package should be identical to the
                ;; classes provided by magicl.

                #:abstract-tensor

                #:matrix
                #:matrix/complex-double-float
                #:matrix/complex-single-float
                #:matrix/double-float
                #:matrix/single-float

                #:tensor
                #:tensor/complex-double-float
                #:tensor/complex-single-float
                #:tensor/double-float
                #:tensor/single-float

                #:vector/complex-double-float
                #:vector/complex-single-float
                #:vector/double-float
                #:vector/single-float

                #:column-major-index
                #:row-major-index
                #:matrix-column-major-index
                #:matrix-row-major-index

                #:define-backend-function
                #:define-backend-implementation
                #:define-compatible-no-applicable-method-behavior
                #:define-extensible-function

                #:polynomial
                #:make-polynomial
                #:polynomial-coefficients
                #:polynomial-diff
                #:polynomial-eval
                #:polynomial-newton-iteration
                #:polynomial-solve

                #:no-applicable-implementation
                #:time-backends
                #:valid-index-p
                #:with-blapack)
  (:reexport :magicl))

(peltadot/utils:defpackage :dense-numericals/magicl/impl
  (:use :peltadot)
  (:shadowing-import-exported-symbols :dense-numericals/more-utils)
  (:local-nicknames (:nu/magicl :dense-numericals/magicl))
  (:import-from #:alexandria
                #:make-gensym-list
                #:parse-ordinary-lambda-list)
  (:import-from #:dense-arrays
                #:dimensions->strides))

(in-package :dense-numericals/magicl/impl)

(defun magicl-tensor-type (rank element-type)
  (let ((abstract-tensor-type (case rank
                                (1 "VECTOR")
                                (2 "MATRIX")
                                (t "TENSOR")))
        (tensor-element-type (alexandria:eswitch (element-type :test #'subtypep)
                               ('single-float "SINGLE-FLOAT")
                               ('double-float "DOUBLE-FLOAT")
                               ('(signed-byte 32) "INT32")
                               ('integer "SINGLE-FLOAT")
                               ('(complex single-float) "COMPLEX-SINGLE-FLOAT")
                               ('(complex double-float) "COMPLEX-DOUBLE-FLOAT"))))
    (find-symbol (concatenate 'string abstract-tensor-type "/" tensor-element-type)
                 :magicl)))

(defun to-magicl-tensor (standard-dense-array &key (copy nil))
  "Converts STANDARD-DENSE-ARRAY to an appropriate subtype of MAGICL:ABSTRACT-TENSOR

If COPY is non-NIL then avoids making a copy of the storage of STANDARD-DENSE-ARRAY
only if the STANDARD-DENSE-ARRAY is a non-view. In other words, a copy will be created
if STANDARD-DENSE-ARRAY is a view (see ARRAY-VIEW-P) regardless of the value of COPY."
  (declare (optimize speed))
  (let* ((a standard-dense-array)
         (a (if (or copy (array-layout a))
                (copy-array a)
                a)))
    (declare (optimize speed)
             (type standard-dense-array standard-dense-array a))
    (magicl:make-tensor (magicl-tensor-type
                         (array-rank a)
                         (array-element-type a))
                        (array-dimensions a)
                        :storage (array-storage a)
                        :layout (array-layout a))))

(defun from-magicl-tensor (magicl-tensor &key (copy nil))
  "Converts MAGICL:ABSTRACT-TENSOR to a STANDARD-DENSE-ARRAY

If COPY is non-NIL, this copies over the underlying MAGICL::STORAGE"
  (declare (optimize speed))
  (let* ((rank         (magicl:order magicl-tensor))
         (storage      (slot-value magicl-tensor 'magicl::storage))
         (storage      (if copy (alexandria:copy-array storage) storage))
         (element-type (magicl:element-type magicl-tensor))
         (layout       (magicl:layout magicl-tensor))
         (dimensions   (magicl:shape magicl-tensor))
         (strides      (dimensions->strides dimensions layout))
         (offsets      (make-list rank :initial-element 0))
         (total-size   (magicl:size magicl-tensor)))
    (make-instance 'standard-dense-array
                   :layout layout
                   :element-type element-type
                   :rank rank
                   :dimensions dimensions
                   :strides strides
                   :offsets offsets
                   :total-size total-size
                   :storage storage
                   :root-array nil)))

(defun magicl-funcall (function &rest arguments)
  "Converts STANDARD-DENSE-ARRAY in ARGUMENTS to appropriate MAGICL TENSOR / MATRIX / VECTOR.
Converts the results back to STANDARD-DENSE-ARRAY"
  (declare (optimize speed))
  (multiple-value-call
      (lambda (&rest values)
        (loop :for rem-values :on values
              :for (value . rest) := rem-values
              :if (cl:typep value 'magicl:abstract-tensor)
                :do (setf (first rem-values)
                          (from-magicl-tensor value :copy nil))
              :finally (return (values-list values))))
    (apply function
           (loop :for rem-arguments :on arguments
                 :for (arg . rest) := rem-arguments
                 :if (cl:typep arg 'standard-dense-array)
                   :do (setf (first rem-arguments)
                             (to-magicl-tensor arg :copy nil))
                 :finally (return arguments)))))

(defun magicl-call-form-from-lambda-list (magicl-name lambda-list array-parameters)
  "Returns three values:
  CALL-FORM
  AUGMENTED-LAMBDA-LIST
  IGNORABLE-DECLARATION

AUGMENTED-LAMBDA-LIST contains the supplied-p and other arguments
that CALL-FORM requires."
  (multiple-value-bind (required optional rest keyword allow-other-keys-p auxilary keyp)
      (parse-ordinary-lambda-list lambda-list
                                  :normalize-optional nil
                                  :normalize-keyword nil)
    (declare (ignore auxilary))

    (let* ((required-forms
             (loop :for var :in required
                   :if (member var array-parameters)
                     :collect `(to-magicl-tensor ,var :copy nil)
                   :else
                     :collect var))
           (optional (loop :for (var default varp) :in optional
                           :collect (list var default (or varp (gensym)))))
           (rest (cond (rest rest)
                       (keyp (gensym "REST"))
                       (t nil)))
           (elt (gensym)))

      (values
       `(let ,(when rest
                `((,rest (loop :for ,elt :in ,rest
                               :collect (if (typep ,elt 'array)
                                            (to-magicl-tensor ,elt :copy nil)
                                            ,elt)))))
          (cond
            ,@(loop :for (var default varp) :in optional
                    :with optional-so-far := nil
                    :do (setq var (if (member var array-parameters)
                                      `(to-magicl-tensor ,var :copy nil)
                                      var))
                        (push var optional-so-far)
                    :collect `(,varp
                               ,(if rest
                                    `(apply #',magicl-name
                                            ,@required-forms
                                            ,@(reverse optional-so-far)
                                            ,rest)
                                    `(,magicl-name ,@required-forms
                                                   ,@(reverse optional-so-far)))))
            (t
             ,(if rest
                  `(apply #',magicl-name ,@required-forms ,rest)
                  `(,magicl-name ,@required-forms)))))

       `(,@required ,@(when optional `(&optional ,@optional))
                    ,@(when rest `(&rest ,rest))
                    ,@(when keyp `(&key ,@keyword))
                    ,@(when allow-other-keys-p `(&allow-other-keys)))
       `(declare (ignorable ,@(loop :for parameter :in keyword
                                    :nconcing
                                    (etypecase parameter
                                      (symbol (list parameter))
                                      ((cons symbol)
                                       (list* (first parameter)
                                              (when (third parameter)
                                                (list (third parameter)))))
                                      ((cons cons)
                                       (list* (first (first parameter))
                                              (when (third parameter)
                                                (list (third parameter)))))))))))))

(defmacro define-magicl-wrapper (name lambda-list
                                 &key array-parameters
                                   returns-magicl-array)
  (let ((magicl-name    (find-symbol (symbol-name name) :magicl))
        (nu-magicl-name (find-symbol (symbol-name name) :dense-numericals/magicl)))
    (assert magicl-name () "~A not found in package MAGICL" name)
    (multiple-value-bind (call-form augmented-lambda-list decl)
        (magicl-call-form-from-lambda-list magicl-name lambda-list array-parameters)
      `(defun ,nu-magicl-name ,augmented-lambda-list
         ,(or (documentation magicl-name 'cl:function) "")
         ,decl
         ,(etypecase returns-magicl-array
            ((eql t)
             `(from-magicl-tensor ,call-form :copy nil))
            ((eql nil)
             call-form)
            (cons
             (let ((values-symbol-list (make-gensym-list (length returns-magicl-array)
                                                         "VALUE")))
               `(multiple-value-bind ,values-symbol-list
                    ,call-form
                  (values
                   ,@(loop :for value-symbol :in values-symbol-list
                           :for tensorp :in returns-magicl-array
                           :collect (if tensorp
                                        `(from-magicl-tensor ,value-symbol :copy nil)
                                        value-symbol)))))))))))
