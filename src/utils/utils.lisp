(in-package :numericals/utils/impl)

(deftype size () `(unsigned-byte 62))
(defmacro the-size (form)
  #+sbcl `(sb-ext:truly-the size ,form)
  #-sbcl `(the size ,form))

(deftype int-index () `(signed-byte 62))
(defmacro the-int-index (form)
  `(#+sbcl sb-ext:truly-the
    #-sbcl the
    int-index ,form))

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(deftype uint32 () `(unsigned-byte 32))
(deftype int64 () '(signed-byte 64))
(deftype int16 () '(signed-byte 16))
(deftype int8  () '(signed-byte 08))


(define-condition runtime-array-allocation (suboptimal-polymorph-note)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unable to avoid array allocation at run time. Consider supplying
the OUT argument, and/or ensuring all the appropriate arguments are
arrays of appropriate types."))))

(defmacro defun* (name lambda-list &body body)
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
                    (mapcar (lm arg (peltadot/form-types:nth-form-type
                                     arg env 0 t t))
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
  (def zeros 0)
  (def ones 1)
  (def empty 0))

(defun rand (&rest args)
  "Lambda List: (shape &key (type default-element-type) (min 0) (max 1))"
  (destructuring-bind
      (shape &key (type default-element-type) (min (coerce 0 type)) (max (coerce 1 type))
               (layout :row-major))
      (split-at-keywords args)
    (when (listp (first shape))
      (assert (null (rest shape)))
      (setq shape (first shape)))
    (ensure-row-major-layout)
    (let* ((a (zeros (the list shape) :type type))
           (asv (array-storage a))
           (range (- max min))
           (min (coerce min type)))
      (declare (type (cl:simple-array * 1) asv))
      ;; A good use of specialized function :)
      (specializing (asv min range a)
        (dotimes (index (array-total-size a))
          (setf (row-major-aref asv index)
                (+ min (random range)))))
      a)))

(defun full (&rest args)
  "Lambda List: (shape &key value (type default-element-type))"
  (destructuring-bind (shape &key value (type default-element-type) (layout :row-major))
      (split-at-keywords args)
    (when (listp (first shape))
      (assert (null (rest shape)) (shape)
              "expected (rest shape) to be null but is ~s" (rest shape))
      (setq shape (first shape)))
    (ensure-row-major-layout)
    (make-array shape :element-type type :initial-element (coerce value type))))

(declaim (inline fill))
(defun fill (array value)
  "Fill each location in ARRAY with VALUE"
  (let* ((type  (array-element-type array))
         (value (coerce value type))
         (size  (array-total-size array))
         (storage (array-storage array))
         (offset  (cl-array-offset array)))
    (cl:fill storage value :start offset :end (+ offset size))
    array))

(declaim (inline empty-like zeros-like ones-like rand-like full-like))
(defun empty-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (empty (array-dimensions array) :type (array-element-type array)))
(defun zeros-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (zeros (array-dimensions array) :type (array-element-type array)))
(defun ones-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (ones (array-dimensions array) :type (array-element-type array)))
(defun rand-like (array)
  ;; FIXME: Expand this to array-like
  (declare (type cl:array array))
  (rand (array-dimensions array) :type (array-element-type array)))
(defun full-like (array value)
  (declare (type cl:array array))
  (full (array-dimensions array) :value value
           :type (array-element-type array)))

(defun shape (array-like &optional (axis nil axis-p))
  ;; This is a potentially domain specific functionality; since
  ;; there exists the ambiguity of what should one do with strings
  (let ((dimensions (typecase array-like
                      (sequence (cons (length array-like)
                                      (shape (elt array-like 0))))
                      (array (array-dimensions array-like))
                      (t nil))))
    (if axis-p
        (elt dimensions axis)
        dimensions)))

(declaim (inline array-size-from-dimensions))
(defun array-size-from-dimensions (dimensions)
  (declare (optimize speed))
  (loop :with product :of-type (integer 0 #.array-total-size-limit) := 1
        :for dim :of-type (integer 0 #.array-dimension-limit) :in dimensions
        :do (setf product (* product dim))
        :finally (return product)))

(declaim (inline reshape))
(defun reshape (array new-dimensions)
  (policy-cond:with-expectations (cl:= 0 safety)
      ((assertion (= (array-total-size array)
                     (array-size-from-dimensions new-dimensions))
                  (array new-dimensions)
                  "Array~%  ~A~%of size ~A must be reshaped into an array with same size.~%But the given dimensions ~S are of size ~A"
                  array (array-total-size array) new-dimensions (array-size-from-dimensions new-dimensions)))
    (make-array new-dimensions
                :element-type (array-element-type array)
                :displaced-to (array-storage array)
                :displaced-index-offset (cl-array-offset array))))

(defpolymorph array= ((array1 cl:array) (array2 cl:array) &key (test #'cl:=)) boolean
  (and (equalp (array-dimensions array1)
               (array-dimensions array2))
       (loop :for i :below (array-total-size array1)
             :always (funcall test
                              (row-major-aref array1 i)
                              (row-major-aref array2 i)))))

(defmacro do-arrays (bindings &body body)
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

(defmacro macro-map-array (result-array function &rest arrays)
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
         (let* ((,result (or ,result-array (zeros-like ,(first array-syms))))
                (,result-type (array-element-type ,result)))
           (dotimes (,i (array-total-size ,(first array-syms)))
             (setf (row-major-aref ,result ,i)
                   (coerce
                    (,function ,@(mapcar (lm array-sym `(row-major-aref ,array-sym ,i))
                                         array-syms))
                    ,result-type)))
           ,result)))))

(define-polymorphic-function out-shape-compatible-p (function-name &rest args)
  :overwrite t)

(define-polymorphic-function out-shape (function-name &rest args)
  :overwrite t)

(declaim (inline ensure-array))
(defun ensure-array (element &optional shape (element-type default-element-type))
  (make-array shape :element-type element-type
                    :initial-element (coerce element element-type)))
