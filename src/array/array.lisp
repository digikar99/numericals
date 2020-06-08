;;; The goal of this package is to provide a drop-in replacement to cl:simple-array
;;; providing a wrapper around it, to enable fast numpy-view-like aref-ing.

;;; What should work without errors then includes:
;;; - parametric types declared using cl:array
;;; - array-dimensions, -dimension, -element-type, aref,

;;; For parametric cl:array declarations to work, we need a deftype array.
;;; This deftype-d array must reduce to the new array, meaning the actual
;;; array type and the deftype wrapper need to have different names.
(cl:in-package :numericals.array)
(cl:defstruct (numericals-array (:conc-name array-))
  ;; TODO: Add more documentation with a proper example
  "- DIMENSIONS is a list of dimensions.
- STRIDES is a list of strides along each dimension.
- OFFSETS is a list of offsets along each dimension."
  (storage-vector
   (cl:error "STORAGE-VECTOR must be supplied during NUMERICALS-ARRAY initialization."))
  (element-type
   (cl:error "ELEMENT-TYPE must be supplied during NUMERICALS-ARRAY initialization."))
  (dimensions
   (cl:error "DIMENSIONS must be supplied during NUMERICALS-ARRAY initialization")
   :read-only cl:t)
  (strides
   (cl:error "STRIDES must be supplied during NUMERICALS-ARRAY initialization"))
  (offset (cl:progn
           (cl:error "OFFSET must be supplied during NUMERICALS-ARRAY initialization")
           0)
          :type cl:fixnum)
  (contiguous-p
   (cl:error "CONTIGUOUS-P must be supplied during NUMERICALS-ARRAY initialization")
   :read-only cl:t))

;; Doing this is necessary to maintain speeds. (See next comment block.)
;; #+sbcl (declare (sb-ext:maybe-inline na:array-dimensions))
;; (setf (fdefinition 'na:array-dimensions) #'numericals-array-dimensions)
;; That does not work to maintain speeds! Because there are other declarations
;; and more things at play. Macroexpand the above defstruct form to take a look!

(cl:in-package :numericals.array.internals)

;;; CLOS is way too slow. For example, in the case of na:array-dimensions:
;; (defun foo ()
;;   (let ((a (na:make-array '(10 10 10 10))))
;;     (declare (optimize speed (safety 0)))
;;     (time (loop :for i :below 100000000
;;              :do (progn (na:array-dimensions a))))))
;; (defun bar ()
;;   (let ((a (make-array '(10 10 10 10))))
;;     (declare (optimize speed))
;;     (time (loop :for i :below 100000000
;;              :do (progn (array-dimensions a))))))
;;; - SLOT-VALUE is about 15 times slower!
;;; - Even safety 0 makes for a 7 times slower implementation than array-dimensions
;;; - Generic functions make for a 5-6 times slower implementation array-dimensions

(deftype numericals-array-element-type ()
  `(member single-float double-float fixnum))

(deftype na:array (&optional (element-type * element-type-p) (dimensions * dimensions-p))
  (declare (ignore element-type dimensions))
  (when (or element-type-p dimensions-p)
    (warn "The NUMERICALS package has not been optimized for parametric types of NUMERICALS:ARRAY. So, parametric type declarations involving NUMERICALS:ARRAY have no effect."))
  'na:numericals-array)

(defun na:arrayp (object) (typep object 'na:numericals-array))

(declaim (type numericals:numericals-array-element-type *type*))
(defparameter *type* 'single-float
  "Can only be one of FIXNUM, SINGLE-FLOAT, or DOUBLE-FLOAT. Depending on the compile-time
value of NUMERICALS:*LOOKUP-TYPE-AT-COMPILE-TIME*, this variable may be looked up at compile-time or run-time by certain constructs.")

(defparameter *lookup-type-at-compile-time* t
  "If the compile-time value is T, looks up the value of *TYPE* at compile-time to aid 
compiler-macros to generate efficient code.")

(declaim (ftype (function (list &key
                                (:element-type numericals-array-element-type)
                                (:initial-element *)
                                (:strides list)
                                (:offset fixnum)
                                (:initial-contents *))
                          (values na:numericals-array &optional))
                na:make-array))
(defun na:make-array
    (dimensions
     &key (element-type *type*)
       (initial-element nil initial-element-p)
       (strides nil strides-p)
       (offset 0)
       (initial-contents nil initial-contents-p))
  ;; TODO: Handle the case with initial-contents
  ;; TODO: Take offset and strides into account while calculating dimensions
  (when (and initial-element-p initial-contents-p)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  (let* ((storage-vector-initial-element
          (cond ((and initial-element-p
                      (typep initial-element 'function-designator))
                 (coerce 0 element-type))
                (initial-element-p initial-element)
                (t (coerce 0 element-type))))
         (storage-vector (make-array (apply #'* dimensions)
                                     :initial-element storage-vector-initial-element
                                     :element-type element-type))
         (array (make-numericals-array
                 :storage-vector storage-vector
                 :element-type element-type
                 :dimensions dimensions
                 :strides (if strides-p
                              strides
                              (nconc (cdr (maplist (lambda (l) (apply #'* l))
                                                   dimensions))
                                     '(1)))
                 :offset offset
                 :contiguous-p t)))
    (cond ((and initial-element-p (typep initial-element 'function-designator))
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index))
             (apply #'map-product ; TODO: This unnecessarily collects the values.
                    (lambda (&rest subscripts)
                      (setf (aref storage-vector row-major-index)
                            (apply initial-element subscripts))
                      (incf row-major-index))
                    (loop :for d fixnum :in dimensions
                       :collect (iota d)))))
          (initial-contents-p
           (let ((row-major-index 0))
             (labels ((set-storage-vector (elt)
                        (if (listp elt)
                            (loop :for e :in elt
                               :do (set-storage-vector e))
                            (progn
                              (setf (aref storage-vector row-major-index) elt)
                              (incf row-major-index)))))
               (set-storage-vector initial-contents)))))
    array))

(defun unable-to-optimize-call-note (callee reason &rest reason-args)
  (format *error-output* "~&; note: Unable to optimize call to ~S because~%;   ~D"
          callee (apply #'format nil reason reason-args)))

(define-compiler-macro na:make-array
    (&whole whole dimensions
            &key (element-type *type* element-type-p)
            (initial-element nil initial-element-p)
            (strides nil strides-p)
            (offset 0)
            (initial-contents nil initial-contents-p)
            &environment env)
  ;; TODO: Write a proper compiler macro!!!
  (when (and initial-element-p initial-contents-p)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  (if (= 3 (policy-quality 'speed env))
      (flet ((utocn (reason &rest reason-args)
               (apply #'unable-to-optimize-call-note
                      'na:make-array reason reason-args)))

        (when initial-contents-p
          (utocn "INITIAL-CONTENTS case has not been optimized yet")
          (return-from na:make-array whole))
        
        (let ((element-type-known-p t)
              (dimensions-known-p t)
              (initial-element-known-p t)
              (make-array-known-p t)
              storage-vector)
          ;; Handle element-type
          (setq element-type                
                (if element-type-p
                    (if (constantp element-type env)
                        (constant-form-value element-type env)
                        (progn
                          (utocn "ELEMENT-TYPE ~D could not be determined to be a constant" element-type)
                          (setq element-type-known-p nil)
                          element-type))
                    (if *lookup-type-at-compile-time*
                        *type*
                        (progn
                          (utocn "Not using the compile-time value of *TYPE* because *LOOKUP-TYPE-AT-COMPILE-TIME* is NIL at compile-time")
                          (setq element-type-known-p nil)
                          '*type*))))
          ;; Handle dimensions
          (setq dimensions
                (if (constantp dimensions env)
                    (constant-form-value dimensions env)
                    (progn
                      (utocn "DIMENSIONS ~D could not be determined to be a constant" dimensions)
                      (setq dimensions-known-p nil)
                      dimensions)))
          
          (setq initial-element
                (if initial-element-p
                    (if (constantp initial-element env)
                        (if element-type-known-p
                            (coerce (constant-form-value initial-element
                                                         env)
                                    element-type)
                            (progn
                              (utocn "INITIAL-ELEMENT cannot be determined at compile-time without knowing ELEMENT-TYPE")
                              (setq initial-element-known-p nil)
                              `(coerce ,(constant-form-value initial-element
                                                             env)
                                       ;; TODO: check this
                                       ,element-type)))
                        (progn
                          (utocn "INITIAL-ELEMENT ~D could not be determined to be a constant"
                                 initial-element)
                          (setq initial-element-known-p nil)
                          `(coerce ,initial-element ,(if element-type-known-p
                                                         (list 'quote element-type)
                                                         element-type))))
                    (if element-type-known-p
                        (coerce 0 element-type)
                        (progn
                          (utocn "INITIAL-ELEMENT cannot be determined at compile-time without knowing ELEMENT-TYPE")
                          (setq initial-element-known-p nil)
                          `(coerce 0 ,(list 'quote element-type))))))

          (let ((storage-vector
                 (cond ((and initial-element-known-p
                             element-type-known-p
                             dimensions-known-p)
                        (make-array (apply #'* dimensions)
                                    :initial-element initial-element
                                    :element-type element-type))
                       (t
                        (utocn "STORAGE-VECTOR could not be allocated at compile-time without knowing all of INITIAL-ELEMENT, ELEMENT-TYPE and DIMENSIONS")
                        (setq make-array-known-p nil)
                        `(make-array ,(if dimensions-known-p
                                          (apply #'* dimensions)
                                          `(apply #'* ,dimensions))
                                     :initial-element ,initial-element
                                     :element-type ,(if element-type-known-p
                                                        (list 'quote element-type)
                                                        element-type)))))
                (strides (if strides-p
                             strides
                             (if dimensions-known-p
                                 (list 'quote
                                       (nconc (cdr (maplist (lambda (l) (apply #'* l))
                                                            dimensions))
                                              '(1)))
                                 `(nconc (cdr (maplist (lambda (l) (apply #'* l))
                                                       ,dimensions))
                                         '(1))))))
            `(make-numericals-array :storage-vector ,storage-vector
                                    :element-type ,(if element-type-known-p
                                                       (list 'quote element-type)
                                                       element-type)
                                    :dimensions ,(if dimensions-known-p
                                                     (list 'quote dimensions)
                                                     dimensions)
                                    :strides ,strides
                                    :offset ,offset
                                    :contiguous-p t))))
      whole))

(defun na:array-rank (na:numericals-array)
  (length (na:array-dimensions na:numericals-array)))
(defun na:array-dimension (na:numericals-array axis-number)
  (elt (na:array-dimensions na:numericals-array) axis-number))
(defun na:array-stride (na:numericals-array axis-number)
  (elt (na:array-strides na:numericals-array) axis-number))
(declaim (ftype (function (na:numericals-array) fixnum) na:array-total-size))
(defun na:array-total-size (na:numericals-array)
  (declare (optimize speed))
  (apply #'* (na:array-dimensions na:numericals-array)))

(defvar *array-rank* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *stream* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *array* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *row-major-index* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *storage-vector* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *current-dimension-stride* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")

(defmethod print-object ((object na:numericals-array) stream)
  (let ((n (na:array-rank object)))
    (format stream "#~DA" n)
    (let* ((*array-rank* (na:array-rank object))
           (*array* object)
           (*stream* stream)
           (*row-major-index* (na:array-offset object))
           (*storage-vector* (na:array-storage-vector object))
           (*current-dimension-stride* (na:array-stride object 0)))
      (print-numericals-array 0))))

(defun print-numericals-array (axis)
  ;; TODO: take *print-pretty* into account
  (if (= axis *array-rank*)
      (progn
        (write (aref *storage-vector* *row-major-index*) :stream *stream*)
        ;; (print (list *row-major-index* *current-dimension-stride*))
        (incf *row-major-index* *current-dimension-stride*))
      (progn
        (write-char #\( *stream*)
        (let* ((axis-size (na:array-dimension *array* axis))
               (axis-size-1 (1- axis-size))
               (*row-major-index* *row-major-index*)
               (*current-dimension-stride* (na:array-stride *array* axis)))
          ;; (print (list 'axis-size axis-size))
          (loop :for i :below axis-size
             :do (print-numericals-array (1+ axis))
               (unless (= i axis-size-1) (write-char #\space *stream*))))
        ;; (print (list 'current *row-major-index* *current-dimension-stride*))
        (incf *row-major-index* *current-dimension-stride*)
        (write-char #\) *stream*)
        ;; TODO: a hacky way to make display manageable
        (when (= (1+ axis) *array-rank*)
          (write-char #\newline *stream*)))))

(defun na:aref (na:numericals-array &rest subscripts)
  ;; TODO: Handle nested aref
  ;; TODO: Handle offsets
  ;; TODO: Handle non-integer subscripts
  ;; TODO [hard?]: Introduce parametric types in CL.
  ;;  In the special case where this reduces to CL:AREF, this function is over 20 times slower.
  ;;  However, I do not see a way of optimizing this even using compiler-macros without
  ;;  parametric user-defined types (for na:numericals-array). However, perhaps, due to
  ;;  the function-call-overhead in python, this is about 2 times faster in that special-case.
  ;;  In fact this is 2-2.5 times faster than python-numpy in either case.
  (declare (optimize speed))
  (etypecase na:numericals-array
    (array (apply #'aref na:numericals-array subscripts))
    (na:numericals-array
     (with-slots (storage-vector element-type strides offset dimensions) na:numericals-array
       (if (let ((use-cl-aref t))
             (loop :for subscript :in subscripts
                :while use-cl-aref
                :unless (numberp subscript)
                :do (setf use-cl-aref nil))
             use-cl-aref)
           (aref storage-vector (loop :for stride :in strides
                                   :for subscript :in subscripts
                                   :summing (the (signed-byte 31)
                                                 (* (the (signed-byte 31) stride)
                                                    (the (signed-byte 31) subscript)))))
           (multiple-value-bind (dimensions strides offset contiguous-p)
               (let ((d dimensions)
                     (s strides)
                     (new-offset 0)
                     new-dimensions new-strides
                     (contiguous-p t)
                     (saw-a-t nil))
                 (declare (type (signed-byte 31) new-offset))
                 (loop :for subscript :in subscripts
                    :do
                      (let ((car-s (car s)))
                        (declare (type (or t (signed-byte 31)) subscript)
                                 (type (signed-byte 31) car-s))
                        (if (eq t subscript)
                            (progn
                              (setq new-dimensions (cons (car d) new-dimensions))
                              (setq new-strides (cons car-s new-strides))
                              (setq saw-a-t t))
                            (setq new-offset (+ new-offset (* car-s (the (signed-byte 31)
                                                                         subscript)))))
                        (when (and saw-a-t (typep subscript '(signed-byte 31)))
                          (setq contiguous-p nil))
                        (print contiguous-p))
                      (setq d (cdr d))
                      (setq s (cdr s)))
                 (values (nconc (nreverse new-dimensions) d)
                         (nconc (nreverse new-strides) s)
                         new-offset
                         contiguous-p))
             (make-numericals-array
              :storage-vector storage-vector
              :element-type element-type
              :dimensions dimensions
              :strides strides
              :offset offset
              :contiguous-p contiguous-p)))))))

(defun (setf na:aref) (new-value na:numericals-array &rest subscripts)
  ;; TODO: Handle nested aref
  ;; TODO: Handle offsets
  ;; TODO: Handle non-integer subscripts
  (declare (optimize speed))
  (etypecase na:numericals-array
    (array (setf (apply #'aref na:numericals-array subscripts) new-value))
    (na:numericals-array
     (with-slots (storage-vector element-type strides offset dimensions) na:numericals-array
       (if (length= subscripts (na:array-dimensions na:numericals-array))
           (setf (aref storage-vector (loop :for stride :in strides
                                         :for subscript :in subscripts
                                         :summing (the (signed-byte 31)
                                                       (* (the (signed-byte 31) stride)
                                                          (the (signed-byte 31) subscript)))))
                 new-value)
           (multiple-value-bind (dimensions strides offset)
               (let ((d dimensions)
                     (s strides)
                     (new-offset 0)
                     new-dimensions new-strides)
                 (declare (type (signed-byte 31) new-offset))
                 (loop :for subscript :in subscripts
                    :do
                      (let ((car-s (car s)))
                        (declare (type (or t (signed-byte 31)) subscript)
                                 (type (signed-byte 31) car-s))
                        (if (eq t subscript)
                            (progn
                              (setq new-dimensions (cons (car d) new-dimensions))
                              (setq new-strides (cons car-s new-strides)))
                            (setq new-offset (+ new-offset (* car-s (the (signed-byte 31)
                                                                         subscript))))))
                      (setq d (cdr d))
                      (setq s (cdr s)))
                 (values (nconc (nreverse new-dimensions) d)
                         (nconc (nreverse new-strides) s)
                         new-offset))
             (make-numericals-array
              :storage-vector storage-vector
              :element-type element-type
              :dimensions dimensions
              :strides strides
              :offset offset)))))))

(defun na:row-major-aref (na:numericals-array index)
  (if (na:array-contiguous-p na:numericals-array)
      (aref (na:array-storage-vector na:numericals-array)
            (+ (na:array-offset na:numericals-array) index))
      (error "non-contigous case not implemented")))

(declaim (ftype (function (na:numericals-array)
                          (values simple-array fixnum))
                na:1d-storage-array))
(defun na:1d-storage-array (na:numericals-array)
  (declare (optimize speed)
           (type na:numericals-array na:numericals-array))
  (values (na:array-storage-vector na:numericals-array)
          (na:array-offset na:numericals-array)))

(defun na:broadcast-array (na:numericals-array broadcast-dimensions)
  ;; TODO: Incorporate offsets!
  ;; TODO: Doing this neatly requires changes in aref
  (with-slots (dimensions element-type strides offset storage-vector) na:numericals-array
    (make-numericals-array :dimensions broadcast-dimensions
                           :element-type element-type
                           :strides
                           ;; TODO: Handle strides for (1) -> (1 10) and (10) -> (1 10) broadcast cases
                           (let* ((blen (length broadcast-dimensions))
                                  (len (length dimensions))
                                  (dimensions (append (make-list (- blen len)
                                                                 :initial-element 1)
                                                      dimensions))
                                  (strides (append (make-list (- blen len)
                                                              :initial-element 0)
                                                   strides)))
                             (loop :for s :in strides
                                :for b :in broadcast-dimensions
                                :for d :in dimensions
                                :collect
                                  (cond ((= b d) s)
                                        ((= d 1) 0)
                                        (t (error "~D of dimensions ~D cannot be broadcasted to dimensions ~D" na:numericals-array dimensions broadcast-dimensions)))))
                           :offset offset
                           :storage-vector storage-vector
                           ;; TODO: Determine contiguity
                           :contiguous-p nil)))

(defun na:cl-array-array (array)
  (declare (type array array))
  ;; TODO: Take *type* into consideration
  (let ((dimensions (array-dimensions array)))
    (multiple-value-bind (storage-vector offset)
        (1d-storage-array array)
      (make-numericals-array :storage-vector storage-vector
                             :offset offset
                             :dimensions dimensions
                             :element-type (array-element-type array)
                             :strides (nreverse (let ((stride 1))
                                                  (loop :for d :in (reverse dimensions)
                                                     :collect stride
                                                     :do (setq stride (* stride d)))))
                             :contiguous-p t))))
