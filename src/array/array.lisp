
(cl:in-package :numericals/array)
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
          :type cl:fixnum))

;; Doing this is necessary to maintain speeds. (See next comment block.)
;; #+sbcl (declare (sb-ext:maybe-inline na:array-dimensions))
;; (setf (fdefinition 'na:array-dimensions) #'numericals-array-dimensions)
;; That does not work to maintain speeds! Because there are other declarations
;; and more things at play. Macroexpand the above defstruct form to take a look!

(cl:in-package :numericals/array/internals)

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

;; (defun numericals-array-single-float (na:numericals-array)
;;   (eq (na:array-element-type na:numericals-array) 'single-float))

;; (deftype my-array (&optional (element-type *type* element-type-p))
;;   (if element-type-p
;;       (ecase element-type
;;         (single-float `(and na:numericals-array
;;                             (satisfies numericals-array-single-float))))
;;       `na:numericals-array))

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
                 :offset offset)))
    (cond ((and initial-element-p (typep initial-element 'function-designator))
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index))
             (apply #'map-product ; TODO: This unnecessarily collects the values.
                    (lambda (&rest subscripts)
                      (setf (aref storage-vector row-major-index)
                            (apply initial-element subscripts))
                      (incf row-major-index))
                    (loop :for d fixnum :in dimensions
                       :collect (iota d))))
           array)
          (initial-contents-p
           (let ((row-major-index 0))
             (labels ((set-storage-vector (elt)
                        (if (listp elt)
                            (loop :for e :in elt
                               :do (set-storage-vector e))
                            (progn
                              (setf (aref storage-vector row-major-index) elt)
                              (incf row-major-index)))))
               (set-storage-vector initial-contents)))
           array)
          (t array))))

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
                                    :offset ,offset))))))

(defun na:array-dimensions-length (na:numericals-array)
  (length (na:array-dimensions na:numericals-array)))
(defun na:array-dimension (na:numericals-array axis-number)
  (elt (na:array-dimensions na:numericals-array) axis-number))
(defun na:array-stride (na:numericals-array axis-number)
  (elt (na:array-strides na:numericals-array) axis-number))
(declaim (ftype (function (na:numericals-array) fixnum) na:array-total-size))
(defun na:array-total-size (na:numericals-array)
  (declare (optimize speed))
  (apply #'* (na:array-dimensions na:numericals-array)))

(defvar *array-dimensions-length* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *stream* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *array* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *row-major-index* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *storage-vector* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *last-dimension-stride* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")

(defmethod print-object ((object na:numericals-array) stream)
  (let ((n (na:array-dimensions-length object)))
    (format stream "#~DA" n)
    (let* ((*array-dimensions-length* (na:array-dimensions-length object))
           (*array* object)
           (*stream* stream)
           (*row-major-index* (na:array-offset object))
           (*storage-vector* (na:array-storage-vector object))
           (*last-dimension-stride* (na:array-stride object (1- *array-dimensions-length*))))
      (print-numericals-array 0))))

(defun print-numericals-array (axis)
  ;; TODO: take *print-pretty* into account
  ;; TODO: take other axis strides into account
  (if (= axis *array-dimensions-length*)
      (progn
        (write (aref *storage-vector* *row-major-index*) :stream *stream*)
        ;; (print (list *row-major-index* *last-dimension-stride*))
        (incf *row-major-index* *last-dimension-stride*))
      (progn
        (write-char #\( *stream*)
        (let* ((axis-size (na:array-dimension *array* axis))
               (axis-size-1 (1- axis-size)))
          (loop :for i :below axis-size
             :do (print-numericals-array (1+ axis))
               (unless (= i axis-size-1) (write-char #\space *stream*))))        
        (write-char #\) *stream*))))

(defun na:aref (na:numericals-array &rest subscripts)
  ;; TODO: Handle nested aref
  ;; TODO: Handle offsets
  ;; TODO: Handle non-integer subscripts
  (declare (optimize speed))
  (etypecase na:numericals-array
    (array (apply #'aref na:numericals-array subscripts))
    (na:numericals-array
     (with-slots (storage-vector element-type strides offset dimensions) na:numericals-array
       (if (length= subscripts (na:array-dimensions na:numericals-array))
           (aref storage-vector (apply #'+ (mapcar #'* subscripts strides)))
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
           (setf (aref storage-vector (apply #'+ (mapcar #'* subscripts strides))) new-value)
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

(declaim (ftype (function (na:numericals-array)
                          (values simple-array fixnum))
                na:1d-storage-array))
(defun na:1d-storage-array (na:numericals-array)
  (declare (optimize speed)
           (type na:numericals-array na:numericals-array))
  (values (na:array-storage-vector na:numericals-array)
          (na:array-offset na:numericals-array)))
