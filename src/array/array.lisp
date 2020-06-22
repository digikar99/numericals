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
  (displaced-to
   (cl:error "DISPLACED-TO must be supplied during NUMERICALS-ARRAY initialization."))
  (element-type
   (cl:error "ELEMENT-TYPE must be supplied during NUMERICALS-ARRAY initialization."))
  (dim
   (cl:error "DIM must be supplied during NUMERICALS-ARRAY initialization")
   :read-only cl:t)
  (strides
   (cl:error "STRIDES must be supplied during NUMERICALS-ARRAY initialization"))
  (displaced-index-offset
   (cl:progn
    (cl:error "DISPLACED-INDEX-OFFSET must be supplied during NUMERICALS-ARRAY initialization")
    0)
   :type cl:fixnum)
  (contiguous-p
   (cl:error "CONTIGUOUS-P must be supplied during NUMERICALS-ARRAY initialization")
   :read-only cl:t)
  (total-size
   (cl:error "TOTAL-SIZE must be supplied during NUMERICALS-ARRAY initialization")
   :type cl:fixnum
   :read-only cl:t)
  (cl-array
   (progn
     (cl:error "CL-ARRAY must be supplied during NUMERICALS-ARRAY initialization")
     cl:nil)
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

(declaim (ftype (function ((or list fixnum) &key
                           (:element-type numericals-array-element-type)
                           (:initial-element *)
                           (:strides list)
                           (:displaced-index-offset fixnum)
                           (:initial-contents *)
                           (:displaced-to simple-array)
                           (:adjustable *)
                           (:fill-pointer *))
                          (values na:numericals-array &optional))
                na:make-array))

(defun na:make-array (dimensions &rest args
                      &key (element-type *type*)
                        (initial-element nil initial-element-p)
                        (initial-contents nil initial-contents-p)
                        (strides nil strides-p)
                        (adjustable nil adjustable-p)
                        (fill-pointer nil fill-pointer-p)
                        (displaced-to nil displaced-to-p)
                        (displaced-index-offset 0))
  ;; TODO: Handle adjustable
  ;; TODO: Handle fill-pointer
  ;; TODO: Sanitize displaced-to and perhaps, displaced-index-offset
  ;; TODO: Take displaced-index-offset and strides into account while calculating dimensions
  ;; TODO: Rethink how CL-ARRAY integrates
  (declare (optimize speed)
           (ignore args adjustable fill-pointer displaced-to-p))
  (when (and initial-element-p initial-contents-p)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
  (when fill-pointer-p
    (error "FILL-POINTER has not been handled yet in NUMERICALS-ARRAY"))
  (when adjustable-p
    (error "FILL-POINTER has not been handled yet in NUMERICALS-ARRAY"))
  (unless (listp dimensions)
    (setq dimensions (list dimensions)))
  (let* ((displaced-to-initial-element
          (cond ((and initial-element-p
                      (typep initial-element 'function-designator))
                 (coerce 0 element-type))
                (initial-element-p initial-element)
                (t (coerce 0 element-type))))
         (cl-array (if (or displaced-to strides)
                       nil
                       (make-array dimensions
                                   :initial-element displaced-to-initial-element
                                   :element-type element-type)))
         (displaced-to (cond (cl-array (1d-storage-array cl-array))
                             (displaced-to displaced-to)
                             (t (make-array (apply #'* dimensions)
                                            :initial-element displaced-to-initial-element
                                            :element-type element-type))))
         (array (make-numericals-array
                 :displaced-to displaced-to
                 :element-type element-type
                 :dim dimensions
                 :strides (if strides-p
                              strides
                              (let ((product 1))
                                (declare (optimize speed)
                                         (type (signed-byte 31) product))
                                (nreverse
                                 (loop :for d fixnum :in (cons 1 (reverse (cdr dimensions)))
                                    :do (setq product (* d product))
                                    :collect product))))
                 :displaced-index-offset displaced-index-offset
                 :contiguous-p t
                 :total-size (apply #'* dimensions)
                 :cl-array cl-array)))
    (cond ((and initial-element-p (typep initial-element 'function-designator))
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index)
                      (optimize (speed 1)))
             (apply #'map-product ; TODO: This unnecessarily collects the values.
                    (lambda (&rest subscripts)
                      (setf (aref displaced-to row-major-index)
                            (apply initial-element subscripts))
                      (incf row-major-index))
                    (loop :for d fixnum :in dimensions
                       :collect (iota d)))))
          (initial-contents-p
           (let ((row-major-index 0))
             (declare (type (signed-byte 31) row-major-index)
                      (optimize (speed 1)))
             (labels ((set-displaced-to (elt)
                        (if (listp elt)
                            (loop :for e :in elt
                               :do (set-displaced-to e))
                            (progn
                              (setf (aref displaced-to row-major-index) elt)
                              (incf row-major-index)))))
               (set-displaced-to initial-contents)))))
    array))

(defun unable-to-optimize-call-note (callee reason &rest reason-args)
  (format *error-output* "~&; note: Unable to optimize call to ~S because~%;   ~D"
          callee (apply #'format nil reason reason-args)))

(defun na:array-rank (na:numericals-array)
  (length (na:array-dimensions na:numericals-array)))
(defun na:array-dimension (na:numericals-array axis-number)
  (elt (na:array-dimensions na:numericals-array) axis-number))
(defun na:array-dimensions (na:numericals-array)
  (copy-list (na:array-dim na:numericals-array)))
(defun na:array-stride (na:numericals-array axis-number)
  (elt (na:array-strides na:numericals-array) axis-number))
(declaim (ftype (function (na:numericals-array) fixnum) na:array-total-size))

(defun na:array-displacement (na:numericals-array)
  (declare (optimize speed)
           (type na:numericals-array na:numericals-array))
  (values (na:array-displaced-to na:numericals-array)
          (na:array-displaced-index-offset na:numericals-array)))
(defun na:1d-storage-array (na:numericals-array)
  (declare (optimize speed)
           (type na:numericals-array na:numericals-array))
  (values (na:array-displaced-to na:numericals-array)
          (na:array-displaced-index-offset na:numericals-array)))

(defvar *array-rank* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *stream* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *array* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *row-major-index* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *displaced-to* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")
(defvar *current-dimension-stride* nil
  "Used by PRINT-OBJECT to print NUMERICALS/ARRAY:NUMERICALS-ARRAY")

(defmethod print-object ((object na:numericals-array) stream)
  (let ((n (na:array-rank object)))
    (format stream "#~DA" n)
    (let* ((*array-rank* (na:array-rank object))
           (*array* object)
           (*stream* stream)
           (*row-major-index* (na:array-displaced-index-offset object))
           (*displaced-to* (na:array-displaced-to object))
           (*current-dimension-stride* (na:array-stride object 0)))
      (print-numericals-array 0))))

(defun print-numericals-array (axis)
  ;; TODO: take *print-pretty* into account
  (if (= axis *array-rank*)
      (progn
        (write (aref *displaced-to* *row-major-index*) :stream *stream*)
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

(defun na:cl-aref (na:numericals-array &rest subscripts)
  "A wrapper around CL:AREF for faster accessing. Use it when you *know* your needs are satisfied by a CL:AREF clone. However, this is still a factor of 3 slower in the best case."
  (declare (optimize speed)
           (type na:numericals-array na:numericals-array))
  (if-let (cl-array (na:array-cl-array na:numericals-array))
    (apply #'aref cl-array subscripts)
    (error "CL-AREF cannot be applied when CL-ARRAY slot is NIL")))

(defun na:aref (na:numericals-array &rest subscripts)
  ;; TODO: Handle nested aref
  ;; TODO: Handle displaced-index-offsets
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
     (with-slots (displaced-to element-type strides displaced-index-offset dim) na:numericals-array
       (if (let ((use-cl-aref t))
             (loop :for subscript :in subscripts
                :while use-cl-aref
                :unless (numberp subscript)
                :do (setf use-cl-aref nil))
             use-cl-aref)
           (aref displaced-to
                 (the (signed-byte 31)
                      (let ((index displaced-index-offset))
                        (declare (type (signed-byte 31) index))
                        (loop :for stride :in strides
                           :for subscript :in subscripts
                           :do (incf index (* (the (signed-byte 31) stride)
                                              (the (signed-byte 31) subscript))))
                        index)))
           (multiple-value-bind (dimensions strides displaced-index-offset contiguous-p)
               (let ((d dim)
                     (s strides)
                     (new-displaced-index-offset 0)
                     new-dimensions new-strides
                     (contiguous-p t)
                     (saw-a-t nil))
                 (declare (type (signed-byte 31) new-displaced-index-offset))
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
                            (setq new-displaced-index-offset
                                  (+ new-displaced-index-offset (* car-s (the (signed-byte 31)
                                                                              subscript)))))
                        (when (and saw-a-t (typep subscript '(signed-byte 31)))
                          (setq contiguous-p nil)))
                      (setq d (cdr d))
                      (setq s (cdr s)))
                 (values (nconc (nreverse new-dimensions) d)
                         (nconc (nreverse new-strides) s)
                         new-displaced-index-offset
                         contiguous-p))
             (make-numericals-array
              :displaced-to displaced-to
              :element-type element-type
              :dim dimensions
              :strides strides
              :displaced-index-offset displaced-index-offset
              :contiguous-p contiguous-p
              :total-size (apply #'* dimensions)
              :cl-array nil)))))))

(defun (setf na:aref) (new-value na:numericals-array &rest subscripts)
  ;; TODO: Handle nested aref
  ;; TODO: Handle displaced-index-offsets
  ;; TODO: Handle non-integer subscripts
  (declare (optimize speed))
  (etypecase na:numericals-array
    (array (apply #'(setf aref) new-value na:numericals-array subscripts))
    (na:numericals-array
     (with-slots (displaced-to element-type strides displaced-index-offset dim) na:numericals-array
       (if (let ((use-cl-aref t))
             (loop :for subscript :in subscripts
                :while use-cl-aref
                :unless (numberp subscript)
                :do (setf use-cl-aref nil))
             use-cl-aref)
           (setf (aref displaced-to
                       (the (signed-byte 31)
                            (+ (the (signed-byte 31) displaced-index-offset)
                               (loop :for stride :in strides
                                  :for subscript :in subscripts
                                  :summing (the (signed-byte 31)
                                                (* (the (signed-byte 31) stride)
                                                   (the (signed-byte 31) subscript)))))))
                 new-value)
           (multiple-value-bind (dimensions strides displaced-index-offset contiguous-p)
               (let ((d dim)
                     (s strides)
                     (new-displaced-index-offset 0)
                     new-dimensions new-strides
                     (contiguous-p t)
                     (saw-a-t nil))
                 (declare (type (signed-byte 31) new-displaced-index-offset))
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
                            (setq new-displaced-index-offset
                                  (+ new-displaced-index-offset (* car-s (the (signed-byte 31)
                                                                              subscript)))))
                        (when (and saw-a-t (typep subscript '(signed-byte 31)))
                          (setq contiguous-p nil)))
                      (setq d (cdr d))
                      (setq s (cdr s)))
                 (values (nconc (nreverse new-dimensions) d)
                         (nconc (nreverse new-strides) s)
                         new-displaced-index-offset
                         contiguous-p))
             (make-numericals-array
              :displaced-to displaced-to
              :element-type element-type
              :dim dimensions
              :strides strides
              :displaced-index-offset displaced-index-offset
              :contiguous-p contiguous-p
              :total-size (apply #'* dimensions)
              :cl-array nil)))))))

#+sbcl (declaim (sb-ext:maybe-inline na:row-major-aref))
(defun na:row-major-aref (na:numericals-array index)
  (declare (optimize (speed 3))
           (type na:numericals-array na:numericals-array)
           (type (unsigned-byte 31) index))
  (if (na:array-contiguous-p na:numericals-array)
      (if (>= index (na:array-total-size na:numericals-array))
          (error "Invalid row-major index ~D for array of size ~D "
                 index (na:array-total-size na:numericals-array))
          (locally (declare (optimize (speed 3) (safety 0)))
            (aref (the simple-array (na:array-displaced-to na:numericals-array))
                  (+ index
                     (the (unsigned-byte 31)
                          (na:array-displaced-index-offset na:numericals-array))))))
      (error "non-contigous case not implemented")))

(defun na:broadcast-array (na:numericals-array broadcast-dimensions)
  ;; TODO: Incorporate displaced-index-offsets!
  ;; TODO: Doing this neatly requires changes in aref
  (with-slots (dim element-type strides displaced-index-offset displaced-to) na:numericals-array
    (make-numericals-array :dim broadcast-dimensions
                           :element-type element-type
                           :strides
                           ;; TODO: Handle strides for (1) -> (1 10) and (10) -> (1 10) broadcast cases
                           (let* ((blen (length broadcast-dimensions))
                                  (len (length dim))
                                  (dim (append (make-list (- blen len)
                                                          :initial-element 1)
                                               dim))
                                  (strides (append (make-list (- blen len)
                                                              :initial-element 0)
                                                   strides)))
                             (loop :for s :in strides
                                :for b :in broadcast-dimensions
                                :for d :in dim
                                :collect
                                  (cond ((= b d) s)
                                        ((= d 1) 0)
                                        (t (error "~D of dim ~D cannot be broadcasted to dim ~D" na:numericals-array dim broadcast-dimensions)))))
                           :displaced-index-offset displaced-index-offset
                           :displaced-to displaced-to
                           ;; TODO: Determine contiguity
                           :contiguous-p nil
                           :total-size (apply #'* broadcast-dimensions)
                           :cl-array nil)))

(defun na:cl-array-array (array)
  (declare (type cl:array array))
  ;; TODO: Take *type* into consideration
  (let ((dimensions (array-dimensions array)))
    (multiple-value-bind (displaced-to displaced-index-offset)
        (1d-storage-array array)
      (make-numericals-array :displaced-to displaced-to
                             :displaced-index-offset displaced-index-offset
                             :dim dimensions
                             :element-type (array-element-type array)
                             :strides (nreverse (let ((stride 1))
                                                  (loop :for d :in (reverse dimensions)
                                                     :collect stride
                                                     :do (setq stride (* stride d)))))
                             :contiguous-p t
                             :total-size (apply #'* dimensions)
                             :cl-array array))))
