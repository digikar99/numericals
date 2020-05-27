(in-package :numericals.internals)

(defun-c use-array-element-type (element-type)
  (ecase element-type
    (single-float '(translate-to-simd-single simd-single-1d-aref +simd-single-1d-aref-stride+ single-float))
    (double-float '(translate-to-simd-double simd-double-1d-aref +simd-double-1d-aref-stride+ double-float))))

(eval-when (:compile-toplevel)
  (defparameter *simd-single-operation-translation-plist*
    '(cl:+ simd-single-+
      cl:- simd-single--
      cl:* simd-single-*
      cl:/ simd-single-/)))

(defun-c simd-single-op (op) (getf *simd-single-operation-translation-plist* op))

(defun-c translate-to-simd-single (body loop-var)
  "Returns BODY with OP replaced with corresponding SIMD-SINGLE-OP, 
and each symbol S replaced with (SIMD-SINGLE-1D-AREF S LOOP-VAR)."
  (cond ((null body) ())
        ((symbolp body) `(simd-single-1d-aref ,body ,loop-var))
        ((listp body)
         (if-let (simd-op (simd-single-op (car body)))
           `(,simd-op ,@(loop for elt in (cdr body)
                           collect (translate-to-simd-single elt loop-var)))
           (error "~D could not be translated to SIMD-OP" (car body))))
        (t (error "Non-exhaustive!"))))

(defun-c collect-symbols (body)
  (cond ((null body) ())
        ((symbolp body) (list body))
        ((listp body)
         (if (simd-single-op (car body))
             (remove-duplicates
              (apply 'append (loop for elt in (cdr body)
                                collect (collect-symbols elt))))
             (error "~D could not be translated to SIMD-OP" (car body))))
        (t (error "Non-exhaustive!"))))

(defun-c translate-to-base (body loop-var element-type)
  "Returns BODY with symbol S not in the CAR replaced with (THE ELEMENT-TYPE (AREF S LOOP-VAR)).
Each FORM in BODY is also surrounded with (THE ELEMENT-TYPE FORM)."
  (cond ((null body) ())
        ((symbolp body) `(the ,element-type (aref ,body ,loop-var)))
        ((listp body)
         `(the ,element-type
               (,(car body) ,@(loop for elt in (cdr body)
                                 collect (translate-to-base elt loop-var element-type)))))
        (t (error "Non-exhaustive!"))))

(defmacro nu:with-simd-operations (element-type result-array body)
  "\"Open codes\" BODY using SIMD operations. BODY is expected to be made up of 
CAR-symbols having corresponding SIMD-OP or REST symbols are expected to be bound 
to an array. An example translation is:
  (with-simd-operations :single result (+ a (* b c)))
This is expanded to a form effective as: 
  (setf (simd-single-1d-aref result loop-var)
        (simd-single-+ (simd-single-1d-aref a loop-var)
                       (simd-single-* (simd-single-1d-aref b loop-var)
                                      (simd-single-1d-aref c loop-var))))"
  (setq element-type (second element-type))
  (with-gensyms (1d-storage-array
                 loop-var
                 final-loop-var)
    (destructuring-bind (translate-to-simd-fn
                         accessor-fn
                         stride-size
                         element-type)
        (use-array-element-type element-type)
      `(let ((,1d-storage-array (1d-storage-array ,result-array))
             ,@(let ((symbols (collect-symbols body)))
                 (loop for symbol in symbols
                    collect `(,symbol (1d-storage-array ,symbol)))))
         (declare (optimize (speed 3)))
         (loop for ,loop-var fixnum
            below (- (length ,1d-storage-array) ,stride-size)
            by ,stride-size
            do (setf (,accessor-fn ,1d-storage-array ,loop-var)
                     ,(funcall translate-to-simd-fn body loop-var))
            finally (loop for ,final-loop-var fixnum from ,loop-var below (length ,1d-storage-array)
                       do (setf (aref ,1d-storage-array ,final-loop-var)
                                ,(translate-to-base body final-loop-var element-type))))
         ,result-array))))

(defun single-+ (result a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float) result a b))
  (nu:with-simd-operations 'single-float result (+ a b)))

(defun single-- (result a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float) result a b))
  (nu:with-simd-operations 'single-float result (- a b)))

(defun single-* (result a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float) result a b))
  (nu:with-simd-operations 'single-float result (* a b)))

(defun single-/ (result a b)
  (declare (optimize (speed 3))
           (type (simple-array single-float) result a b))
  (nu:with-simd-operations 'single-float result (/ a b)))

(defun-c non-broadcast-operation (operation type)
  (intern (concatenate 'string
                       (ecase type
                         (single-float "SINGLE")
                         (double-float "DOUBLE")
                         (fixnum "FIXNUM"))
                       "-" (symbol-name operation))
          :numericals.internals))

;; (let ((size 1048576))
;;   (defparameter a (make-array size :element-type 'single-float
;;                               :initial-contents (loop for i below size collect (+ i 0.1))))

;;   (defparameter b (make-array size :element-type 'single-float
;;                               :initial-contents (loop for i below size collect (+ i 1.1))))

;;   (defparameter d (make-array size :element-type 'single-float
;;                               :initial-contents (loop for i below size collect (+ i 2.1))))

;;   (defparameter c (make-array size :element-type 'single-float :initial-element 0.0)))
;; There are 16 AVX2 registers in intel 8750H! With full usage,
;; things can be over thrice as fast as numpy even in the presence of cache bottlenecks!!!

;; (defun foo ()
;;   (declare (optimize (speed 3)))
;;   (with-simd-accessors :single c (+ a a a a a a a a a a a a a a a)))

