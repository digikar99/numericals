(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

(defun-c use-array-element-type (element-type)
  (ecase element-type
    (single-float '(translate-to-simd-single simd-single-1d-aref +simd-single-1d-aref-stride+ single-float))
    (double-float '(translate-to-simd-double simd-double-1d-aref +simd-double-1d-aref-stride+ double-float))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *simd-single-operation-translation-plist*
    '(:+ simd-single-+
      :- simd-single--
      :* simd-single-*
      :/ simd-single-/
      :sqrt simd-single-sqrt))
  (defparameter *simd-double-operation-translation-plist*
    '(:+ simd-double-+
      :- simd-double--
      :* simd-double-*
      :/ simd-double-/
      :sqrt simd-double-sqrt))
  (defparameter *with-elementwise-operations-symbol-translation-alist* nil
    "Bound inside WITH-ELEMENTWISE-OPERATIONS to help TRANSLATE-TO-SIMD-SINGLE and
TRANSLATE-TO-BASE with the symbol translation."))

(defun-c simd-single-op (op)
  (getf *simd-single-operation-translation-plist*
        (intern (symbol-name op) :keyword)))

(defun-c simd-double-op (op)
  (getf *simd-double-operation-translation-plist*
        (intern (symbol-name op) :keyword)))

(defun-c translate-symbol (symbol)
  (cdr (assoc symbol *with-elementwise-operations-symbol-translation-alist*)))

(defun-c translate-to-simd-single (body)
  "Returns BODY with OP replaced with corresponding SIMD-SINGLE-OP, 
and each symbol S replaced with (SIMD-SINGLE-1D-AREF S LOOP-VAR)."
  (cond ((null body) ())
        ((symbolp body) `(simd-single-1d-aref ,@(translate-symbol body)))
        ((listp body)
         (if-let (simd-op (simd-single-op (car body)))
           `(,simd-op ,@(loop for elt in (cdr body)
                           collect (translate-to-simd-single elt)))
           (error "~D could not be translated to SIMD-OP" (car body))))
        (t (error "Non-exhaustive!"))))

(defun-c translate-to-simd-double (body)
  "Returns BODY with OP replaced with corresponding SIMD-DOUBLE-OP, 
and each symbol S replaced with (SIMD-DOUBLE-1D-AREF S LOOP-VAR)."
  (cond ((null body) ())
        ((symbolp body) `(simd-double-1d-aref ,@(translate-symbol body)))
        ((listp body)
         (if-let (simd-op (simd-double-op (car body)))
           `(,simd-op ,@(loop for elt in (cdr body)
                           collect (translate-to-simd-double elt)))
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

(defun-c translate-to-base (body element-type)
  "Returns BODY with symbol S not in the CAR replaced with (THE ELEMENT-TYPE (AREF S LOOP-VAR)).
Each FORM in BODY is also surrounded with (THE ELEMENT-TYPE FORM)."
  (cond ((null body) ())
        ((symbolp body) `(the ,element-type (cl:aref ,@(translate-symbol body))))
        ((listp body)
         `(the ,element-type
               (,(intern (symbol-name (car body)) :common-lisp)
                 ,@(loop for elt in (cdr body)
                      collect (translate-to-base elt element-type)))))
        (t (error "Non-exhaustive!"))))

(defmacro nu:with-elementwise-operations (element-type result-array body)
  "\"Open codes\" BODY using SIMD operations. BODY is expected to be made up of 
CAR-symbols having corresponding SIMD-OP or REST symbols are expected to be bound 
to an array. An example translation is:
  (nu:with-elementwise-operations :single result (+ a (* b c)))
This is expanded to a form effective as: 
  (setf (simd-single-1d-aref result loop-var)
        (simd-single-+ (simd-single-1d-aref a loop-var)
                       (simd-single-* (simd-single-1d-aref b loop-var)
                                      (simd-single-1d-aref c loop-var))))"
  (setq element-type (second element-type))
  (with-gensyms (1d-storage-array
                 offset
                 loop-var)
    (destructuring-bind (translate-to-simd-fn
                         accessor-fn
                         stride-size
                         element-type)
        (use-array-element-type element-type)
      `(multiple-value-bind (,1d-storage-array ,offset) (1d-storage-array ,result-array)
         (declare (type (simple-array ,element-type) ,1d-storage-array)
                  (optimize speed))
         ,(let* ((original-symbols (collect-symbols body))
                 (symbols (make-gensym-list (length original-symbols) "SYMBOL"))
                 (offsets (make-gensym-list (length original-symbols) "OFFSET"))
                 (loop-vars (make-gensym-list (length original-symbols) "LOOP"))
                 (*with-elementwise-operations-symbol-translation-alist*
                  (cons (list result-array 1d-storage-array loop-var)
                        (mapcar #'list original-symbols symbols loop-vars))))
            `(let* (,@(loop :for s :in symbols
                         :for os :in original-symbols
                         :collect `(,s (nth-value 0 (1d-storage-array ,os))))
                    ,@(loop :for offset :in offsets
                         :for os :in original-symbols
                         :collect `(,offset (nth-value 1 (1d-storage-array ,os))))
                      (start ,offset)
                      ;; should we wrap 1d-storage-array in once-only?
                      ;; it's intended to be a symbol though
                      (stop (+ ,offset (array-total-size ,result-array)))
                      (simd-stop (+ ,offset (* ,stride-size
                                               (floor (- stop start) ,stride-size)))))
               (declare (type (simple-array ,element-type) ,@symbols)
                        (type (signed-byte 31) ,@offsets)
                        (type (signed-byte 31) start stop simd-stop)
                        (optimize (speed 3) (safety 0)))
               ;; ,@(loop :for symbol :in symbols
               ;;      :for offset :in offsets
               ;;      :for original-symbol :in original-symbols
               ;;      :collect `(multiple-value-setq (,symbol ,offset)
               ;;                  (1d-storage-array ,original-symbol)))
               (loop
                  ,@(loop :for loop-var :in loop-vars
                       :for offset :in offsets
                       :appending `(:for ,loop-var fixnum :from ,offset :by ,stride-size))
                  :for ,loop-var fixnum :from start
                  :below simd-stop :by ,stride-size                  
                  :do ;; (print (list ,loop-var ,@loop-vars))
                  (setf (,accessor-fn ,1d-storage-array ,loop-var)
                        ,(funcall translate-to-simd-fn body))
                  finally (loop :for ,loop-var fixnum :from ,loop-var
                             :below stop
                               ,@(loop :for loop-var :in loop-vars
                                    :appending `(:for ,loop-var fixnum :from ,loop-var))
                             :do ;; (print (list ,loop-var ,@loop-vars))
                               (setf (cl:aref ,1d-storage-array ,loop-var)
                                     ,(translate-to-base body element-type))))))))))

(defun-c non-broadcast-operation (operation type)
  (intern (concatenate 'string
                       (ecase type
                         (single-float "SINGLE")
                         (double-float "DOUBLE")
                         (fixnum "FIXNUM"))
                       "-" (symbol-name operation))
          numericals.helper:*numericals-internals-package*))

(macrolet ((def-binary (type cl-op)
             (setq type (cadr type)) ; assume quoted
             `(defun ,(non-broadcast-operation cl-op type)
                  (result a b)
                (declare (optimize (speed 3))
                         (type (array ,type) result a b))
                (nu:with-elementwise-operations ',type result (,cl-op a b)))))
  (def-binary 'single-float +)
  (def-binary 'single-float -)
  (def-binary 'single-float /)
  (def-binary 'single-float *)
  (def-binary 'double-float +)
  (def-binary 'double-float -)
  (def-binary 'double-float /)
  (def-binary 'double-float *))

(macrolet ((def-unary (type cl-op)
             (setq type (cadr type)) ; assume quoted
             `(defun ,(non-broadcast-operation cl-op type)
                  (result a)
                (declare (optimize (speed 3))
                         (type (array ,type) result a))
                (nu:with-elementwise-operations ',type result (,cl-op a)))))
  (def-unary 'single-float sqrt)
  (def-unary 'double-float sqrt))

(defmacro nu:weop (out expression &environment env)
  "\"Open codes\" EXPRESSION using SIMD operations. EXPRESSION is expected to be made up of 
CAR-symbols having corresponding SIMD-OP or REST symbols are expected to be bound 
to an array. An example translation is:
  (weop result (+ a (* b c))) 
This is expanded to a form effective as: 
  (setf (simd-single-1d-aref result loop-var)
        (simd-single-+ (simd-single-1d-aref a loop-var)
                       (simd-single-* (simd-single-1d-aref b loop-var)
                                      (simd-single-1d-aref c loop-var))))"
  (let ((symbols (collect-symbols expression))
        (new-out (gensym))
        (unsafe-p (zerop (policy-quality 'safety env))))
    `(locally
         #+sbcl(declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
         (let ((,new-out (or ,out
                             (nu:zeros (array-dimensions ,(car symbols))
                                       :type (array-element-type ,(car symbols))))))
           ,(unless unsafe-p
              `(progn
                 ,@(loop :for s :in symbols
                      :collect
                        `(assert (eq (array-element-type ,new-out)
                                     (array-element-type ,s))
                                 nil
                                 "Elementwise operations cannot be performed on arrays ~A and ~A"
                                 ,new-out ,s))))
           ,(unless unsafe-p
              `(progn
                 ,@(loop :for s :in symbols
                      :collect
                        `(assert (equalp (array-dimensions ,new-out)
                                         (array-dimensions ,s))
                                 nil
                                 "Elementwise operations cannot be performed on arrays ~A and ~A"
                                 ,new-out ,s))))
           (ecase (array-element-type ,new-out)
             (single-float
              (nu:with-elementwise-operations 'single-float ,new-out ,expression))
             (double-float
              (nu:with-elementwise-operations 'double-float ,new-out ,expression)))
           ,new-out))))

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

