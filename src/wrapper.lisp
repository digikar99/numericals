(in-package :numericals.internals)

(defun %broadcast-compatible-p (dimensions-a dimensions-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two dimensions are broadcast compatible.
  The second value is the dimensions of the array resulting from the broadcast."
  (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1))
        (for dim-a initially (nreverse dimensions-a)
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (nreverse dimensions-b)
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b)
                         (= a 1)
                         (= b 1))
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
    (1 t)
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
                                        broadcast-dimensions-rest))))))))

(defun array-like-p (object) (or (listp object) (arrayp object)))
;; support only 1d lists currently

;; What do we do when this variable is modified?
(eval-when (:compile-toplevel)
  (defparameter *max-broadcast-dimensions* 4))

(defun ensure-array (type object)
  (etypecase object
    (list (make-array (nu:shape object) :element-type type
                      :initial-contents (cast type object)))
    (array (if (eq type (array-element-type object))
               object
               (nu:astype object type)))
    (number (make-array '(1) :element-type type
                        :initial-element (cast type object)))))

(defun nu:+ (&rest args)
  "ARGS must be a lambda list with the initial elements comprising of the list 
of operands, and the final optional elements comprise of :OUT and :TYPE 
keyword args. For example
  (+ a b :out c :type 'single-float)
  (+ a b c :out d)"
  ;; Do the checks to provide helpful error messages.
  (destructuring-bind (args (&key (out nil out-supplied-p)
                                  (type *type*)))
      (split-at-keywords args)
    (let (broadcast-dimensions)
      (setq args (mapcar (curry 'ensure-array type) args))
      (setq broadcast-dimensions
            (nth-value 1 (apply 'broadcast-compatible-p args)))
      (when out-supplied-p
        (assert (some 'array-like-p args) nil
                "Cannot supply result in ~D when no argument is array-like."
                out)
        ;; Might do some checks with TYPE-SUPPLIED-P as well.
        (assert (arrayp out) nil
                "Cannot supply result in non-array type ~D" out)        
        ;; Since we allow arg to be of type list for flexibility;
        ;; ensure-array-dimensions need to be taken care of.
        (assert (equalp (array-dimensions out) broadcast-dimensions)))
      (when (not out-supplied-p)
        (setq out (apply 'nu:zeros (append broadcast-dimensions (list :type type)))))

      (if (some 'array-like-p args)
          (apply '%+ type broadcast-dimensions out args)
          (apply '+ args)))))

;; To be able to handle *max-broadcast-dimensions*, we need to %+ using a macro
(eval-when (:compile-toplevel)
  (defun specialized-operation (operation type num-dimensions)
    (intern (concatenate 'string
                         (ecase type
                           (single-float "SINGLE")
                           (double-float "DOUBLE")
                           (fixnum "FIXNUM"))
                         "-" (write-to-string num-dimensions) "D-" (symbol-name operation))
            :numericals.internals)))

(defmacro define-broadcast-operation (name operation)
  `(defun ,name (type broadcast-dimensions result &rest args)
     (declare (optimize (speed 3)))
     (ecase type
       ,@(loop for type in '(single-float double-float fixnum)
            collect `(,type
                      (ecase (length broadcast-dimensions)
                        ,@(loop for i from 1 to *max-broadcast-dimensions*
                             collect `(,i ,(let ((specialized-op
                                                  (specialized-operation operation type i)))
                                             `(loop while (cddr args)
                                                 for a = (car args)
                                                 do (,specialized-op result result a)
                                                   (setq args (cdr args))
                                                 finally
                                                   (if (cdr args) ; given (cddr args) is null
                                                       (,specialized-op result (car args) (cadr args))
                                                       (,specialized-op result result (car args)))
                                                   (return result)))))))))))

(declaim (notinline %+))
(define-broadcast-operation %+ +)
