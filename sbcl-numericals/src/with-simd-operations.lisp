(in-package :sbcl-numericals.internals)

(defparameter *simd-operation-translation-table* (make-hash-table))


;;; abstract them out in case representation changes
(defun simd-equivalent (op) (gethash op *simd-operation-translation-table*))
(defun (setf simd-equivalent) (new-value op)
  (setf (gethash op *simd-operation-translation-table*)
        new-value))

(defun simd-operable-sexp-p (body)
  (cond ((typep body 'atom) t)
        ((listp body)
         (destructuring-bind (op &rest args) body
           (if (simd-equivalent op)
               (every 'simd-operable-sexp-p args)
               nil)))))

(defmacro with-simd-operations (&body body))
