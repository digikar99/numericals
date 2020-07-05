(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

(defmacro fixnum-+ (&rest args)
  (cond ((null args) 0)
        ((null (cdr args)) `(the fixnum ,(car args)))
        (t `(the fixnum (+ (the fixnum ,(car args))
                           ,(macroexpand `(fixnum-+ ,@(cdr args))))))))

(defun-c symbols (prefix n)
  (loop for dim below n
     for i = (intern (concatenate 'string prefix (write-to-string dim))
                     numericals.helper:*numericals-internals-package*)
     collect i))

(defun-c index-calculation-code (num-dimensions)
  (loop for s in (symbols "S" num-dimensions)
     for i in (symbols "I" num-dimensions)
     collect `(the fixnum (* ,s ,i))))
