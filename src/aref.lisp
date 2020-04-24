(in-package :numericals.internals)

;; Numpy uses an array wrapper, and a slice of the matrix
;; is actually a part of the original matrix - meaning writing to the
;; slice actually writes to the original array - and this is imaginable
;; to be efficient. Yet, make-array happens to be sufficiently good.

;; The closest thing is select:select - and that happens to be about 20 times
;; for the cl:aref equivalent.

(defun-c specialized-aref (n)
  (intern (string (concatenate 'string
                               "AREF-"
                               (write-to-string n)
                               "D"))
          :numericals.internals))

(declaim (inline tp))
(defun-c tp (object) (eq t object))

(declaim (inline fixnump))
(defun-c fixnump (object) (typep object 'fixnum))

(declaim (inline aref-applicable-p))
(defun-c aref-applicable-p (array &rest subscripts)
  (and (length= (array-dimensions array) subscripts)
       (every 'fixnump subscripts)))

(defun-c normalize-subscripts (array &rest subscripts)
  (declare (optimize (speed 3)))
  (let ((num-dim (length (array-dimensions array)))
        (len (length subscripts))
        (subscript nil))
    (declare (type (signed-byte 31) num-dim len))
    (assert (<= len num-dim) nil
            "Using ~D subscripts to index into an array of ~D dimensions"
            len num-dim)
    (loop for dim fixnum in (array-dimensions array)
       do (setq subscript (car subscripts)
                subscripts (cdr subscripts))
       collect
         (etypecase subscript
           (list (destructuring-bind (&optional (start t) (end t) (step nil step-supplied-p))
                     subscript
                   (nconc (list (if (tp start) 0 start)
                                (if (tp end) dim end))
                          (if (or (tp step) (null step-supplied-p))
                              (list 1)
                              (list step)))))
           (number subscript)
           (t (list 0 dim 1))))))

(declaim (inline required-array-size))
(defun-c required-array-size (start end step)
  (ceiling (- end start)
           step))

(progn
  (defmacro define-aref ()
    `(defun nu:aref (&rest args)
       "ARGS: (array &rest subscripts &key out)
Examples: (array '(t t 3) :out out)."
       (declare (optimize (speed 3)))
       (destructuring-bind ((array &rest subscripts) (&key out))
           (split-at-keywords args)
         (if (apply 'aref-applicable-p array subscripts)
             (apply 'aref array subscripts)
             (let ((normalized-subscripts (apply 'normalize-subscripts
                                                 array subscripts))
                   (num-dim (length (array-dimensions array))))
               
               (unless out
                 (setq out
                       (apply 'nu:zeros
                              (nconc (loop for subscript in normalized-subscripts
                                        when (listp subscript)
                                        collect (apply 'required-array-size subscript))
                                     (list :type (array-element-type array))))))
               (ecase num-dim
                 ,@(loop for i from 1 to *max-broadcast-dimensions*
                      collect `(,i
                                (,(specialized-aref i) out array
                                  ,@(loop for j below i
                                       collect
                                         `(elt normalized-subscripts ,j)))))))))))
  (define-aref))

(defmacro nested-db (max-depth current-depth indices out-array out-array-indices array
                     array-indices
                     start-symbols end-symbols step-symbols)
  (if (= max-depth current-depth)
      `(progn
         ;; performance! eliminate repeated calls to remove-if
         (setf (aref ,out-array ,@(remove-if 'null out-array-indices))
               (aref ,array ,@array-indices)))
      (let ((start (elt start-symbols current-depth))
            (end (elt end-symbols current-depth))
            (step (elt step-symbols current-depth))
            (index (elt indices current-depth)))
        `(if (listp ,index)
             (destructuring-bind (,start ,end ,step)
                 ,index
               (loop for ,(elt array-indices current-depth) fixnum
                  from ,start
                  below ,end
                  by ,step
                  for ,(elt out-array-indices current-depth) fixnum from 0
                  do ,(macroexpand-1 `(nested-db ,max-depth ,(1+ current-depth)
                                                 ,indices
                                                 ,out-array
                                                 ,out-array-indices
                                                 ,array
                                                 ,array-indices
                                                 ,start-symbols
                                                 ,end-symbols
                                                 ,step-symbols))))
             (let ((,(elt array-indices current-depth) ,index))
               (declare (type (signed-byte 31) ,(elt array-indices current-depth)))
               ,(macroexpand-1 `(nested-db ,max-depth ,(1+ current-depth)
                                           ,indices
                                           ,out-array
                                           ,(progn
                                              (let ((l (copy-list out-array-indices)))
                                                (setf (elt l current-depth) nil)
                                                l))
                                           ,array
                                           ,array-indices
                                           ,start-symbols
                                           ,end-symbols
                                           ,step-symbols)))))))

(progn
  (defmacro define-specialized-aref ()
    `(progn
       ,@(loop for i from 1 to *max-broadcast-dimensions*
            collect (let ((indices (make-gensym-list i))
                          (array-indices (make-gensym-list i))
                          (out-array-indices (make-gensym-list i))
                          (start-symbols (make-gensym-list i))
                          (end-symbols (make-gensym-list i))
                          (step-symbols (make-gensym-list i)))
                      `(defun ,(specialized-aref i) (out array ,@indices)
                         (declare (optimize (speed 3))
                                  (type (or list fixnum) ,@indices))
                         ,(macroexpand-1 `(nested-db ,i 0
                                                     ,indices
                                                     out ,out-array-indices
                                                     array 
                                                     ,array-indices
                                                     ,start-symbols
                                                     ,end-symbols
                                                     ,step-symbols))
                         out)))))
  (define-specialized-aref))


;; INCOMPLETE EXAMPLE
;; (declaim (inline aref-1d))
;; (defun aref-1d (out array i0)
;;   (declare (optimize (speed 3)))
;;   (destructuring-bind (start end step) i0
;;     (declare (fixnum start end step))
;;     (loop for i fixnum from start below end by step
;;        for j fixnum from 0
;;        do (setf (aref out j)
;;                 (aref array i))
;;        finally (return out))))
