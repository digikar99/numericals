(in-package :numericals.internals)

(defmacro lm (&rest var-body)
  `(lambda ,(butlast var-body)
     ,@(last var-body)))

(defun strides-for-broadcast (original-dimensions broadcast-dimensions)
  (flet ((incompatible ()
           (error "Dimensions ~D cannot be broadcast to dimensions ~D"
                  original-dimensions broadcast-dimensions)))
    (let ((len-diff (- (length broadcast-dimensions)
                       (length original-dimensions))))
      (if (< len-diff 0)
          (incompatible)
          (nconc (make-list len-diff :initial-element 0)
                 (nreverse (loop :for b :in (reverse broadcast-dimensions)
                                 :for d :in (reverse original-dimensions)
                                 :with s := 1
                                 :collect
                                 (cond ((= b d) s)
                                       ((= d 1) 0)
                                       (t (incompatible)))
                                 :do (setq s (* d s)))))))))

(defun strides (dimensions)
  (strides-for-broadcast dimensions dimensions))

(defun %broadcast-compatible-p (dimensions-a dimensions-b)
  "Returns two values:
  The first value is a generalized boolean indicating whether the two dimensions are broadcast compatible.
  The second value is the dimensions of the array resulting from the broadcast."
  (if (equalp dimensions-a dimensions-b)
      (values t dimensions-a)
      (iter (for a = (if dim-a (first dim-a) 1))
        (for b = (if dim-b (first dim-b) 1))
        ;; We do not use a "for in" clause because we want to terminate at the
        ;; maximum of the two lists rather than the minimum
        (for dim-a initially (reverse dimensions-a)
             then (if dim-a (cdr dim-a) nil))
        (for dim-b initially (reverse dimensions-b)
             then (if dim-b (cdr dim-b) nil))
        (while (or dim-a dim-b))
        (collect (if (or (= a b) (= a 1) (= b 1))
                     (max a b)
                     (return nil))
          into broadcast-dimensions-reversed)
        (finally (return (values t
                                 (nreverse broadcast-dimensions-reversed)))))))

(defun broadcast-compatible-p (&rest dimensions)
  "Returns two values:
  The first value is a generalized boolean indicating whether the dimensions can be broadcasted.
  The second value is the dimension of the array resulting from the broadcast."
  (declare (dynamic-extent dimensions)
           (optimize speed))
  (case (length dimensions)
    (0 t)
    (1 (values t (first dimensions)))
    (2 (%broadcast-compatible-p (first dimensions)
                                (second dimensions)))
    (t (multiple-value-bind (compatible-p broadcast-dimensions)
           (%broadcast-compatible-p (first dimensions) (second dimensions))
         ;; Can this be simplified?
         (when compatible-p
           (multiple-value-bind (compatible-p-rest broadcast-dimensions-rest)
               (apply #'broadcast-compatible-p (cddr dimensions))
             (when compatible-p-rest
               (%broadcast-compatible-p broadcast-dimensions
                                        broadcast-dimensions-rest))))))))

(define-condition incompatible-broadcast-dimensions (error)
  ((dimensions :initarg :dimensions :reader condition-dimensions)
   (array-likes :initarg :array-likes :reader condition-array-likes))
  (:report (lambda (c s)
             (pprint-logical-block (s nil)
               (format s "The following array-likes with dimensions~{~%  ~S~}~%cannot be broadcast together:~%" (condition-dimensions c))
               (pprint-logical-block (s nil :per-line-prefix "  ")
                 (format s "~S" (condition-array-likes c)))))))
