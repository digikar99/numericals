(in-package :numericals/basic-math/impl)

(5am:in-suite :numericals)

(defun ensure-array-with-type (array-like type)
  ;; FIXME: Signal a condition when the conversion is unsafe
  ;; TODO: Enable different upgrade-rules
  (if (arrayp array-like)
      (if (type= type (array-element-type array-like))
          array-like
          (nu:astype array-like type))
      (nu:asarray array-like :type type)))

(defun normalize-arguments/dmas (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((out-type
               (if out
                   (array-element-type out)
                   (multiple-value-bind (out-type array-likes)
                       ;; Set up an initial guess for OUT-TYPE
                       (values (if (arrayp (first array-likes))
                                   (array-element-type (first array-likes))
                                   (traits:element-type (first array-likes)))
                               (rest array-likes))
                     ;; Refine the initial guess
                     (loop :for array-like :in array-likes
                           :do (setq out-type
                                     (max-type out-type
                                               (if (arrayp array-like)
                                                   (array-element-type array-like)
                                                   (traits:element-type array-like)))))
                     (if (eq t default-element-type)
                         out-type
                         (max-type out-type default-element-type)))))
             (out-type (upgraded-c-array-element-type out-type))
             (arrays
               (loop :for array-like :on array-likes
                     :do (setf (first array-like)
                               (ensure-array-with-type (first array-like) out-type))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              ;; FIXME: CL:ARRAY cannot be broadcasted
              (values arrays
                      (or out (nu:zeros dimensions :type out-type)))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'traits:dimensions array-likes)))))))

;;; These functions cannot have (much) benefits of a compiler-macro
;;; until it becomes possible to tell the array dimensions at compile time.

;;; TODO: Handle BROADCAST option

(macrolet ((def (name reduce-fn initial-value)
             `(defun ,name (&rest args)
                (destructuring-bind (array-likes &key out)
                    (split-at-keywords args)
                  ;; TODO: Incorporate WHERE (?)
                  (multiple-value-bind (array-likes out)
                      (normalize-arguments/dmas array-likes out)
                    (if out
                        (let ((initial-value ,initial-value))
                          (cond ((null array-likes)
                                 (,reduce-fn initial-value out :out out))
                                ((null (cdr array-likes))
                                 (,reduce-fn initial-value (first array-likes) :out out))
                                (t
                                 (,reduce-fn (first array-likes)
                                             (second array-likes)
                                             :out out)
                                 (loop :for array-like :in (cddr array-likes)
                                       :do (,reduce-fn out array-like :out out))))
                          out)
                        (if (rest array-likes)
                            (reduce #',(swank/backend:function-name (cl-name reduce-fn))
                                    (rest array-likes)
                                    :initial-value (first array-likes))
                            (funcall #',(swank/backend:function-name (cl-name reduce-fn))
                                     (first array-likes)))))))))
  (def nu:+ nu:add      0)
  (def nu:- nu:subtract 0)
  (def nu:* nu:multiply 1)
  (def nu:/ nu:divide   1))

(defun normalize-arguments/cmp (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((common-type
               (multiple-value-bind (common-type array-likes)
                   ;; Set up an initial guess for COMMON-TYPE
                   (values (if (arrayp (first array-likes))
                               (array-element-type (first array-likes))
                               (traits:element-type (first array-likes)))
                           (rest array-likes))
                 ;; Refine the initial guess
                 (loop :for array-like :in array-likes
                       :do (setq common-type
                                 (max-type common-type
                                           (if (arrayp array-like)
                                               (array-element-type array-like)
                                               (traits:element-type array-like)))))
                 (if (eq t default-element-type)
                     common-type
                     (max-type common-type default-element-type))))
             (common-type (upgraded-c-array-element-type common-type))
             (arrays
               (loop :for array-like :on array-likes
                     :do (setf (first array-like)
                               (ensure-array-with-type (first array-like) common-type))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values arrays
                      (if out
                          (nu:copy 1 :out out)
                          (nu:ones dimensions :type '(unsigned-byte 8))))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'traits:dimensions array-likes)))))))

(defmacro define-splice-list-fn (name args &body body)
  (multiple-value-bind (doc body)
      (if (and (stringp (first body))
                      (rest body))
          (values (first body) (rest body))
          (values (format nil "LAMBDA-LIST: ~A" args)
                  body))
    `(progn
       (declaim (inline ,name))
       (declaim (ftype (function * simple-array) ,name))
       (defun ,name (&rest args)
         ,doc
         (destructuring-bind ,args (split-at-keywords args)
           ,@body))
       (declaim (notinline ,name)))))

(macrolet ((def (name reduce-fn)
             `(define-splice-list-fn ,name (array-likes &key out)
                ;; TODO: Incorporate WHERE (?)
                (multiple-value-bind (array-likes out)
                    (normalize-arguments/cmp array-likes out)
                  ;; (print array-likes)
                  (cond ((null array-likes)
                         (error "Need at least one argument"))
                        (out
                         (cond ((null (cdr array-likes))
                                out)
                               ((null (cddr array-likes))
                                (,reduce-fn (first array-likes)
                                            (second array-likes)
                                            :out out))
                               (t
                                (,reduce-fn (first array-likes)
                                            (second array-likes)
                                            :out out)
                                (let ((tmp (nu:zeros-like out)))
                                  (loop :for first :in (cdr array-likes)
                                        :for second :in (cddr array-likes)
                                        :do (,reduce-fn first second :out tmp)
                                            (nu:two-arg-logand out tmp :out out)))))
                         out)
                        (t
                         (if (apply #',(swank/backend:function-name (cl-name reduce-fn)) array-likes)
                             1
                             0)))))))
  (def nu:<  nu:two-arg-<)
  (def nu:<= nu:two-arg-<=)
  (def nu:=  nu:two-arg-=)
  (def nu:/= nu:two-arg-/=)
  (def nu:>  nu:two-arg->)
  (def nu:>= nu:two-arg->=))


(defun normalize-arguments/bitwise (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((common-type
               (multiple-value-bind (common-type array-likes)
                   ;; Set up an initial guess for COMMON-TYPE
                   (values (if (arrayp (first array-likes))
                               (array-element-type (first array-likes))
                               (traits:element-type (first array-likes)))
                           (rest array-likes))
                 ;; Refine the initial guess
                 (loop :for array-like :in array-likes
                       :do (setq common-type
                                 (max-type common-type
                                           (if (arrayp array-like)
                                               (array-element-type array-like)
                                               (traits:element-type array-like)))))
                 common-type))
             (arrays
               (loop :for array-like :on array-likes
                     :do (setf (first array-like)
                               (ensure-array-with-type (first array-like) common-type))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values arrays
                      (if out
                          (nu:copy 1 :out out)
                          (ones dimensions :type '(unsigned-byte 8))))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'traits:dimensions array-likes)))))))

(macrolet ((def (name reduce-fn initial-value)
             `(defun ,name (&rest args)
                (destructuring-bind (array-likes &key out)
                    (split-at-keywords args)
                  ;; TODO: Incorporate WHERE (?)
                  (multiple-value-bind (array-likes out)
                      (normalize-arguments/bitwise array-likes out)
                    (if out
                        (progn
                          (cond ((null array-likes)
                                 (,reduce-fn ,initial-value out :out out))
                                ((null (cdr array-likes))
                                 (,reduce-fn ,initial-value (first array-likes) :out out))
                                (t
                                 (,reduce-fn (first array-likes)
                                             (second array-likes)
                                             :out out)
                                 (loop :for array-like :in (cddr array-likes)
                                       :do (,reduce-fn out array-like :out out))))
                          out)
                        (apply #',(swank/backend:function-name (cl-name reduce-fn)) (rest array-likes))))))))

  (def nu:logand nu:two-arg-logand -1)
  (def nu:logior nu:two-arg-logior 0)
  (def nu:logxor nu:two-arg-logxor 0))
