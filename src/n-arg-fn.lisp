(in-package :numericals.internals)

;; TODO: Add compiler-macros for this

(defun normalize-arguments/dmas (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((arrays (loop :for array-like :on array-likes
                           :do (setf (first array-like)
                                     (ensure-appropriate-array (first array-like)))
                           :finally (return array-likes)))
             (array-dimensions (mapcar #'array-dimensions arrays)))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p (array-dimensions out) array-dimensions)
                (apply #'broadcast-compatible-p  array-dimensions))
          (unless broadcast-compatible-p
            (error 'incompatible-broadcast-dimensions
                   :array-likes array-likes
                   :dimensions array-dimensions))
          (values arrays (or out (nu:zeros dimensions
                                           :type (array-element-type (first arrays)))))))))

;;; FIXME: The only difference is the element-type of OUT

(defun normalize-arguments/cmp (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((arrays (loop :for array-like :on array-likes
                           :do (setf (first array-like)
                                     (ensure-appropriate-array (first array-like)))
                           :finally (return array-likes)))
             (array-dimensions (mapcar #'array-dimensions arrays)))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p (array-dimensions out) array-dimensions)
                (apply #'broadcast-compatible-p  array-dimensions))
          (unless broadcast-compatible-p
            (error 'incompatible-broadcast-dimensions
                   :array-likes array-likes
                   :dimensions array-dimensions))
          (values arrays (or out (nu:zeros dimensions :type '(unsigned-byte 8))))))))

;;; These functions cannot have (much) benefits of a compiler-macro
;;; until it becomes possible to tell the array dimensions at compile time.
;;; TODO: Think about type normalization and upgradation and compiler-macro

(macrolet ((def (name reduce-fn normalize-fn initial-value)
             `(defun ,name (&rest args)
                (destructuring-bind (array-likes &key out)
                    (split-at-keywords args)
                  ;; TODO: Incorporate WHERE (?)
                  (multiple-value-bind (array-likes out)
                      (,normalize-fn array-likes out)
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
                        ,(let ((number-code `(if (rest array-likes)
                                                 (reduce #',(cl-name reduce-fn)
                                                         (rest array-likes)
                                                         :initial-value (first array-likes))
                                                 (funcall #',(cl-name reduce-fn)
                                                          (first array-likes)))))
                           (if (typep name 'comparison-operator)
                               `(if ,number-code 1 0)
                               number-code))))))))
  (def nu:+ nu:two-arg-+ normalize-arguments/dmas 0)
  (def nu:- nu:two-arg-- normalize-arguments/dmas 0)
  (def nu:* nu:two-arg-* normalize-arguments/dmas 1)
  (def nu:/ nu:two-arg-/ normalize-arguments/dmas 1)

  (def nu:<  nu:two-arg-<  normalize-arguments/cmp 0)
  (def nu:<= nu:two-arg-<= normalize-arguments/cmp 0)
  (def nu:=  nu:two-arg-=  normalize-arguments/cmp 0)
  (def nu:/= nu:two-arg-/= normalize-arguments/cmp 0)
  (def nu:>  nu:two-arg->  normalize-arguments/cmp 0)
  (def nu:>= nu:two-arg->= normalize-arguments/cmp 0))
