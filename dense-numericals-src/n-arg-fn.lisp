(in-package :dense-numericals.impl)

(define-constant dn:+optimized-types+
    '(single-float double-float)
  :test #'equal)
;; TODO: Add compiler-macros for this

(defun normalize-arguments/dmas (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((out-type
               (if out
                   (array-element-type out)
                   (multiple-value-bind (out-type array-likes)
                       (loop :for array-likes :on array-likes
                             :if (arrayp (first array-likes))
                               :do (return (values (array-element-type (first array-likes))
                                                   (rest array-likes)))
                             :finally (return (values default-element-type nil)))
                     (loop :for array-like :in array-likes
                           :if (arrayp array-like)
                             :do (setq out-type
                                       (type-max out-type
                                                 (array-element-type array-like))))
                     out-type)))
             (arrays
               (loop :for array-like :on array-likes
                     :do (unless (and (arrayp (first array-like))
                                      (type= out-type (array-element-type (first array-like))))
                           (setf (first array-like)
                                 ;; TODO: Use TRIVIAL-COERCE:COERCE and simd based backends
                                 ;; FIXME: Signal a condition when the conversion is unsafe
                                 ;; TODO: Enable different upgrade-rules
                                 (asarray (first array-like) :type out-type)))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values (apply #'broadcast-arrays arrays)
                      (or out (zeros dimensions :type out-type)))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'dimensions array-likes)))))))

;;; These functions cannot have (much) benefits of a compiler-macro
;;; until it becomes possible to tell the array dimensions at compile time.

(macrolet ((def (name reduce-fn initial-value)
             `(defun ,name (&rest args)
                (destructuring-bind (array-likes &key out)
                    (split-at-keywords args)
                  ;; TODO: Incorporate WHERE (?)
                  (multiple-value-bind (array-likes out)
                      (normalize-arguments/dmas array-likes out)
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
                        (if (rest array-likes)
                            (reduce #',(cl-name reduce-fn)
                                    (rest array-likes)
                                    :initial-value (first array-likes))
                            (funcall #',(cl-name reduce-fn)
                                     (first array-likes)))))))))
  (def dn:+ dn:two-arg-+ 0)
  (def dn:- dn:two-arg-- 0)
  (def dn:* dn:two-arg-* 1)
  (def dn:/ dn:two-arg-/ 1))

(defun normalize-arguments/cmp (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let ((arrays (loop :for array-like :on array-likes
                            :do (setf (first array-like)
                                      (ensure-appropriate-dense-array (first array-like)))
                          :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values (apply #'broadcast-arrays arrays)
                      (or out (zeros dimensions :type '(unsigned-byte 8))))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'dimensions array-likes)))))))

(macrolet ((def (name reduce-fn)
             `(define-splice-list-fn ,name (array-likes &key out)
                ;; TODO: Incorporate WHERE (?)
                (multiple-value-bind (array-likes out)
                    (normalize-arguments/cmp array-likes out)
                  (reduce (lambda (old-out new-value)
                            (,reduce-fn old-out new-value :out out))
                          (rest array-likes)
                          :initial-value (first array-likes))))))
  (def dn:<  dn:two-arg-<)
  (def dn:<= dn:two-arg-<=)
  (def dn:=  dn:two-arg-=)
  (def dn:/= dn:two-arg-/=)
  (def dn:>  dn:two-arg->)
  (def dn:>= dn:two-arg->=))
