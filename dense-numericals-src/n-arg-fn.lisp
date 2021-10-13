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
                       ;; Set up an initial guess for OUT-TYPE
                       (values (if (arrayp (first array-likes))
                                   (array-element-type (first array-likes))
                                   (element-type (first array-likes)))
                               (rest array-likes))
                     ;; Refine the initial guess
                     (loop :for array-like :in array-likes
                           :do (setq out-type
                                     (max-type out-type
                                               (if (arrayp array-like)
                                                   (array-element-type array-like)
                                                   (element-type array-like)))))
                     (max-type out-type default-element-type))))
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
                        (let ((initial-value (broadcast-array
                                              (asarray ',(list initial-value)
                                                       :type (array-element-type out))
                                              (array-dimensions out))))
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
      (let* ((common-type
               (multiple-value-bind (common-type array-likes)
                   ;; Set up an initial guess for COMMON-TYPE
                   (values (if (arrayp (first array-likes))
                               (array-element-type (first array-likes))
                               (element-type (first array-likes)))
                           (rest array-likes))
                 ;; Refine the initial guess
                 (loop :for array-like :in array-likes
                       :do (setq common-type
                                 (max-type common-type
                                           (if (arrayp array-like)
                                               (array-element-type array-like)
                                               (element-type array-like)))))
                 common-type))
             (arrays
               (loop :for array-like :on array-likes
                     :do (unless (and (arrayp (first array-like))
                                      (type= common-type (array-element-type (first array-like))))
                           (setf (first array-like)
                                 ;; TODO: Use TRIVIAL-COERCE:COERCE and simd based backends
                                 ;; FIXME: Signal a condition when the conversion is unsafe
                                 ;; TODO: Enable different upgrade-rules
                                 (asarray (first array-like) :type common-type)))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values (apply #'broadcast-arrays arrays)
                      (if out
                          (dn:copy 1 :out out)
                          (ones dimensions :type '(unsigned-byte 8))))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'dimensions array-likes)))))))

(macrolet ((def (name reduce-fn)
             `(define-splice-list-fn ,name (array-likes &key out)
                ;; TODO: Incorporate WHERE (?)
                (multiple-value-bind (array-likes out)
                    (normalize-arguments/cmp array-likes out)
                  (print array-likes)
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
                                (let ((tmp (zeros-like out)))
                                  (loop :for first :in (cdr array-likes)
                                        :for second :in (cddr array-likes)
                                        :do (,reduce-fn first second :out tmp)
                                            (dn:two-arg-logand out tmp :out out)))))
                         out)
                        (t
                         (if (apply #',(cl-name reduce-fn) array-likes)
                             1
                             0)))))))
  (def dn:<  dn:two-arg-<)
  (def dn:<= dn:two-arg-<=)
  (def dn:=  dn:two-arg-=)
  (def dn:/= dn:two-arg-/=)
  (def dn:>  dn:two-arg->)
  (def dn:>= dn:two-arg->=))


(defun normalize-arguments/bitwise (array-likes out)
  (if (every #'numberp array-likes)
      (values array-likes out)
      (let* ((common-type
               (multiple-value-bind (common-type array-likes)
                   ;; Set up an initial guess for COMMON-TYPE
                   (values (if (arrayp (first array-likes))
                               (array-element-type (first array-likes))
                               (element-type (first array-likes)))
                           (rest array-likes))
                 ;; Refine the initial guess
                 (loop :for array-like :in array-likes
                       :do (setq common-type
                                 (max-type common-type
                                           (if (arrayp array-like)
                                               (array-element-type array-like)
                                               (element-type array-like)))))
                 common-type))
             (arrays
               (loop :for array-like :on array-likes
                     :do (unless (and (arrayp (first array-like))
                                      (type= common-type (array-element-type (first array-like))))
                           (setf (first array-like)
                                 ;; TODO: Use TRIVIAL-COERCE:COERCE and simd based backends
                                 ;; FIXME: Signal a condition when the conversion is unsafe
                                 ;; TODO: Enable different upgrade-rules
                                 (asarray (first array-like) :type common-type)))
                     :finally (return array-likes))))
        (multiple-value-bind (broadcast-compatible-p dimensions)
            (if out
                (apply #'broadcast-compatible-p out arrays)
                (apply #'broadcast-compatible-p arrays))
          (if broadcast-compatible-p
              (values (apply #'broadcast-arrays arrays)
                      (if out
                          (dn:copy 1 :out out)
                          (ones dimensions :type '(unsigned-byte 8))))
              (error 'incompatible-broadcast-dimensions
                     :array-likes array-likes
                     :dimensions (mapcar #'dimensions array-likes)))))))

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
                        (apply #',(cl-name reduce-fn) (rest array-likes))))))))

  (def dn:logand dn:two-arg-logand -1)
  (def dn:logior dn:two-arg-logior 0)
  (def dn:logxor dn:two-arg-logxor 0))
