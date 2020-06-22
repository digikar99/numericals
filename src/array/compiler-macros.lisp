(in-package :numericals.array.internals)

;; TODO: Take CL-ARRAY into account below

;; (define-compiler-macro na:make-array
;;     (&whole whole dimensions
;;             &rest args
;;             &key (element-type *type* element-type-p)
;;             (initial-element nil initial-element-p)
;;             (strides nil strides-p)
;;             (displaced-index-offset 0)
;;             (adjustable nil adjustable-p)
;;             (fill-pointer nil fill-pointer-p)
;;             (initial-contents nil initial-contents-p)
;;             &environment env)
;;   ;; TODO: Write a proper compiler macro handling all the edge cases!!!
;;   (when (and initial-element-p initial-contents-p)
;;     (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))
;;   (when fill-pointer-p
;;     (error "FILL-POINTER has not been handled yet in NUMERICALS-ARRAY"))
;;   (when adjustable-p
;;     (error "FILL-POINTER has not been handled yet in NUMERICALS-ARRAY"))
;;   (if (= 3 (policy-quality 'speed env))
;;       (flet ((utocn (reason &rest reason-args)
;;                (apply #'unable-to-optimize-call-note
;;                       'na:make-array reason reason-args)))

;;         (when initial-contents-p
;;           (utocn "INITIAL-CONTENTS case has not been optimized yet")
;;           (return-from na:make-array whole))

;;         (let ((element-type-known-p t)
;;               (dimensions-known-p t)
;;               (initial-element-known-p t)
;;               (make-array-known-p t)
;;               displaced-to)
;;           ;; Handle element-type
;;           (setq element-type                
;;                 (if element-type-p
;;                     (if (constantp element-type env)
;;                         (constant-form-value element-type env)
;;                         (progn
;;                           (utocn "ELEMENT-TYPE ~D could not be determined to be a constant" element-type)
;;                           (setq element-type-known-p nil)
;;                           element-type))
;;                     (if *lookup-type-at-compile-time*
;;                         *type*
;;                         (progn
;;                           (utocn "Not using the compile-time value of *TYPE* because *LOOKUP-TYPE-AT-COMPILE-TIME* is NIL at compile-time")
;;                           (setq element-type-known-p nil)
;;                           '*type*))))
;;           ;; Handle dimensions
;;           (setq dimensions
;;                 (if (constantp dimensions env)
;;                     (constant-form-value dimensions env)
;;                     (progn
;;                       (utocn "DIMENSIONS ~D could not be determined to be a constant" dimensions)
;;                       (setq dimensions-known-p nil)
;;                       dimensions)))

;;           (when dimensions-known-p
;;             (unless (listp dimensions)
;;               (setq dimensions (list dimensions))))

;;           (setq initial-element
;;                 (if initial-element-p
;;                     (if (constantp initial-element env)
;;                         (if element-type-known-p
;;                             (coerce (constant-form-value initial-element
;;                                                          env)
;;                                     element-type)
;;                             (progn
;;                               (utocn "INITIAL-ELEMENT cannot be determined at compile-time without knowing ELEMENT-TYPE")
;;                               (setq initial-element-known-p nil)
;;                               `(coerce ,(constant-form-value initial-element
;;                                                              env)
;;                                        ;; TODO: check this
;;                                        ,element-type)))
;;                         (progn
;;                           (utocn "INITIAL-ELEMENT ~D could not be determined to be a constant"
;;                                  initial-element)
;;                           (setq initial-element-known-p nil)
;;                           `(coerce ,initial-element ,(if element-type-known-p
;;                                                          (list 'quote element-type)
;;                                                          element-type))))
;;                     (if element-type-known-p
;;                         (coerce 0 element-type)
;;                         (progn
;;                           (utocn "INITIAL-ELEMENT cannot be determined at compile-time without knowing ELEMENT-TYPE")
;;                           (setq initial-element-known-p nil)
;;                           `(coerce 0 ,(list 'quote element-type))))))

;;           ;; (when (null args) ; only dimensions is supplied
;;           ;;   (return-from na:make-array
;;           ;;     `(make-numericals-array :displaced-to ,displaced-to
;;           ;;                             :element-type ,
;;           ;;                             :dim ,dimensions
;;           ;;                             :strides ,strides
;;           ;;                             :displaced-index-offset 0
;;           ;;                             :contiguous-p t)))

;;           ;; TODO: Take strides into account!
;;           (let* ((cl-array
;;                   (cond ((and initial-element-known-p
;;                               element-type-known-p
;;                               dimensions-known-p)
;;                          (make-array dimensions
;;                                      :initial-element initial-element
;;                                      :element-type element-type))
;;                         ((null args)
;;                          (utocn "CL-ARRAY could not be allocated at compile-time without knowing all of INITIAL-ELEMENT, ELEMENT-TYPE and DIMENSIONS")
;;                          (setq make-array-known-p nil)
;;                          `(make-array ,dimensions
;;                                       :initial-element ,initial-element
;;                                       :element-type ,(if element-type-known-p
;;                                                          (list 'quote element-type)
;;                                                          element-type)))
;;                         (t
;;                          (utocn "DISPLACED-TO could not be allocated at compile-time without knowing all of INITIAL-ELEMENT, ELEMENT-TYPE and DIMENSIONS")
;;                          (setq make-array-known-p nil)
;;                          `(make-array ,dimensions
;;                                       :initial-element ,initial-element
;;                                       :element-type ,(if element-type-known-p
;;                                                          (list 'quote element-type)
;;                                                          element-type)))))
;;                  (displaced-to
;;                   (cond (make-array-known-p (1d-storage-array cl-array))
;;                         (t
;;                          (utocn "DISPLACED-TO could not be allocated at compile-time without knowing all of INITIAL-ELEMENT, ELEMENT-TYPE and DIMENSIONS")
;;                          (setq make-array-known-p nil)
;;                          `(make-array ,(if dimensions-known-p
;;                                            (apply #'* dimensions)
;;                                            `(apply #'* ,dimensions))
;;                                       :initial-element ,initial-element
;;                                       :element-type ,(if element-type-known-p
;;                                                          (list 'quote element-type)
;;                                                          element-type))                        )))
;;                  (strides (if strides-p
;;                               strides
;;                               (if dimensions-known-p
;;                                   (list 'quote
;;                                         (let ((product 1))
;;                                           (declare (optimize speed)
;;                                                    (type (signed-byte 31) product))
;;                                           (nreverse
;;                                            (loop :for d fixnum
;;                                               :in (cons 1 (reverse (cdr dimensions)))
;;                                               :do (setq product (* d product))
;;                                               :collect product))))
;;                                   `(let ((product 1))
;;                                      (declare (optimize speed)
;;                                               (type (signed-byte 31) product))
;;                                      (nreverse
;;                                       (loop :for d fixnum
;;                                          :in (cons 1 (reverse (cdr ,dimensions)))
;;                                          :do (setq product (* d product))
;;                                          :collect product)))))))
;;             `(make-numericals-array :displaced-to ,displaced-to
;;                                     :element-type ,(if element-type-known-p
;;                                                        (list 'quote element-type)
;;                                                        element-type)
;;                                     :dim ,(if dimensions-known-p
;;                                               (list 'quote dimensions)
;;                                               dimensions)
;;                                     :strides ,strides
;;                                     :displaced-index-offset ,displaced-index-offset
;;                                     :contiguous-p t
;;                                     :total-size ,(if dimensions-known-p
;;                                                      (apply #'* dimensions)
;;                                                      `(apply #'* ,dimensions))
;;                                     :cl-array cl-array))))
;;       whole))

(define-compiler-macro na:cl-aref
    (na:numericals-array &rest subscripts &environment env)
  (case (policy-quality 'safety env)
    (0 `(aref (na:array-cl-array ,na:numericals-array) ,@subscripts))
    ;; TODO: create specialized na:array to allow dispatching over the existence of CL-ARRAY
    (t `(if-let (cl-array (na:array-cl-array ,na:numericals-array))
          (aref cl-array ,@subscripts)
          (error "CL-AREF cannot be applied when CL-ARRAY slot is NIL")))))
