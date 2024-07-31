(in-package :dense-numericals/utils)

;; We can't get rid of PTR-ITERATE-BUT-INNER because each axis may have different strides :D
;; However, in cases when the array is simple, we can certainly have J-like broadcasting
;; semantics.

(defmacro with-simple-array-broadcast ((operator &rest operator-ranks)
                                       &body (&rest arrays-elt-sizes) &environment env)
  "OPERATOR should be a symbol bound to a function which takes
the following positional arguments:

  size of the first array after it is reduced to the appropriate rank,
  in succession, a pointer to an array, and dimensions of the reduced array, for each of the array
  "
  (check-type operator (or symbol
                           (cons (eql cl:setf) (cons symbol null))))
  (assert (= (length operator-ranks) (length arrays-elt-sizes))
          ()
          "Number of OPERATOR-RANKS must match number of ARRAYS")
  ;; (assert (every (lambda (r)
  ;;                  (or (null r) (non-negative-integer-p r)))
  ;;                operator-ranks)
  ;;         ()
  ;;         "Each of OPERATOR-RANKS must be NIL (indicating infinite rank) or a non-negative integer")
  (let* ((n (length arrays-elt-sizes))
         (arrays    (mapcar #'first arrays-elt-sizes))
         (elt-sizes (mapcar #'second arrays-elt-sizes))
         (array-syms      (make-gensym-list n "ARRAY"))
         (op-rank-syms    (make-gensym-list n "OP-RANK"))
         (array-rank-syms (make-gensym-list n "ARRAY-RANK"))
         (pointer-syms    (make-gensym-list n "ARRAY-PTR"))
         (stride-syms     (make-gensym-list n "ARRAY-STRIDE"))
         (inner-dim-syms  (make-gensym-list n "ARRAY-INNER-DIMS")))
    (with-gensyms (d outer-repeat-rank outer-repeat-count inner-repeat-count)
      `(let (,@(mapcar (lm s a `(,s ,a)) op-rank-syms operator-ranks)
             ,@(mapcar (lm s a `(,s ,a)) array-syms arrays))
         (declare ,@(mapcar (lambda (s a)
                              `(type ,(nth-form-type a env 0 t t) ,s))
                            array-syms arrays)
                  (ignorable ,@op-rank-syms))
         (let (,@(mapcar (lm s a `(,s (array-rank ,a)))
                         array-rank-syms array-syms))
           (declare (ignorable ,@array-rank-syms)
                    (type (integer 0 ,array-rank-limit) ,@array-rank-syms))
           ;; TODO: Rank-check the arguments
           (with-pointers-to-vectors-data
               (,@(mapcar (lm p a `(,p (array-storage ,a)))
                          pointer-syms array-syms))
             (cl:let* ((,outer-repeat-rank (if ,(first op-rank-syms)
                                                (cl:- ,(first array-rank-syms)
                                                      ,(first op-rank-syms))
                                                0))
                       (,outer-repeat-count
                         (loop :with ,outer-repeat-count :of-type size := 1
                               :for ,d :of-type (unsigned-byte 32)
                                 :in (narray-dimensions ,(first array-syms))
                               :repeat ,outer-repeat-rank
                               :do (setq ,outer-repeat-count
                                         (the-size (cl:* ,d ,outer-repeat-count)))
                               :finally (return ,outer-repeat-count)))
                       (,inner-repeat-count
                         (nth-value 0 (cl:floor (array-total-size ,(first array-syms))
                                                ,outer-repeat-count))))
               (declare (cl:type size ,outer-repeat-rank ,outer-repeat-count
                                 ,inner-repeat-count))
               (let (,@(mapcar (lm s a `(,s (if (zerop ,outer-repeat-rank)
                                                (array-total-size ,a)
                                                (array-stride ,a (1- ,outer-repeat-rank)))))
                               stride-syms array-syms)
                     ,@(mapcar (lm s a `(,s (nthcdr ,outer-repeat-rank
                                                    (narray-dimensions ,a))))
                               inner-dim-syms array-syms))
                 (declare (type (unsigned-byte 32) ,@stride-syms))
                 ;; (print (list :rank ,outer-repeat-rank
                 ;;              :outer-count ,outer-repeat-count
                 ;;              :inner-count ,inner-repeat-count
                 ;;              :strides (list ,@stride-syms)))
                 (loop :repeat ,outer-repeat-count
                       :do (,operator ,inner-repeat-count
                                      ,@(mappend (lm p i `(,p ,i))
                                                 pointer-syms inner-dim-syms))
                       ,@(mapcar (lm elt-size p s `(cffi:incf-pointer ,p
                                              (the-size (* ,elt-size ,s))))
                                 elt-sizes pointer-syms stride-syms))))))))))
