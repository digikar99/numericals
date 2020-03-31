(in-package :sbcl-numericals.internals)

;;; See https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c
;;; for a tutorial on what this macro is up to.

;;; We want to abstract just "the right amount" - we don't want to
;;; abstract too much to introduce layers and layers of complexity;
;;; but enough to avert code repetition.
(defmacro define-binary-vectorized-op (name base-op element-type mode
                                       (dest &rest args)
                                       &body assembly-instructions)
  (with-gensyms (vop-name
                 loop-var
                 loop-final-var)
    (destructuring-bind (register-type
                         simd-pack-type
                         vop-arg-type
                         vop-wrapper-name
                         array-type
                         loop-step-size
                         vector-accessor)
        (eswitch ((list element-type mode) :test 'equalp)
          ('(:double :avx2) (list 'sb-vm::double-avx2-reg
                                  '(simd-pack-256 double-float)
                                  'sb-vm::simd-pack-256-double
                                  (intern (concatenate 'string "D4"
                                                       (symbol-name base-op))
                                          :sbcl-numericals.internals)
                                  '(simple-array double-float)
                                  4
                                  'd4-ref))
          ('(:single :avx2) (list 'sb-vm::single-avx2-reg
                                  '(simd-pack-256 single-float)
                                  'sb-vm::simd-pack-256-single
                                  (intern (concatenate 'string "S8"
                                                       (symbol-name base-op))
                                          :sbcl-numericals.internals)
                                  '(simple-array single-float)
                                  8
                                  's8-ref))
          ('(:double :sse) (list 'sb-vm::double-sse-reg
                                 '(simd-pack double-float)
                                 'sb-vm::simd-pack-double
                                 (intern (concatenate 'string "D2"
                                                      (symbol-name base-op))
                                         :sbcl-numericals.internals)
                                 '(simple-array double-float)
                                 2
                                 'd2-ref))
          ('(:single :sse) (list 'sb-vm::single-sse-reg
                                 '(simd-pack single-float)
                                 'sb-vm::simd-pack-single
                                 (intern (concatenate 'string "S4"
                                                      (symbol-name base-op))
                                         :sbcl-numericals.internals)
                                 '(simple-array single-float)
                                 4
                                 's4-ref)))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defknown (,vop-name)
               ,(make-list (length args)
                           :initial-element simd-pack-type)
               ,simd-pack-type
               (movable flushable always-translatable)
             :overwrite-fndb-silently t)
           (define-vop (,vop-name)
             (:translate ,vop-name)
             (:policy :fast-safe)
             (:args ,@(loop for arg-name in args
                         collect `(,arg-name :scs (,register-type))))
             (:arg-types ,@(make-list (length args)
                                      :initial-element vop-arg-type))
             (:results (,dest :scs (,register-type)))
             (:result-types ,vop-arg-type)
             (:generator 1 ;; what should be the cost?
                         ,@assembly-instructions)))
         (declaim (inline ,vop-wrapper-name))
         (defun ,vop-wrapper-name ,args
           (declare (optimize (speed 3)))
           (,vop-name ,@args))
         (defun ,name (,@args
                       ,dest)
           (declare (optimize (speed 3))
                    (type ,array-type ,@args ,dest))
           (if (not (and ,@(loop for arg in args
                              collect `(equalp (array-dimensions ,dest)
                                               (array-dimensions ,arg)))))
               (error "Arrays cannot have different dimensions!"))
           (let ((,dest (array-storage-vector ,dest))
                 ,@(loop for arg in args
                      collect `(,arg (array-storage-vector ,arg))))
             (loop for ,loop-var fixnum
                below (- (length ,dest) ,loop-step-size)
                by ,loop-step-size
                do (setf (,vector-accessor ,dest ,loop-var)
                         (,vop-wrapper-name ,@(loop for arg in args
                                                 collect `(,vector-accessor ,arg ,loop-var))))
                finally
                  (loop for ,loop-final-var from ,loop-var below (length ,dest)
                     do (setf (aref ,dest ,loop-final-var)
                              (,base-op ,@(loop for arg in args
                                             collect `(aref ,arg ,loop-final-var)))))))
           ,dest)))))
