(cl:in-package #.numericals.helper:*numericals-internals-package*)
;;; The value is set in package / package+array file.

;; Examples for final codes generated from the broadcast are given near the bottom
;; of this file in the form of comments.
(defmacro with-broadcast (type num-dimensions stride-symbols
                          broadcast-fn-name array
                          (&rest required-dimensions) &body body)
  (let* ((vector (gensym))
         (offset (gensym))
         (broadcasted-array (gensym))
         (index-symbols (symbols "I" num-dimensions))
         (index-code `(the fixnum (+ ,offset ,@index-symbols)))
         (reversed-stride-symbols (reverse stride-symbols))
         (simd-aref (ecase type
                      (single-float 'simd-single-1d-aref)
                      (double-float 'simd-double-1d-aref)))
         (simd-broadcast-aref (ecase type
                                (single-float 'simd-single-broadcast-1d-aref)
                                (double-float 'simd-double-broadcast-1d-aref)))
         (aref 'cl:aref)
         (broadcast-fn-name-simd (intern (concatenate 'string
                                                      (symbol-name broadcast-fn-name)
                                                      "-SIMD"))))
    (once-only (array required-dimensions)
      `(let (,vector
             ,offset
             ,@(loop for s in stride-symbols collect `(,s 0))
             (,broadcasted-array (if (equalp ,required-dimensions (array-dimensions ,array))
                                     ,array
                                     (broadcast-array ,array ,required-dimensions))))
         (declare (type (or null (simple-array ,type)) ,vector)
                  (type (or null (signed-byte 31)) ,offset)
                  (type (signed-byte 31) ,@stride-symbols)
                  (optimize (speed 3)))
         ;; (setq ,vector (1d-storage-array ,array))
         (multiple-value-setq (,vector ,offset) (1d-storage-array ,array))
         ;; https://ipython-books.github.io/46-using-stride-tricks-with-numpy/
         (dsetq ,stride-symbols (array-strides ,broadcasted-array))
         ;; The most obvious way to calculate the "true" index, as stated 
         ;; in the link few lines above, is to take the "dot" product of
         ;; index-symbols with stride-symbols.
         ;; However, this is expensive. Instead, the multiplying step is offloaded
         ;; to the loop variables in define-nd-broadcast-operation below. Only addition
         ;; is performed by index-code .
         (flet ((,broadcast-fn-name (,@index-symbols)
                  (declare (optimize (speed 3))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (,aref ,vector ,index-code))
                ((setf ,broadcast-fn-name) (new-value ,@index-symbols)
                  (declare (optimize (speed 3))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (setf (,aref ,vector ,index-code) new-value))
                ((setf ,broadcast-fn-name-simd) (new-value ,@index-symbols)
                  (declare (optimize (speed 3) (safety 0))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (setf (,simd-aref ,vector ,index-code) new-value))
                (,broadcast-fn-name-simd (,@index-symbols)
                  (declare (optimize (speed 3) (safety 0))
                           (type (signed-byte 31) ,@index-symbols ,@reversed-stride-symbols))
                  (if (zerop ,(car reversed-stride-symbols))
                      ;; Doing this is more performant than keeping separate branches
                      (,simd-broadcast-aref ,vector ,index-code)
                      (,simd-aref ,vector ,index-code))))
           (declare (inline ,broadcast-fn-name (setf ,broadcast-fn-name)
                            (setf ,broadcast-fn-name-simd)
                            ,broadcast-fn-name-simd)
                    (ignorable (function ,broadcast-fn-name)
                               (function ,broadcast-fn-name-simd)
                               (function (setf ,broadcast-fn-name))
                               (function (setf ,broadcast-fn-name-simd)))
                    (optimize (speed 3) (safety 0))
                    (type (signed-byte 31) ,@reversed-stride-symbols))
           ,@body)))))
