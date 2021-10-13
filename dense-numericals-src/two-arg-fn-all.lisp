(in-package :dense-numericals.impl)

;;; Currently unused; bitwise operators - logical and shift - could use it though

(define-polymorphic-function two-arg-fn/all (name x y &key out broadcast) :overwrite t)

(defpolymorph two-arg-fn/all ((name symbol) (x number) (y number)
                              &key ((out null)) broadcast)
    (values number &optional)
  (declare (ignore out broadcast)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x list) (y list)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/all name (asarray x) (asarray y) :out out :broadcast broadcast))
(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x number) (y list)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/all name x (asarray y :type (array-element-type out))
                    :out out
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x list) (y number)
     &key ((out array))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (two-arg-fn/all name (asarray x :type (array-element-type out))
                    y :out
                    out :broadcast broadcast))

(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x list) (y list)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/all name
                    (asarray x)
                    (asarray y)
                    :broadcast broadcast))
(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x number) (y list)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/all name x (asarray y) :broadcast broadcast))
(defpolymorph (two-arg-fn/all :inline t)
    ((name symbol) (x list) (y number)
     &key ((out null))
     (broadcast dn:*broadcast-automatically*))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/all name (asarray x) y))



;; 3 x 11 polymorphs
;; TODO: Are there better ways to do this in lesser number of polymorphs?

(macrolet ((def (type c-type fn-retriever size)

             
             `(progn
                
                (defpolymorph two-arg-fn/all
                    ((name symbol) (x (array ,type)) (y number)
                     &key ((out (array ,type))
                           (zeros (narray-dimensions x) :type ',type))
                     ;; The very fact that we are allowing Y to be NUMBER implies
                     ;; BROADCAST must be non-NIL
                     ((broadcast (not null)) dn:*broadcast-automatically*))
                    (array ,type)
                  (declare (ignorable name broadcast))
                  (let ((,fn-retriever (,fn-retriever name)))
                    (unless (equalp (narray-dimensions x)
                                    (narray-dimensions out))
                      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
                          (broadcast-compatible-p x out)
                        (assert broadcast-compatible-p (x out)
                                'incompatible-broadcast-dimensions
                                :dimensions (mapcar #'narray-dimensions (list x out))
                                :array-likes (list x out))
                        (setq x   (broadcast-array x   broadcast-dimensions))
                        (setq out (broadcast-array out broadcast-dimensions))))
                    (cffi:with-foreign-pointer (ptr-y ,size)
                      (setf (cffi:mem-ref ptr-y ,c-type) (trivial-coerce:coerce y ',type))
                      (with-thresholded-multithreading (array-total-size out)
                          (x out)
                        (ptr-iterate-but-inner n ((ptr-x ,size ix x)
                                                  (ptr-o ,size io out))
                          (funcall ,fn-retriever
                                   n
                                   ptr-x ix
                                   ptr-y 0
                                   ptr-o io)))))
                  out)

                (defpolymorph two-arg-fn/all
                    ((name symbol) (x number) (y (array ,type)) 
                     &key ((out (array ,type))
                           (zeros (narray-dimensions y) :type ',type))
                     ;; The very fact that we are allowing X to be NUMBER implies
                     ;; BROADCAST must be non-NIL
                     ((broadcast (not null)) dn:*broadcast-automatically*))
                    (array ,type)
                  (declare (ignorable name broadcast))
                  (let ((,fn-retriever (,fn-retriever name)))
                    (unless (equalp (narray-dimensions y)
                                    (narray-dimensions out))
                      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
                          (broadcast-compatible-p y out)
                        (assert broadcast-compatible-p (y out)
                                'incompatible-broadcast-dimensions
                                :dimensions (mapcar #'narray-dimensions (list y out))
                                :array-likes (list y out))
                        (setq y   (broadcast-array y   broadcast-dimensions))
                        (setq out (broadcast-array out broadcast-dimensions))))
                    (cffi:with-foreign-pointer (ptr-x ,size)
                      (setf (cffi:mem-ref ptr-x ,c-type) (trivial-coerce:coerce X ',type))
                      (with-thresholded-multithreading (array-total-size out)
                          (y out)
                        (ptr-iterate-but-inner n ((ptr-y ,size iy y)
                                                  (ptr-o ,size io out))
                          (funcall ,fn-retriever
                                   n
                                   ptr-x 0
                                   ptr-y iy
                                   ptr-o io)))))
                  out)

                (defpolymorph two-arg-fn/all
                    ((name symbol) (x (array ,type)) (y (array ,type))
                     ;; We are not producing OUT in the lambda-list because
                     ;; it is non-trivial to work out the dimensions in a short space.
                     ;; This work has been done just below.
                     &key ((out (or null (array ,type))))
                     (broadcast dn:*broadcast-automatically*))
                    (array ,type)
                  ;; We are not broadcasting OUT because doing so would mean
                  ;; OUT would be written multiple times leading to all sorts of bad things
                  (declare (ignorable name))
                  (if (or (not broadcast)
                          (equalp (narray-dimensions x)
                                  (narray-dimensions y)))
                      (setq out (or out (zeros (narray-dimensions x) :type ',type)))
                      (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
                          (broadcast-compatible-p x y out)
                        (assert broadcast-compatible-p (x y out)
                                'incompatible-broadcast-dimensions
                                :dimensions (mapcar #'narray-dimensions (list x y out))
                                :array-likes (list x y out))
                        (setq x (broadcast-array x broadcast-dimensions))
                        (setq y (broadcast-array y broadcast-dimensions))
                        (setq out (or out (zeros broadcast-dimensions :type ',type)))))
                  (policy-cond:with-expectations (= safety 0)
                      ((assertion (or broadcast
                                      (equalp (narray-dimensions x)
                                              (narray-dimensions y))))
                       (assertion (equalp (narray-dimensions x)
                                          (narray-dimensions out))))
                    (let ((,fn-retriever (,fn-retriever name)))
                      (with-thresholded-multithreading (array-total-size (the array out))
                          (x y out)
                        (ptr-iterate-but-inner n ((ptr-x ,size ix x)
                                                  (ptr-y ,size iy y)
                                                  (ptr-o ,size io out))
                          (funcall ,fn-retriever
                                   n
                                   ptr-x ix
                                   ptr-y iy
                                   ptr-o io)))))
                  out))))

  (def (signed-byte 64) :long  int64-c-name 8)
  (def (signed-byte 32) :int   int32-c-name 4)
  (def (signed-byte 16) :short int16-c-name 2)
  (def (signed-byte 08) :char  int8-c-name  1)

  (def (unsigned-byte 64) :unsigned-long  uint64-c-name 8)
  (def (unsigned-byte 32) :unsigned-int   uint32-c-name 4)
  (def (unsigned-byte 16) :unsigned-short uint16-c-name 2)
  (def (unsigned-byte 08) :unsigned-char  uint8-c-name  1)

  (def fixnum       :long   fixnum-c-name       8)
  (def single-float :float  single-float-c-name 4)
  (def double-float :double double-float-c-name 8))
