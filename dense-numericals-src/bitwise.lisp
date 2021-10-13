(in-package :dense-numericals.impl)

(define-polymorphic-function two-arg-fn/bitwise (name x y &key out) :overwrite t)

(defpolymorph two-arg-fn/bitwise ((name symbol)
                          (x number) (y number) &key ((out null) nil))
    (values number &optional)
  (declare (ignore out)
           (ignorable name))
  (let ((cl-name (cl-name name)))
    (funcall cl-name x y)))

;; list - 3x2 polymorphs: we do need the two variants
;;   because below, the OUT is initialized in the lambda-list itself
(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x list) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn/bitwise name
                      (asarray x :type '(unsigned-byte 8))
                      (asarray y :type '(unsigned-byte 8))
                      :out out))

(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x number) (y list) &key ((out array)))
    (values array &optional)
  (two-arg-fn/bitwise name x (asarray y :type '(unsigned-byte 8)) :out out))

(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x list) (y number) &key ((out array)))
    (values array &optional)
  (two-arg-fn/bitwise name (asarray x :type '(unsigned-byte 8)) y :out out))


(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x list) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/bitwise name
                      (asarray x :type '(unsigned-byte 8))
                      (asarray y :type '(unsigned-byte 8))))

(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x number) (y list) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/bitwise name x
                      (asarray y :type '(unsigned-byte 8))))

(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x list) (y number) &key ((out null)))
    (values array &optional)
  (declare (ignore out))
  (two-arg-fn/bitwise name
                      (asarray x :type '(unsigned-byte 8)) y))



;; (unsigned-byte 8) - 4 polymorphs

(defpolymorph two-arg-fn/bitwise
    ((name symbol) (x (array (unsigned-byte 8))) (y number)
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions x) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (with-thresholded-multithreading (array-total-size x)
      (x out)
    (cffi:with-foreign-pointer (ptr-y 1)
      (setf (cffi:mem-ref ptr-y :char) (coerce y '(unsigned-byte 8)))
      (let ((int8-c-name (int8-c-name name)))
        (ptr-iterate-but-inner n ((ptr-x 1 ix x)
                                  (ptr-o 1 io out))
          (funcall int8-c-name
                   n
                   ptr-x ix
                   ptr-y 0
                   ptr-o io)))))
  out)

(defpolymorph (two-arg-fn/bitwise :inline t)
    ((name symbol) (x number) (y (array (unsigned-byte 8)))
     &key ((out (array (unsigned-byte 8)))
           (zeros (narray-dimensions y) :type '(unsigned-byte 8))))
    (array (unsigned-byte 8))
  (two-arg-fn/bitwise name y x :out out)
  out)


(defpolymorph two-arg-fn/bitwise
    ((name symbol) (x (array (unsigned-byte 8))) (y (array (unsigned-byte 8)))
     &key ((out (or null (array (unsigned-byte 8))))))
    (array (unsigned-byte 8))
  (declare (ignorable name))
  (unless (and out
               (equalp (narray-dimensions x)
                       (narray-dimensions y)))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (broadcast-compatible-p x y)
      (assert broadcast-compatible-p (x y)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x y))
              :array-likes (list x y))
      (setq x (broadcast-array x broadcast-dimensions))
      (setq y (broadcast-array y broadcast-dimensions))
      (setq out (or out (zeros broadcast-dimensions :type '(unsigned-byte 8))))))
  (let ((int8-c-name (int8-c-name name)))
    (with-thresholded-multithreading (array-total-size out)
        (x y out)
      (ptr-iterate-but-inner n ((ptr-x 1 ix x)
                                (ptr-y 1 iy y)
                                (ptr-o 1 io out))
        (funcall int8-c-name
                 n
                 ptr-x ix
                 ptr-y iy
                 ptr-o io))))
  out)

(defpolymorph two-arg-fn/bitwise
    ((name symbol) (x (simple-array (unsigned-byte 8))) (y (simple-array (unsigned-byte 8)))
     &key ((out (or null (simple-array (unsigned-byte 8))))))
    (simple-array (unsigned-byte 8))
  (declare (ignorable name))
  (let ((int8-c-name (int8-c-name name)))
    (if (and out
             (equalp (narray-dimensions x)
                     (narray-dimensions y))
             (equalp (narray-dimensions out)
                     (narray-dimensions y)))
        (let ((x x)
              (y y)
              (out out))
          (declare (type (array (unsigned-byte 8))))
          (with-thresholded-multithreading (array-total-size out)
              (x y out)
            (with-pointers-to-vectors-data ((ptr-x (array-storage x))
                                            (ptr-y (array-storage y))
                                            (ptr-o (array-storage out)))
              (cffi:incf-pointer ptr-x (array-total-offset x))
              (cffi:incf-pointer ptr-y (array-total-offset y))
              (cffi:incf-pointer ptr-o (array-total-offset out))
              (funcall int8-c-name
                       (array-total-size (the array out))
                       ptr-x 1
                       ptr-y 1
                       ptr-o 1))))
        (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
            (broadcast-compatible-p x y)
          (assert broadcast-compatible-p (x y)
                  'incompatible-broadcast-dimensions
                  :dimensions (mapcar #'narray-dimensions (list x y))
                  :array-likes (list x y))
          (let ((x (broadcast-array x broadcast-dimensions))
                (y (broadcast-array y broadcast-dimensions))
                (out (or out (zeros broadcast-dimensions :type '(unsigned-byte 8)))))
            (declare (type (array (unsigned-byte 8)) x y out))
            (with-thresholded-multithreading (array-total-size (the array out))
                (x y out)
              (ptr-iterate-but-inner n ((ptr-x 1 ix x)
                                        (ptr-y 1 iy y)
                                        (ptr-o 1 io out))
                (funcall int8-c-name
                         n
                         ptr-x ix
                         ptr-y iy
                         ptr-o io))))))
    out))


(macrolet ((def (name)
             `(progn
                (define-polymorphic-function ,name (x y &key out) :overwrite t)
                (defpolymorph ,name (x y &key ((out null))) t
                  (declare (ignore out))
                  (two-arg-fn/bitwise ',name x y))
                (defpolymorph ,name (x y &key ((out (not null)))) t
                  (two-arg-fn/bitwise ',name x y :out out)))))

  (def dn:two-arg-logand)
  (def dn:two-arg-logior)
  (def dn:two-arg-logxor)

  (def dn:logandc1))

(defpolymorph dn:logandc2 (x y &key out) t (dn:logandc2 y x :out out))

(defpolymorph dn:lognot ((x number) &key ((out null))) (values number &optional)
  (declare (ignore out))
  (trivial-coerce:coerce (the integer (funcall (cl-name 'dn:lognot) x))
                         '(unsigned-byte 8)))
(defpolymorph (dn:lognot :inline t) ((x list) &key ((out (array (unsigned-byte 8)))))
    (array (unsigned-byte 8))
  (dn:lognot (asarray x :type '(unsigned-byte 8)) :out out)
  out)
(defpolymorph (dn:lognot :inline t) ((x list) &key ((out null)))
    (array (unsigned-byte 8))
  (declare (ignore out))
  (let ((array (asarray x :type '(unsigned-byte 8))))
    (dn:lognot array :out array)
    array))
(defpolymorph dn:lognot ((x (array (unsigned-byte 8))) &key ((out (array (unsigned-byte 8)))
                                                             (zeros-like x)))
    (array (unsigned-byte 8))
  (unless (equalp (narray-dimensions x)
                  (narray-dimensions out))
    (multiple-value-bind (broadcast-compatible-p broadcast-dimensions)
        (%broadcast-compatible-p (narray-dimensions x)
                                 (narray-dimensions out))
      (assert broadcast-compatible-p (x out)
              'incompatible-broadcast-dimensions
              :dimensions (mapcar #'narray-dimensions (list x out))
              :array-likes (list x out))
      (setq x (broadcast-array x broadcast-dimensions))))
  (with-thresholded-multithreading (array-total-size out)
      (x out)
    (ptr-iterate-but-inner n ((ptr-x   1 ix   x)
                              (ptr-out 1 iout out))
      (funcall (int8-c-name 'dn:lognot) n ptr-x ix ptr-out iout)))
  out)

(defpolymorph dn:lognand (x y &key out) t
  (let ((and-out (dn:two-arg-logand y x :out out)))
    (if (arrayp and-out)
        (dn:lognot and-out :out (the (array (unsigned-byte 8)) and-out))
        (dn:lognot (the number and-out)))))

(defpolymorph dn:lognor (x y &key out) t
  (let ((or-out (dn:two-arg-logior y x :out out)))
    (if (arrayp or-out)
        (dn:lognot or-out :out (the (array (unsigned-byte 8)) or-out))
        (dn:lognot (the number or-out)))))

(defpolymorph dn:logorc1 (x y &key out) t
  (let ((c-out (dn:lognot x :out out)))
    (if (arrayp c-out)
        (dn:two-arg-logior (the (array (unsigned-byte 8)) c-out) y :out out)
        (dn:two-arg-logior (the number c-out) y))))

(defpolymorph dn:logorc1 (x y &key out) t
  (let ((c-out (dn:lognot y :out out)))
    (if (arrayp c-out)
        (dn:two-arg-logior (the (array (unsigned-byte 8)) c-out) x :out out)
        (dn:two-arg-logior (the number c-out) x))))

