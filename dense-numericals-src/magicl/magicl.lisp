(in-package :dense-numericals/magicl/impl)

(defmacro define-simple-magicl-wrapper (name lambda-list array-parameters)
  `(define-magicl-wrapper ,name ,lambda-list
     :array-parameters ,array-parameters
     :returns-magicl-array t))

(macrolet ((def (name)
             `(define-magicl-wrapper ,name
                  (source1 source2 &optional (target nil targetp))
                :array-parameters (source1 source2 target)
                :returns-magicl-array t)))
  (def .+)
  (def .*)
  (def .-)
  (def ./)
  (def .^))

(defun nu/magicl:@ (matrix &rest matrices)
  (from-magicl-tensor
   (apply #'magicl:@
          (to-magicl-tensor matrix)
          (mapcar #'to-magicl-tensor matrices))))

(define-magicl-wrapper arange (range &key (type default-element-type))
  :returns-magicl-array t)

(define-magicl-wrapper binary-operator (function source1 source2 &optional target)
  :array-parameters (source1 source2 target)
  :returns-magicl-array t)

(define-simple-magicl-wrapper cast (tensor class) (tensor))
(define-simple-magicl-wrapper column (matrix index) (matrix))
(define-simple-magicl-wrapper conjugate-transpose  (matrix) (matrix))
(define-simple-magicl-wrapper conjugate-transpose! (matrix) (matrix))
(define-simple-magicl-wrapper const (const shape &key type layout) ())
(define-simple-magicl-wrapper copy-tensor (tensor &rest args) (tensor))

(define-magicl-wrapper csd (matrix p q)
  :array-parameters (matrix)
  :returns-magicl-array (t t t))

(define-simple-magicl-wrapper dagger  (matrix) (matrix))
(define-simple-magicl-wrapper dagger! (matrix) (matrix))
(define-simple-magicl-wrapper deep-copy-tensor (tensor &rest args) (tensor))

(define-magicl-wrapper det (matrix)
  :array-parameters (matrix)
  :returns-magicl-array nil)

(define-magicl-wrapper diag (matrix)
  :array-parameters (matrix)
  :returns-magicl-array nil)

(define-magicl-wrapper direct-sum (a b)
  :array-parameters (a b)
  :returns-magicl-array t)

(define-magicl-wrapper dot (vector1 vector2)
  :array-parameters (vector1 vector2)
  :returns-magicl-array nil)

(define-magicl-wrapper eig (matrix)
  :array-parameters (matrix)
  :returns-magicl-array (nil t))

(define-magicl-wrapper element-type (tensor)
  :array-parameters (tensor)
  :returns-magicl-array nil)

(define-simple-magicl-wrapper empty
    (shape &key (type magicl::*default-tensor-type*) layout)
  ())

(defun nu/magicl:every (predicate tensor &rest more-tensors)
  (apply #'magicl:every predicate (to-magicl-tensor tensor)
         (mapcar #'to-magicl-tensor more-tensors)))

(define-simple-magicl-wrapper expm (matrix) (matrix))

(define-simple-magicl-wrapper eye
    (shape &key value (offset 0) (type magicl::*default-tensor-type*) layout)
  ())

(define-simple-magicl-wrapper from-array
    (array shape &key (type (cl:array-element-type array))
           (layout :row-major) (input-layout :row-major))
  ())

(define-simple-magicl-wrapper from-diag
    (list &key (order 2) (offset 0) type layout)
  ())

(define-simple-magicl-wrapper from-list
    (list shape &key type layout (input-layout :row-major))
  ())

(define-simple-magicl-wrapper generalize-tensor (tensor) (tensor))
(define-simple-magicl-wrapper hermitian-eig (matrix) (matrix))

(define-magicl-wrapper hermitian-matrix-p
    (matrix &optional (epsilon magicl::*double-comparison-threshold*))
  :array-parameters (matrix)
  :returns-magicl-array nil)

(define-magicl-wrapper identity-matrix-p
    (matrix &optional (epsilon magicl::*double-comparison-threshold*))
  :array-parameters (matrix)
  :returns-magicl-array nil)

(define-simple-magicl-wrapper kron (a b &rest rest) (a b))
(define-simple-magicl-wrapper layout (a) (a))

(define-magicl-wrapper lisp-array (tensor &optional target)
  :array-parameters (tensor)
  :returns-magicl-array nil)

(define-simple-magicl-wrapper logm (matrix) (matrix))
(define-simple-magicl-wrapper lower-triangular (matrix &key (square nil)) (matrix))
(define-simple-magicl-wrapper lq (matrix) (matrix))
(define-simple-magicl-wrapper lu (matrix) (matrix))

(define-simple-magicl-wrapper make-tensor
    (class shape &key initial-element layout storage)
  ())
(define-simple-magicl-wrapper map (function tensor) (tensor))
(define-simple-magicl-wrapper mult
    (a b &key target alpha beta transa transb)
  (a b target))

(define-magicl-wrapper ncols (m)
  :array-parameters (m)
  :returns-magicl-array nil)

(define-magicl-wrapper norm (vector &optional (p 2))
  :array-parameters (vector)
  :returns-magicl-array nil)

(defun nu/magicl:notany (predicate tensor &rest more-tensors)
  (apply #'magicl:notany predicate (to-magicl-tensor tensor)
         (mapcar #'to-magicl-tensor more-tensors)))

(defun nu/magicl:notevery (predicate tensor &rest more-tensors)
  (apply #'magicl:notevery predicate (to-magicl-tensor tensor)
         (mapcar #'to-magicl-tensor more-tensors)))

(define-magicl-wrapper nrows (m) :array-parameters (m))
(define-simple-magicl-wrapper ones
    (shape &key (type magicl::*default-tensor-type*) layout)
  ())
(define-magicl-wrapper order (tensor) :array-parameters (tensor))

(define-magicl-wrapper ql (matrix)
  :array-parameters (matrix)
  :returns-magicl-array (t t))
(define-magicl-wrapper qr (matrix)
  :array-parameters (matrix)
  :returns-magicl-array (t t))

(define-simple-magicl-wrapper rand
    (shape &key (type magicl::*default-tensor-type*) layout distribution)
  ())
(define-simple-magicl-wrapper random-unitary
    (n &key (type `(complex ,magicl::*default-tensor-type*)))
  ())
(define-simple-magicl-wrapper reshape (tensor shape) (tensor))
(define-simple-magicl-wrapper row (matrix index) (matrix))

(define-magicl-wrapper rq (matrix)
  :array-parameters (matrix)
  :returns-magicl-array (t t))
(define-simple-magicl-wrapper scale  (tensor factor) (tensor))
(define-simple-magicl-wrapper scale! (tensor factor) (tensor))

(define-magicl-wrapper shape (tensor) :array-parameters (tensor))
(define-magicl-wrapper size  (tensor) :array-parameters (tensor))
(define-simple-magicl-wrapper slice    (tensor from to) (tensor))

(defun nu/magicl:some (predicate tensor &rest more-tensors)
  (apply #'magicl:some predicate (to-magicl-tensor tensor)
         (mapcar #'to-magicl-tensor more-tensors)))

(define-simple-magicl-wrapper specialize-tensor (tensor) (tensor))
(define-magicl-wrapper square-matrix-p (matrix) :array-parameters (matrix))
(define-magicl-wrapper svd (matrix &key reduced)
  :array-parameters (matrix)
  :returns-magicl-array (t t t))
(define-magicl-wrapper trace (matrix) :array-parameters (matrix))
(define-simple-magicl-wrapper transpose (matrix) (matrix))
(define-magicl-wrapper tref (tensor &rest pos) :array-parameters (tensor))
(define-simple-magicl-wrapper tril (matrix &key square) (matrix))
(define-simple-magicl-wrapper triu (matrix &key square) (matrix))

(define-magicl-wrapper unitary-matrix-p
    (matrix &optional (epsilon magicl::*double-comparison-threshold*))
  :array-parameters (matrix))

(define-simple-magicl-wrapper upper-triangular (matrix &key square) (matrix))

(define-simple-magicl-wrapper zeros
    (shape &key (type magicl::*default-tensor-type*) layout)
  ())
