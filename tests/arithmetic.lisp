(in-package :numericals/tests)

;;; With allocation
;;; Without allocation
;;; Sizes 100 10000 1000000 100000000
;;; Number of operations 1e10

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "
def arithmetic_timeit(fn, a_sizes, b_sizes, c_sizes, num_operations):
  import time
  import numpy as np
  
  timings = []

  for i in range(len(num_operations)):
    a = np.ones(a_sizes[i]).astype('float32')
    b = np.ones(b_sizes[i]).astype('float32')
    c = np.ones(c_sizes[i]).astype('float32')
    num_operation = int(num_operations[i] / (np.product(c_sizes[i])))
    start = time.time()
    for i in range(num_operation):
      fn(a, b, out = c)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(defpyfun "arithmetic_timeit" nil :lisp-fun-name "PY-ARITHMETIC-TIMEIT")

;; TODO: Add speed tests for unary operators


;; This is better suited as a function because we don't want to care about the
;; "form" of the arguments - we want to play with the values of the arguments.
(defun lisp-arithmetic-timeit (&key fn a-sizes b-sizes c-sizes num-operations)
  (let ((nu:*type* 'single-float))
    (eval
     `(let ((fn ',fn))
        (loop :for i :below (length ',num-operations)
           :for a-size := (elt ',a-sizes i)
           :for b-size := (elt ',b-sizes i)
           :for c-size := (elt ',c-sizes i)
           :collect
             (eval
              `(let (start
                     end
                     (a (nu:ones ',a-size))
                     (b (nu:ones ',b-size))
                     (c (nu:ones ',c-size))
                     (num-operation (floor ,(/ (elt ',num-operations i)
                                               (apply #'* c-size)))))
                 (declare (optimize (speed 3))
                          (type (simple-array single-float ,a-size) a)
                          (type (simple-array single-float ,b-size) b)
                          (type (simple-array single-float ,c-size) c))
                 (setq start (get-internal-real-time))
                 (loop :for i fixnum :below num-operation
                    :do (,fn a b :out c))
                 (setq end (get-internal-real-time))
                 (coerce (/ (- end start)
                            internal-time-units-per-second)
                         'single-float))))))
    ;; (loop :for i :below (length num-operations)
    ;;    :for a = (nu:ones (elt a-sizes i))
    ;;    :for b = (nu:ones (elt b-sizes i))
    ;;    :for c = (nu:ones (elt c-sizes i))
    ;;    :for num-operation := (floor (/ (elt num-operations i)
    ;;                                    (apply #'* (elt c-sizes i))))
    ;;    :do (eval `(let (start end)
    ;;                 (declare (optimize (speed 3))
    ;;                          (type (simple-array single-float ,(elt a-sizes i)) a)
    ;;                          (type (simple-array single-float ,(elt b-sizes i)) b)
    ;;                          (type (simple-array single-float ,(elt c-sizes i)) c))
    ;;                 (setq start (get-internal-real-time))
    ;;                 (loop :for i fixnum :below num-operation
    ;;                    :do (,fn a b :out c))
    ;;                 (setq end (get-internal-real-time))))
    ;;    :collect (coerce (/ (- end start)
    ;;                        internal-time-units-per-second)
    ;;                     'single-float))
    ))

(def-test non-broadcast-speed (:suite speed)
  (flet ((within-acceptable-limits (lisp-time numpy-time)
           (<= (/ lisp-time numpy-time) 1.35)))
    (let* ((a-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
           (b-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
           (c-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
           (num-operations '(1e7 1e8 1e8 1e9 1e9))
           (numpy-operations '(np.add np.subtract np.multiply np.divide))
           (numericals-operations '(nu:+ nu:- nu:* nu:/)))
      (when *write-to-readme*
        (who:with-html-output (*write-to-readme-stream* nil :indent t)
          (:tr (:th "Non-broadcast array operations")
               (loop :for size :in (mapcar (lambda (l) (apply #'* l)) c-sizes)
                  :do (who:htm (:th (who:str size)))))))
      (loop :for numpy-operation :in numpy-operations
         :for numericals-operation :in numericals-operations
         :do (let ((numpy-timings (py-arithmetic-timeit :fn numpy-operation
                                                        :a-sizes a-sizes
                                                        :b-sizes b-sizes
                                                        :c-sizes c-sizes
                                                        :num-operations num-operations))
                   (numericals-timings (lisp-arithmetic-timeit :fn numericals-operation
                                                               :a-sizes a-sizes
                                                               :b-sizes b-sizes
                                                               :c-sizes c-sizes
                                                               :num-operations num-operations)))
               (when *write-to-readme*
                 (who:with-html-output (*write-to-readme-stream* nil :indent t)
                   (:tr (:td (who:fmt "~D" numericals-operation))
                        (loop :for lisp-time :in numericals-timings
                           :for numpy-time :in numpy-timings
                           :do (who:htm (:td (who:fmt "~,2f" (/ numpy-time lisp-time)) "x"))))))
               (is (every #'within-acceptable-limits
                          numericals-timings numpy-timings)))))))

(def-test non-broadcast-correctness (:suite correctness)
  (let ((dim (iota 8 :start 8))
        (binary-numericals-op '(nu:+ nu:- nu:* nu:/))
        (binary-numpy-op '(np.add np.subtract np.multiply np.divide))
        (unary-numericals-op '(nu:sqrt))
        (unary-numpy-op '(np.sqrt)))
    (iter (for np-op in binary-numpy-op)
          (for nu-op in binary-numericals-op)
          (iter (for d in dim)
                (for a = (pycall 'np.random.random (list 3 d)))
                (for b = (pycall 'np.random.random (list 3 d)))
                (is (np:allclose :a (pycall np-op a b)
                                 :b (funcall nu-op a b)))))
    (iter (for np-op in unary-numpy-op)
          (for nu-op in unary-numericals-op)
          (iter (for d in dim)
                (for a = (pycall 'np.random.random (list 3 d)))
                (is (np:allclose :a (pycall np-op a)
                                 :b (funcall nu-op a)))))))

(def-test broadcast-correctness (:suite correctness)
  (iter (for np-op in '(np.add np.subtract np.multiply np.divide))
        (for nu-op in '(nu:+ nu:- nu:* nu:/))
        (iter (for a-size in '(   (1) (01 1) (01 10) (01 01 01) (10 01 10) (10 01 10 01)))
              (for b-size in '((1 10) (10 1) (10 01) (10 10 10) (10 10 01) (01 10 01 10)))
              (for c-size in '((1 10) (10 1) (10 10) (10 10 10) (10 10 10) (10 10 10 10)))
              (for a = (nu:asarray (pycall 'np.random.random a-size)))
              (for b = (nu:asarray (pycall 'np.random.random b-size)))
              (for c = (nu:asarray (pycall 'np.zeros c-size)))
              (is (np:allclose :a (pycall np-op a b)
                               :b (funcall nu-op a b :out c)
                               :atol 1e-7)))))

(def-test broadcast-speed (:suite speed)
  (flet ((within-acceptable-limits (lisp-time numpy-time)
           (<= (/ lisp-time numpy-time) 2)))
    (let* ((a-sizes '((01 1) (01 10) (001 100) (0001 1000) (00001 10000)))
           (b-sizes '((10 1) (10 01) (100 001) (1000 0001) (10000 00001)))
           (c-sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
           (num-operations '(1e7 1e8 1e8 1e9 1e9))
           (numpy-operations '(np.add np.subtract np.multiply np.divide))
           (numericals-operations '(nu:+ nu:- nu:* nu:/)))
      (when *write-to-readme*
        (who:with-html-output (*write-to-readme-stream* nil :indent t)
          (:tr (:th "Broadcast array operations (warning: can vary quite a bit depending
on actual array dimensions)")
               (loop :for size :in (mapcar (lambda (l) (apply #'* l)) c-sizes)
                  :do (who:htm (:th (who:str size)))))))
      (loop :for numpy-operation :in numpy-operations
         :for numericals-operation :in numericals-operations
         :do (let ((numpy-timings (py-arithmetic-timeit :fn numpy-operation
                                                        :a-sizes a-sizes
                                                        :b-sizes b-sizes
                                                        :c-sizes c-sizes
                                                        :num-operations num-operations))
                   (numericals-timings (lisp-arithmetic-timeit :fn numericals-operation
                                                               :a-sizes a-sizes
                                                               :b-sizes b-sizes
                                                               :c-sizes c-sizes
                                                               :num-operations num-operations)))
               (when *write-to-readme*
                 (who:with-html-output (*write-to-readme-stream* nil :indent t)
                   (:tr (:td (who:fmt "~D" numericals-operation))
                        (loop :for lisp-time :in numericals-timings
                           :for numpy-time :in numpy-timings
                           :do (who:htm (:td (who:fmt "~,2f" (/ numpy-time lisp-time)) "x"))))))
               (is (every #'within-acceptable-limits
                          numericals-timings numpy-timings)))))))


;; (defparameter a (nu:zeros 100000000))
;; (defparameter b (nu:zeros 100000000))
;; (defparameter c (nu:zeros 100000000))



