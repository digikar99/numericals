(in-package :numericals/tests)

;;; With allocation
;;; Without allocation
;;; Sizes 100 10000 1000000 100000000
;;; Number of operations 1e10

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "
def timeit(fn, array_size_list, num_operations):
  import time
  import numpy as np
  
  timings = []

  for sz in array_size_list:
    lim = int(num_operations / sz)
    dim = int(np.floor(np.sqrt(sz)))
    a = np.ones((dim, dim)).astype('float32')
    b = np.ones((dim, dim)).astype('float32')
    c = np.ones((dim, dim)).astype('float32')
    start = time.time()
    for i in range(lim):
      fn(a, b, out = c)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(defpyfun "timeit" nil :lisp-fun-name "PY-TIMEIT")

(defun lisp-timeit (&key fn array-size-list num-operations)
  (let ((nu:*type* 'single-float)
        (start nil)
        (end nil))
    (loop :for size :in array-size-list
       :for lim := (floor num-operations size)
       :for dim := (floor (sqrt size))
       :do (let ((a (nu:ones dim dim))
                 (b (nu:ones dim dim))
                 (c (nu:ones dim dim)))
             (declare (optimize (speed 3))
                      (type (simple-array single-float) a b c))
             (setq start (get-internal-real-time))
             (loop :for i fixnum :below lim
                :do (funcall fn a b :out c))
             (setq end (get-internal-real-time)))
       :collect (coerce (/ (- end start)
                           internal-time-units-per-second)
                        'single-float))))

(def-suite arithmetic :in :numericals)
(in-suite arithmetic)

(def-test non-broadcast-speed (:suite arithmetic)
  (flet ((within-acceptable-limits (lisp-time numpy-time)
           (<= (/ lisp-time numpy-time) 1.35)))
    (let* ((array-size-list (mapcar #'floor '(100 1e4 1e6 1e8)))
           (num-operations 1e8)
           (numpy-operations '(np.add np.subtract np.multiply np.divide))
           (numericals-operations '(nu:+ nu:- nu:* nu:/)))
      (when *write-to-readme*
        (who:with-html-output (*write-to-readme-stream* nil :indent t)
          (:tr (:th "Non-broadcast array operations")
               (loop :for size :in array-size-list
                  :do (who:htm (:th (who:str size)))))))
      (loop :for numpy-operation :in numpy-operations
         :for numericals-operation :in numericals-operations
         :do (let ((numpy-timings (py-timeit :fn numpy-operation
                                             :array-size-list array-size-list
                                             :num-operations num-operations))
                   (numericals-timings (lisp-timeit :fn numericals-operation
                                                    :array-size-list array-size-list
                                                    :num-operations num-operations)))
               (when *write-to-readme*
                 (who:with-html-output (*write-to-readme-stream* nil :indent t)
                   (:tr (:td (who:fmt "~D" numericals-operation))
                        (loop :for lisp-time :in numericals-timings
                           :for numpy-time :in numpy-timings
                           :do (who:htm (:td (who:fmt "~,2f" (/ numpy-time lisp-time)) "x"))))))
               (is (every #'within-acceptable-limits
                          numericals-timings numpy-timings)))))))

(def-test non-broadcast-correctness (:suite arithmetic)
  (let ((dim (iota 8 :start 8)))
    (iter (for np-op in '(np.add np.subtract np.multiply np.divide))
          (for nu-op in '(nu:+ nu:- nu:* nu:/))
          (iter (for d in dim)
                (for a = (pycall 'np.random.random (list 3 d)))
                (for b = (pycall 'np.random.random (list 3 d)))
                (is (np:allclose :a (pycall np-op a b)
                                 :b (funcall nu-op a b)))))))

;; (defparameter a (nu:zeros 100000000))
;; (defparameter b (nu:zeros 100000000))
;; (defparameter c (nu:zeros 100000000))



