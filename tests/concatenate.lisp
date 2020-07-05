(cl:in-package #.numericals.helper:*numericals-tests-package*)
;;; The value is set in package / package+array file.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "
def concatenate_timeit(sizes, num_operations, axis):
  import time
  import numpy as np
  
  timings = []

  for i in range(len(num_operations)):
    a = np.ones(sizes[i]).astype('float32')
    b = np.ones(sizes[i]).astype('float32')
    c_size = list(sizes[i][0:axis]) + [2*sizes[i][axis]] + list(sizes[i][axis+1:])
    c = np.ones(c_size).astype('float32')
    num_operation = int(num_operations[i] / (2*np.product(sizes[i])))
    start = time.time()
    for i in range(num_operation):
      np.concatenate((a, b), out = c, axis = axis)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(defpyfun "concatenate_timeit" nil :lisp-fun-name "PY-CONCATENATE-TIMEIT")

(defun lisp-concatenate-timeit (&key sizes num-operations axis)
  (let ((nu:*type* 'single-float)
        (start nil)
        (end nil))
    (declare (special nu:*type*))
    (loop :for i :below (length num-operations)
         
       :for a = (apply #'nu:ones (elt sizes i))
       :for b = (apply #'nu:ones (elt sizes i))
       :for c-size = (let ((sizes-i (elt sizes i)))
                       (append (subseq sizes-i 0 axis)
                               (list (* 2 (elt sizes-i axis)))
                               (subseq sizes-i (1+ axis))))
       :for c = (apply #'nu:ones c-size)
       :for num-operation := (floor (/ (elt num-operations i)
                                       (apply #'* 2 (elt sizes i))))
       :do (let ()
             (declare (optimize (speed 3))
                      (type (array single-float) a b c))
             (setq start (get-internal-real-time))
             (loop :for i fixnum :below num-operation
                :do (nu:concatenate a b :out c :axis axis))
             (setq end (get-internal-real-time)))
       :collect (coerce (/ (- end start)
                           internal-time-units-per-second)
                        'single-float))))


(def-test concatenate-speed (:suite speed)
  (flet ((within-acceptable-limits (lisp-time numpy-time)
           (<= (/ lisp-time numpy-time) 2)))
    (let* ((sizes '((5 1) (5 10) (50 100) (500 1000) (5000 10000)))
           (num-operations '(1e7 1e8 1e9 1e9 1e9))
           (axes '(0)))
      (when *write-to-readme*
        (who:with-html-output (*write-to-readme-stream* nil :indent t)
          (:tr (:th "Concatenate (currently unoptimized for axis != 0; 
as such this can be slower than numpy by a factor of 50)")
               (loop :for size :in (mapcar (lambda (l) (apply #'* 2 l)) sizes)
                  :do (who:htm (:th (who:str size)))))))
      (loop :for axis :in axes
         :do (let ((numpy-timings (py-concatenate-timeit :sizes sizes :axis axis
                                                         :num-operations num-operations))
                   (numericals-timings
                    (lisp-concatenate-timeit :sizes sizes :axis axis
                                             :num-operations num-operations)))
               (when *write-to-readme*
                 (who:with-html-output (*write-to-readme-stream* nil :indent t)
                   (:tr (:td (who:fmt "Axis ~D" axis))
                        (loop :for lisp-time :in numericals-timings
                           :for numpy-time :in numpy-timings
                           :do (who:htm (:td (who:fmt "~,2f" (/ numpy-time lisp-time)) "x"))))))
               (is (every #'within-acceptable-limits
                          numericals-timings numpy-timings)))))))

(def-test concatenate-correctness (:suite correctness)
  (let ((dim (iota 8 :start 8))
        ;; should export and rearrange
        (axes (iota numericals.internals::*max-broadcast-dimensions*))) 
    (iter (for axis in axes)
          (iter (for d in dim)
                (for a = (pycall 'np.random.random (list 3 d)))
                (for b = (pycall 'np.random.random (list 3 d)))
                (is (np:allclose :a (pycall 'np.concatenate (list a b))
                                 :b (funcall 'nu:concatenate a b)
                                 :atol 1e-7))))))
