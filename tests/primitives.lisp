(cl:in-package #.numericals.helper:*numericals-tests-package*)
;;; The value is set in package / package+array file.

;; Seems like SBCL allocates arrays during compile time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "
def allocation_timeit(allocate_fn, sizes, num_operations):
  import time
  import numpy as np
  
  timings = []

  for i in range(len(num_operations)):
    num_operation = int(num_operations[i] / (np.product(sizes[i])))
    sz = sizes[i]
    start = time.time()
    for j in range(num_operation):
      allocate_fn(sz, dtype = 'float32')
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(defpyfun "allocation_timeit" nil :lisp-fun-name "PY-ALLOCATION-TIMEIT")

(defmacro lisp-allocation-timeit (&key allocate-fn sizes num-operations)
  `(let ((nu:*type* 'single-float)
         (start nil)
         (end nil)
         (sizes ,sizes)
         (num-operations ,num-operations))
     (declare (special nu:*type*))
     (loop :for i :below (length num-operations)
        :for num-operation := (floor (/ (elt num-operations i)
                                        (apply #'* (elt sizes i))))
        :for size := (elt sizes i)
        :do (let ()
              (declare (optimize (speed 3))
                       (type list size))
              (setq start (get-internal-real-time))
              (loop :for i fixnum :below num-operation
                 :do (funcall #',allocate-fn size))
              (setq end (get-internal-real-time)))
        :collect (coerce (/ (- end start)
                            internal-time-units-per-second)
                         'single-float))))

(def-test allocation-speed (:suite speed)
  (flet ((within-acceptable-limits (lisp-time numpy-time)
           (<= (/ lisp-time numpy-time) 4)))
    (let* ((nu:*type* 'single-float)
           (sizes '((10 1) (10 10) (100 100) (1000 1000) (10000 10000)))
           (num-operations '(1e6 1e7 1e9 1e9 1e9))
           (numpy-operations '(np.ones np.zeros np.empty))
           (numericals-operations '(nu:ones nu:zeros nu:empty)))
      (when *write-to-readme*
        (who:with-html-output (*write-to-readme-stream* nil :indent t)
          (:tr (:th "Allocation array operations (seems like SBCL allocates arrays
during compilation time)")
               (loop :for size :in (mapcar (lambda (l) (apply #'* l)) sizes)
                  :do (who:htm (:th (who:str size)))))))
      (loop :for numpy-operation :in numpy-operations
         :for numericals-operation :in numericals-operations
         :do (let ((numpy-timings (py-allocation-timeit :allocate-fn numpy-operation
                                                        :sizes sizes
                                                        :num-operations num-operations))
                   (numericals-timings
                    (ecase numericals-operation
                      (nu:ones (lisp-allocation-timeit :allocate-fn nu:ones
                                                       :sizes sizes
                                                       :num-operations num-operations))
                      (nu:zeros (lisp-allocation-timeit :allocate-fn nu:zeros
                                                        :sizes sizes
                                                        :num-operations num-operations))
                      (nu:empty (lisp-allocation-timeit :allocate-fn nu:empty
                                                        :sizes sizes
                                                        :num-operations num-operations)))))
               (when *write-to-readme*
                 (who:with-html-output (*write-to-readme-stream* nil :indent t)
                   (:tr (:td (who:fmt "~D" numericals-operation))
                        (loop :for lisp-time :in numericals-timings
                           :for numpy-time :in numpy-timings
                           :do (who:htm (:td (or (ignore-errors
                                                   (who:fmt "~,2f" (/ numpy-time lisp-time)))
                                                 (who:str "∞")) "x"))))))
               (is (every #'within-acceptable-limits
                          numericals-timings numpy-timings)))))))
