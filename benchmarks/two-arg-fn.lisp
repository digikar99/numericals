(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (py4cl2:pyexec "
def numpy_two_arg_fn(fn, a_sizes, b_sizes, o_sizes, num_operations, elt_type):
  import time
  import numpy as np

  timings = []

  for i in range(len(num_operations)):
    a = np.ones(a_sizes[i]).astype(elt_type)
    b = np.ones(b_sizes[i]).astype(elt_type)
    o = np.ones(o_sizes[i]).astype(elt_type)
    num_operation = int(num_operations[i] / (np.product(o_sizes[i])))
    start = time.time()
    for i in range(num_operation):
      fn(a, b, out = o)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
")

  (py4cl2:pyexec "
def torch_two_arg_fn(fn, a_sizes, b_sizes, o_sizes, num_operations, elt_type):
  import time
  import math as m
  import torch as t

  timings = []

  for i in range(len(num_operations)):
    a = t.ones(a_sizes[i], dtype=elt_type)
    b = t.ones(b_sizes[i], dtype=elt_type)
    o = t.ones(o_sizes[i], dtype=elt_type)
    num_operation = int(num_operations[i] / (m.prod(o_sizes[i])))
    start = time.time()
    for i in range(num_operation):
      fn(a, b, out = o)
    end = time.time()
    timings.append(end - start)

  return tuple(timings)
"))

(py4cl2:defpyfun "numpy_two_arg_fn")
(py4cl2:defpyfun "torch_two_arg_fn")

(defun nu-two-arg-fn (&key fn a-sizes b-sizes o-sizes num-operations elt-type)
  (loop :for i :below (length num-operations)
        :for a-size := (elt a-sizes i)
        :for b-size := (elt b-sizes i)
        :for o-size := (elt o-sizes i)
        :collect
        (let* ((a (nu:ones a-size :type elt-type))
               (b (nu:ones b-size :type elt-type))
               (o (nu:ones o-size :type elt-type))
               (total-size (apply #'* o-size))
               (num-operation (floor (/ (elt num-operations i) total-size)))
               (type (list (if (and (equalp a-size b-size)
                                    (equalp a-size o-size))
                               'simple-array
                               'array)
                           elt-type
                           (length o-size)))
               (fn (compile nil
                            `(cl:lambda (a b o)
                               (declare (cl:type ,type a b o)
                                        #+extensible-compound-types
                                        (type ,type a b o))
                               (time-it
                                 (locally ,(if (> total-size nu:*multithreaded-threshold*)
                                               ()
                                               `(declare (optimize speed)))
                                   (loop :for i :of-type fixnum :below ,num-operation
                                         :do (,fn a b :out o
                                                      :broadcast ,(not
                                                                   (and (equalp a-size b-size)
                                                                        (equalp a-size o-size)))))))))))
          (funcall fn a b o))))

(defun two-arg-fn (lisp-names numpy-names &optional torch-names)
  (when *numpy* (py4cl2:pyexec "import numpy as np"))
  (when *torch* (py4cl2:pyexec "import torch as t"))
  (let* ((a-sizes '((10 1) (10 10) (100 100) (100 1)   (100 1 100)   #-arm64 (10000 10000)))
         (b-sizes '((10 1) (10 10) (100 100) (1 100)   (1 100 100)   #-arm64 (10000 10000)))
         (o-sizes '((10 1) (10 10) (100 100) (100 100) (100 100 100) #-arm64 (10000 10000)))
         (num-operations '(1e7 1e8 1e9 1e9 1e9 #-arm64 1e9)))
    (loop :for idx :below (length lisp-names)
          :for numpy-name := (nth idx numpy-names)
          :for lisp-name  := (nth idx lisp-names)
          :for torch-name := (nth idx torch-names)
          :do (let ((nu-timings
                      (nu-two-arg-fn :fn lisp-name
                                     :a-sizes a-sizes
                                     :b-sizes b-sizes
                                     :o-sizes o-sizes
                                     :num-operations num-operations
                                     :elt-type default-element-type))
                    (numpy-timings
                      (when *numpy*
                        (numpy-two-arg-fn :fn numpy-name
                                          :a-sizes a-sizes
                                          :b-sizes b-sizes
                                          :o-sizes o-sizes
                                          :num-operations num-operations
                                          :elt-type (numpy-element-type default-element-type))))
                    (torch-timings
                      (when *torch*
                        (torch-two-arg-fn :fn torch-name
                                          :a-sizes a-sizes
                                          :b-sizes b-sizes
                                          :o-sizes o-sizes
                                          :num-operations num-operations
                                          :elt-type (torch-element-type default-element-type)))))
                (push (make-fun-report :name lisp-name
                                       :array-sizes o-sizes
                                       :numpy numpy-timings
                                       :lisp nu-timings
                                       :torch torch-timings)
                      (report-fun-reports *report*))))))
