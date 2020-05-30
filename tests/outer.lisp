(in-package :numericals/tests)

(def-test map-outer-correctness (:suite correctness)
  ;; The following actually ensures the correctness of arithmetic operations
  ;; for displaced arrays.
  (let ((dim (iota 8 :start 8))) 
    (iter (for np-op in '(np.add np.subtract np.multiply np.divide))
          (for nu-op in '(nu:+ nu:- nu:* nu:/))
          (iter (for d in dim)
                (for a = (pycall 'np.random.random (list 2 3 d)))
                (for b = (pycall 'np.random.random (list 2 3 1)))
                (is (np:allclose :a (pyeval "np.array(list(map(" np-op "," a ", " b ")))")
                                 :b (nu:map-outer 'array nu-op
                                                  (nu:asarray a)
                                                  (nu:asarray b))
                                 :atol 1e-7)))))
  ;; The following ensures the correctness of map-outer itself for displaced arrays
  (let ((dim (iota 8 :start 8))) 
    (iter (initially (pyexec "
def map_outer_nested(a, b):
  import numpy as np
  return np.array(list(map(
    lambda x, y: list(map(np.add, x, y))
  , a, b)))"))
          (for d in dim)
          (for a = (pycall 'np.random.random (list 2 15 d)))
          (for b = (pycall 'np.random.random (list 2 15 1)))
          (is (np:allclose :a (pycall 'map-outer-nested a b)
                           :b (nu:map-outer 'array
                                            (lambda (x y)
                                              (nu:map-outer 'array #'nu:+ x y))
                                            (nu:asarray a)
                                            (nu:asarray b))
                           :atol 1e-7))))
  ;; The OUT supplied to NU:MAP-OUTER checks both for CONCATENATE when OUT
  ;; is a displaced array, as well as the code-path in MAP-OUTER when OUT is a displaced
  ;; array.
  (let ((dim (iota 8 :start 8))) 
    (iter (initially (pyexec "
def map_outer_nested(a, b):
  import numpy as np
  return np.array(list(map(
    lambda x, y: list(map(np.add, x, y))
  , a, b)))"))
          (for d in dim)
          (for a = (pycall 'np.random.random (list 2 15 d)))
          (for b = (pycall 'np.random.random (list 2 15 1)))
          (for out = (nu:zeros 2 15 d))
          (is (np:allclose :a (pycall 'map-outer-nested a b)
                           :b (nu:map-outer out
                                            (lambda (x y &key out)
                                              (nu:map-outer out #'nu:+ x y))
                                            (nu:asarray a)
                                            (nu:asarray b))
                           :atol 1e-7)))))

