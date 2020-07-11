(cl:in-package #.numericals.helper:*numericals-tests-package*)
;;; The value is set in package / package+array file.

(def-test transpose-correctness (:suite correctness)
  (let ((dim (iota 8 :start 8))
        ;; should export and rearrange
        (axes (iota numericals.internals::*max-broadcast-dimensions*))) 
    (iter (for axis in axes)
          (iter (for d in dim)
                (for a = (nu:asarray (pycall 'np.random.random (list 3 d))))
				(let ((py (pyeval (pycall 'np.transpose a)))
                      (nu (pyeval (funcall 'nu:transpose a))))
                  (is (np:allclose :a py
                                   :b nu
                                   :atol 1e-7)
                      "~%~D~% and ~%~D~% differ from each other: ~%~D~%"
                      py nu (np:subtract py nu)))))))
