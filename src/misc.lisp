(in-package :numericals.internals)

;; arbitrary arrays
(defpolymorph (one-arg-fn :inline t) ((name symbol) (x array) &key ((out array))) array
  ;; FIXME: We are assuming OUT is either single-float or double-float
  (if (type= (array-element-type x)
             (array-element-type out))
      (one-arg-fn name x :out out)
      (one-arg-fn name (nu:copy x :out out) :out out))
  out)

(defpolymorph (two-arg-fn :inline t) ((name non-comparison-operator) (x array) (y array)
                                      &key ((out array)))
    array
  ;; FIXME: We are assuming OUT is either single-float or double-float
  (cond ((and (type= (array-element-type x)
                     (array-element-type out))
              (type= (array-element-type y)
                     (array-element-type out)))
         (two-arg-fn name x y :out out))
        ((type= (array-element-type x)
                (array-element-type out))
         (two-arg-fn name
                     x
                     (nu:copy y :out out)
                     :out out))
        ((type= (array-element-type y)
                (array-element-type out))
         (two-arg-fn name
                     (nu:copy x :out out)
                     y                     
                     :out out))
        (t
         (two-arg-fn name
                     (nu:copy x :out out)
                     (nu:copy y :out (nu:zeros-like out)) 
                     :out out)))
  out)

(defpolymorph (two-arg-fn :inline t) ((name comparison-operator) (x array) (y array)
                                      &key ((out (array (unsigned-byte 8)))))
    array
  (cond ((type= (array-element-type x)
                (array-element-type y))
         (two-arg-fn name x y :out out))
        ;; FIXME: Should we upgrade both to double-float or demote to single-float?
        ((type= 'double-float (array-element-type x))
         (two-arg-fn name
                     x
                     (nu:copy y :out (nu:zeros-like x))
                     :out out))
        ((type= 'double-float (array-element-type y))
         (two-arg-fn name
                     (nu:copy x :out (nu:zeros-like x))
                     y                     
                     :out out))
        (t (error "Unexpected case!")))
  out)

