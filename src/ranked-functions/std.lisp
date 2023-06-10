(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(5am:in-suite nu::array)

(define-polymorphic-function nu:std (array-like &key out axes keep-dims (ddof 0))
  :overwrite t
  :documentation "See https://numpy.org/doc/stable/reference/generated/numpy.std.html")

(defpolymorph nu:std (array-like &key ((out null)) axes keep-dims (ddof 0)) t
  (declare (ignore out))
  (nu:sqrt (nu:variance array-like :axes axes :keep-dims keep-dims :ddof ddof)))

(defpolymorph nu:std (array-like
                      &key ((out (simple-array <type>))) axes keep-dims (ddof 0))
    (simple-array <type>)
  (nu:sqrt (nu:variance array-like :axes axes :keep-dims keep-dims :ddof ddof :out out)
           :out out))

(defpolymorph nu:std (array-like
                      &key ((out (array <type>))) axes keep-dims (ddof 0))
    (array <type>)
  (nu:sqrt (nu:variance array-like :axes axes :keep-dims keep-dims :ddof ddof :out out)
           :out out))
