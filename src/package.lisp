(defpackage :sbcl-numericals.internals
  (:use :cl :sb-c :sb-ext :sb-vm :alexandria)
  (:export :define-binary-vectorized-op :move))

(defpackage :sbcl-numericals
  (:use :cl :sbcl-numericals.internals)
  (:export

   ;; AVX2 based array operations 
   :s-
   :s+
   :s*
   :s/
   :d-
   :d+
   :d*
   :d/

   ;; SSE based array operations
   :s2-
   :s2+
   :s2*
   :s2/
   :d2-
   :d2+
   :d2*
   :d2/))
