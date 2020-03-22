(defpackage :sbcl-numericals.internals
  (:use :cl :sb-c :sb-ext :sb-vm))

(defpackage :sbcl-numericals
  (:use :cl)
  (:export

   ;; AVX array operations 
   :s-
   :s+
   :s*
   :s/
   :d-
   :d+
   :d*
   :d/

   ;; SSE array operations
   :s2-
   :s2+
   :s2*
   :s2/
   :d2-
   :d2+
   :d2*
   :d2/))
