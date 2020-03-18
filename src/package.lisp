(defpackage :sbcl-numericals.internals
  (:use :cl :sb-c :sb-ext :sb-vm))

(defpackage :sbcl-numericals
  (:use :cl)
  (:export :s-
           :s+
           :s*
           :s/
           :d-
           :d+
           :d*
           :d/))
