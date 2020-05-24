(defpackage :sbcl-numericals
  (:use)
  (:export

   ;; configuration
   :*array-element-type*
   :*automatically-upgrade-array-element-type*
   :*automatically-convert-lists-to-arrays*
   ;; utilities
   :with-simd-operations

   ;; unoptimized array-operations
   :asarray
   :array-like-p

   ;; AVX2 based array operations

   :s-
   :s+
   :s*
   :s/
   :d-
   :d+
   :d*
   :d/

   :ddot
   :sdot

   ;; SSE based array operations
   :s2-
   :s2+
   :s2*
   :s2/
   :d2-
   :d2+
   :d2*
   :d2/

   :broadcast-d+
   ))

(defpackage :sbcl-numericals.internals
  (:use :cl :sb-c :sb-ext :sb-vm :alexandria :iterate)
  (:export :define-binary-vectorized-op :move)
  (:local-nicknames (:sn :sbcl-numericals)))


