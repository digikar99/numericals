(defpackage :numericals/benchmarks
  (:use :py4cl2 :alexandria :cl :fiveam)
  (:import-from :numericals.internals
                #:default-element-type)
  (:export #:benchmark
           #:report))

(in-package :numericals/benchmarks)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :nu :numericals))

(def-suite* :numericals/benchmarks)

