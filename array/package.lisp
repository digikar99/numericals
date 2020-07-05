(cl:in-package :numericals.helper)


;; This package implements a multidimensional displaced array. This is required to implement ;; faster aref-ing. Without this, aref can be 50 times slower than numpy - since all numpy
;; does while arefing is provides a "view", without actually copying over the data.


;;; Ideally, we'd like to keep only one interface package - however, there are certain
;;; array- functions we might not want to be public, but want them to be available for
;;; internal use.
(defpackage :numericals.array
  ;; TODO: THink of a name that better expresses our intention
  (:use)
  #.`(:intern ,@+numericals-array-slots+)
  #.`(:export
      ,@+cl-array-symbols+)
  #.`(:export
      ,@+numericals-array-symbols+))


(defpackage :numericals.array.internals
  (:use :cl :alexandria :iterate :introspect-environment)
  #+sbcl (:import-from :sb-ext :array-storage-vector)
  (:import-from :trivial-types :function-designator)
  #.`(:shadowing-import-from :numericals.array
                             :make-numericals-array
                             ,@+numericals-array-symbols+
                             ,@+numericals-array-slots+)
  (:local-nicknames (:na :numericals.array)))



