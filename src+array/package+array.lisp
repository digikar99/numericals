(cl:in-package :numericals.helper)

(defpackage :numericals+array
  #.`(:shadowing-import-from :numericals.array ,@+cl-array-symbols+)
  #.`(:export ,@+cl-array-symbols+)
  (:export

   :with-broadcast
   :with-elementwise-operations
   :weop ; convenient wrapper for with-elementwise-operations
   :with-inline
   :with-array
   :with-arrays*
   :with-constant
   :with-constants
   :maybe-form-not-constant-error
   :def-array
   :make-array
   :array-like-p
   :numericals-array-element-type
   :map-outer
   
   :*type*
   :*lookup-type-at-compile-time*
   :astype
   :asarray
   :concatenate
   :zeros
   :ones
   :empty
   :shape

   :+
   :-
   :/
   :*

   :sqrt))

#+sbcl
`(declaim (sb-ext:maybe-inline
           ,@(iter (for s in-package :numericals+array external-only t)
                   (collect s))))

(defpackage :numericals+array.internals
  (:use :cl :alexandria :iterate :introspect-environment
        #+sbcl :numericals.sbcl)
  (:local-nicknames (:nu :numericals+array))
  (:import-from :numericals+array
                :maybe-form-not-constant-error
                :*type*
                :*lookup-type-at-compile-time*)
  #.`(:shadowing-import-from :numericals.array
                             ,@+cl-array-symbols+
                             ,@+numericals-array-symbols+))

(cl:in-package :numericals+array.internals)

(setq numericals.helper:*numericals-internals-package* :numericals+array.internals)
