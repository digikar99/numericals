(cl:in-package :numericals.helper)

(defpackage :numericals
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
   :aref
   :concatenate
   :zeros
   :ones
   :empty
   :shape
   :transpose

   :+
   :-
   :/
   :*

   :sqrt))

(defpackage :numericals.internals
  (:use :cl :alexandria :iterate :introspect-environment
        #+sbcl :numericals.sbcl)
  (:local-nicknames (:nu :numericals))
  (:import-from :numericals
                :maybe-form-not-constant-error
                :*type*
                :*lookup-type-at-compile-time*))


(cl:in-package :numericals.internals)
#+sbcl
`(declaim (sb-ext:maybe-inline
           ,@(iter (for s in-package :numericals external-only t)
                   (collect s))))

(setq numericals.helper:*numericals-internals-package* :numericals.internals)

