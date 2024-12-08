(uiop:define-package :dense-numericals/utils
  (:mix :dense-arrays-plus-lite :peltadot)
  (:use :dense-numericals/common :abstract-arrays)
  (:reexport :dense-arrays-plus-lite)
  (:shadow #:fill
           #:zeros
           #:ones
           #:full
           #:rand
           #:empty
           #:zeros-like
           #:ones-like
           #:full-like
           #:rand-like
           #:empty-like)
  (:import-from :dense-arrays
                #:array-strides
                #:array-offsets
                #:broadcast-arrays
                #:broadcast-compatible-p
                #:default-element-type)
  (:export #:*default-float-format*
           #:*broadcast-automatically*
           #:*array-layout*
           #:*array-element-type*
           #:*array-element-type-alist*
           #:default-element-type
           #:*multithreaded-threshold*
           #:*inline-with-multithreading*

           #:array
           #:simple-array
           #:array-rank
           #:array-dimensions
           #:narray-dimensions
           #:array-element-type
           #:array-total-size
           #:array-stride
           #:array-layout
           #:cl-array-offset
           #:array-storage
           #:array-type-element-type)
  (:export #:uint32
           #:int64
           #:int16
           #:int8

           #:coerce

           #:ones
           #:zeros
           #:empty
           #:rand
           #:full
           #:fill
           #:ones-like
           #:zeros-like
           #:ones-like
           #:rand-like
           #:full-like
           #:empty-like

           #:array=
           #:asarray

           #:shape
           #:reshape
           #:concat
           #:do-arrays
           #:macro-map-array

           #:defun*

           #:define-c-translation
           #:pushnew-c-translations
           #:c-name
           #:single-float-c-name
           #:double-float-c-name
           #:cl-name
           #:c-size
           #:c-type

           #:aref*

           #:broadcast-compatible-p
           #:incompatible-broadcast-dimensions
           #:broadcast-array

           #:transpose

           #:runtime-array-allocation))

(uiop:define-package :dense-numericals/utils/impl
  (:mix :dense-numericals/utils :iterate :peltadot :dense-numericals/common)
  (:use)
  (:reexport :dense-numericals/common)
  (:import-from #:alexandria
                #:lastcar
                #:with-gensyms
                #:make-gensym-list
                #:switch
                #:eswitch
                #:mappend
                #:non-negative-fixnum-p
                #:iota)
  (:import-from :peltadot/form-types
                #:constant-form-value)
  (:import-from :dense-arrays-plus-lite
                #:max-type
                #:split-at-keywords
                #:define-splice-list-fn
                #:dimensions
                #:element-type)
  (:import-from :dense-arrays
                #:dimensions->strides
                #:lm
                #:size
                #:the-size
                #:int-index
                #:the-int-index
                #:%broadcast-compatible-p)
  (:export #:size
           #:the-size
           #:int-index
           #:the-int-index
           #:lm

           #:defun*

           #:define-c-translation
           #:pushnew-c-translations
           #:c-name
           #:single-float-c-name
           #:double-float-c-name
           #:cl-name
           #:c-size
           #:c-type

           #:%broadcast-compatible-p
           #:broadcast-compatible-p
           #:do-with-broadcasting
           #:ptr-iterate-but-inner
           #:with-simple-array-broadcast
           #:with-thresholded-multithreading/cl

           #:out-shape-compatible-p
           #:out-shape))

(5am:def-suite :dense-numericals)

(in-package :dense-numericals/utils/impl)

(trivial-package-local-nicknames:add-package-local-nickname
 :cl-form-types :peltadot/form-types)
(trivial-package-local-nicknames:add-package-local-nickname
 :polymorphic-functions :peltadot/polymorphic-functions)

(numericals/common:export-all-external-symbols :dense-numericals/utils :dense-numericals)

(in-package :dense-numericals/utils/impl)

(defun parametric-type-symbol-p (s)
  (member s '(<type>)))
(pushnew 'parametric-type-symbol-p *parametric-type-symbol-predicates*)
