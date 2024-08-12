(peltadot/utils:defpackage :numericals/utils
  (:use :peltadot :numericals/basic-utils)
  (:shadow #:fill)
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
           #:do-arrays
           #:macro-map-array

           #:aref*

           #:incompatible-broadcast-dimensions
           #:broadcast-array

           #:transpose

           #:runtime-array-allocation))

(peltadot/utils:defpackage #:numericals/utils/impl
  (:use :peltadot :numericals/common :numericals/basic-utils :numericals/utils)
  (:import-from #:alexandria
                #:lastcar
                #:with-gensyms
                #:make-gensym-list
                #:switch
                #:eswitch
                #:mappend
                #:non-negative-fixnum-p
                #:iota)
  (:import-from #:peltadot-traits-library
                #:element-type)
  (:shadowing-import-from #:numericals/utils #:fill)
  (:shadowing-import-exported-symbols #:iterate)
  (:reexport #:numericals/basic-utils #:numericals/common)
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

           #:out-shape-compatible-p
           #:out-shape))

(5am:def-suite :numericals)

(numericals/common:export-all-external-symbols :numericals/utils :numericals)
