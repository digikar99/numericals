(peltadot/utils:defpackage :numericals/more-utils
  (:use :peltadot :numericals/utils :numericals/common)
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
  (:shadowing-import-exported-symbols #:iterate)
  (:shadow #:fill)
  (:reexport #:numericals/utils #:numericals/common)
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

           #:%broadcast-compatible-p
           #:broadcast-compatible-p
           #:incompatible-broadcast-dimensions
           #:do-with-broadcasting
           #:broadcast-array

           #:transpose

           #:ptr-iterate-but-inner
           #:with-simple-array-broadcast

           #:runtime-array-allocation

           #:out-shape-compatible-p
           #:out-shape))

(5am:def-suite :numericals)

(numericals/common:export-all-external-symbols :numericals/more-utils :numericals)
