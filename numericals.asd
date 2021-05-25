(asdf:defsystem "numericals/helper"
  :pathname ""
  :depends-on ("alexandria" "iterate")
  :components ((:file "helper")))

(asdf:defsystem "numericals/sbcl"
  :pathname "sbcl/"
  :depends-on ("numericals/helper"
               "alexandria"
               "iterate")
  :components ((:file "package")
               (:file "accessors")
               (:file "1d-storage-array")
               (:file "arithmetic")
               (:file "arithmetic-single")
               (:file "arithmetic-double")
               (:file "boolean")))

(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("polymorphic-functions"
               "array-operations"
               "cl-form-types"
               "compiler-macro-notes"
               "bmas"
               "cblas"
               "fiveam"
               #+sbcl "numericals/sbcl"
               "alexandria"
               "iterate"
               "cffi"
               "lparallel"
               "policy-cond"
               "specialized-function"
               "trivial-types"
               "trivial-package-local-nicknames"
               "introspect-environment")
  :components ((:file "package")
               ;; FIXME: Simplify primitives
               (:file "utils")
               (:file "primitives")               
               (:file "translations")
               (:file "lparallel")
               (:file "test")
               (:file "one-arg-fn")
               (:file "two-arg-fn-non-broadcast")
               (:file "broadcast")
               (:file "ptr-iterate-but-inner")
               (:file "two-arg-fn")
               ;; FIXME: with-elementwise-operations
               ;; It's not sanely possible to implement this without SB-SIMD or CL-SIMD
               ;; or being able to call CFFI with SIMD packs, because of cases like
               ;;   (nu:weop c (+ (+ a b) (+ b b)))
               ;; ^This necessitates temporary memory allocation
               ;; This operation stays important for large arrays because of cpu caches
               ;; (:file "with-elementwise-operations")
               (:file "n-arg-fn")
               (:file "n-arg-fn-compiler-macro")
               (:file "outer")
               ;; (:file "concatenate")
               ;; FIXME: Do we really want a "good" AREF? Because that was one of the
               ;; main points of DENSE-ARRAYS; besides, NUMCL and SELECT already provide
               ;; the "good" aref
			   ;; (:file "aref")
			   (:file "transpose")))

(asdf:defsystem "numericals/tests"
  :pathname "tests/"
  :version "0.1.0"
  :serial t
  :depends-on ("numericals"
               "alexandria"
               "iterate"
               "py4cl2"
               "cl-ppcre"
               "numericals"
               "fiveam"
               "cl-who")
  :components ((:file "package")
               (:file "primitives")
               (:file "arithmetic")
               (:file "concatenate")
               (:file "outer")
			   (:file "transpose")
               (:static-file "readme" :pathname #P"../README.md")))
