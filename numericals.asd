(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("polymorphic-functions"
               "cl-form-types"
               "array-operations"
               "compiler-macro-notes"
               "compiler-macro"
               "bmas"
               "cblas"
               "fiveam"
               ;; #+sbcl "numericals/sbcl"
               "alexandria"
               "iterate"
               "cffi"
               "lparallel"
               "policy-cond"
               "specialized-function"
               "trivial-coerce"
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
               (:file "copy-coerce")
               (:file "two-arg-fn")
               ;; FIXME: with-elementwise-operations
               ;; It's not sanely possible to implement this without SB-SIMD or CL-SIMD
               ;; or being able to call CFFI with SIMD packs, because of cases like
               ;;   (nu:weop c (+ (+ a b) (+ b b)))
               ;; ^This necessitates temporary memory allocation
               ;; This operation stays important for large arrays because of cpu caches
               ;; (:file "with-elementwise-operations")
               (:file "n-arg-fn")
               (:file "n-arg-fn-compiler-macros")
               (:file "n-arg-fn-tests")
               (:file "outer")
               ;; (:file "concatenate")
               ;; FIXME: Do we really want a "good" AREF? Because that was one of the
               ;; main points of DENSE-ARRAYS; besides, NUMCL and SELECT already provide
               ;; the "good" aref
               ;; (:file "aref")
               (:file "transpose")
               (:file "misc"))
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                       (5AM:RUN :NUMERICALS))"))))
