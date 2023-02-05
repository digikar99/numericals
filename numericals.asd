(defsystem "numericals"
  :pathname "src/"
  :version "2023.02.0" ; year, month, patch
  :description "A high performance numerical computing library for Common Lisp (focus: basic math operations)"
  :license "MIT"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :depends-on ("numericals.common"
               "polymorphic-functions"
               "cl-form-types"
               "compiler-macro-notes"
               "ctype"
               "bmas"
               "magicl/ext-blas"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               "fiveam"
               ;; #+sbcl "numericals/sbcl"
               "alexandria"
               "iterate"
               "cffi"
               "lparallel"
               "policy-cond"
               "specialized-function"
               "swank"
               "trivial-coerce"
               "trivial-types"
               "trivial-package-local-nicknames"
               "introspect-environment")
  :components ((:file "package")
               ;; FIXME: Simplify primitives
               (:file "utils"                 :depends-on ("package"))
               (:file "primitives"            :depends-on ("utils"))
               (:file "broadcast"             :depends-on ("primitives"
                                                           "utils"))
               ;; FIXME: Do we really want a "good" AREF? Because that was one of the
               ;; main points of DENSE-ARRAYS; besides, NUMCL and SELECT already provide
               ;; the "good" aref
               (:file "transpose"             :depends-on ("package"))
               (:file "aref"                  :depends-on ("primitives"
                                                           "utils"))

               (:file "ptr-iterate-but-inner" :depends-on ("package"))
               (:file "lparallel"             :depends-on ("package"))
               (:file "test"                  :depends-on ("primitives"
                                                           "broadcast"
                                                           "aref"))
               (:file "translations"          :depends-on ("package"))
               (:file "copy-coerce"           :depends-on ("primitives"
                                                           "ptr-iterate-but-inner"
                                                           "lparallel"))

               (:file "one-arg-fn-float"      :depends-on ("copy-coerce"
                                                           "test"
                                                           "translations"))
               (:file "one-arg-fn-all"        :depends-on ("copy-coerce"
                                                           "test"
                                                           "translations"))
               (:file "one-arg-reduce-fn"     :depends-on ("translations"
                                                           "primitives"))

               (:file "two-arg-fn-float"      :depends-on ("copy-coerce"
                                                           "test"
                                                           "translations"))
               (:file "two-arg-fn-comparison" :depends-on ("copy-coerce"
                                                           "test"
                                                           "translations"))
               (:file "two-arg-fn-non-comparison" :depends-on ("copy-coerce"
                                                               "test"
                                                               "translations"))
               (:file "in-place-operators" :depends-on ("one-arg-fn-float"
                                                        "two-arg-fn-float"
                                                        "two-arg-fn-comparison"
                                                        "two-arg-fn-non-comparison"))
               ;; FIXME: with-elementwise-operations
               ;; It's not sanely possible to implement this without SB-SIMD or CL-SIMD
               ;; or being able to call CFFI with SIMD packs, because of cases like
               ;;   (nu:weop c (+ (+ a b) (+ b b)))
               ;; ^This necessitates temporary memory allocation
               ;; This operation stays important for large arrays because of cpu caches
               ;; (:file "with-elementwise-operations")
               (:file "n-arg-fn"               :depends-on ("two-arg-fn-comparison"
                                                            "two-arg-fn-non-comparison"))
               (:file "n-arg-fn-compiler-macros" :depends-on ("n-arg-fn"))
               (:file "n-arg-fn-tests"          :depends-on ("n-arg-fn-compiler-macros"))
               ;; (:file "outer")
               ;; (:file "concatenate")
               (:file "blas"                    :depends-on ("primitives"
                                                             "ptr-iterate-but-inner"))
               ;; (:file "misc")
               )
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string "(LET ((5AM:*ON-ERROR* :DEBUG)
                                            (5AM:*ON-FAILURE* :DEBUG))
                                       (5AM:RUN! :NUMERICALS))"))))

(defsystem "numericals/benchmarks"
  :pathname "benchmarks/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "cl-ascii-table"
               "numericals"
               "py4cl2"
               "fiveam"
               "jsown-utils")
  :components ((:file "benchmark")
               (:file "one-arg-fn")
               (:file "two-arg-fn")))
