(defsystem "dense-numericals"
  :pathname "dense-numericals-src/"
  :version "2023.02.0" ; year, month, patch
  :description "A high performance numerical computing library for Common Lisp (focus: basic math operations)"
  :license "MIT"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :depends-on ("numericals.common"
               "dense-arrays-plus-lite"
               "magicl/ext-blas"
               "bmas"
               "cl-autowrap"
               "alexandria"
               "iterate"
               "uiop"
               "cffi"
               (:feature :extensible-compound-types "extensible-compound-types-cl")
               "fiveam"
               "lparallel"
               "policy-cond"
               "polymorphic-functions"
               "dense-arrays+static-vectors"
               "swank"
               "trivial-coerce"
               "trivial-package-local-nicknames")
  ;; TODO: Use CFFI-GROVEL or something to manage shared library / c files
  :components ((:file "package")
               (:file "utils"                 :depends-on ("package"))
               (:file "primitives"            :depends-on ("utils"))
               (:file "ptr-iterate-but-inner" :depends-on ("utils"))
               (:file "lparallel"             :depends-on ("ptr-iterate-but-inner"))
               (:file "test"                  :depends-on ("package"))
               (:file "translations"          :depends-on ("package"))
               (:file "copy-coerce"           :depends-on ("utils"
                                                           "ptr-iterate-but-inner"
                                                           "lparallel"))
               (:file "one-arg-fn-float"      :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"))
               (:file "one-arg-fn-all"        :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"))
               (:file "two-arg-fn-float"      :depends-on ("utils"
                                                           "test"
                                                           "translations"))
               (:file "two-arg-fn-comparison" :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"))
               (:file "two-arg-fn-non-comparison" :depends-on ("copy-coerce"
                                                               "translations"
                                                               "test"))
               (:file "in-place-operators" :depends-on ("one-arg-fn-float"
                                                        "two-arg-fn-float"
                                                        "two-arg-fn-comparison"
                                                        "two-arg-fn-non-comparison"))
               (:file "n-arg-fn"              :depends-on ("two-arg-fn-comparison"
                                                           "two-arg-fn-non-comparison"))
               (:file "n-arg-fn-compiler-macros" :depends-on ("n-arg-fn"
                                                              "two-arg-fn-comparison"
                                                              "two-arg-fn-non-comparison"))
               (:file "n-arg-fn-tests"        :depends-on ("n-arg-fn-compiler-macros"))
               (:file "blas"                  :depends-on ("utils"
                                                           "ptr-iterate-but-inner"))
               (:file "concatenate"           :depends-on ("blas"))
               (:file "one-arg-reduce-fn"     :depends-on ("translations"))
               (:file "misc"                  :depends-on ("package")))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Or should we use STATIC?
             (eval (read-from-string "(LET* ((5AM:*ON-ERROR* :DEBUG)
                                             (5AM:*ON-FAILURE* :DEBUG)
                                             (DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                               'DENSE-ARRAYS:STANDARD-DENSE-ARRAY))
                                        (5AM:RUN! 'DENSE-NUMERICALS.IMPL::ARRAY))"))))

(defsystem "dense-numericals/magicl"
  :pathname "dense-numericals-src/"
  :depends-on ("dense-numericals"
               "magicl"
               "swank")
  :components ((:file "magicl-wrapper")
               (:file "magicl")))

(defsystem "dense-numericals/benchmarks"
  :pathname "benchmarks/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "cl-ascii-table"
               "dense-numericals"
               "py4cl2"
               "fiveam")
  :components ((:file "benchmark")
               (:file "one-arg-fn")
               (:file "two-arg-fn"))
  :perform (test-op (o c)
             (declare (ignore o c))
             ;; Or should we use STATIC?
             (eval (read-from-string "(LET* ((DENSE-ARRAYS:*DENSE-ARRAY-CLASS*
                                               'DENSE-ARRAYS:STANDARD-DENSE-ARRAY))
                                        (5AM:RUN! :DENSE-NUMERICALS/BENCHMARKS))"))))

(defsystem "dense-numericals/examples"
  :pathname "examples/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "dense-numericals"
               "numpy-file-format")
  :components ((:file "mnist")))

