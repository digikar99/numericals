(defsystem "dense-numericals"
  :pathname "dense-numericals-src/"
  :version "2024.01.0" ; year, month, patch
  :description "A high performance numerical computing library for Common Lisp (focus: basic math operations)"
  :license "MIT"
  :author "Shubhamkar B. Ayare (shubhamayare@yahoo.co.in)"
  :depends-on ("numericals.common"

               "alexandria"
               "bmas"
               "ceigen-lite"
               "cffi"
               "cl-autowrap"
               "dense-arrays-plus-lite"
               "fiveam"
               "gsll"
               "iterate"
               "lparallel"
               "peltadot"
               "policy-cond"
               "swank"
               "trivial-package-local-nicknames"
               "uiop")
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
               (:module "ranked-functions" :depends-on ("translations"
                                                        "ptr-iterate-but-inner"
                                                        "primitives")
                :components
                ((:file "simple-array-broadcast")
                 (:file "sum")
                 (:file "maximum")
                 (:file "minimum")
                 (:file "mean")
                 (:file "variance")
                 (:file "std")
                 (:file "matmul")
                 (:file "concat")
                 (:file "arg-maximum")
                 (:file "arg-minimum")
                 (:module "linear-algebra"
                  :components ((:file "eigen")
                               (:file "det")
                               (:file "inv")
                               (:file "pinv")
                               (:file "norm2")
                               (:file "solve")
                               (:file "rank")
                               (:file "qr")
                               (:file "lu")
                               (:file "svd")
                               (:file "cholesky")
                               (:file "eig")))))
               (:file "blas"                  :depends-on ("utils"
                                                           "ptr-iterate-but-inner"))
               (:module "random"  :depends-on ("translations"
                                               "ptr-iterate-but-inner"
                                               "primitives")
                :components ((:file "gsll")
                             (:file "gaussian")
                             (:file "beta")
                             (:file "chisquare")))
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

