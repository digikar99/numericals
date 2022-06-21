(defsystem "dense-numericals"
  :pathname "dense-numericals-src/"
  :version "0.1.0"
  :licence "MIT"
  :depends-on ("numericals.common"
               "dense-arrays-plus-lite"
               "cblas"
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
               (:file "two-arg-fn-all"        :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"))
               (:file "n-arg-fn"              :depends-on ("two-arg-fn-all"))
               (:file "n-arg-fn-compiler-macros" :depends-on ("n-arg-fn"
                                                              "two-arg-fn-all"))
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

(defsystem "dense-numericals/benchmarks"
  :pathname "benchmarks/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "cl-ascii-table"
               "dense-numericals"
               "py4cl2"
               "fiveam")
  :components ((:file "package")
               (:file "benchmark")
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

