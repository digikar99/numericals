(defsystem "dense-numericals"
  :pathname "dense-numericals-src/"
  :version "0.1.0"
  :depends-on ("dense-arrays-plus-lite"
               "cblas"
               "bmas"
               "cl-autowrap"
               "alexandria"
               "iterate"
               "uiop"
               "cffi"
               "fiveam"
               "lparallel"
               "policy-cond"
               "polymorphic-functions"
               "dense-arrays+static-vectors"
               "trivial-coerce"
               "trivial-package-local-nicknames")
  ;; TODO: Use CFFI-GROVEL or something to manage shared library / c files
  :components ((:file "package")
               (:file "utils"                 :depends-on ("package"))
               (:file "ptr-iterate-but-inner" :depends-on ("utils"))
               (:file "lparallel"             :depends-on ("ptr-iterate-but-inner"))
               (:file "test"                  :depends-on ("package"))
               (:file "translations"          :depends-on ("package"))
               (:file "copy-coerce"           :depends-on ("utils"))
               (:file "one-arg-fn-float"      :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"
                                                           "lparallel"
                                                           "ptr-iterate-but-inner"))
               (:file "one-arg-fn-all"        :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"
                                                           "lparallel"
                                                           "ptr-iterate-but-inner"))
               (:file "two-arg-fn-float"      :depends-on ("utils"
                                                           "test"
                                                           "lparallel"
                                                           "translations"
                                                           "ptr-iterate-but-inner"))
               (:file "two-arg-fn-non-broadcast" :depends-on ("utils"
                                                              "translations"
                                                              "test"
                                                              "ptr-iterate-but-inner"))
               (:file "bitwise"               :depends-on ("copy-coerce"
                                                           "translations"
                                                           "test"
                                                           "lparallel"
                                                           "ptr-iterate-but-inner"))
               (:file "n-arg-fn"              :depends-on ("one-arg-fn-float"
                                                           "bitwise"
                                                           "two-arg-fn-non-broadcast"))
               (:file "n-arg-fn-compiler-macros" :depends-on ("n-arg-fn"
                                                              "two-arg-fn-non-broadcast"))
               (:file "n-arg-fn-tests"        :depends-on ("n-arg-fn-compiler-macros"))
               (:file "blas"                  :depends-on ("package"
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

