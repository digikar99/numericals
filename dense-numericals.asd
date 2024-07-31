(uiop:define-package :dense-numericals
  (:use))

(defsystem "dense-numericals/common"
  :pathname ""
  :version "0.1.0"
  :depends-on ("alexandria"
               "cffi"
               "peltadot"
               "peltadot-traits-library")
  :components ((:file "common")))

(defsystem "dense-numericals/utils"
  :pathname "dense-numericals-src/utils/"
  :depends-on ("dense-arrays-plus-lite"
               "dense-numericals/common"
               "iterate"
               "lparallel"
               ;; This system does not require policy-cond itself,
               ;; but most of who depend on it do require it.
               "policy-cond")
  :description "More utilities for dense-numericals. These require PELTADOT."
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "primitives")
               (:file "translations")
               (:file "lparallel")
               (:file "ptr-iterate-but-inner")
               (:file "simple-array-broadcast")))

(defsystem "dense-numericals/basic-math"
  :pathname "dense-numericals-src/basic-math/"
  :depends-on ("dense-numericals/utils"
               "bmas"
               "iterate")
  :serial t
  :perform (test-op (o c)
             (load-system "dense-numericals/basic-math/tests")
             (symbol-call :fiveam :run! :dense-numericals))
  :components ((:file "package")
               (:file "fixnum-c-funs")
               (:file "copy-coerce")
               (:file "one-arg-fn-all")
               (:file "one-arg-fn-float")
               (:file "lognot")
               (:file "two-arg-fn-logical")
               (:file "two-arg-fn-comparison")
               (:file "two-arg-fn-non-comparison")
               (:file "in-place-operators")
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
               (:file "sum")
               (:file "maximum")
               (:file "minimum")
               (:file "arg-maximum")
               (:file "arg-minimum")))

;; This has been separated out here, because the compilation time of this is significant
(defsystem "dense-numericals/basic-math/tests"
  :pathname "dense-numericals-src/basic-math/"
  :depends-on ("dense-numericals/basic-math")
  :components ((:file "test-definition-macros")
               (:file "tests")
               (:file "n-arg-fn-tests")))

(defsystem "dense-numericals/transcendental"
  :pathname "dense-numericals-src/transcendental/"
  :depends-on ("dense-numericals/basic-math")
  :serial t
  :components ((:file "package")
               (:file "one-arg-fn-float")
               (:file "two-arg-fn-float")
               (:file "in-place-operators")))

(defsystem "dense-numericals/transcendental/tests"
  :pathname "dense-numericals-src/transcendental/"
  :depends-on ("dense-numericals/transcendental"
               "dense-numericals/basic-math/tests")
  :components ((:file "tests")))

(defsystem "dense-numericals/statistics"
  :pathname "dense-numericals-src/statistics/"
  :depends-on ("dense-numericals/basic-math"
               "dense-numericals/transcendental")
  :serial t
  :components ((:file "package")
               (:file "mean")
               (:file "variance")
               (:file "std")))

(defsystem "dense-numericals/linalg"
  :pathname "dense-numericals-src/linalg/"
  :depends-on ("dense-numericals/utils"
               "bmas"
               "ceigen-lite")
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "vdot")
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
               (:file "eig")))

(defsystem "dense-numericals/random"
  :pathname "dense-numericals-src/random/"
  :depends-on ("dense-numericals/utils"
               "ceigen-lite")
  :components ((:file "package")
               (:file "seed")
               (:file "gaussian")       ; empty, c-name
               (:file "beta")
               (:file "chisquare")))

(defsystem "dense-numericals"
  :pathname "dense-numericals-src/"
  :version "2024.08.0"                  ; year, month, patch
  :description "A high performance numerical computing library for Common Lisp (focus: basic math operations)"
  :license "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :depends-on ("dense-numericals/common"
               "dense-numericals/basic-math"
               "dense-numericals/transcendental"
               "dense-numericals/linalg"
               "dense-numericals/random"
               "dense-numericals/statistics")
  :components (;; FIXME: with-elementwise-operations
               ;; It's not sanely possible to implement this without SB-SIMD or CL-SIMD
               ;; or being able to call CFFI with SIMD packs, because of cases like
               ;;   (nu:weop c (+ (+ a b) (+ b b)))
               ;; ^This necessitates temporary memory allocation
               ;; This operation stays important for large arrays because of cpu caches
               ;; (:file "with-elementwise-operations")
               ;; (:file "outer")
               )
  :in-order-to ((test-op (test-op "dense-numericals/tests"))))

(defsystem "dense-numericals/magicl"
  :pathname "dense-numericals-src/magicl/"
  :depends-on ("dense-numericals/utils"
               "magicl")
  :components ((:file "magicl-wrapper")
               (:file "magicl")))

(defsystem "dense-numericals/benchmarks"
  :pathname "benchmarks/"
  :version "0.1.0"
  :depends-on ("alexandria"
               "cl-ascii-table"
               "dense-numericals"
               "py4cl2"
               "fiveam"
               "jsown-utils")
  :components ((:file "benchmark")
               (:file "one-arg-fn")
               (:file "two-arg-fn")))

(defsystem "dense-numericals/tests"
  :depends-on ("dense-numericals"
               "dense-numericals/basic-math/tests"
               "dense-numericals/transcendental/tests")
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string
                    "(LET ((5AM:*ON-ERROR* :DEBUG)
                           (5AM:*ON-FAILURE* :DEBUG)
                           (COMPILER-MACRO-NOTES:*MUFFLED-NOTES-TYPE* T))
                       (5AM:RUN! :DENSE-NUMERICALS))"))))
