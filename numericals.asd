(uiop:define-package :numericals
  (:use))

(defsystem "numericals/common"
  :pathname ""
  :version "0.1.0"
  :depends-on ("alexandria"
               "cffi"
               "peltadot"
               "peltadot-traits-library")
  :components ((:file "common")))

(defsystem "numericals/utils"
  :pathname "src/utils/"
  :depends-on ("alexandria")
  :description "A collection of minimal-dependency utilities for numericals. These do not require PELTADOT."
  :components ((:file "utils")))

(defsystem "numericals/more-utils"
  :pathname "src/more-utils/"
  :depends-on ("numericals/common"
               "numericals/utils"
               "iterate"
               ;; This system does not require policy-cond itself,
               ;; but most of who depend on it do require it.
               "policy-cond")
  :description "More utilities for numericals. These require PELTADOT."
  :serial t
  :components ((:file "package")
               (:file "more-utils")
               (:file "asarray")
               (:file "translations")
               (:file "aref")
               (:file "broadcast")
               (:File "transpose")
               (:file "ptr-iterate-but-inner")
               (:file "simple-array-broadcast")))

(defsystem "numericals/basic-math"
  :pathname "src/basic-math/"
  :depends-on ("numericals/more-utils"
               "bmas"
               "iterate")
  :serial t
  :perform (test-op (o c)
             (load-system "numericals/basic-math/tests")
             (symbol-call :fiveam :run! :numericals))
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
(defsystem "numericals/basic-math/tests"
  :pathname "src/basic-math/"
  :depends-on ("numericals/basic-math")
  :components ((:file "test-definition-macros")
               (:file "tests")
               (:file "n-arg-fn-tests")))

(defsystem "numericals/transcendental"
  :pathname "src/transcendental/"
  :depends-on ("numericals/basic-math"
               "bmas"
               "lparallel")
  :serial t
  :components ((:file "package")
               (:file "lparallel")
               (:file "one-arg-fn-float")
               (:file "two-arg-fn-float")
               (:file "in-place-operators")))

(defsystem "numericals/transcendental/tests"
  :pathname "src/transcendental/"
  :depends-on ("numericals/transcendental"
               "numericals/basic-math/tests")
  :components ((:file "tests")))

(defsystem "numericals/statistics"
  :pathname "src/statistics/"
  :depends-on ("numericals/basic-math"
               "numericals/transcendental")
  :serial t
  :components ((:file "package")
               (:file "mean")
               (:file "variance")
               (:file "std")))

(defsystem "numericals/linalg"
  :pathname "src/linalg/"
  :depends-on ("numericals/more-utils"
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

(defsystem "numericals/random"
  :pathname "src/random/"
  :depends-on ("numericals/more-utils"
               "ceigen-lite")
  :components ((:file "package")
               (:file "seed")
               (:file "gaussian")       ; empty, c-name
               (:file "beta")
               (:file "chisquare")))

(defsystem "numericals"
  :pathname "src/"
  :version "2024.08.0"                  ; year, month, patch
  :description "A high performance numerical computing library for Common Lisp (focus: basic math operations)"
  :license "MIT"
  :author "Shubhamkar Ayare (shubhamayare@yahoo.co.in)"
  :depends-on ("numericals/common"
               "numericals/basic-math"
               "numericals/transcendental"
               "numericals/linalg"
               "numericals/random"
               "numericals/statistics")
  :components (;; FIXME: with-elementwise-operations
               ;; It's not sanely possible to implement this without SB-SIMD or CL-SIMD
               ;; or being able to call CFFI with SIMD packs, because of cases like
               ;;   (nu:weop c (+ (+ a b) (+ b b)))
               ;; ^This necessitates temporary memory allocation
               ;; This operation stays important for large arrays because of cpu caches
               ;; (:file "with-elementwise-operations")
               ;; (:file "outer")
               )
  :in-order-to ((test-op (test-op "numericals/tests"))))

(defsystem "numericals/magicl"
  :pathname "src/magicl/"
  :depends-on ("numericals/utils"
               "magicl")
  :components ((:file "magicl-wrapper")
               (:file "magicl")))

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

(defsystem "numericals/tests"
  :depends-on ("numericals"
               "numericals/basic-math/tests"
               "numericals/transcendental/tests")
  :perform (test-op (o c)
             (declare (ignore o c))
             (eval (read-from-string
                    "(LET ((5AM:*ON-ERROR* :DEBUG)
                           (5AM:*ON-FAILURE* :DEBUG)
                           (COMPILER-MACRO-NOTES:*MUFFLED-NOTES-TYPE* T))
                       (5AM:RUN! :NUMERICALS))"))))
