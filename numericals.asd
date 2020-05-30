(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("alexandria"
               "iterate")
  :components ((:file "package")
               (:module "sbcl"
                        :components
                        ((:file "accessors")
                         (:file "1d-storage-array")
                         (:file "arithmetic")))
               (:file "primitives")
               (:file "broadcast")
               (:file "with-simd-operations")
               (:file "arithmetic")
               (:file "aref")
               (:file "concatenate")
               (:file "outer")))

(asdf:defsystem "numericals/tests"
  :pathname "tests/"
  :version "0.1.0"
  :serial t
  :depends-on ("alexandria"
               "iterate"
               "py4cl2"
               "cl-ppcre"
               "numericals"
               "fiveam"
               "cl-who")
  :components ((:file "package")
               (:file "arithmetic")
               (:file "concatenate")
               (:static-file "readme" :pathname #P"../README.md")))
