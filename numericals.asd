(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("alexandria"
               "iterate"
               "trivial-types"
               "introspect-environment")
  :components ((:file "package")
               (:module "sbcl"
                        :components
                        ((:file "accessors")
                         (:file "1d-storage-array")
                         (:file "arithmetic")
                         (:file "arithmetic-single")
                         (:file "arithmetic-double")
                         (:file "boolean")))
               (:module "array"
                        :components
                        ((:file "array")
                         (:file "compiler-macros")))
               (:file "primitives")
               (:file "broadcast")
               (:file "with-elementwise-operations")
               (:file "arithmetic")
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
               (:file "primitives")
               (:file "arithmetic")
               (:file "concatenate")
               (:file "outer")
               (:static-file "readme" :pathname #P"../README.md")))
