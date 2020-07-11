(asdf:defsystem "numericals+array"
  :pathname "src+array/"
  :version "0.1.0"
  :serial t
  :depends-on ("numericals/helper"
               #+sbcl "numericals/sbcl"
               "numericals/array"
               "alexandria"
               "iterate"
               "trivial-types"
               "introspect-environment")
  :components ((:file "package+array")
               (:file "primitives")
               (:module "broadcast"
                        :components
                        ((:file "broadcast-core")
                         (:file "broadcast+array")
                         (:file "broadcast-operations")))
               (:file "with-elementwise-operations")
               (:file "arithmetic")
               (:file "concatenate")
               (:file "outer")))

(asdf:defsystem "numericals+array/tests"
  :pathname "tests+array/"
  :version "0.1.0"
  :serial t
  :depends-on ("numericals+array"
               "alexandria"
               "iterate"
               "py4cl2"
               "cl-ppcre"
               "numericals"
               "fiveam"
               "cl-who")
  :components ((:file "package+array")
               (:file "primitives")
               (:file "arithmetic")
               (:file "concatenate")
               (:file "outer")
			   (:file "transpose")
               (:static-file "readme" :pathname #P"../README.md")))
