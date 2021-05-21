(asdf:defsystem "numericals/helper"
  :pathname ""
  :depends-on ("alexandria" "iterate")
  :components ((:file "helper")))

(asdf:defsystem "numericals/sbcl"
  :pathname "sbcl/"
  :depends-on ("numericals/helper"
               "alexandria"
               "iterate")
  :components ((:file "package")
               (:file "accessors")
               (:file "1d-storage-array")
               (:file "arithmetic")
               (:file "arithmetic-single")
               (:file "arithmetic-double")
               (:file "boolean")))

(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("numericals/helper"
               #+sbcl "numericals/sbcl"
               "alexandria"
               "iterate"
               "trivial-types"
               "introspect-environment")
  :components ((:file "package")
               (:file "primitives")
               (:module "broadcast"
                        :components
                        ((:file "broadcast-core")
                         (:file "broadcast")
                         (:file "broadcast-operations")))
               (:file "with-elementwise-operations")
               (:file "arithmetic")
               (:file "outer")
               (:file "concatenate")
			   (:file "aref")
			   (:file "transpose")))

(asdf:defsystem "numericals/tests"
  :pathname "tests/"
  :version "0.1.0"
  :serial t
  :depends-on ("numericals"
               "alexandria"
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
			   (:file "transpose")
               (:static-file "readme" :pathname #P"../README.md")))
