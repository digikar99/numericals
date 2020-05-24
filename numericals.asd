(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("trivial-package-local-nicknames"
               "alexandria"
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
