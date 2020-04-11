(asdf:defsystem "numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("alexandria"
               "iterate"
               "with")
  :components ((:file "package")
               (:module "sbcl"
                        :components
                        ((:file "accessors")
                         (:file "1d-storage-array")
                         (:file "arithmetic")))
               (:file "with-simd-operations")))
