(asdf:defsystem "sbcl-numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "internals-single")
               (:file "internals-double")
               (:file "sbcl-numericals")))
