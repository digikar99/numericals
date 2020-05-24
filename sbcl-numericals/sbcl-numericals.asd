(asdf:defsystem "sbcl-numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :depends-on ("alexandria" "trivial-package-local-nicknames" "iterate")
  :components ((:file "package")
               (:file "accessors")
               (:file "internals")
               (:file "sbcl-numericals")
               (:file "dot")))
