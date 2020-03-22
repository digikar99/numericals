(asdf:defsystem "sbcl-numericals"
  :pathname "src/"
  :version "0.1.0"
  :serial t
  :components ((:file "package")
               (:file "avx-single")
               (:file "avx-double")
               (:file "sse-single")
               (:file "sse-double")
               (:file "sbcl-numericals")))
