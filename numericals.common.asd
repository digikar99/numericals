
(defsystem "numericals.common"
  :pathname ""
  :version "0.1.0"
  :depends-on ("alexandria"
               "fiveam"
               "cffi"
               "peltadot")
  :components ((:file "common")))
