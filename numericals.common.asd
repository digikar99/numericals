(defsystem "numericals.common"
  :pathname ""
  :version "0.1.0"
  :depends-on ("alexandria"
               "cffi"
               "peltadot"
               "peltadot-traits-library")
  :components ((:file "common")))
