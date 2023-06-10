
(defsystem "numericals.common"
  :pathname ""
  :version "0.1.0"
  :depends-on ("alexandria"
               "fiveam"
               "cl-form-types"
               "polymorphic-functions")
  :components ((:file "common")))
