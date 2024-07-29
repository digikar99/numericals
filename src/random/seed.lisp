(in-package :numericals/random)

(defun seed (unsigned-byte-64)
  (declare (type (unsigned-byte 64) unsigned-byte-64))
  (ceigen-lite:seed unsigned-byte-64))
