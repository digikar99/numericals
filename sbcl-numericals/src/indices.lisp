(in-package :sbcl-numericals.internals)

(defparameter 0001+ (%make-simd-pack-single 0.0 0.0 0.0 1.0))
(defparameter 0010+ (%make-simd-pack-single 0.0 0.0 1.0 0.0))
(defparameter 0100+ (%make-simd-pack-single 0.0 1.0 0.0 0.0))
(defparameter 1000+ (%make-simd-pack-single 1.0 0.0 0.0 0.0))

(defun 0001+ (index)
  (declare (optimize (speed 3)))
  (s4+ index 0001+))

(defun 0010+ (index)
  (declare (optimize (speed 3)))
  (s4+ index 0010+))

(defun 0100+ (index)
  (declare (optimize (speed 3)))
  (s4+ index 0100+))

(declaim (inline 1000+))
(defun 1000+ (index)
  (declare (optimize (speed 3)))
  (s4+ index 1000+))

(%make-simd-pack-ub32 0 0 0 1)

(defun foo ()
  (declare (optimize (speed 3)))
  (let ((start (%make-simd-pack-single 0.0 0.0 0.0 0.0)))
    (loop until (= 1000000 (floor (%simd-pack-singles start)))
       do (setq start (1000+ start)))))
