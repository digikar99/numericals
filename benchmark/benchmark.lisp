(in-package :cl-user)
(use-package :alexandria)

(defmacro time-average (n &body body)
  "N-average the execution time of BODY in seconds"
  (with-gensyms (start end)
    `(let (,start ,end)
       (setq ,start (get-internal-real-time))
       (loop for i below ,n
          do ,@body)
       (setq ,end (get-internal-real-time))
       (/ (- ,end ,start) ,n internal-time-units-per-second))))

(defmacro time-total (n &body body)
  "N-average the execution time of BODY in seconds"
  (with-gensyms (start end)
    `(let (,start ,end)
       (setq ,start (get-internal-real-time))
       (loop for i below ,n
          do ,@body)
       (setq ,end (get-internal-real-time))
       (coerce (/ (- ,end ,start) internal-time-units-per-second)
               'float))))

(defun benchmark (&rest v-lens)
  (loop for len in v-lens
     do (format t "Doing ~A~%" len)
     collect (let ((a (make-array len
                                  :element-type 'double-float
                                  :initial-contents
                                  (mapcar (lambda (i) (+ i 0.1d0)) (iota len))))
                   (b (make-array len
                                  :element-type 'double-float
                                  :initial-contents
                                  (mapcar (lambda (i) (+ i 0.2d0)) (iota len))))
                   (r (make-array len
                                  :element-type 'double-float
                                  :initial-element 0.0d0)))
               (time-total (/ 1e8 len) (sbcl-numericals:d+ a b r)))))

(benchmark 10 100 1000 10000 100000 1000000 10000000)
(apply 'benchmark (loop for i from 1 to 10 collect (* 2000 i)))


