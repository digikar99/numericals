(in-package :sbcl-numericals)
(defparameter float-list (loop for i below 1024
                            collect (loop for j below 1024
                                       collect (+ i j 0.1))))
(defparameter arr-a (make-array '(1024 1024) :initial-contents float-list
                                :element-type 'single-float))
(defparameter arr-b (make-array '(1024 1024) :initial-contents float-list
                                :element-type 'single-float))
(defparameter arr-c (make-array '(1024 1024) :initial-element 0.0
                                :element-type 'single-float))

(defparameter double-list (loop for i below 1024
                             collect (loop for j below 1024
                                        collect (+ i j 0.1d0))))
(defparameter arr-a (make-array '(1024 1024) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-b (make-array '(1024 1024) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-c (make-array '(1024 1024) :initial-element 0.0d0
                                :element-type 'double-float))


