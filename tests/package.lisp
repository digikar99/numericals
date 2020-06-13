(defpackage :numericals/tests
  (:use :cl :alexandria :py4cl2 :iterate :fiveam)
  (:local-nicknames (:nu :numericals))
  (:export :run-speed-tests
           :run-correctness-tests)
  (:import-from :py4cl2
                :*numpy-pickle-index*
                :numpy-pickle-lower-bound
                :numpy-pickle-lower-bound
                :pythonize)
  #.(append '(:shadowing-import-from :numericals)
            cl::+cl-array-symbols+))
(in-package :numericals/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod pythonize :around (obj)
             (if (typep obj 'array)
                 (progn
                   (when (and (config-var 'numpy-pickle-lower-bound)
                              (config-var 'numpy-pickle-location)
                              (>= (array-total-size obj)
                                  (config-var 'numpy-pickle-lower-bound)))
                     (let ((filename (concatenate 'string
                                                  (config-var 'numpy-pickle-location)
                                                  ".to." (write-to-string (incf *numpy-pickle-index*)))))
                       (numpy-file-format:store-array obj filename)
                       (return-from pythonize
                         (concatenate 'string "_py4cl_load_pickled_ndarray('"
                                      filename"')"))))
                   
                   ;; Handle case of empty array
                   (if (= (array-total-size obj) 0)
                       (return-from pythonize "[]"))
                   
                   ;; First convert the array to 1D [0,1,2,3,...]
                   (let ((array1d (with-output-to-string (stream)
                                    (write-char #\[ stream)
                                    (princ (pythonize (row-major-aref obj 0)) stream)
                                    (do ((indx 1 (1+ indx)))
                                        ((>= indx (array-total-size obj)))
                                      (write-char #\, stream)
                                      (princ (pythonize (row-major-aref obj indx)) stream))
                                    (write-char #\] stream))))
                     (if (= (array-rank obj) 1)
                         ;; 1D array return as-is
                         array1d
                         ;; Multi-dimensional array. Call NumPy to resize
                         (concatenate 'string
                                      "_py4cl_numpy.resize(" array1d ", "
                                      (pythonize (array-dimensions obj)) ")"))))
                 (call-next-method))))


(py4cl2:defpymodule "numpy" nil :lisp-package "NP")
(def-suite :numericals)

(defparameter *write-to-readme* nil)
(defparameter *write-to-readme-stream* nil)

(defun write-to-readme (string)
  (let* ((readme-pathname (asdf:component-pathname
                           (asdf:find-component "numericals/tests"
                                                "readme")))
         (readme (read-file-into-string readme-pathname)))
    (assert (ppcre:scan-to-strings "(?s)<div id='benchmark'>.*</div>" string) nil
            "STRING must be of the for <div id='benchmark'>.*</div> (possibly with newlines)")
    (write-string-into-file (ppcre:regex-replace "(?s)<div id='benchmark'>.*</div>"
                                                 readme string)
                            readme-pathname
                            :if-exists :supersede)
    t))

(defun conc (&rest strings)
  (apply #'concatenate 'string (mapcar (lambda (s) (if s s "")) strings)))

(defun run-speed-tests (&optional write-to-readme)
  (let* ((*write-to-readme* write-to-readme))
    (if write-to-readme
        (write-to-readme
         (let ((rows (with-output-to-string (*write-to-readme-stream*)
                       (5am:run! 'speed))))
           (who:with-html-output-to-string (s nil :indent t)
             (:div :id "benchmark"
                   (:p "SBCL is faster than NUMPY by (horizontal indicates array sizes; vertical indicates various operations): ")
                   (:table (who:str rows))))))
        (5am:run! 'speed))))


(def-suite correctness :in :numericals)
(def-suite speed :in :numericals)

(defun run-correctness-tests ()
  (pyexec "import numpy as np")
  (5am:results-status (5am:run 'correctness)))
