(defpackage :numericals+array/tests
  (:use :cl :alexandria :py4cl2 :iterate :fiveam)
  (:local-nicknames (:nu :numericals+array))
  (:export :run-speed-tests
           :run-correctness-tests)
  (:import-from :py4cl2
                :*numpy-pickle-index*
                :numpy-pickle-lower-bound
                :numpy-pickle-lower-bound
                :pythonize)
  #.(append '(:shadowing-import-from :numericals+array)
            numericals.helper:+cl-array-symbols+))

(setq numericals.helper:*numericals-tests-package* :numericals+array/tests)

(in-package :numericals+array/tests)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pyexec "def _pythonize_numericals_array(array, strides):
  array.strides = tuple(s*8 for s in strides) # double-float
  return array
"))

(defmethod pythonize ((object numericals.array::numericals-array))
  
  ;; Handle case of empty array
  (if (= (array-total-size object) 0)
      (return-from pythonize "_py4cl_numpy.array([])"))
  
  ;; First convert the array to 1D [0,1,2,3,...]
  (let ((array1d (with-output-to-string (stream)
                   (write-char #\[ stream)
                   (princ (pythonize (row-major-aref object 0)) stream)
                   (do ((indx 1 (1+ indx)))
                       ((>= indx (array-total-size object)))
                     (write-char #\, stream)
                     (princ (pythonize (row-major-aref object indx)) stream))
                   (write-char #\] stream))))
    (concatenate 'string
                 "_pythonize_numericals_array(_py4cl_numpy.resize(" array1d ", "
                 (pythonize (array-dimensions object)) "), "
                 (pythonize (numericals.array:array-strides object)) ")")))

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
