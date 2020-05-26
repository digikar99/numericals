(defpackage :numericals/tests
  (:use :cl :alexandria :py4cl2 :iterate :fiveam)
  (:local-nicknames (:nu :numericals))
  (:export :run-tests))

(in-package :numericals/tests)

(defpymodule "numpy" nil :lisp-package "NP")
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

(defun run-tests (&optional write-to-readme)
  (let* ((*write-to-readme* write-to-readme))
    (if write-to-readme
        (write-to-readme
         (let ((rows (with-output-to-string (*write-to-readme-stream*)
                       (5am:run! :numericals))))
           (who:with-html-output-to-string (s nil :indent t)
             (:div :id "benchmark"
                   (:p "SBCL is faster than NUMPY by (horizontal indicates array sizes; vertical indicates various operations): ")
                   (:table (who:str rows))))))
        (5am:run! :numericals))))
