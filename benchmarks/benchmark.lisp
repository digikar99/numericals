(numericals.common:compiler-in-package numericals.common:*compiler-package*)

(defstruct fun-report
  (name)
  (array-sizes)
  (lisp)
  (numpy)
  (torch))

(defstruct report
  (element-type)
  (fun-reports))

(defvar *report*)

(defun save-json-report (reports &optional savep)
  ;; Layers: library > element-type > function-name > sizes > framework
  (let*
      ((library (string-downcase (package-name (find-package :nu))))
       (filename (trivial-coerce:coerce
                  (uiop:strcat
                   (trivial-coerce:coerce
                    (uiop:pathname-parent-directory-pathname
                     (asdf:component-pathname (asdf:find-system "numericals")))
                    'cl:string)
                   "docs/" library ".json")
                  'cl:pathname))
       (json
         (jsown:pretty-json
          (jsown:to-json
           (plist-hash-table
            (list library
                  (loop :for report :in reports
                        :collect
                        (plist-hash-table
                         (list (string-downcase (report-element-type report))
                               (loop :for fun-report :in (report-fun-reports report)
                                     :collect
                                     (plist-hash-table
                                      (list (string-downcase (fun-report-name fun-report))
                                            (plist-hash-table
                                             ;; FIXME: Average over array sizes
                                             (with-slots (array-sizes lisp numpy torch) fun-report
                                               (loop :for array-size :in array-sizes
                                                     :for size := (apply #'* array-size)
                                                     :for lisp-time  :in lisp
                                                     :for numpy-time :in numpy
                                                     :for torch-time :in torch
                                                     :for speedup-over-numpy
                                                       := (write-to-string
                                                           (coerce (/ numpy-time lisp-time)
                                                                   'single-float))
                                                     :for speedup-over-torch
                                                       := (write-to-string
                                                           (coerce (/ torch-time lisp-time)
                                                                   'single-float))
                                                     :appending
                                                     (list size
                                                           (plist-hash-table
                                                            (list "numpy"
                                                                  speedup-over-numpy
                                                                  "torch"
                                                                  speedup-over-torch))))))))))))))))))

    (if savep
        (with-output-to-file (f filename :if-exists :supersede)
          (write-string json f))
        (format t json))))

(defun report (reports &optional (stream *standard-output*))
  (let ((s stream)
        (ascii-table:*default-value-formatter*
          (lambda (value)
            (typecase value
              (float (format nil "~,2fx" value))
              (symbol (format nil "~S" value))
              (t (format nil "~A" value))))))
    (format s "ELEMENT-TYPE: ~A~%" (string-downcase (report-element-type report)))
    (dotimes (i 80) (write-char #\= s))
    (terpri s)
    (loop :for fun-report :in (reverse (report-fun-reports report))
          :do (with-slots (name array-sizes lisp numpy torch) fun-report
                (let* ((num-cols (apply #'min (mapcar #'length
                                                      (list array-sizes
                                                            lisp
                                                            numpy
                                                            torch))))
                       (array-sizes (subseq array-sizes 0 num-cols))
                       (lisp  (subseq lisp 0 num-cols))
                       (numpy (subseq numpy 0 num-cols))
                       (torch (subseq torch 0 num-cols)))
                  (ascii-table:display
                   (let ((table (ascii-table:make-table (cons "Library"
                                                              (mapcar #'write-to-string
                                                                      array-sizes))
                                                        :header (string name))))
                     (ascii-table:add-row table
                                          (cons 'numpy
                                                (mapcar #'/ numpy lisp)))
                     (when torch
                       (ascii-table:add-row table
                                            (cons 'torch
                                                  (mapcar #'/ torch lisp))))
                     table)
                   s))))))

(defun numpy-element-type (lisp-element-type)
  (ecase lisp-element-type
    (single-float 'np.float32)
    (double-float 'np.float64)))

(defun torch-element-type (lisp-element-type)
  (ecase lisp-element-type
    (single-float 't.float32)
    (double-float 't.float64)))

(defmacro time-it (&body body)
  (with-gensyms (start end body-result)
    `(let (,start ,end ,body-result)
       (setq ,start (/ (get-internal-real-time)
                       1.0 internal-time-units-per-second))
       (setq ,body-result (progn ,@body))
       (setq ,end (/ (get-internal-real-time)
                     1.0 internal-time-units-per-second))
       (values (- ,end ,start) ,body-result))))

;;; May need to run: (cffi:foreign-funcall "fedisableexcept" :int -1)
;;; Reference:
;;; - https://stackoverflow.com/questions/19363484/representing-infinity-and-nan-independent-of-implementation
;;; - https://linux.die.net/man/3/fedisableexcept

(defparameter *numpy* t)
(defparameter *torch* #+arm64 nil #-arm64 t)

(defun benchmark (&rest element-types)
  (let (reports)
    (unwind-protect
         (loop :for element-type :in element-types :do
           (let ((nu:*array-element-type* element-type)
                 (*report* (make-report :element-type element-type)))
             (push *report* reports)

             ;; (one-arg-fn '(nu:sin nu:cos nu:tan)
             ;;             '(np.sin np.cos np.tan)
             ;;             '( t.sin  t.cos  t.tan))

             ;; (one-arg-fn '(nu:sinh nu:cosh nu:tanh)
             ;;             '(np.sinh np.cosh np.tanh)
             ;;             '( t.sinh  t.cosh  t.tanh))

             ;; (one-arg-fn '(nu:asin   nu:acos   nu:atan)
             ;;             '(np.arcsin np.arccos np.arctan)
             ;;             '( t.arcsin  t.arccos  t.arctan))

             ;; (one-arg-fn '(nu:asinh   nu:acosh   nu:atanh)
             ;;             '(np.arcsinh np.arccosh np.arctanh)
             ;;             '( t.arcsinh  t.arccosh  t.arctanh))

             ;; (two-arg-fn '(nu:expt  nu:atan)
             ;;             '(np.power np.arctan2)
             ;;             '( t.pow    t.atan2))

             ;; (two-arg-fn '(nu:atan)
             ;;             '(np.arctan2)
             ;;             '(t.atan2))
             ;; (two-arg-fn '(nu:expt)
             ;;             '(np.power)
             ;;             '( t.pow))
             (two-arg-fn '(nu:two-arg-+   nu:two-arg--        nu:two-arg-*        nu:two-arg-/)
                         '(np.add np.subtract np.multiply np.divide)
                       '( t.add  t.subtract  t.multiply  t.divide))
             ))
      (return-from benchmark (values-list reports)))))
