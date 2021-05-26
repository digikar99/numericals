(in-package :numericals.internals)

(5am:def-suite :numericals)
(5am:def-suite nu::array :in :numericals)

(defmacro define-numericals-one-arg-test
    (name array-type
     (single-float-error
      &optional (single-float-min 0.0f0) (single-float-max 1.0f0))
     (double-float-error
      &optional (double-float-min 0.0d0) (double-float-max 1.0d0)))

  (flet ((verification-form (type error min max)
           `(progn
              (flet ((float-close-p (x y)
                       (let ((close-p (or (= x y)
                                          (< (/ (abs (- x y)) (abs x))
                                             ,error))))
                         (if close-p
                             t
                             (progn
                               (print (list x y))
                               nil)))))
                (let ((nu:*multithreaded-threshold* 10000000))
                  (5am:is-true (let ((rand (nu:rand 1000 :type ',type :min ,min :max ,max)))
                                 (nu:array= (aops:each* ',type ',name rand)
                                            (,name rand)
                                            :test #'float-close-p))
                               "Simplest case")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 10000)
                                      (rand (nu:rand 2 nu:*multithreaded-threshold*
                                                     :type ',type :min ,min :max ,max)))
                                 (nu:array= (aops:each* ',type ',name rand)
                                            (,name rand)
                                            :test #'float-close-p))
                               "Simple multithreaded")
                  (5am:is-true (let* ((rand (cl:make-array '(50 100)
                                                           :element-type ',type
                                                           :displaced-to
                                                           (array-storage
                                                            (nu:rand '(100 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max))
                                                           :displaced-index-offset 2500)))
                                 (nu:array= (aops:each* ',type ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple arrays")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand (cl:make-array '(500 100)
                                                           :element-type ',type
                                                           :displaced-to
                                                           (array-storage
                                                            (nu:rand '(1000 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max))
                                                           :displaced-index-offset 25000)))
                                 (nu:array= (aops:each* ',type ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple multithreaded"))))))

    `(5am:def-test ,name (:suite ,array-type)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max))))


;; TODO: Add tests for broadcasting

(defmacro define-numericals-two-arg-test
    (name array-type
     (single-float-error
      &optional (single-float-min 0.0f0) (single-float-max 1.0f0)
        (single-float-return-type nil))
     (double-float-error
      &optional (double-float-min 0.0d0) (double-float-max 1.0d0)
        (double-float-return-type nil)))

  (flet ((verification-form (type error min max return-type)
           `(progn
              (flet ((close-p (x y)
                       (or (= x y)
                           (< (/ (abs (- x y)) (abs x))
                              ,error))))
                (let ((nu:*multithreaded-threshold* 1000000000))
                  (5am:is-true (let* ((rand1 (nu:rand 1000 :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand 1000 :type ',type :min ,min :max ,max)))
                                 (nu:array= (aops:each* ',return-type ',name rand1 rand2)
                                            (,name rand1 rand2)
                                            :test #'close-p))
                               "Simplest case 1")
                  (5am:is-true (let* ((rand1 (nu:rand 1000 :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand 1000 :type ',type :min ,min :max ,max))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (aops:each* ',return-type ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Simplest case 2")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 10000)
                                      (rand1 (nu:rand 2 nu:*multithreaded-threshold*
                                                      :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand 2 nu:*multithreaded-threshold*
                                                      :type ',type :min ,min :max ,max))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (aops:each* ',return-type ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Simple Multithreaded")
                  ;; FIXME: Add a test for broadcasting
                  (5am:is-true (let* ((rand1 (cl:make-array '(10 50)
                                                            :element-type ',type
                                                            :displaced-to
                                                            (nu:rand '(100 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max)
                                                            :displaced-index-offset 2000))
                                      (rand2 (cl:make-array '(10 50)
                                                            :element-type ',type
                                                            :displaced-to
                                                            (nu:rand '(100 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max)
                                                            :displaced-index-offset 3000))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (aops:each* ',return-type ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple arrays")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand1 (cl:make-array '(1000 50)
                                                            :element-type ',type
                                                            :displaced-to
                                                            (nu:rand '(1000 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max)
                                                            :displaced-index-offset 20000))
                                      (rand2 (cl:make-array '(1000 50)
                                                            :element-type ',type
                                                            :displaced-to
                                                            (nu:rand '(1000 100)
                                                                     :type ',type
                                                                     :min ,min :max ,max)
                                                            :displaced-index-offset 20005))
                                      (return-array (cl:make-array '(1000 50)
                                                                   :element-type ',return-type
                                                                   :displaced-to
                                                                   (nu:zeros '(1000 100)
                                                                             :type ',return-type)
                                                                   :displaced-index-offset 30000)))
                                 (nu:array= (aops:each* ',return-type ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple multithreaded"))))))

    `(5am:def-test ,name (:suite ,array-type)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max single-float-return-type)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max double-float-return-type))))
