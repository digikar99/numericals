(in-package :dense-numericals.impl)

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
                (let ((dn:*multithreaded-threshold* 10000000))
                  (5am:is-true (let ((rand (rand 1000 :type ',type :min ,min :max ,max)))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand)
                                         :test #'float-close-p))
                               "Simplest case")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 10000)
                                      (rand (rand 2 dn:*multithreaded-threshold*
                                                  :type ',type :min ,min :max ,max)))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand)
                                         :test #'float-close-p))
                               "Simple multithreaded inners")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 10000)
                                      (rand (rand dn:*multithreaded-threshold* 2
                                                  :type ',type :min ,min :max ,max)))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand)
                                         :test #'float-close-p))
                               "Simple multithreaded outers")
                  (5am:is-true (let* ((rand (aref (rand '(100 100) :type ',type
                                                                   :min ,min :max ,max)
                                                  '(10 :step 2))))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p))
                               "Non-simple arrays 1")
                  (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type
                                                                  :min ,min :max ,max)
                                                 '(10 :step 2)
                                                 '(10 :step 2))))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p))
                               "Non-simple arrays 2")
                  (5am:is-true (let ((rand (aref (rand '(100 100) :type ',type
                                                                  :min ,min :max ,max)
                                                 nil
                                                 '(10 :step -2))))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p))
                               "Non-simple arrays 3")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 1000)
                                      (rand (aref (rand '(1000 100) :type ',type
                                                                    :min ,min :max ,max)
                                                  '(10 :step 2)
                                                  '(10 :step 2))))
                                 (array= (macro-map-array nil ',name rand)
                                         (,name rand :out rand)
                                         :test #'float-close-p))
                               "Non-simple multithreaded")
                  (5am:is-true (let* ((array (rand '(2 3) :type ',type
                                                          :min ,min :max ,max))
                                      (orig  (list (aref array 0 0)
                                                   (aref array 0 2)
                                                   (aref array 1 0)
                                                   (aref array 1 2))))
                                 (,name (aref array nil 1)
                                        :out (aref array nil 1))
                                 (equalp orig
                                         (list (aref array 0 0)
                                               (aref array 0 2)
                                               (aref array 1 0)
                                               (aref array 1 2))))
                               "Inplace only"))))))

    `(5am:def-test ,name (:suite ,array-type)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max))))


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
                (let ((dn:*multithreaded-threshold* 1000000000))
                  (5am:is-true (let* ((rand1 (rand 1000 :type ',type :min ,min :max ,max))
                                      (rand2 (rand 1000 :type ',type :min ,min :max ,max))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2)
                                         :test #'close-p))
                               "Simplest case")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 10000)
                                      (rand1 (rand 2 dn:*multithreaded-threshold*
                                                   :type ',type :min ,min :max ,max))
                                      (rand2 (rand 2 dn:*multithreaded-threshold*
                                                   :type ',type :min ,min :max ,max))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2)
                                         :test #'close-p))
                               "Simple Multithreaded insides")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 10000)
                                      (rand1 (rand dn:*multithreaded-threshold* 2
                                                   :type ',type :min ,min :max ,max))
                                      (rand2 (rand dn:*multithreaded-threshold* 2
                                                   :type ',type :min ,min :max ,max))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2)
                                         :test #'close-p))
                               "Simple Multithreaded outsides")
                  (5am:is-true (let* ((rand1 (aref (rand '(100 100) :type ',type
                                                                    :min ,min :max ,max)
                                                   '(10 :step 2)))
                                      (rand2 (aref (rand '(200 100) :type ',type
                                                                    :min ,min :max ,max)
                                                   '(20 :step 4)))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2 :out return-array)
                                         :test #'close-p))
                               "Non-simple arrays 1")
                  (5am:is-true (let* ((rand1 (aref (rand '(100 100) :type ',type
                                                                    :min ,min :max ,max)
                                                   '(10 :step 2)
                                                   '(10 :step 2)))
                                      (rand2 (aref (rand '(100 200) :type ',type
                                                                    :min ,min :max ,max)
                                                   '(10 :step 2)
                                                   '(20 :step 4)))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2 :out return-array)
                                         :test #'close-p))
                               "Non-simple arrays 2")
                  (5am:is-true (let* ((dn:*multithreaded-threshold* 1000)
                                      (rand1 (aref (rand '(1000 100) :type ',type
                                                                     :min ,min :max ,max)
                                                   '(10 :step 2)))
                                      (rand2 (aref (rand '(2000 100) :type ',type
                                                                     :min ,min :max ,max)
                                                   '(20 :step 4)))
                                      (return-array (aref (zeros '(1000 100)
                                                                 :type ',return-type)
                                                          '(10 :step 2))))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2 :out return-array)
                                         :test #'close-p))
                               "Non-simple multithreaded")
                  (5am:is-true (let* ((rand1 (aref (rand '(100 100) :type ',type
                                                                    :min ,min :max ,max)
                                                   nil
                                                   '(10 :step -2)))
                                      (rand2 (aref (rand '(100 200) :type ',type
                                                                    :min ,min :max ,max)
                                                   nil
                                                   '(20 :step -4)))
                                      (return-array (zeros (array-dimensions rand1)
                                                           :type ',return-type)))
                                 (array= (macro-map-array return-array ',name rand1 rand2)
                                         (,name rand1 rand2 :out return-array)
                                         :test #'close-p)))
                  (5am:is-true (let* ((array (rand '(2 3) :type ',type
                                                          :min ,min :max ,max))
                                      (return-array (zeros '(2 3) :type ',return-type))
                                      (orig  (list (aref array 0 0)
                                                   (aref array 0 2)
                                                   (aref array 1 0)
                                                   (aref array 1 2))))
                                 (,name (aref array nil 1)
                                        (aref array nil 1)
                                        :out (aref return-array nil 1))
                                 (equalp orig
                                         (list (aref array 0 0)
                                               (aref array 0 2)
                                               (aref array 1 0)
                                               (aref array 1 2))))))))))

    `(5am:def-test ,name (:suite ,array-type)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max single-float-return-type)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max double-float-return-type))))
