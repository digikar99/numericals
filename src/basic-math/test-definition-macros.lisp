(in-package :numericals/basic-math/impl)

(5am:in-suite :numericals)

(defmacro define-numericals-one-arg-test
    (name suite
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
                               ;; (print (list x y))
                               nil)))))
                (let ((nu:*multithreaded-threshold* 10000000)
                      (nu:*broadcast-automatically* t))
                  (5am:is-true (let ((rand (nu:rand 200 :type ',type :min ,min :max ,max)))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand)
                                            :test #'float-close-p))
                               "Simplest case")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand (nu:rand 2 nu:*multithreaded-threshold*
                                                     :type ',type :min ,min :max ,max)))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand)
                                            :test #'float-close-p))
                               "Simple multithreaded inners")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand (nu:rand nu:*multithreaded-threshold* 2
                                                     :type ',type :min ,min :max ,max)))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand)
                                            :test #'float-close-p))
                               "Simple multithreaded outers")
                  (5am:is-true (let* ((rand (nu:aref* (nu:rand '(100 10) :type ',type
                                                                         :min ,min :max ,max)
                                                      '(10 :step 2))))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple arrays 1")
                  (5am:is-true (let ((rand (nu:aref* (nu:rand '(100 100) :type ',type
                                                                         :min ,min :max ,max)
                                                     '(10 :step 2)
                                                     '(10 :step 2))))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple arrays 2")
                  (5am:is-true (let ((rand (nu:aref* (nu:rand '(10 100) :type ',type
                                                                        :min ,min :max ,max)
                                                     nil
                                                     '(10 :step -2))))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple arrays 3")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand (nu:aref* (nu:rand '(100 100) :type ',type
                                                                          :min ,min :max ,max)
                                                      '(10 :step 2)
                                                      '(10 :step 2))))
                                 (nu:array= (nu:macro-map-array nil ',name rand)
                                            (,name rand :out rand)
                                            :test #'float-close-p))
                               "Non-simple multithreaded")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand (nu:aref* (nu:rand '(100 100) :type ',type
                                                                          :min ,min :max ,max)
                                                      '(10 :step 2)
                                                      '(10 :step 2))))
                                 (nu:array= (nu:broadcast-array (nu:macro-map-array nil ',name rand)
                                                                '(10 45 45))
                                            (,name rand :out (nu:zeros '(10 45 45) :type ',type))
                                            :test #'float-close-p))
                               "Non-simple multithreaded broadcast")
                  (5am:is-true (let* ((array (nu:rand '(2 3) :type ',type
                                                             :min ,min :max ,max))
                                      (orig  (list (aref array 0 0)
                                                   (aref array 0 2)
                                                   (aref array 1 0)
                                                   (aref array 1 2))))
                                 (,name (nu:aref* array nil 1)
                                        :out (nu:aref* array nil 1))
                                 (equalp orig
                                         (list (aref array 0 0)
                                               (aref array 0 2)
                                               (aref array 1 0)
                                               (aref array 1 2))))
                               "Inplace only"))))))

    `(5am:def-test ,name (:suite ,suite)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max))))

(defmacro define-numericals-one-arg-test/integers
    (name suite &optional (return-type nil))

  (flet ((verification-form (type min max return-type)
           `(progn
              (let ((nu:*multithreaded-threshold* 1000000000)
                    (nu:*broadcast-automatically* t))
                (5am:is-true (let* ((rand (nu:rand 100 :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand)))
                             "Simplest case")
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand (nu:rand 2 nu:*multithreaded-threshold*
                                                   :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand)))
                             "Simple Multithreaded insides")
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand (nu:rand nu:*multithreaded-threshold* 2
                                                   :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand)))
                             "Simple Multithreaded outsides")
                (5am:is-true (let* ((rand (nu:aref* (nu:rand '(100 10) :type ',type
                                                                       :min ,min :max ,max)
                                                    '(10 :step 2)))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand :out return-array)))
                             "Non-simple arrays 1")
                (5am:is-true (let* ((rand (nu:aref* (nu:rand '(100 100) :type ',type
                                                                        :min ,min :max ,max)
                                                    '(10 :step 2)
                                                    '(10 :step 2)))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand :out return-array)
                                          :test #'=)))
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand (nu:aref* (nu:rand '(100 100) :type ',type
                                                                        :min ,min :max ,max)
                                                    '(10 :step 2)))
                                    (return-array (nu:aref* (nu:zeros '(100 100)
                                                                      :type ',return-type)
                                                            '(10 :step 2))))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand :out return-array)))
                             "Non-simple multithreaded")
                (5am:is-true (let* ((rand (nu:aref* (nu:rand '(10 100) :type ',type
                                                                       :min ,min :max ,max)
                                                    nil
                                                    '(10 :step -2)))
                                    (return-array (nu:zeros (array-dimensions rand)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand)
                                          (,name rand :out return-array))))
                (5am:is-true (let* ((array (nu:rand '(2 3) :type ',type
                                                           :min ,min :max ,max))
                                    (orig  (list (aref array 0 0)
                                                 (aref array 0 2)
                                                 (aref array 1 0)
                                                 (aref array 1 2))))
                               ;; This tests that it is potentially only
                               ;; the VIEW elements that have changed and the
                               ;; "other" elements have remained the same
                               (,name (nu:aref* array nil 1)
                                      :out (nu:aref* array nil 1))
                               (equalp orig
                                       (list (aref array 0 0)
                                             (aref array 0 2)
                                             (aref array 1 0)
                                             (aref array 1 2)))
                               "Inplace only"))))))

    (let ((suite-name (intern (concatenate 'string
                                           (symbol-name name)
                                           "/INTEGERS")
                              (symbol-package name))))
      `(progn
         (5am:def-suite ,suite-name :in ,suite)
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U64")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 64)
                               0 (expt 2 63)
                               (or return-type '(unsigned-byte 64))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U32")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 32)
                               0 (expt 2 31)
                               (or return-type '(unsigned-byte 32))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U16")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 16)
                               0 (expt 2 15)
                               (or return-type '(unsigned-byte 16))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U08")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 8)
                               0 (expt 2 7)
                               (or return-type '(unsigned-byte 8))))

         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S64")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 64)
                               (- (expt 2 62)) (1- (expt 2 62))
                               (or return-type '(signed-byte 64))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S32")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 32)
                               (- (expt 2 30)) (1- (expt 2 30))
                               (or return-type '(signed-byte 32))))
         (5am:def-test ,(intern (concatenate 'string

                                             (symbol-name suite-name)
                                             "/S16")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 16)
                               (- (expt 2 14)) (1- (expt 2 14))
                               (or return-type '(signed-byte 16))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S08")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 8)
                               (- (expt 2 6)) (1- (expt 2 6))
                               (or return-type '(signed-byte 8))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/FIXNUM")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(if (eq name 'nu:multiply)
                (verification-form 'fixnum
                                   (- (expt 2 30)) (1- (expt 2 30))
                                   (or return-type 'fixnum))
                (verification-form 'fixnum
                                   most-negative-fixnum
                                   most-positive-fixnum
                                   (or return-type 'fixnum))))))))


(defmacro define-numericals-two-arg-test
    (name suite broadcast-p
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
                (let ((nu:*multithreaded-threshold* 1000000000)
                      (nu:*broadcast-automatically* t))
                  (5am:is-true (let* ((rand1 (nu:rand 200 :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand 200 :type ',type :min ,min :max ,max))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2)
                                            :test #'close-p))
                               "Simplest case")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand1 (nu:rand 2 nu:*multithreaded-threshold*
                                                      :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand 2 nu:*multithreaded-threshold*
                                                      :type ',type :min ,min :max ,max))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2)
                                            :test #'close-p))
                               "Simple Multithreaded insides")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand1 (nu:rand nu:*multithreaded-threshold* 2
                                                      :type ',type :min ,min :max ,max))
                                      (rand2 (nu:rand nu:*multithreaded-threshold* 2
                                                      :type ',type :min ,min :max ,max))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2)
                                            :test #'close-p))
                               "Simple Multithreaded outsides")
                  (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(100 10) :type ',type
                                                                          :min ,min :max ,max)
                                                       '(10 :step 2)))
                                      (rand2 (nu:aref* (nu:rand '(200 10) :type ',type
                                                                          :min ,min :max ,max)
                                                       '(20 :step 4)))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple arrays 1")
                  (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(100 100) :type ',type
                                                                           :min ,min :max ,max)
                                                       '(10 :step 2)
                                                       '(10 :step 2)))
                                      (rand2 (nu:aref* (nu:rand '(100 200) :type ',type
                                                                           :min ,min :max ,max)
                                                       '(10 :step 2)
                                                       '(20 :step 4)))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple arrays 2")
                  (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(10 100) :type ',type
                                                                          :min ,min :max ,max)
                                                       nil
                                                       '(10 :step -2)))
                                      (rand2 (nu:aref* (nu:rand '(10 200) :type ',type
                                                                          :min ,min :max ,max)
                                                       nil
                                                       '(20 :step -4)))
                                      (return-array (nu:zeros (array-dimensions rand1)
                                                              :type ',return-type)))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple arrays 3")
                  (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                      (rand1 (nu:aref* (nu:rand '(100 100) :type ',type
                                                                           :min ,min :max ,max)
                                                       '(10 :step 2)))
                                      (rand2 (nu:aref* (nu:rand '(200 100) :type ',type
                                                                           :min ,min :max ,max)
                                                       '(20 :step 4)))
                                      (return-array (nu:aref* (nu:zeros '(100 100)
                                                                        :type ',return-type)
                                                              '(10 :step 2))))
                                 (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                            (,name rand1 rand2 :out return-array)
                                            :test #'close-p))
                               "Non-simple multithreaded")
                  ,(when broadcast-p
                     `(5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                          (rand1 (nu:aref* (nu:rand '(100 10) :type ',type
                                                                              :min ,min :max ,max)
                                                           '(10 :step 2)))
                                          (rand2 (nu:aref* (nu:rand '(10 200 10) :type ',type
                                                                                 :min ,min :max ,max)
                                                           nil
                                                           '(20 :step 4)))
                                          (return-array (nu:aref* (nu:zeros '(2 10 100 10)
                                                                            :type ',return-type)
                                                                  nil
                                                                  nil
                                                                  '(10 :step 2))))
                                     (nu:array= (nu:broadcast-array
                                                 (nu:macro-map-array
                                                  nil
                                                  ',name (nu:broadcast-array rand1
                                                                             '(10 45 10))
                                                  rand2)
                                                 '(2 10 45 10))
                                                (,name rand1 rand2 :out return-array)
                                                :test #'close-p))
                                   "Non-simple multithreaded broadcast"))
                  (5am:is-true (let* ((array (nu:rand '(2 3) :type ',type
                                                             :min ,min :max ,max))
                                      (return-array (nu:zeros '(2 3) :type ',return-type))
                                      (orig  (list (aref array 0 0)
                                                   (aref array 0 2)
                                                   (aref array 1 0)
                                                   (aref array 1 2))))
                                 (,name (nu:aref* array nil 1)
                                        (nu:aref* array nil 1)
                                        :out (nu:aref* return-array nil 1))
                                 (equalp orig
                                         (list (aref array 0 0)
                                               (aref array 0 2)
                                               (aref array 1 0)
                                               (aref array 1 2))))))))))

    `(5am:def-test ,name (:suite ,suite)
       ,(verification-form 'single-float single-float-error
                           single-float-min single-float-max single-float-return-type)
       ,(verification-form 'double-float double-float-error
                           double-float-min double-float-max double-float-return-type))))


(defmacro define-numericals-two-arg-test/integers
    (name suite &optional (return-type nil))

  (flet ((verification-form (type min max return-type)
           `(progn
              (let ((nu:*multithreaded-threshold* 1000000000)
                    (nu:*broadcast-automatically* t))
                (5am:is-true (let* ((rand1 (nu:rand 100 :type ',type :min ,min :max ,max))
                                    (rand2 (nu:rand 100 :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2)))
                             "Simplest case")
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand1 (nu:rand 2 nu:*multithreaded-threshold*
                                                    :type ',type :min ,min :max ,max))
                                    (rand2 (nu:rand 2 nu:*multithreaded-threshold*
                                                    :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2)))
                             "Simple Multithreaded insides")
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand1 (nu:rand nu:*multithreaded-threshold* 2
                                                    :type ',type :min ,min :max ,max))
                                    (rand2 (nu:rand nu:*multithreaded-threshold* 2
                                                    :type ',type :min ,min :max ,max))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2)))
                             "Simple Multithreaded outsides")
                (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(100 10) :type ',type
                                                                        :min ,min :max ,max)
                                                     '(10 :step 2)))
                                    (rand2 (nu:aref* (nu:rand '(200 10) :type ',type
                                                                        :min ,min :max ,max)
                                                     '(20 :step 4)))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2 :out return-array)))
                             "Non-simple arrays 1")
                (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(100 100) :type ',type
                                                                         :min ,min :max ,max)
                                                     '(10 :step 2)
                                                     '(10 :step 2)))
                                    (rand2 (nu:aref* (nu:rand '(100 200) :type ',type
                                                                         :min ,min :max ,max)
                                                     '(10 :step 2)
                                                     '(20 :step 4)))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2 :out return-array)
                                          :test #'=)))
                (5am:is-true (let* ((nu:*multithreaded-threshold* 1000)
                                    (rand1 (nu:aref* (nu:rand '(100 100) :type ',type
                                                                         :min ,min :max ,max)
                                                     '(10 :step 2)))
                                    (rand2 (nu:aref* (nu:rand '(200 100) :type ',type
                                                                         :min ,min :max ,max)
                                                     '(20 :step 4)))
                                    (return-array (nu:aref* (nu:zeros '(100 100)
                                                                      :type ',return-type)
                                                            '(10 :step 2))))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2 :out return-array)))
                             "Non-simple multithreaded")
                (5am:is-true (let* ((rand1 (nu:aref* (nu:rand '(10 100) :type ',type
                                                                        :min ,min :max ,max)
                                                     nil
                                                     '(10 :step -2)))
                                    (rand2 (nu:aref* (nu:rand '(10 200) :type ',type
                                                                        :min ,min :max ,max)
                                                     nil
                                                     '(20 :step -4)))
                                    (return-array (nu:zeros (array-dimensions rand1)
                                                            :type ',return-type)))
                               (nu:array= (nu:macro-map-array return-array ',name rand1 rand2)
                                          (,name rand1 rand2 :out return-array))))
                (5am:is-true (let* ((array (nu:rand '(2 3) :type ',type
                                                           :min ,min :max ,max))
                                    (orig  (list (aref array 0 0)
                                                 (aref array 0 2)
                                                 (aref array 1 0)
                                                 (aref array 1 2))))
                               (,name (nu:aref* array nil 1)
                                      (nu:aref* array nil 1))
                               (equalp orig
                                       (list (aref array 0 0)
                                             (aref array 0 2)
                                             (aref array 1 0)
                                             (aref array 1 2)))))))))

    (let ((suite-name (intern (concatenate 'string
                                           (symbol-name name)
                                           "/INTEGERS")
                              (symbol-package name))))
      `(progn
         (5am:def-suite ,suite-name :in ,suite)
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U64")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 64)
                               0 (expt 2 63)
                               (or return-type '(unsigned-byte 64))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U32")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 32)
                               0 (expt 2 31)
                               (or return-type '(unsigned-byte 32))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U16")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 16)
                               0 (expt 2 15)
                               (or return-type '(unsigned-byte 16))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/U08")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(unsigned-byte 8)
                               0 (expt 2 7)
                               (or return-type '(unsigned-byte 8))))

         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S64")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 64)
                               (- (expt 2 62)) (1- (expt 2 62))
                               (or return-type '(signed-byte 64))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S32")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 32)
                               (- (expt 2 30)) (1- (expt 2 30))
                               (or return-type '(signed-byte 32))))
         (5am:def-test ,(intern (concatenate 'string

                                             (symbol-name suite-name)
                                             "/S16")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 16)
                               (- (expt 2 14)) (1- (expt 2 14))
                               (or return-type '(signed-byte 16))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/S08")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(verification-form '(signed-byte 8)
                               (- (expt 2 6)) (1- (expt 2 6))
                               (or return-type '(signed-byte 8))))
         (5am:def-test ,(intern (concatenate 'string
                                             (symbol-name suite-name)
                                             "/FIXNUM")
                                (symbol-package suite-name))
             (:suite ,suite-name)
           ,(if (eq name 'nu:multiply)
                (verification-form 'fixnum
                                   (- (expt 2 30)) (1- (expt 2 30))
                                   (or return-type 'fixnum))
                (verification-form 'fixnum
                                   most-negative-fixnum
                                   most-positive-fixnum
                                   (or return-type 'fixnum))))))))
