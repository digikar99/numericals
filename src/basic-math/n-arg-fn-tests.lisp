(in-package :numericals/basic-math/impl)

(5am:in-suite :numericals)

;; FIXME: Categorize optimizable and non-optimizable tests correctly

(5am:def-test nu:+ (:suite :numericals)

  (symbol-macrolet ((non-optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 2 (nu:+ 2)))
                         (5am:is (= 5 (nu:+ 2 3)))
                         (5am:is (= 9 (nu:+ 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (nu:array= array-1
                                              (nu:+ array-1)))
                           (5am:is (nu:array= (nu:asarray '(2 4 6))
                                              (nu:+ array-1
                                                    array-1)))

                           (5am:is (nu:array= (nu:asarray '(2 3 4))
                                              (nu:+ 1 '(1 2 3))))
                           (5am:is (nu:array= (nu:asarray '(2 3 4))
                                              (nu:+ '(1 2 3) 1)))
                           (5am:is (nu:array= (nu:asarray '(2 3 4))
                                              (nu:+ '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 2 (nu:+ 2)))
                         (5am:is (= 5 (nu:+ 2 3)))
                         (5am:is (= 9 (nu:+ 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3)))
                               (array-2 (nu:asarray '(1)))
                               (array-3 (nu:asarray '(2 3 4)))
                               (zeros   (nu:zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is-true (nu:array= array-1
                                                   (nu:+ array-1)))
                           (5am:is-true (nu:array= (nu:asarray '(2 4 6))
                                                   (nu:+ array-1
                                                         array-1)))
                           (5am:is (nu:array= (nu:asarray '(2 4 6))
                                              (nu:+ array-1
                                                    array-1
                                                    :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(3 6 9))
                                              (nu:+ array-1
                                                    array-1
                                                    array-1
                                                    :out zeros)))

                           (5am:is (nu:array= (nu:asarray '(2 3 4))
                                              (nu:+ array-1 array-2 :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(4 6 8))
                                              (nu:+ array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline nu:array=)
                             (compiler-macro-notes:muffle
                              compiler-macro-notes:optimization-failure-note))
             ,optimizable-tests))))


(5am:def-test nu:- (:suite :numericals)

  (symbol-macrolet ((non-optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= -2 (nu:- 2)))
                         (5am:is (= -1 (nu:- 2 3)))
                         (5am:is (= -5 (nu:- 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (nu:array= (nu:asarray '(-1 -2 -3))
                                              (nu:- array-1)))
                           (5am:is (nu:array= (nu:zeros 3)
                                              (nu:- array-1
                                                    array-1)))

                           (5am:is (nu:array= (nu:asarray '(0 -1 -2))
                                              (nu:- 1 '(1 2 3))))
                           (5am:is (nu:array= (nu:asarray '(0 1 2))
                                              (nu:- '(1 2 3) 1)))
                           (5am:is (nu:array= (nu:asarray '(0 1 2))
                                              (nu:- '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= -2 (nu:- 2)))
                         (5am:is (= -1 (nu:- 2 3)))
                         (5am:is (= -5 (nu:- 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3)))
                               (array-2 (nu:asarray '(1)))
                               (array-3 (nu:asarray '(2 3 4)))
                               (zeros   (nu:zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (nu:array= (nu:asarray '(-1 -2 -3))
                                              (nu:- array-1)))
                           (5am:is (nu:array= (nu:zeros 3)
                                              (nu:- array-1
                                                    array-1)))
                           (5am:is (nu:array= (nu:zeros 3)
                                              (nu:- array-1
                                                    array-1
                                                    :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(-1 -2 -3))
                                              (nu:- array-1
                                                    array-1
                                                    array-1
                                                    :out zeros)))

                           (5am:is (nu:array= (nu:asarray '(0 1 2))
                                              (nu:- array-1 array-2 :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(-2 -2 -2))
                                              (nu:- array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline nu:array=)
                             (compiler-macro-notes:muffle
                              compiler-macro-notes:optimization-failure-note))
             ,optimizable-tests))))

(5am:def-test nu:* (:suite :numericals)

  (symbol-macrolet ((non-optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 2 (nu:* 2)))
                         (5am:is (= 6 (nu:* 2 3)))
                         (5am:is (= 24 (nu:* 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 4 9))
                                              (nu:* array-1
                                                    array-1)))

                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* 1 '(1 2 3))))
                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* '(1 2 3) 1)))
                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 2 (nu:* 2)))
                         (5am:is (= 6 (nu:* 2 3)))
                         (5am:is (= 24 (nu:* 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3)))
                               (array-2 (nu:asarray '(1)))
                               (array-3 (nu:asarray '(2 3 4)))
                               (zeros   (nu:zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 4 9))
                                              (nu:* array-1
                                                    array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 4 9))
                                              (nu:* array-1
                                                    array-1
                                                    :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(1 8 27))
                                              (nu:* array-1
                                                    array-1
                                                    array-1
                                                    :out zeros)))

                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:* array-1 array-2 :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(2 6 12))
                                              (nu:* array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline nu:array=)
                             (compiler-macro-notes:muffle
                              compiler-macro-notes:optimization-failure-note))
             ,optimizable-tests))))



(5am:def-test nu:/ (:suite :numericals)

  (symbol-macrolet ((non-optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 1/2 (nu:/ 2)))
                         (5am:is (= 2/3 (nu:/ 2 3)))
                         (5am:is (= 1/6 (nu:/ 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (nu:array= (nu:asarray '(1 1/2 1/3))
                                              (nu:/ array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 1 1))
                                              (nu:/ array-1
                                                    array-1)))

                           (5am:is (nu:array= (nu:asarray '(1 1/2 1/3))
                                              (nu:/ 1 '(1 2 3))))
                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:/ '(1 2 3) 1)))
                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:/ '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(let ((nu:*array-element-type* 'single-float))
                         (5am:is (= 1/2 (nu:/ 2)))
                         (5am:is (= 2/3 (nu:/ 2 3)))
                         (5am:is (= 1/6 (nu:/ 2 3 4)))

                         (let ((array-1 (nu:asarray '(1 2 3)))
                               (array-2 (nu:asarray '(1)))
                               (array-3 (nu:asarray '(2 3 4)))
                               (zeros   (nu:zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (nu:array= (nu:asarray '(1 1/2 1/3))
                                              (nu:/ array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 1 1))
                                              (nu:/ array-1
                                                    array-1)))
                           (5am:is (nu:array= (nu:asarray '(1 1 1))
                                              (nu:/ array-1
                                                    array-1
                                                    :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(1 1/2 1/3))
                                              (nu:/ array-1
                                                    array-1
                                                    array-1
                                                    :out zeros)))

                           (5am:is (nu:array= (nu:asarray '(1 2 3))
                                              (nu:/ array-1 array-2 :out zeros)))
                           (5am:is (nu:array= (nu:asarray '(1/2 2/3 3/4))
                                              (nu:/ array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline nu:array=)
                             (compiler-macro-notes:muffle
                              compiler-macro-notes:optimization-failure-note))
             ,optimizable-tests))))
