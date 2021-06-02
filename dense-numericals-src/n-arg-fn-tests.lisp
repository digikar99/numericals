(in-package :dense-numericals.impl)

;; FIXME: Categorize optimizable and non-optimizable tests correctly

(5am:def-test dn:+ (:suite array)

  (symbol-macrolet ((non-optimizable-tests

                      `(progn
                         (5am:is (= 2 (dn:+ 2)))
                         (5am:is (= 5 (dn:+ 2 3)))
                         (5am:is (= 9 (dn:+ 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (array= array-1
                                           (dn:+ array-1)))
                           (5am:is (array= (asarray '(2 4 6))
                                           (dn:+ array-1
                                                 array-1)))

                           (5am:is (array= (asarray '(2 3 4))
                                           (dn:+ 1 '(1 2 3))))
                           (5am:is (array= (asarray '(2 3 4))
                                           (dn:+ '(1 2 3) 1)))
                           (5am:is (array= (asarray '(2 3 4))
                                           (dn:+ '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(progn
                         (5am:is (= 2 (dn:+ 2)))
                         (5am:is (= 5 (dn:+ 2 3)))
                         (5am:is (= 9 (dn:+ 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3)))
                               (array-2 (asarray '(1)))
                               (array-3 (asarray '(2 3 4)))
                               (zeros   (zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is-true (array= array-1
                                                (dn:+ array-1)))
                           (5am:is-true (array= (asarray '(2 4 6))
                                                (dn:+ array-1
                                                      array-1)))
                           (5am:is (array= (asarray '(2 4 6))
                                           (dn:+ array-1
                                                 array-1
                                                 :out zeros)))
                           (5am:is (array= (asarray '(3 6 9))
                                           (dn:+ array-1
                                                 array-1
                                                 array-1
                                                 :out zeros)))

                           (5am:is (array= (asarray '(2 3 4))
                                           (dn:+ array-1 array-2 :out zeros)))
                           (5am:is (array= (asarray '(4 6 8))
                                           (dn:+ array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline array=))
             ,optimizable-tests))))


(5am:def-test dn:- (:suite array)

  (symbol-macrolet ((non-optimizable-tests

                      `(progn
                         (5am:is (= -2 (dn:- 2)))
                         (5am:is (= -1 (dn:- 2 3)))
                         (5am:is (= -5 (dn:- 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (array= (asarray '(-1 -2 -3))
                                           (dn:- array-1)))
                           (5am:is (array= (zeros 3)
                                           (dn:- array-1
                                                 array-1)))

                           (5am:is (array= (asarray '(0 -1 -2))
                                           (dn:- 1 '(1 2 3))))
                           (5am:is (array= (asarray '(0 1 2))
                                           (dn:- '(1 2 3) 1)))
                           (5am:is (array= (asarray '(0 1 2))
                                           (dn:- '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(progn
                         (5am:is (= -2 (dn:- 2)))
                         (5am:is (= -1 (dn:- 2 3)))
                         (5am:is (= -5 (dn:- 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3)))
                               (array-2 (asarray '(1)))
                               (array-3 (asarray '(2 3 4)))
                               (zeros   (zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (array= (asarray '(-1 -2 -3))
                                           (dn:- array-1)))
                           (5am:is (array= (zeros 3)
                                           (dn:- array-1
                                                 array-1)))
                           (5am:is (array= (zeros 3)
                                           (dn:- array-1
                                                 array-1
                                                 :out zeros)))
                           (5am:is (array= (asarray '(-1 -2 -3))
                                           (dn:- array-1
                                                 array-1
                                                 array-1
                                                 :out zeros)))

                           (5am:is (array= (asarray '(0 1 2))
                                           (dn:- array-1 array-2 :out zeros)))
                           (5am:is (array= (asarray '(-2 -2 -2))
                                           (dn:- array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline array=))
             ,optimizable-tests))))

(5am:def-test dn:* (:suite array)

  (symbol-macrolet ((non-optimizable-tests

                      `(progn
                         (5am:is (= 2 (dn:* 2)))
                         (5am:is (= 6 (dn:* 2 3)))
                         (5am:is (= 24 (dn:* 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* array-1)))
                           (5am:is (array= (asarray '(1 4 9))
                                           (dn:* array-1
                                                 array-1)))

                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* 1 '(1 2 3))))
                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* '(1 2 3) 1)))
                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(progn
                         (5am:is (= 2 (dn:* 2)))
                         (5am:is (= 6 (dn:* 2 3)))
                         (5am:is (= 24 (dn:* 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3)))
                               (array-2 (asarray '(1)))
                               (array-3 (asarray '(2 3 4)))
                               (zeros   (zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* array-1)))
                           (5am:is (array= (asarray '(1 4 9))
                                           (dn:* array-1
                                                 array-1)))
                           (5am:is (array= (asarray '(1 4 9))
                                           (dn:* array-1
                                                 array-1
                                                 :out zeros)))
                           (5am:is (array= (asarray '(1 8 27))
                                           (dn:* array-1
                                                 array-1
                                                 array-1
                                                 :out zeros)))

                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:* array-1 array-2 :out zeros)))
                           (5am:is (array= (asarray '(2 6 12))
                                           (dn:* array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline array=))
             ,optimizable-tests))))



(5am:def-test dn:/ (:suite array)

  (symbol-macrolet ((non-optimizable-tests

                      `(progn
                         (5am:is (= 1/2 (dn:/ 2)))
                         (5am:is (= 2/3 (dn:/ 2 3)))
                         (5am:is (= 1/6 (dn:/ 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3))))
                           (declare (type (simple-array single-float) array-1))

                           (5am:is (array= (asarray '(1 1/2 1/3))
                                           (dn:/ array-1)))
                           (5am:is (array= (asarray '(1 1 1))
                                           (dn:/ array-1
                                                 array-1)))

                           (5am:is (array= (asarray '(1 1/2 1/3))
                                           (dn:/ 1 '(1 2 3))))
                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:/ '(1 2 3) 1)))
                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:/ '(1 2 3) '(1)))))))

                    (optimizable-tests

                      `(progn
                         (5am:is (= 1/2 (dn:/ 2)))
                         (5am:is (= 2/3 (dn:/ 2 3)))
                         (5am:is (= 1/6 (dn:/ 2 3 4)))

                         (let ((array-1 (asarray '(1 2 3)))
                               (array-2 (asarray '(1)))
                               (array-3 (asarray '(2 3 4)))
                               (zeros   (zeros 3)))
                           (declare (type (simple-array single-float)
                                          array-1 array-2 array-3 zeros))

                           (5am:is (array= (asarray '(1 1/2 1/3))
                                           (dn:/ array-1)))
                           (5am:is (array= (asarray '(1 1 1))
                                           (dn:/ array-1
                                                 array-1)))
                           (5am:is (array= (asarray '(1 1 1))
                                           (dn:/ array-1
                                                 array-1
                                                 :out zeros)))
                           (5am:is (array= (asarray '(1 1/2 1/3))
                                           (dn:/ array-1
                                                 array-1
                                                 array-1
                                                 :out zeros)))

                           (5am:is (array= (asarray '(1 2 3))
                                           (dn:/ array-1 array-2 :out zeros)))
                           (5am:is (array= (asarray '(1/2 2/3 3/4))
                                           (dn:/ array-1 array-2 array-3 :out zeros)))))))

    (eval non-optimizable-tests)

    (eval `(locally (declare (optimize speed)
                             #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note)
                             (notinline array=))
             ,optimizable-tests))))
