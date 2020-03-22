(in-package :sbcl-numericals.test)

(defmacro generate-single-tests (&body op-single-op-pairs)
  `(progn
     ,@(loop for (op single-op) in op-single-op-pairs
          collect `(test ,single-op
                     (with-all-offsets 8 'single-float
                         (lambda (i) (+ 0.1 i)) (lambda (i) (+ 0.2 i))
                       (is (array-equal (,single-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,single-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1)))
                       (is (array-equal (,single-op arr-a2 arr-b2 arr-c2)
                                        (map-array ',op arr-a2 arr-b2 arr-r2)))
                       (is (array-equal (,single-op arr-a3 arr-b3 arr-c3)
                                        (map-array ',op arr-a3 arr-b3 arr-r3)))
                       (is (array-equal (,single-op arr-a4 arr-b4 arr-c4)
                                        (map-array ',op arr-a4 arr-b4 arr-r4)))
                       (is (array-equal (,single-op arr-a5 arr-b5 arr-c5)
                                        (map-array ',op arr-a5 arr-b5 arr-r5)))
                       (is (array-equal (,single-op arr-a6 arr-b6 arr-c6)
                                        (map-array ',op arr-a6 arr-b6 arr-r6)))
                       (is (array-equal (,single-op arr-a7 arr-b7 arr-c7)
                                        (map-array ',op arr-a7 arr-b7 arr-r7))))))))

(generate-single-tests
  (+ s+)
  (- s-)
  (* s*)
  (/ s/))
