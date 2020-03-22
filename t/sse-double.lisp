(in-package :sbcl-numericals.test)

(defmacro generate-double-tests (&body op-double-op-pairs)
  `(progn
     ,@(loop for (op double-op) in op-double-op-pairs
          collect `(test ,double-op
                     (with-all-offsets 4 'double-float
                         (lambda (i) (+ 0.1d0 i)) (lambda (i) (+ 0.2d0 i))
                       (is (array-equal (,double-op arr-a0 arr-b0 arr-c0)
                                        (map-array ',op arr-a0 arr-b0 arr-r0)))
                       (is (array-equal (,double-op arr-a1 arr-b1 arr-c1)
                                        (map-array ',op arr-a1 arr-b1 arr-r1)))
                       (is (array-equal (,double-op arr-a2 arr-b2 arr-c2)
                                        (map-array ',op arr-a2 arr-b2 arr-r2)))
                       (is (array-equal (,double-op arr-a3 arr-b3 arr-c3)
                                        (map-array ',op arr-a3 arr-b3 arr-r3))))))))

(generate-double-tests
  (+ d2+)
  (- d2-)
  (* d2*)
  (/ d2/))

