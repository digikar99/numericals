(in-package :sbcl-numericals.internals)

(define-binary-vectorized-op sbcl-numericals:d+
    + :double :avx2 (r a b)
  (sb-assem:inst vaddpd r a b))
(define-binary-vectorized-op sbcl-numericals:d-
    - :double :avx2 (r a b)
  (sb-assem:inst vsubpd r a b))
(define-binary-vectorized-op sbcl-numericals:d*
    * :double :avx2 (r a b)
  (sb-assem:inst vmulpd r a b))
(define-binary-vectorized-op sbcl-numericals:d/
    / :double :avx2 (r a b)
  (sb-assem:inst vdivpd r a b))



(define-binary-vectorized-op sbcl-numericals:s+
    + :single :avx2 (r a b)
  (sb-assem:inst vaddps r a b))
(define-binary-vectorized-op sbcl-numericals:s-
    - :single :avx2 (r a b)
  (sb-assem:inst vsubps r a b))
(define-binary-vectorized-op sbcl-numericals:s/
    / :single :avx2 (r a b)
  (sb-assem:inst vdivps r a b))
(define-binary-vectorized-op sbcl-numericals:s*
    * :single :avx2 (r a b)
  (sb-assem:inst vmulps r a b))

(define-binary-vectorized-op sbcl-numericals:d2+
    + :double :sse (r a b)
  (move r a)
  (sb-assem:inst addpd r b))
(define-binary-vectorized-op sbcl-numericals:d2-
    - :double :sse (r a b)
  (move r a)
  (sb-assem:inst subpd r b))
(define-binary-vectorized-op sbcl-numericals:d2/
    / :double :sse (r a b)
  (move r a)
  (sb-assem:inst divpd r b))
(define-binary-vectorized-op sbcl-numericals:d2*
    * :double :sse (r a b)
  (move r a)
  (sb-assem:inst mulpd r b))


(define-binary-vectorized-op sbcl-numericals:s2+
    + :single :sse (r a b)
  (move r a)
  (sb-assem:inst addps r b))
(define-binary-vectorized-op sbcl-numericals:s2-
    - :single :sse (r a b)
  (move r a)
  (sb-assem:inst subps r b))
(define-binary-vectorized-op sbcl-numericals:s2/
    / :single :sse (r a b)
  (move r a)
  (sb-assem:inst divps r b))
(define-binary-vectorized-op sbcl-numericals:s2*
    * :single :sse (r a b)
  (move r a)
  (sb-assem:inst mulps r b))

