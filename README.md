# numericals

> THIS PROJECT IS (VERY) UNSTABLE YET. THINGS ARE SUBJECT TO CHANGE.

The project intends to offer a numpy-like (not exactly numpy!) API and equivalent performance for high performance number crunching. This is enabled by the use of SIMD using libraries like [BMAS](https://github.com/digikar99/bmas) backed by [SLEEF](https://sleef.org/) as well as [BLAS](http://www.netlib.org/blas/). This is further coupled with multithreading using [lparallel](https://lparallel.org/).

## Functionality So Far

Performant functionality so far includes:

- Utilities
  - ones
  - zeros
  - rand
  - ones-like
  - zeros-like
  - rand-like
  - asarray
- Actual Math
  - Supported single-float/double-float functions so far (see [src/package.lisp](src/package.lisp)):
    - DMAS: `+ - * /`
    - Comparisons: `< <= = /= >= >`
    - Trigonometric: `sin cos tan asin acos atan`
    - Hyperbolic `sinh cosh tanh asinh acosh atanh`
    - `log exp expt`
    - `ffloor fceiling fround ftruncate abs`
    - `copy`

These functions should be within a factor of two of numpy/torch for "common cases". If they are not inspite of type declarations, feel free to report an [issue](https://github.com/digikar99/numericals/issues)!

## Project Predecessors

- [sbcl-numericals](./sbcl-numericals)
- [numericals-2020.08](https://github.com/digikar99/numericals/releases/tag/2020.08)

The project renaming reflects an attempt to separate the ANSI standard parts of the codebase
from the SBCL-specific part, so that a portability attempt may be made in the future.

## Background

Curiosity got the better of me one day, and I set out to explore the limits of numerical computing with Common Lisp. I mean - what does speed require? Just memory-locality and SIMD?
SBCL has memory-locality. What about SIMD? Well, the functionality hasn't been "standardized" yet, and there are several attempts. Indeed,
SBCL needs more documentation - think Emacs! But knowledge exists in people's heads. People are willing to share it. So, this was possible.

PS: This library began as a [reddit post](https://www.reddit.com/r/lisp/comments/fkfgjn/sbcl_with_simd_how_to_optimize_sseavx2_to_pointer/), that, in turn, was triggered by [this reddit post](https://www.reddit.com/r/lisp/comments/fjmm6y/deep_learning_with_gpus/).

You should probably use the latest [SBCL (get from git)](https://github.com/sbcl/sbcl), at least SBCL-2.0.4. The build is fairly easy: `sh make.sh && sh run-sbcl.sh # or install.sh`.

## Why not matlisp?

Because ...

```lisp
(let ((a (zeros 1000 1000))
      (b (zeros 1000 1000)))
  (declare (optimize (speed 3)))
  (time (loop :for i :below 1000 :do (m+ a b))))
; Evaluation took:
;   4.001 seconds of real time
;   4.004066 seconds of total run time (3.807962 user, 0.196104 system)
;   [ Run times consist of 0.361 seconds GC time, and 3.644 seconds non-GC time. ]
;   100.07% CPU
;   8,828,045,634 processor cycles
;   8,000,016,000 bytes consed
NIL
```

```lisp
(let ((a (nu:zeros '(1000 1000)))
      (b (nu:zeros '(1000 1000))))
  (time (loop :for i :below 1000 :do (nu:+ a b))))
; Evaluation took:
;   1.727 seconds of real time
;   1.728858 seconds of total run time (1.556757 user, 0.172101 system)
;   [ Run times consist of 0.199 seconds GC time, and 1.530 seconds non-GC time. ]
;   100.12% CPU
;   3,810,797,604 processor cycles
;   4,000,343,680 bytes consed
NIL

(let ((a (nu:zeros '(1000 1000)))   ; There are other macros with help which automatically declare
      (b (nu:zeros '(1000 1000)))   ; things for you as well, though more work still needs to be
      (c (nu:zeros '(1000 1000))))  ; done to optimize for more special cases.
  (time (loop :for i :below 1000 :do
             (nu:weop c (+ a b))))) ; elementwise operations
; Evaluation took:
;   0.775 seconds of real time
;   0.775492 seconds of total run time (0.775492 user, 0.000000 system)
;   100.00% CPU
;   1,712,306,568 processor cycles
;   131,072 bytes consed
NIL
```

And I didn't see any equivalent of `nu:aref` there:

```lisp
(let ((a (nu:asarray '((1 2 3) (4 5 6)))))
  (nu:aref a t 2))
;=> #1A(3.0 6.0)
```


## What about others?

I don't know. There are [many others](https://www.cliki.net/linear%20algebra).

## The Plan

The plan is to enable number crunching using this, coupled with py4cl/2. For "light" array
manipulation, we stay within lisp. While for heavy manipulation - deep learning - we offload
to the existing python ecosystem. This should eliminate the ~10000 op/sec limitations of py4cl/2.

SIMD is rich. See [Introduction
to Intel Advanced Vector Extensions](https://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions) for the full realm of possibilities.

## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
- Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki) as well as [numcl](https://github.com/numcl/numcl).
- All the [SLEEF](https://github.com/shibatch/sleef) contributors
- All the contributors of [c2ffi](https://github.com/rpav/c2ffi/) and [cl-autowrap](https://github.com/rpav/cl-autowrap)
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!

