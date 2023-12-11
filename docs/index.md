# numericals | dense-numericals: write fast code fast

<!-- Place this tag in your head or just before your close body tag. -->
<script async defer src="https://buttons.github.io/buttons.js"></script>
<a class="github-button" href="https://github.com/digikar99/numericals/" aria-label="See numericals on GitHub">Github</a>
<a href="https://github.com/digikar99/numericals/actions/workflows/CI.yml">
<img style="margin-top:-0.75em;"     src="https://github.com/digikar99/numericals/actions/workflows/CI.yml/badge.svg"/>
</a>
<a class="github-button" href="https://github.com/digikar99/numericals/subscription" data-icon="octicon-eye" data-show-count="true" aria-label="Watch digikar99/numericals on GitHub">Watch</a>
<a class="github-button" href="https://github.com/digikar99/numericals" data-icon="octicon-star" data-show-count="true" aria-label="Star digikar99/numericals on GitHub">Star</a>
<a class="github-button" href="https://github.com/digikar99/numericals/issues" data-icon="octicon-issue-opened" data-show-count="true" aria-label="Issue digikar99/numericals on GitHub">Issue</a>


*In case of any inaccuracies, ambiguities or suggestions, please [create an issue here](https://github.com/digikar99/numericals/issues).*

`numericals` / `dense-numericals` brings to you the performance of proto-NumPy with the debugging and development abilities of Common Lisp.

Even before that

## Why two names: numericals and dense-numericals?

These are the names of the two main asdf systems provided as a part of this project. 

- `numericals` is designed to work with `cl:array` so that interfacing with the rest of the lisp ecosystem is trivial.
- `dense-numericals` is designed to work with [dense-arrays:array](https://github.com/digikar99/dense-arrays) which provides a numpy-like array object for common lisp. `dense-arrays` itself is fairly extensible, and trivial extensions are provided for [static-vectors](https://github.com/sionescu/static-vectors) as well as [cl-cuda](https://github.com/takagi/cl-cuda)*.

*Currently `dense-numericals` only works with `cl:vector` as the backend storage. Extension to static-vectors or cl-cuda will be provided in the future if at least one user expresses their needs (through an email or an [issue](https://github.com/digikar99/numericals/issues)).

And while, in theory, this choice can be provided through a configuration variable, but it will only add to the usage overhead.

## About common lisp

Common Lisp is great for prototyping:

- [REPL-based programming](https://mikelevins.github.io/posts/2020-12-18-repl-driven/) means that you can write your program one function at a time. Type, structure, function, and class redefinitions are handled more gracefully compared to most other languages.
- Global variables in common lisp have dynamic scope, while local variables have default lexical scope. This means you can use global variables locally :). See [this example](TODO).
- Its [condition system](https://lisper.in/restarts) builds upon this and goes beyond traditional exception handling, allowing one to provide restarts for resuming a "crashed" program.

But it is also great in the longer run:

- Optional static typing with implementations like SBCL means you do not have to worry yourself with type declarations during prototyping. But as your code and types stabilize, you can add the type declarations to aid documentation as well as speed and safety, while also inlining the small functions whose call overhead exceeds the work they do.
- A 1994 ANSI standard coupled with several [portability libraries](https://portability.cl/) means that it is possible to write code that works without changes even after decades.
- Common Lisp also allows for the deployment of binaries, and there are tools to distribute the shared libraries that the binary depends upon.

Indeed, there are limitations:

- If you want to write fast code, then redefinitions, dynamic scoping, and condition systems can get in the way. So, to some extent, it is a tradeoff that you can work around as your program stabilizes.
- The type system is much limited than ML like systems and does not provide true parametric types. But see [this example](TODO). [Coalton](https://github.com/coalton-lang/coalton) is an effort to work around this. It provides a ML like programming system built on top of Common Lisp.

## About numericals

Like Julia, Common Lisp (SBCL) already offers a solution to the purported two-language problem. However, lisp code built using generic functions is not helpful for solving this. [specialized-function](https://github.com/numcl/specialized-function) and [static-dispatch](https://github.com/alex-gutev/static-dispatch) help here, but generic functions have other disadvantages:

- inability to dispatch on specialized array types
- inability to dispatch on optional or keyword arguments

In addition, while Common Lisp's ANSI standard offers compiler macros that allows one to control what specialized form a particular call site compiles to (see [this](https://stackoverflow.com/questions/61094156/macro-expanding-to-same-operator) or [this](http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm)), compiler macros are of limited use without access to the type information of the arguments. Unfortunately, [this type information is not available trivially](https://stackoverflow.com/questions/67149358/portable-type-propagation-in-common-lisp-inlined-functions-without-compiler-macr). To access it, one requires a somewhat sophisticated machinery of CLTL2 in the form of [cl-environments](https://github.com/alex-gutev/cl-environments), [cl-form-types](https://github.com/alex-gutev/cl-form-types), and [polymorphic-functions](https://github.com/digikar99/polymorphic-functions). In particular, the latter overcomes both the disadvantages of generic functions, and allows dispatching over specialized array types, as well as optional or keyword arguments. And coupled with the former two, it is also able to dispatch statically to a reasonable extent if the call site is compiled with `(optimize speed)` with `safety<speed` and `debug<speed`.

Put together, this allows compiling the following code:

```lisp
(disassemble
 (lambda (x)
   (declare (optimize speed)
            (type (simple-array single-float 1) x))
   (nu:sin! x)))
```

into code containing no high level function calls but only:

```
...
; BFA:       4D8B55F0         MOV R10, [R13-16]               ; thread.alien-linkage-table-base
; BFE:       41FF92082E0000   CALL [R10+11784]                ; &BMAS_ssin
; C05:       488BE3           MOV RSP, RBX
...
```

and the following slight variant (using `double-float` instead of `single-float`)

```lisp
(disassemble
 (lambda (x)
   (declare (optimize speed)
            (type (simple-array double-float 1) x))
   (nu:sin! x)))
```

into the following:

```
...
; 672:       4D8B55F0         MOV R10, [R13-16]               ; thread.alien-linkage-table-base
; 676:       41FF92C82E0000   CALL [R10+11976]                ; &BMAS_dsin
; 67D:       488BE3           MOV RSP, RBX
...
```

What this aggressive inlining means is that the performance is minimally impacted with increasing loop lengths. Below, notice that the overall number of `sin` calculated are the same in each of the three cases, although the loop lengths are different.

```lisp
IMPL> (let ((a (nu:rand 100000 :type 'single-float))
            (b (nu:rand 100000 :type 'single-float)))
        (declare (optimize speed)
                 (type (simple-array single-float) a b))
        (time (loop repeat 1000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.252 seconds of real time
  0.252051 seconds of total run time (0.252051 user, 0.000000 system)
  100.00% CPU
  556,048,756 processor cycles
  32,752 bytes consed

NIL
IMPL> (let ((a (nu:rand 1000 :type 'single-float))
            (b (nu:rand 1000 :type 'single-float)))
        (declare (optimize speed)
                 (type (simple-array single-float) a b))
        (time (loop repeat 100000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.256 seconds of real time
  0.258519 seconds of total run time (0.258331 user, 0.000188 system)
  101.17% CPU
  570,228,290 processor cycles
  3,176,944 bytes consed
  
NIL
IMPL> (let ((a (nu:rand 10 :type 'single-float))
            (b (nu:rand 10 :type 'single-float)))
        (declare (optimize speed)
                 (type (simple-array single-float 1) a b))
        (time (loop repeat 10000000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  1.820 seconds of real time
  1.822983 seconds of total run time (1.822983 user, 0.000000 system)
  [ Real times consist of 0.040 seconds GC time, and 1.780 seconds non-GC time. ]
  [ Run times consist of 0.038 seconds GC time, and 1.785 seconds non-GC time. ]
  100.16% CPU
  4,022,371,122 processor cycles
  319,987,040 bytes consed
  
NIL
```

Without inlining, the performance can be impacted severely. For instance, below, the lack of `(optimize speed)` declaration prevents inlining. Note that the below can be optimized further by avoiding type checks if a `(safety 0)` declaration is added.

```lisp
IMPL> (let ((a (nu:rand 100000 :type 'single-float))
            (b (nu:rand 100000 :type 'single-float)))
        (declare (type (simple-array single-float) a b))
        (time (loop repeat 1000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.251 seconds of real time
  0.251034 seconds of total run time (0.251034 user, 0.000000 system)
  100.00% CPU
  554,269,126 processor cycles
  97,792 bytes consed
  
NIL
IMPL> (let ((a (nu:rand 1000 :type 'single-float))
            (b (nu:rand 1000 :type 'single-float)))
        (declare (type (simple-array single-float) a b))
        (time (loop repeat 100000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.308 seconds of real time
  0.308766 seconds of total run time (0.308766 user, 0.000000 system)
  [ Run times consist of 0.013 seconds GC time, and 0.296 seconds non-GC time. ]
  100.32% CPU
  679,509,546 processor cycles
  9,583,360 bytes consed
  
NIL
IMPL> (let ((a (nu:rand 10 :type 'single-float))
            (b (nu:rand 10 :type 'single-float)))
        (declare (type (simple-array single-float 1) a b))
        (time (loop repeat 10000000 do
          (nu:sin a :out b :broadcast nil))))
Evaluation took:
  7.144 seconds of real time
  7.145966 seconds of total run time (7.142114 user, 0.003852 system)
  [ Real times consist of 0.076 seconds GC time, and 7.068 seconds non-GC time. ]
  [ Run times consist of 0.078 seconds GC time, and 7.068 seconds non-GC time. ]
  100.03% CPU
  15,773,699,348 processor cycles
  959,986,352 bytes consed
  
NIL
```

This may or may not matter for most use cases. But when it does matter, a naive lisp implementation using generic functions stops being as useful. Then, one is either forced to reimplement the lisp code to suit their needs or move to a different language altogether which handles this.

Thus, numericals and dense-numericals intend to provide a solution to this separation between "write code fast" and "write fast code".

PS: The above `BMAS_ssin` foreign function is built over [SLEEF](https://sleef.org/) and uses SIMD to compute the sine. The equivalent SBCL non-SIMD equivalent is about 30 times slower:

```lisp
IMPL> (let ((a (nu:rand 100000 :type 'single-float :max 1.0))
            (b (nu:rand 100000 :type 'single-float)))
        (declare (type (simple-array single-float 1) a b)
                 (optimize speed))
        (time (loop repeat 1000 do
          (loop for i below 100000 do
            (setf (row-major-aref b i) (sin (row-major-aref a i)))))))
Evaluation took:
  5.576 seconds of real time
  5.579197 seconds of total run time (5.575260 user, 0.003937 system)
  100.05% CPU
  12,320,119,856 processor cycles
  0 bytes consed
  
NIL
```

Some overhead of the computation indeed stems from the conversion of single-float to double-float and back again; however, even if we stick to double-float, there is a difference of about 10x. But that's also the point, if an application wants performance over accuracy, they should not be forced to look beyond lisp.

## What numericals/dense-numericals do not intend to provide?

`numericals` / `dense-numericals` do not intend to offer a one-stop solution to the numerical computation ecosystem in common lisp. Even planning to do so is delusionary and should be considered a severe case of NIH syndrome. To that extent, we widely adopt:

- [BMAS](https://github.com/digikar99/bmas) backed by [SLEEF](https://sleef.org/),
- and more recently, [Eigen](https://eigen.tuxfamily.org/) backed by [BLAS](https://www.netlib.org/blas/) and [LAPACK](https://www.netlib.org/lapack/), as well as [gsll](http://common-lisp.net/project/gsll/).

This is further coupled with multithreading using [lparallel](https://lparallel.org/) and the C-library builtin [OpenMP](https://www.openmp.org/).

As further testament to the *profoundly found elsewhere* attitude adopted here, we have

- `numericals` working with `cl:array` that can be used in conjunction with other libraries
- two systems `numericals/magicl` and `dense-numericals/magicl` are provided that are essentially wrappers around [magicl](https://github.com/quil-lang/magicl)

Thus, we intend to provide facilities for writing fast code fast, but at the same time, we want to promote the use of existing facilities wherever appropriate.

## Where next?

- [Install it](./install),
- [Checkout the current state of numericals/dense-numericals](./current-state),
- Or visit the [API reference](./api)
- [Old documentation page](./old-index.html)

## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
- Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki) as well as [numcl](https://github.com/numcl/numcl).
- All the [SLEEF](https://github.com/shibatch/sleef) contributors
- All the contributors of [c2ffi](https://github.com/rpav/c2ffi/) and [cl-autowrap](https://github.com/rpav/cl-autowrap)
- [u/moon-chilled's sassy comment](https://www.reddit.com/r/lisp/comments/wei81o/comment/iitttgb/?utm_source=share&utm_medium=web2x&context=3) (Is that the term?)
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!
