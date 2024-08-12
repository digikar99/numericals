# Writing Fast Code Fast

## Common Lisp

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

## Numericals

Like Julia, Common Lisp (SBCL) already offers a solution to the purported two-language problem. One can write high level code in Common Lisp. Coupled with the above mentioned facilities, this makes prototyping faster. However, in some cases, the same facilities, in particular the dynamic typing, can be prohibitive for performance.

### 1. Inlining with generics

The particular aspect numericals / dense-numericals aim to solve concerns type declarations. Type declarations aids lisp compilers optimize code. It also aids documentation. However, while prototyping or for providing another number processing library unopinionated about the choice of types, one wants to leave the code untyped or atleast undertyped.

To illustrate this, let us write our own `very-simple-numpy` package containing just one function. This will be a simple function `add-array` that takes in three arrays. It sums up the corresponding elements of the first two arrays and writes them to the third.

We want `add-array` to be generic with respect to element types of the arrays. Thus, we declare that the three parameters `array1 array2 out` of a function `add-array` has type `(simple-array * 1)` rather than the more specific type `(simple-array single-float 1)`.

```lisp
(defpackage :very-simple-numpy
  (:use :cl)
  (:export #:add-array))

(in-package :very-simple-numpy)

(defun add-array (array1 array2 out)
  (declare (type (simple-array * 1) array1 array2 out))
  (loop :for i :below (array-total-size array1)
        :do (setf (aref out i)
                  (+ (aref array1 i)
                     (aref array2 i))))
  out)
```

This enables a user of `very-simple-numpy` to call `add-array` with any 1-dimensional arrays. Below, `add-array-caller/single-float` calls `add-array` with single-float arrays and `add-array-caller/double-float` calls `add-array` with double-float arrays.

```lisp
(defpackage :very-simple-numpy-user
  (:documentation "Demo package demonstrating the user of VERY-SIMPLE-NUMPY.")
  (:use :cl)
  (:local-nicknames (:nu :very-simple-numpy)))

(in-package :very-simple-numpy-user)

;;; Let's ignore how exactly we got the arrays A B and OUT

(defun add-array-caller/single-float (a b out)
  (declare (type (simple-array single-float 1) a b out))
  (nu:add-array a b out))

(defun add-array-caller/double-float (a b out)
  (declare (type (simple-array double-float 1) a b out))
  (nu:add-array a b out))
```

But what price are we paying for this generic-ness? What is the performance penalty compared to writing `add-array` directly for the specialized case of single-float?

```lisp
(defun add-array/single-float (a b out)
  (declare (type (simple-array single-float 1) a b out)
           (optimize speed))
  (loop :for i :below (array-total-size a)
        :do (setf (aref out i)
                  (+ (aref a i)
                     (aref b i))))
  out)
```

```lisp
VERY-SIMPLE-NUMPY-USER> (asdf:load-system "array-operations")
T
VERY-SIMPLE-NUMPY-USER> (let ((a (aops:rand* 'single-float 1000))
                              (b (aops:rand* 'single-float 1000))
                              (o (aops:rand* 'single-float 1000)))
                          (time (loop repeat 100000 do
                            (add-array-caller/single-float a b o))))
Evaluation took:
  2.974 seconds of real time
  2.973779 seconds of total run time (2.973779 user, 0.000000 system)
  100.00% CPU
  8,336,265,453 processor cycles
  0 bytes consed

NIL
VERY-SIMPLE-NUMPY-USER> (let ((a (aops:rand* 'single-float 1000))
                              (b (aops:rand* 'single-float 1000))
                              (o (aops:rand* 'single-float 1000)))
                          (time (loop repeat 1000000 do
                            (add-array/single-float a b o))))
Evaluation took:
  1.865 seconds of real time
  1.864698 seconds of total run time (1.864698 user, 0.000000 system)
  100.00% CPU
  5,227,236,485 processor cycles
  0 bytes consed

NIL
```

One notes that using `add-array/single-float`, one can perform ten times as many operations in about 2/3rd of the time. Thus, `add-array/single-float` is about 15 times faster than its generic counterpart `add-array`.

It turns out that on SBCL, this problem can be easily overcome by declaring `add-array` to be inline and then recompiling `add-array-caller/single-float` again:

```lisp
(in-package :very-simple-numpy)

(declaim (inline add-array))
(defun add-array (array1 array2 out)
  (declare (type (simple-array * 1) array1 array2 out))
  (loop :for i :below (array-total-size array1)
        :do (setf (aref out i)
                  (+ (aref array1 i)
                     (aref array2 i))))
  out)

(in-package :very-simple-numpy-user)

;;; Let's ignore how exactly we got the arrays A B and OUT

(defun add-array-caller/single-float (a b out)
  (declare (type (simple-array single-float 1) a b out))
  (nu:add-array a b out))
```

If you evaluate the performance again, both `add-array-caller/single-float` and `add-array/single-float` would come out to be equivalent:

```lisp
VERY-SIMPLE-NUMPY-USER> (let ((a (aops:rand* 'single-float 1000))
                              (b (aops:rand* 'single-float 1000))
                              (o (aops:rand* 'single-float 1000)))
                          (time (loop repeat 1000000 do
                            (add-array-caller/single-float a b o))))
Evaluation took:
  1.870 seconds of real time
  1.870364 seconds of total run time (1.870364 user, 0.000000 system)
  100.00% CPU
  5,243,342,790 processor cycles
  0 bytes consed

NIL
```

The problem seems solved.

### 2. Non-generic fast C functions

Over the past few decades, excellent C libraries have been written for performant numerical computation. [BLAS](https://www.netlib.org/blas/) and [LAPACK](https://www.netlib.org/lapack/) being the prime ones. Un/Fortunately, these are non-generic: `CBLAS_sdot` computes a dot product of two single-float vectors, while to compute a dot product of two double-float vectors, one needs to use `CBLAS_ddot`. Plus ultimately, hardware instructions are non-generic.

Does that matter? Isn't SBCL fast enough? Fast _enough_ is a tricky notion. It depends on your task. If your computations run for a few minutes, it doesn't matter if they take 10 minutes or 1 minute. But, if they run for hours or days, perhaps, it would be great if they could run in 1 hour instead of 10 hours?

_Are we as fast as we could be?_ For instance, the equivalent `numericals/basic-math:add` (or `numericals:add`) ultimately calls out such non-generic C functions. And coupled with inlining, this gets us about another 10x performance boost.

```lisp
VERY-SIMPLE-NUMPY-USER> (let ((a (aops:rand* 'single-float 1000))
                              (b (aops:rand* 'single-float 1000))
                              (o (aops:rand* 'single-float 1000)))
                          (declare (type (simple-array single-float 1) a b o)
                                   (optimize speed))
                          ;; You can throw in a (safety 0) declaration if you are confident
                          ;; and don't want consing.
                          (time (loop repeat 10000000 do
                            (numericals:add a b :out o :broadcast nil))))
Evaluation took:
  2.651 seconds of real time
  2.650522 seconds of total run time (2.646750 user, 0.003772 system)
  [ Real times consist of 0.040 seconds GC time, and 2.611 seconds non-GC time. ]
  [ Run times consist of 0.040 seconds GC time, and 2.611 seconds non-GC time. ]
  100.00% CPU
  7,429,671,871 processor cycles
  640,002,160 bytes consed

NIL
```

There isn't really anything mystic about the performance of code generated using C compilers vs SBCL. _If we put in enough developer hours_, we can get SBCL to produce code that is as fast as C compilers. What C compilers and the ecosystem currently has are

1. A wide support for SIMD instructions across different platforms. SBCL only recently gained support for SIMD on x86-64, and even then AVX512 is missing.
2. Optimized libraries for lots of different numerical computing tasks. It isn't merely about hardware support, the algorithms that do the computations need to be optimized too. And the C ecosystem just excels the Common Lisp ecosystem by miles.

So, let's give credit where it's due! With Common Lisp and SBCL, you don't need to worry about memory management, you can develop your code one lisp form at a time, you have global-variables-done-right and the excellent condition system that allows you to inspect the stack without unwinding it, and lots. But when it comes to performance across a wide variety of platforms, the C ecosystem marches ahead.

Thus, we want to provide generic lisp functions that ultimately call specialized C functions, and which can also be static dispatched and inlined if we want them to. We have two problems to solve:

1. Dispatch on specialized lisp arrays
2. Provide users the option to static-dispatch and inline the individual specializations

Standard Common Lisp generic functions are unsuitable for both. They dispatch on classes, while specialized lisp arrays are not necessarily classes. Enabling this using meta-object protocol seems non-trivial. (But someone may prove me wrong!) Generic functions are also designed with dynamic dispatch in mind. However, [static-dispatch](https://github.com/alex-gutev/static-dispatch) can enable static dispatch. 

In addition, Common Lisp's ANSI standard offers compiler macros that allows one to control what specialized form a particular call site compiles to (see [this](https://stackoverflow.com/questions/61094156/macro-expanding-to-same-operator) or [this](http://www.lispworks.com/documentation/HyperSpec/Body/m_define.htm)). However, compiler macros are of limited use without access to the type information of the arguments. Unfortunately, [this type information is not available trivially](https://stackoverflow.com/questions/67149358/portable-type-propagation-in-common-lisp-inlined-functions-without-compiler-macr). To access it, one requires a somewhat sophisticated machinery of CLTL2. And even then it is limited, because even on SBCL, type propagation happen in later stages of compilation after (compiler) macro expansions take place.

### 3. Peltadot and CLTL2 

Portable CLTL2 is available in the form of [cl-environments](https://github.com/alex-gutev/cl-environments). [peltadot](https://gitlab.com/digikar/peltadot) builds over this and provides [polymorphic-functions](https://gitlab.com/digikar/peltadot/-/blob/main/docs/tutorial-polymorphic-functions.org) which overcome all of the above disadvantages of generic functions. These also perform type propagation using compiler macros, and also allows dispatching over optional or keyword arguments. An attempt to dispatch statically is only made if the call site is compiled with `(optimize speed)` with `safety<speed` and `debug<speed`.

```lisp
(defpackage :numericals-user
  (:use :peltadot)
  (:local-nicknames (:nu :numericals)))

(in-package :numericals-user)
```

Overall, this allows compiling the following code:

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

What this aggressive inlining means is that the performance is minimally impacted with increasing loop lengths. Below, notice that the overall number of `sin` operations calculated are the same in each of the three cases, although the loop lengths are different.

```lisp
NUMERICALS-USER> (let ((a (nu:rand 100000 :type 'single-float))
                       (b (nu:rand 100000 :type 'single-float)))
                   (declare (optimize speed)
                            (type (simple-array single-float) a b))
                   (time (loop repeat 1000 do
                     (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.100 seconds of real time
  0.101061 seconds of total run time (0.101059 user, 0.000002 system)
  101.00% CPU
  283,283,628 processor cycles
  32,752 bytes consed

NIL
NUMERICALS-USER> (let ((a (nu:rand 1000 :type 'single-float))
                       (b (nu:rand 1000 :type 'single-float)))
                   (declare (optimize speed)
                            (type (simple-array single-float) a b))
                   (time (loop repeat 100000 do
                     (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.103 seconds of real time
  0.104586 seconds of total run time (0.104558 user, 0.000028 system)
  101.94% CPU
  293,246,978 processor cycles
  3,176,944 bytes consed

NIL
NUMERICALS-USER> (let ((a (nu:rand 10 :type 'single-float))
                       (b (nu:rand 10 :type 'single-float)))
                   (declare (optimize speed)
                            (type (simple-array single-float 1) a b))
                   (time (loop repeat 10000000 do
                     (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.720 seconds of real time
  0.722276 seconds of total run time (0.720217 user, 0.002059 system)
  [ Real times consist of 0.032 seconds GC time, and 0.688 seconds non-GC time. ]
  [ Run times consist of 0.031 seconds GC time, and 0.692 seconds non-GC time. ]
  100.28% CPU
  2,021,508,676 processor cycles
  320,001,568 bytes consed

NIL
NUMERICALS-USER> (let ((a (nu:rand 10 :type 'single-float))
                       (b (nu:rand 10 :type 'single-float)))
                   (declare (optimize speed (safety 0))
                            (type (simple-array single-float 1) a b))
                   (time (loop repeat 10000000 do
                     (nu:sin a :out b :broadcast nil))))
Evaluation took:
  0.365 seconds of real time
  0.365534 seconds of total run time (0.365534 user, 0.000000 system)
  100.27% CPU
  1,025,879,135 processor cycles
  0 bytes consed

NIL
```

Again, this may not matter for most use cases. But when it does matter, a naive pure lisp implementation using generic functions stops being as useful. Then, one is either forced to reimplement the lisp code to suit their needs or move to a different language altogether which handles this.

PS: The above `BMAS_ssin` foreign function is built over [SLEEF](https://sleef.org/) and uses SIMD to compute the sine. The equivalent SBCL non-SIMD equivalent is about 40 times slower:

```lisp
NUMERICALS-USER> (let ((a (nu:rand 100000 :type 'single-float :max 1.0))
                       (b (nu:rand 100000 :type 'single-float)))
                   (declare (type (simple-array single-float 1) a b)
                            (optimize speed))
                   (time (loop repeat 1000 do
                     (loop for i below 100000 do
                       (setf (row-major-aref b i) (sin (row-major-aref a i)))))))
Evaluation took:
  4.044 seconds of real time
  4.039446 seconds of total run time (4.039446 user, 0.000000 system)
  99.88% CPU
  11,334,909,278 processor cycles
  0 bytes consed

NIL
```

Some overhead of the computation indeed stems from the conversion of single-float to double-float and back again; however, even if we stick to double-float, there is a difference of about 10x.

### Summary

Thus, "write fast code fast" can be broken down into two parts:

- "write code fast": get a first implementation -- a prototype -- built quickly without worrying about type declarations
- "write fast code": once the prototype is ready, optimize and document it by sprinkling it using type declarations

This isn't much different from the standard ways of writing Common Lisp. However, as discussed above, there are limitations of standard Common Lisp, which the extension layer [peltadot](https://gitlab.com/digikar/peltadot) underneath `numericals` and `dense-numericals` tries to solve. In the future, this may be replaced or augmented by [coalton](https://github.com/coalton-lang/coalton).

