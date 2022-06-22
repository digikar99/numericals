# numericals

> THIS PROJECT IS (VERY) UNSTABLE YET. THINGS ARE SUBJECT TO CHANGE.

The project intends to offer a numpy-like (not exactly numpy!) API and equivalent performance for high performance number crunching. This is enabled by the use of SIMD using libraries like [BMAS](https://github.com/digikar99/bmas) backed by [SLEEF](https://sleef.org/) as well as [BLAS](http://www.netlib.org/blas/). This is further coupled with multithreading using [lparallel](https://lparallel.org/).

It provides mainly two ASDF systems: `numericals` for use with `cl:array` and `dense-numericals` for use with [dense-arrays:array](https://github.com/digikar99/dense-arrays), that is to say, wherever possible functionality in `numericals` attempts to output `cl:array`, while wherever possible, `dense-numericals` attempts to output `dense-arrays:array`.

At the moment, work is primarily happening under `dense-numericals`, so it has slightly greater functionality than `numericals`; raise an issue if you'd like to shift the focus to `numericals`.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [numericals](#numericals)
    - [Functionality So Far](#functionality-so-far)
    - [Usage and Features](#usage-and-features)
        - [Native CL arrays](#native-cl-arrays)
        - [Non-native CL arrays](#non-native-cl-arrays)
        - [Usage](#usage)
    - [Why separate `numericals` and `dense-numericals` packages?](#why-separate-numericals-and-dense-numericals-packages)
    - [Contributing and Future Plans](#contributing-and-future-plans)
    - [Project Predecessors](#project-predecessors)
    - [Background](#background)
    - [Acknowledgements](#acknowledgements)

<!-- markdown-toc end -->


## Functionality So Far

Below:

- `-` indicates unimplemented
- `+` indicates implemented and possibly unoptimized
- `+++` indicates implemented and optimized
- "all integer types" include: (signed-byte 08/16/32/64) and (unsigned-byte 08/16/32/64); fixnum is specified explicitly
- fixnum for multiplication (or just about any operation for that matter!) will work correctly only while the results fit within machine word on SBCL; in fact, on SBCL `(type= 'fixnum '(signed-byte 63))`

Feature parity of `numericals` vs `dense-neumericals`:

| Feature                                                               | numericals | dense-numericals |
|-----------------------------------------------------------------------|:----------:|:----------------:|
| <u>**Basic Utilities**</u>                                            |            |                  |
| ones                                                                  | +++        | +                |
| zeros                                                                 | +++        | +                |
| rand                                                                  | +          | +                |
| ones-like                                                             | +++        | +                |
| zeros-like                                                            | +++        | +                |
| rand-like                                                             | +          | +                |
| asarray                                                               | +          | +                |
| copy (all to same or floats)                                          | -          | +++              |
| <u>**Basic Operations**</u>                                           |            |                  |
| `/` (single-float / double-float)                                     | +++        | +                |
| `+ - * /` (single-float / double-float)                               | +++        | +++              |
| `+ - *` (all integer types / fixnum)                                  | -          | +++              |
| `< <= = /= >= >` (single-float / double-float)                        | +++        | +                |
| `< <= = /= >= >` (all integer types / fixnum)                         | -          | +                |
| `(log) not and ior xor nand andc1 andc2` (unsigned-byte 8)            | -          | +                |
| <u>**Transcendental Functions**</u> (single-float / double-float)     |            |                  |
| ` sin   cos   tan `                                                   | +++        | +++              |
| `asin  acos  atan `                                                   | +++        | +++              |
| ` sinh  cosh  tanh`                                                   | +++        | +++              |
| `asinh acosh atanh`                                                   | +++        | +++              |
| `log exp expt`                                                        | +++        | +++              |
| <u>**More Operations**</u>                                            |            |                  |
| `ffloor fceiling fround ftruncate` (single-float / double-float)      | +++        | +++              |
| `abs` (single-float / double-float / all integer types / fixnum)      | -          | +++              |
| `vdot sum` (single-float / double-float / all integer types / fixnum) | -          | +++              |
| `max min` (single-float / double-float / all integer types / fixnum)  | -          | +++              |



Supported conversions:

- from signed/unsigned integers to single-float/double-float
- between single-float and double-float

These functions should be within a factor of two of numpy/torch for "common cases". If they are not inspite of type declarations, feel free to report an [issue](https://github.com/digikar99/numericals/issues)!

## Usage and Features

### Native CL arrays

As of this writing,

- The only libraries that offer broadcasted operations on arrays are this and [numcl](https://github.com/numcl/numcl)
- `numcl` does not yet have a focus on high performance - though, it [should be possible to implement the current einsum based backend using BLAS and BMAS](https://github.com/numcl/numcl/issues/57); instead the focus there is on functionality; by contrast, the focus here is on performance first, and functionality second. Users do not have to choose. `:mix` option of `uiop:define-package` can be useful for mixing the two libraries as per user preferences
- Other minor differences wrt numcl include:
  - Both `(ones 2 3 :type 'single-float)` and `(ones '(2 3) :type 'single-float)` are legal in numericals; while only the latter is legal in numcl/numpy
  - `numericals` provides a `*array-element-type-alist*` equivalent to `swank:*readtable-alist*` to provide a package local way of specifying the default element-type for arrays. This can be further overriden by binding `*array-element-type*`. This does impose performance penalties however.
  - `numcl` relies on JIT backed by [specialized-function](https://github.com/numcl/specialized-function), while `numericals` relies on AOT backed by [polymorphic-functions](https://github.com/digikar99/polymorphic-functions/) and [cl-form-types](https://github.com/alex-gutev/cl-form-types). Again, these are not either-or, high level user functions can (in theory) utilize specialized-function, while the innards can use static-dispatch either by polymorphic-functions or [static-dispatch](https://github.com/alex-gutev/static-dispatch) or [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions/).
- In addition to these two, another performant library operating on CL arrays includes [lla](https://github.com/tpapp/lla). Again, `uiop:define-package` with `:mix` can be used suitably.

The author of `numericals` [did not find other libraries operating on native CL arrays](https://gist.github.com/digikar99/16066dbf24b8789c969ea58837e0fbef).

### Non-native CL arrays

There are [quite](https://github.com/CodyReichert/awesome-cl#machine-learning) a [few](https://github.com/CodyReichert/awesome-cl#numerical-and-scientific) libraries in Common Lisp in this domain. I have only managed to take a peak at [femlisp-matlisp](https://gist.github.com/digikar99/16066dbf24b8789c969ea58837e0fbef#femlisp-matlisp).

That said, the goal of `numericals` is not to *replace* python ecosystems, at least not in the short run, but instead to overcome the limitations of libraries like [py4cl](https://github.com/bendudson/py4cl)/[2](https://github.com/digikar99/py4cl2) of sub-10,000 instructions per second.

### Usage

For the time being, preferably, [fetch from this dist of ultralisp](https://github.com/digikar99/polymorphic-functions#getting-it-from-ultralisp). Users may need to `bash make.sh` [bmas](https://github.com/digikar99/bmas) manually; this currently requires `gcc`, but the `make.sh` should be trivial enough to edit per the user's configurations. This step may be automated in the future.

Once `(ql:quickload "numericals")` or `(ql:quickload "dense-numericals")` is successful; use inside your own package using `:mix` option of `uiop:define-package` (see above discussion), or [package-local-nicknames](https://common-lisp-libraries.readthedocs.io/#libraries).

Run tests using `(asdf:test-system "numericals")` or `(asdf:test-system "dense-numericals")`; these are scattered throughout the system.

Configurable parameters include (see docstrings for their description):

- `*multithreaded-threshold*`
- `*default-float-format*`
- `*inline-with-multithreading*`
- `*array-element-type*` and `*array-element-type-alist*`
- `*broadcast-automatically*`

## Why separate `numericals` and `dense-numericals` packages?

Not doing so would mean an additional configuration option. That implies an usage overhead on the part of the user, besides employing a decision factor into the return type of the functions. Separate packages, functions, symbols simply by-passes this issue. If a user is interfacing with the lisp ecosystem at large, they can choose `numericals` without a second thought.

## Contributing and Future Plans

In no order of priority:

- Compiler macros for `< <= = /= > >=`
- Testing bitwise operations and adding compiler macros for them
- Optimizing fixnum by breaking larger double passes into smaller ones to make use of cache
- `transpose` with arbitrary axes for CL arrays
- More operations from BLAS
- Reviving sum/min/max/dot on not-all axes
- Reviving `weop` and `with-elementwise-operations` using [sb-simd](https://github.com/marcoheisig/sb-simd/) or [cl-simd](https://github.com/angavrilov/cl-simd) (or one of its forks!)
- Automate installation of [bmas](https://github.com/digikar99/bmas), and avoid hardcoding paths in cl-bmas and cl-cblas

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


## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
- Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki) as well as [numcl](https://github.com/numcl/numcl).
- All the [SLEEF](https://github.com/shibatch/sleef) contributors
- All the contributors of [c2ffi](https://github.com/rpav/c2ffi/) and [cl-autowrap](https://github.com/rpav/cl-autowrap)
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!

