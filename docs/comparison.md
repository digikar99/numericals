# Comparison with other libraries

There are two main asdf systems:

- `numericals` is designed to work with `cl:array` so that interfacing with the rest of the lisp ecosystem is trivial.
- `dense-numericals` is designed to work with [dense-arrays:array](https://github.com/digikar99/dense-arrays)

Each of these has a number of systems and corresponding packages:

- utils
- basic-math
- transcendental
- statistics
- linalg
- random

These can be loaded individually. For example `(asdf:load-system "numericals/random")`.

### Native CL arrays

As of this writing,

- The only libraries that offer broadcasted operations on arrays are this and [numcl](https://github.com/numcl/numcl)
- `numcl` does not yet have a focus on high performance - though, it [should be possible to implement the current einsum based backend using BLAS and BMAS](https://github.com/numcl/numcl/issues/57); instead the focus there is on functionality; by contrast, the focus here is on performance first, and functionality second. Users do not have to choose. `:mix` option of `uiop:define-package` can be useful for mixing the two libraries as per user preferences
- Other minor differences wrt numcl include:
    - Both `(ones 2 3 :type 'single-float)` and `(ones '(2 3) :type 'single-float)` are legal in numericals; while only the latter is legal in numcl/numpy
    - `numericals` provides a `*array-element-type-alist*` similar to `swank:*readtable-alist*` to provide a package local way of specifying the default element-type for arrays. This can be further overriden by binding `*array-element-type*`. This does impose performance penalties however.
    - `numcl` relies on JIT backed by [specialized-function](https://github.com/numcl/specialized-function), while `numericals` relies on AOT backed by [polymorphic-functions](https://github.com/digikar99/peltadot/). Again, these are not either-or, high level user functions can (in theory) utilize specialized-function, while the innards can use static-dispatch either by polymorphic-functions or [static-dispatch](https://github.com/alex-gutev/static-dispatch) or [fast-generic-functions](https://github.com/marcoheisig/fast-generic-functions/).
- In addition to these two, another performant library operating on CL arrays includes [lla](https://github.com/tpapp/lla). Again, `uiop:define-package` with `:mix` and `:local-nicknames` can be used suitably.

The author of `numericals` [did not find other libraries operating on native CL arrays](https://gist.github.com/digikar99/16066dbf24b8789c969ea58837e0fbef).

### Non-native CL arrays

There are [quite](https://github.com/CodyReichert/awesome-cl#machine-learning) a [few](https://github.com/CodyReichert/awesome-cl#numerical-and-scientific) libraries in Common Lisp in this domain. I have only managed to take a peak at [femlisp-matlisp](https://gist.github.com/digikar99/16066dbf24b8789c969ea58837e0fbef#femlisp-matlisp).

That said, the goal of `numericals` is not to *replace* python ecosystems, at least not in the short run, but instead to overcome the limitations of libraries like [py4cl](https://github.com/bendudson/py4cl)/[2](https://github.com/digikar99/py4cl2) of sub-10,000 instructions per second. However, now there's also [py4cl2-cffi](https://github.com/digikar99/py4cl2-cffi) which is about a 10 times slower than CPython itself.

### Foreign function interfaces

#### gsll

A number of functions in [gsl](https://www.gnu.org/software/gsl/) and [gsll](https://gsll.common-lisp.dev/) operate on double precision floats, while some applications appreciate operators working on single precision floats without a conversion overhead. Thus, while extremely featureful, `gsll` is insufficient for everyone's needs.

#### eigen

[eigen](https://eigen.tuxfamily.org/index.php) has wonderful documentation. I have only ever done a basic course in linear algebra, and often feel as if I am missing out on something when people work on linear algebra, but I found eigen's documentation to be superb! For instance, see

- [this QuickRef page](https://eigen.tuxfamily.org/dox/group__QuickRefPage.html),
- or [this Linear Algebra tutorial comparing different decompositions](https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html),
- or [this linked page on benchmark comparison of various decompositions](https://eigen.tuxfamily.org/dox/group__DenseDecompositionBenchmark.html).

So, if eigen and C++ suit your needs, you might as well use it! That said, it isn't the fastest in everything - [SLEEF](https://sleef.org/) can often be faster in areas where it is specifically designed for.
