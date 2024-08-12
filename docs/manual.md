# Manual

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

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Utilities](#utilities)
    - [Configuration](#configuration)
    - [Generating arrays](#generating-arrays)
    - [Modifying arrays](#modifying-arrays)
    - [Transforming arrays](#transforming-arrays)
    - [Element-wise operators](#element-wise-operators)
    - [Array reduction operators](#array-reduction-operators)
- [Basic Math](#basic-math)
    - [Utilities and array operations](#utilities-and-array-operations)
    - [Standard arithmetic operations](#standard-arithmetic-operations)
    - [Comparison operations](#comparison-operations)
    - [Rounding operations](#rounding-operations)
    - [arg-maximum](#arg-maximum)
    - [arg-minimum](#arg-minimum)
    - [Other basic operations](#other-basic-operations)
- [Transcendental Operations](#transcendental-operations)
- [Linear Algebra](#linear-algebra)
- [Random](#random)
- [Statistics](#statistics)
- [magicl](#magicl)
- [tests](#tests)

<!-- markdown-toc end -->

## Utilities

Package: `numericals/utils` or `dense-numericals/utils`

### Configuration

`numericals` and `dense-numericals` come with a number of dynamically bound configuration variables that are put to use in non-inlined code. These include:

#### \*array-element-type\*

```lisp
Variable
Default Unbound
```

If BOUND, this is the default value of the or TYPE (or also ELEMENT-TYPE for DENSE-ARRAYS) argument. Overrides [\*array-element-type-alist\*](#array-element-type-alist).
Is overriden by explicitly passing an TYPE (or also ELEMENT-TYPE for DENSE-ARRAYS) argument.

#### \*array-element-type-alist\*

```lisp
Variable
Default Value: NIL
```

An ALIST mapping package to the default element-type used in that package.

- Inspired from `SWANK:*READTABLE-ALIST*`
- Overrides none.
- Is overriden by [\*array-element-type\*](#array-element-type) when bound, or by explicitly passing an TYPE (or also ELEMENT-TYPE for DENSE-ARRAYS) argument.

#### \*array-layout\*

```lisp
Variable
Default Value: :ROW-MAJOR
```

For `dense-numericals` this specifies the default layout constructed by `make-array` and
constructor functions like [asarray](#asarray), [zeros](#zeros), [ones](#ones), etc in the
DENSE-ARRAYS-PLUS-LITE package.

For `numericals`, this is a dummy variable provided so that code written for `numericals` may be easily upgradeable to `dense-numericals`.

#### \*broadcast-automatically\*

```lisp
Variable
Default Value: T
```

If non-NIL, operations automatically perform broadcasting as necessary.
If NIL, broadcasting is expected to be performed by the user. Such strictness
can be helpful to locate bugs. Broadcasting follows the [numpy broadcasting semantics](https://numpy.org/doc/stable/user/basics.broadcasting.html).

#### \*default-float-format\*

```lisp
Variable
Default Value: SINGLE-FLOAT
```

Used for converting non-float arrays to float arrays for floating-point
operations like trigonometric functions.

#### \*inline-with-multithreading\*

```lisp
Variable
Default Value: NIL
```

Inlining is usually necessary for smaller arrays; for such arrays multithreading
becomes unnecessary. If this parameter is non-NIL, code using multithreading
would be emitted; otherwise, the code would be skipped.

This is only relevant for transcendental functions which uses lparallel for multithreading.

#### \*multithreaded-threshold\*

```lisp
Variable
Default Value: 80000
```

The lower bound of the array size beyond which LPARALLEL is used for distributing
[transcendental] operations across multiple threads.

NOTE: It is not defined if this bound is inclusive or exclusive.

### Generating arrays

Beyond the `cl:make-array` and `dense-arrays:make-array`, a number of utilities are provided to  generate arrays:

#### From lists of elements: asarray

```lisp
Function: (asarray array-like &key out type layout)
```

- `array-like` can be a list, nested lists, with the nodes ultimately containing numbers or arrays.
- `type` indicates the element-type of the array to be generated. The elements from `array-like` will be coerced to this `type`
    - `type` can also be `auto`, in which case, the element type of the array to be generated will be guessed from the element types of `array-like`
- `layout` is either `:row-major` or `:column-major`. However, `cl:array` can only be `:row-major`, thus, `:column-major` is only applicable for `dense-arrays:array`.

#### Direct ways (avoid list allocation)

More direct ways to generate arrays (without allocating lists) include the functions `zeros` and `ones`. Both have the common lambda list: 

```lisp
(shape &key type layout)
```

The `shape` is a list of numbers, but it can also be a spliced list of numbers.

```lisp
(numericals:zeros 3 :type 'single-float) ;=> #(0.0 0.0 0.0)
(numericals:zeros 2 3 :type 'double-float)
;=> #2A((0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0))
(numericals:zeros '(2 3) :type 'double-float)
;=> #2A((0.0d0 0.0d0 0.0d0) (0.0d0 0.0d0 0.0d0))
(numericals:ones '(2 3) :type 'double-float)
;=> #2A((1.0d0 1.0d0 1.0d0) (1.0d0 1.0d0 1.0d0))
```

In cases where prefilling an array with `0` or `1` is not important, there is also the `empty` function.

A generalization of `ones` is the `full` function, which takes in the additional keyword argument `value`.

```lisp
(numericals:full 4 :type 'double-float :value 42)
;=> #(42.0d0 42.0d0 42.0d0 42.0d0)
```

In addition to these, arrays with uniform random numbers can be generated using the `rand` function. The lambda list for this is similar to `zeros` or `ones` but it has the additional keyword arguments `min` and `max` indicating the range of the uniform number distribution. These have the default values `0` and `1` respectively.

```lisp
(numericals:rand 3 :type 'single-float)
;=> #(0.90184736 0.7570008 0.094017744)
(numericals:rand 3 :type 'single-float :min -10.0 :max 10.0)
;=> #(8.339874 1.0285072 2.144558)
```

All of these have a XXX`-like` counterpart which takes in an existing array and generates an array with shape and element-type similar to the input array.

### Modifying arrays

#### fill

#### aref*

### Transforming arrays

`numericals/utils` and `dense-numericals/utils` only provides `transpose` and `reshape`. The other tranformation function `concat` is provided by [basic-math].

#### transpose

Transposes an array along one or more axes. 

#### reshape

### Element-wise operators

- abs 

Miscellaneous:

- two-arg-max
- two-arg-min

### Array reduction operators

Package: `numericals` or `dense-numericals`

- sum
- vdot
- maximum
- minimum
- arg-maximum
- arg-minimum

## Basic Math

This functionality is provided by the `numericals/basic-math` or `dense-numericals/basic-math` systems and packages. Broadly, These can be divided into the following groups.

### Utilities and array operations

- copy
- coerce
- astype
- concat
- shape
- reshape

### Standard arithmetic operations

Binary operations that take in two arguments and return a new result:

```
add subtract multiply divide
```

Their common lambda list can be given by:

```lisp
Lambda List: (x y &key broadcast out)
```

- The inputs `X` and `Y` to these functions can be numbers, arrays, or array-like objects (such as lists or lists of lists).

- The keyword `OUT` argument can be supplied to use an existing pre-allocated array and avoid array allocation.
- The default value of `BROADCAST` is given by [\*broadcast-automatically\*](#broadcast-automatically) but can be overriden by supplying the keyword `BROADCAST` argument. When this is NIL, arguments must be of the same shape.

Equivalent operations which modify the first argument assuming it is an array end with a '!'.

```
add! subtract! divide! multiply!
```

- In contrast to their non-destructive counterparts, their lambda lists do not contain the `OUT` argument. The first argument is implicitly taken as the `OUT`.

```lisp
Lambda List: (x y &key broadcast)
```

Finally, there are the n-ary operations corresponding to the lisp functions.

```
+ - / *
```

These take in any number of arguments, which can be number, arrays, lists or nested lists, and also an optional keyword argument `OUT`.

In cases where the arguments are non-arrays,

- for binary operations, they are converted to a type given by

```lisp
(or (when (boundp '*array-element-type*)
      *array-element-type*)
    (cdr (assoc *package* *array-element-type-alist*))
    t)
```

- for n-ary operations, the types are upgraded according to the function [numericals/basic-math/impl::normalize-arguments/dmas](https://github.com/digikar99/numericals/blob/master/src/basic-math/n-arg-fn.lisp).

The type upgradation also occurs if the arrays are of heterogeneous types and the `OUT` argument is unsupplied. This upgradation is performed by [numericals/common:max-type](https://github.com/digikar99/numericals/blob/master/common.lisp).

When compiled with `(optimize speed)`, an attempt is made using compiler macros to convert calls to n-ary operations into the binary operations. However, this can be fragile, and for performance reasons, users are recommended to use the binary operations.

### Comparison operations

Similar to the [arithmetic](#standard-arithmetic-operations), these operations can again be grouped into functions that take in two arguments: 

- two-arg-<
- two-arg-<=
- two-arg-=
- two-arg-/=
- two-arg->
- two-arg->=

Their lambda lists is exactly identical to the arithmetic operations.

```lisp
Lambda List: (x y &key broadcast out)
```

CL counterparts that take in two or more than two arguments are given by:

- <
- <=
- =
- /=
- \>
- \>=

In contrast to their CL counterparts, these functions return a `0` or a `1` instead of `NIL` or `T` respectively when their arguments are scalar. When the arguments are arrays or array-like, the output is an array of element type `(unsigned-byte 8)`.

This makes it easy to use SIMD-accelerated operations from [BMAS](https://github.com/digikar99/bmas) and can make a massive difference in performance. Note below that `numericals:two-arg-<` is about 25 times faster than `cl:<`.

```lisp
CL-USER> (let ((a (numericals:rand 10 10 :type 'single-float))
               (b (numericals:rand 10 10 :type 'single-float))
               (c (numericals:rand 10 10 :type t)))
           (declare (optimize speed)
                    (type (simple-array single-float (10 10)) a b)
                    (type (simple-array t (10 10)) c))
           (time (loop repeat 1000000
                       do (loop for i below 10
                                do (loop for j below 10
                                         do (setf (aref c i j)
                                                  (cl:< (aref a i j)
                                                        (aref b i j))))))))
Evaluation took:
  4.939 seconds of real time
  4.938077 seconds of total run time (4.938077 user, 0.000000 system)
  99.98% CPU
  13,843,343,776 processor cycles
  0 bytes consed

NIL
CL-USER> (let ((a (numericals:rand 10 10 :type 'single-float))
               (b (numericals:rand 10 10 :type 'single-float))
               (c (numericals:rand 10 10 :type '(unsigned-byte 8))))
           (declare (optimize speed)
                    (type (simple-array single-float (10 10)) a b)
                    (type (simple-array (unsigned-byte 8) (10 10)) c))
           (time (loop repeat 1000000
                       do (numericals:two-arg-< a b :out c :broadcast nil))))
Evaluation took:
  0.187 seconds of real time
  0.187028 seconds of total run time (0.187028 user, 0.000000 system)
  100.00% CPU
  524,357,913 processor cycles
  144,019,840 bytes consed

NIL
```
### Rounding operations

CL has four rounding operations. Their counterparts are given by the following shadowing symbols in `numericals/basic-math` or `dense-numericals/basic-math`.

- ffloor
- fceiling
- fround
- ftruncate

```lisp
Lambda List: (value &key broadcast out)
```

Similar to the [arithmetic functions](#standard-arithmetic-operations), these also have the in-place counterparts which assume that the first argument is an array and is the implicit `OUT` argument.

- ffloor!
- fceiling!
- fround!
- ftruncate!

### arg-maximum

```lisp
Polymorphic Function: (arg-maximum array-like &rest args923 &key
                       (axis NIL axis924) (keep-dims NIL keep-dims925)
                       (out NIL out926))
```

Find the index of the maximum element along the `axis`.

### arg-minimum

```lisp
Polymorphic Function: (arg-minimum array-like &rest args932 &key
                       (axis NIL axis933) (keep-dims NIL keep-dims934)
                       (out NIL out935))
```

Find the index of the minimum element along the `axis`.


### Other basic operations

- abs!
- matmul
- two-arg-matmul
- dot
- max
- two-arg-max
- min
- two-arg-min
- sum
- maximum
- minimum

- logand
- two-arg-logand
- logior
- two-arg-logior
- logxor
- two-arg-logxor

- lognot
- logandc1
- logandc2
- lognand
- lognor
- logorc1
- logorc2
   ;; #:logtest
   ;; #:logbitp
   ;; #:logcount


## Transcendental Operations

## Linear Algebra

Package: `numericals/linalg` or `dense-numericals/linalg`

The functions in these packages use the [Eigen](https://eigen.tuxfamily.org/index.php?title=Main_Page) library of C++. In actuality, a [lite C interface](https://github.com/digikar99/ceigen_lite/) is used. 

### cholesky

```lisp
Function: (cholesky array-like &key out)
```

Compute the cholesky decomposition of positive definite 2D
matrices given by `array-like`. This uses the `Eigen::LLT` to perform the computation.

For a matrix A, it returns L such that `A = L * L^C` where L is lower triangular, and
`L^C` is the conjugate of L.

References:

- http://www.eigen.tuxfamily.org/dox/classEigen_1_1LLT.html

### det

```lisp
Function: (det array-like &key out)
```

Calculate the determinant of 2D matrices given by `array-like`

### eigvals

```lisp
Function: (eigvals array-like &key eigvals)
```

Use `Eigen::EigenSolver` to compute the eigenvalues of the 2D square matrix
given by `array-like`.

### eigvecs

```lisp
Function: (eigvecs array-like &key eigvals eigvecs)
```

Use `Eigen::EigenSolver` to compute the eigenvalues and
eigvectors of the 2D square matrix given by `array-like`.

### inv

```lisp
Function: (inv array-like &key out)
```

Calculate the inverse of 2D matrices given by `array-like`

### lu

```lisp
Function: (lu array-like &key lu p q)
```

Calculate the `lu` decomposition of `array-like` using `Eigen::FullPivLU`.

For input A, it returns three matrices `p`, `lu`, and `q` such that

                         A=P^{−1} L U Q^{−1}

where L is unit-lower-triangular, U is upper-triangular, and `p` and `q` are permutation matrices.
The matrix `lu` contains L below the diagonal and U above the diagonal.

TODO: matmul seems missing.

The following code illustrates the decomposition and the reconstruction

```lisp
    (let ((a (asarray '((1 2 3) (4 5 6)) :type 'single-float)))
      (multiple-value-bind (p lu q) (lu a)
        (print lu)
        (matmul (inv p)
                   (asarray '((1 0 0) (0.5 1 0)) :type 'single-float)   ; unit lower triangular
                   (asarray '((6 4 5) (0 -1 -0.5)) :type 'single-float) ; upper triangular
                   (inv q))))

    #|

    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  6.000       4.000       5.000    )
      (  0.500      -1.000      -0.500    )
       {101110E403}>
    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  1.000       2.000       3.000    )
      (  4.000       5.000       6.000    )
       {101110E893}>

    |#
```

References:

- [https://eigen.tuxfamily.org/dox/classEigen_1_1FullPivLU.html](https://eigen.tuxfamily.org/dox/classEigen_1_1FullPivLU.html)

### norm2

```lisp
Function: (norm2 array-like)
```

Calculate the L2 norm of vectors or the frobenius norm of 2D matrix.

### outer

No documentation found for `outer`

### pinv

```lisp
Function: (pinv array-like &key out)
```

Calculate the psuedo inverse of 2D matrices given by `array-like`.

### qr

```lisp
Function: (qr array-like &key q r)
```

Calculate the `qr` decomposition of `array-like`.

### rank

```lisp
Function: (rank array-like &key out tol)
```

Use `Eigen::ColPivHouseholderQR` to calculate the rank of the matrix given by `array-like`.

The tolerance or threshold is given by `tol`. If not supplied or given as zero,
it is the default value set by the eigen's methods.

### solve

```lisp
Function: (solve a b &rest args987 &key (out NIL out988))
```

Solves a system of linear equation A*X = `b` and returns X as the output.
At the time of this writing, it uses the `Eigen::partialPivQr` for square matrices `Eigen::householderQR` for non-square matrices.

References:

1. [Eigen::ColPivHouseholderQR documentation](https://eigen.tuxfamily.org/dox/classEigen_1_1ColPivHouseholderQR.html#ad4825c3d43dffdf0c883de09ba891646) for more details

2. [Eigen Linear Algebra Tutorial](https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html)

### svd

```lisp
Function: (svd array-like s u v)
```

Calculate the `svd` decomposition of `array-like` using `Eigen::BDCSVD`.
For input m-by-n input A, it returns `u`, `s`, and `v` such that

                         A = U S V^H

where

-  `u` is a m-by-m unitary,
-  `v` is a n-by-n unitary,
-  and `s` is a m-by-n real positive matrix which is zero outside of its main diagonal

The diagonal entries of `s` are known as the singular values of A and the columns of `u` and `v` are known as the left and right singular vectors of A respectively.

The following code illustrates the decomposition and the reconstruction (FIXME: Update):

```lisp
    (let ((a (asarray '((1 2 3) (4 5 6)) :type 'single-float)))
      (multiple-value-bind (p lu q) (lu a)
        (print lu)
        (matmul (inv p)
                   (asarray '((1 0 0) (0.5 1 0)) :type 'single-float)   ; unit lower triangular
                   (asarray '((6 4 5) (0 -1 -0.5)) :type 'single-float) ; upper triangular
                   (inv q))))

    #|

    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  6.000       4.000       5.000    )
      (  0.500      -1.000      -0.500    )
       {101110E403}>
    #<STANDARD-DENSE-ARRAY :ROW-MAJOR 2x3 SINGLE-FLOAT
      (  1.000       2.000       3.000    )
      (  4.000       5.000       6.000    )
       {101110E893}>

    |#
```

References:

- [https://eigen.tuxfamily.org/dox/classEigen_1_1BDCSVD.html](https://eigen.tuxfamily.org/dox/classEigen_1_1BDCSVD.html)
- [https://eigen.tuxfamily.org/dox/classEigen_1_1JacobiSVD.html](https://eigen.tuxfamily.org/dox/classEigen_1_1JacobiSVD.html)

### vdot

```lisp
Function: (vdot a b)
```

Treat the two input arrays as 1D vectors and calculate their dot product.
 
## Random

## Statistics

## magicl

The following two systems provide packages for using `magicl` functions:

- `numericals/magicl`
- and `dense-numericals/magicl`

Functions in the `numericals/magicl` and the `dense-numericals/magicl` package are essentially wrappers around [magicl](https://github.com/quil-lang/magicl) and return `cl:array` and `dense-arrays:array` respectively. This can be helpful for using magicl with other lisp packages such as numcl or lisp-stat.

## tests

Run `(asdf:test-system "numericals")` or `(asdf:test-system "dense-numericals")`. This will load the `"numericals/tests"` or `"dense-numericals/tests"` system respectively and run the tests.
