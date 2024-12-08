# Manual

There are two main asdf systems:

- `numericals` is designed to work with `cl:array` so that interfacing with the rest of the lisp ecosystem is trivial
- `dense-numericals` is designed to work with [dense-arrays:array](https://github.com/digikar99/dense-arrays)

Each of these has a number of systems and corresponding packages:

- utils
- basic-math
- transcendental
- statistics
- linalg
- random

These can be loaded individually. For example `(asdf:load-system "numericals/random")`. Except `utils` all the others depend on C foreign libraries.

See [Installation](./install.md) to get started.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Manual](#manual)
    - [Utilities](#utilities)
        - [Configuration](#configuration)
        - [Generating arrays](#generating-arrays)
        - [Modifying arrays](#modifying-arrays)
        - [Transforming arrays](#transforming-arrays)
    - [Basic Math](#basic-math)
        - [Standard arithmetic operations](#standard-arithmetic-operations)
        - [Comparison operations](#comparison-operations)
        - [Rounding operations](#rounding-operations)
        - [Bitwise Operators](#bitwise-operators)
        - [abs](#abs)
        - [arg-maximum/arg-minimum](#arg-maximumarg-minimum)
        - [maximum/minimum](#maximumminimum)
        - [sum](#sum)
    - [Transcendental Operations](#transcendental-operations)
        - [Trigonometric Operations](#trigonometric-operations)
        - [Exponentiation](#exponentiation)
        - [Natural Logarithm](#natural-logarithm)
        - [In-place Operations](#in-place-operations)
    - [Linear Algebra](#linear-algebra)
        - [cholesky](#cholesky)
        - [det](#det)
        - [eigvals](#eigvals)
        - [eigvecs](#eigvecs)
        - [inv](#inv)
        - [lu](#lu)
        - [norm2](#norm2)
        - [outer](#outer)
        - [pinv](#pinv)
        - [qr](#qr)
        - [rank](#rank)
        - [solve](#solve)
        - [svd](#svd)
        - [vdot](#vdot)
    - [Random](#random)
        - [seed](#seed)
        - [gaussian](#gaussian)
        - [chisquare](#chisquare)
        - [beta](#beta)
    - [Statistics](#statistics)
        -  [mean](#mean)
        -  [variance](#variance)
        -  [std](#std)
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

#### Transposing existing arrays

```lisp
Function: (transpose array &key axes)
```

Transposes an array along one or more `axes`. See [numpy's tranpose documentation](See https://numpy.org/doc/stable/reference/generated/numpy.transpose.html) for more details. 

#### Reshaping existing arrays

```lisp
Function: (reshape array new-shape)
```

See [numpy's reshape documentation](https://numpy.org/doc/stable/reference/generated/numpy.reshape.html).

#### Copying existing arrays

This functionality depends on C foreign libraries and is made available after loading `numericals/basic-math` or `dense-numericals/basic-math`. An alternative is to use `alexandria:copy-array` or `dense-arrays:copy-array`.

#### Type-casting existing arrays

 astype

### Modifying arrays

#### fill

```lisp
Lambda List: (fill array value)
```

Fill each location in `array` with `value`.

#### aref*

Accessor function for arrays with semantics similar to numpy's indexing semantics.
See https://numpy.org/doc/stable/user/basics.indexing.html

```lisp
[Enhanced] Lambda List: (aref* array &rest subscripts &key out)
```

Each element of SUBSCRIPTS can be
- either an integer denoting the position within the axis which is to be indexed
- or a list of the form (&OPTIONAL START &KEY END STEP) with each of START END
  STEP being integers if supplied. START denotes the start position within the
  axis, END denotes the ending position within the axis, STEP denotes at what
  distance within the axis the next element should come after the previous,
  starting from START

Each of the SUBSCRIPTS, START, END, STEP can also be negative integers, in which
case the last element along the axis is given the index -1, the second last is
given the index -2 and so on. Thus, `(aref ... '(-1 :step -1))` can reverse a one
dimensional array.

Like, `cl:aref` or `abstract-arrays:aref`, returns the element corresponding to SUBSCRIPTS
if all the subscripts are integers and there as many subscripts as the rank of the array.

The performance of this function is slightly different for `cl:array` compared to
`dense-arrays:array`. In particular, numpy-like indexing requires multidimensional offsets. `cl:array` only have a single dimensional offset, thus, when using `aref*` a copy of the `cl:array` is created. The copy may be made into a preallocated array supplied using the `:out` keyword argument. In contrast, because `dense-arrays:array` support multidimensional offsets and strides, merely a wrapper object (a "view") is created. A view is a window into the original array and thus avoids copying the elements of the original array. This occurs when the number (aka length) of SUBSCRIPTS were less than the array's rank, or if some of the SUBSCRIPTS were lists described above.

Examples illustrating the numpy-equivalent indexes:

    a[::]       (aref a nil)
    a[::2]      (aref a '(0 :step 2))
    a[3, ::-1]  (aref a 3 '(-1 :step -1))
    a[3::, -1]  (aref a '(3) -1)

The SUBSCRIPTS can also be integer or boolean arrays, denoting which elements
to select from each of the axes. But in this case the corresponding elements
of the array are copied over into a new array.

### Transforming arrays

`numericals/utils` and `dense-numericals/utils` only provides `transpose` and `reshape`. The other tranformation function `concat` is provided by [basic-math].

## Basic Math

This functionality is provided by the `numericals/basic-math` or `dense-numericals/basic-math` systems and packages. Broadly, These can be divided into the following groups.

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

### Bitwise Operators

Currently, only the following bitwise operators are implemented:

- lognot
- two-arg-logand
- two-arg-logior
- two-arg-logxor

These employ integer arrays as both input and output. 

### abs

```lisp
Polymorphic Function: (abs x &key out broadcast)
Polymorphic Function: (abs! x)
```

Takes a number or array as an input and computes their absolute value.

`abs!` computes the absolute value in place and necessarily requires that the input is an array.

### arg-maximum/arg-minimum

```lisp
Polymorphic Function: (arg-maximum array-like &key axis keep-dims out)
Polymorphic Function: (arg-minimum array-like &key axis keep-dims out)
```

Find the index of the maximum/minimum element along the `axis`.

### maximum/minimum

```lisp
Polymorphic Function: (maximum array-like &key axes keep-dims out)
Polymorphic Function: (minimum array-like &key axes keep-dims out)
```

Find the maximum/minimum elements along one or multiple `axes` of `array-like` into `out`.

If `out` is unsupplied, allocates a new array of appropriate dimensions.

If `keep-dims` is non-NIL, the rank of `out` will be the same as `array-like`, otherwise it will be reduced for each `axes`.

```lisp
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:maximum '((1 2 3)
                                 (-1 4 -1))
                               :axes 0))
#(1.0 4.0 3.0)
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:maximum '((1 2 3)
                                 (-1 4 -1))
                               :axes 1))
#(3.0 4.0)
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:maximum '((1 2 3)
                                 (-1 4 -1))
                               :axes '(0 1)))
4.0
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:maximum '((1 2 3) 
                                 (-1 4 -1))))
4.0
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:maximum '((1 2 3)
                                 (-1 4 -1)) 
                               :axes '(0 1) :keep-dims t))
#2A((4.0))
```

### sum

```lisp
Polymorphic Function: (sum array-like &key axes keep-dims out)
```

Find the sum of elements along one or multiple `axes` of `array-like` into `out`.

If `out` is unsupplied, allocates a new array of appropriate dimensions.

If `keep-dims` is non-NIL, the rank of `out` will be the same as `array-like`, otherwise it will be reduced for each `axes`.

```lisp
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:sum '((1 2 3)
                             (-1 4 -1))
                           :axes 0))
#(0.0 6.0 2.0)
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:sum '((1 2 3)
                             (-1 4 -1))
                           :axes 1))
#(6.0 2.0)
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:sum '((1 2 3)
                             (-1 4 -1))
                           :axes '(0 1)))
8.0
CL-USER> (let ((numericals:*array-element-type* 'single-float))
           (numericals:sum '((1 2 3)
                             (-1 4 -1))
                           :keep-dims t))
#2A((8.0))
```

## Transcendental Operations

Package: `numericals/transcendental` or `dense-numericals/transcendental`

The following operations have been implemented that use SIMD powered by [SLEEF](https://sleef.org/).

### Trigonometric Operations

```
| Standard           | sin   | cos   | tan   |
| Inverse            | asin  | acos  | atan  |
| Hyperbolic         | sinh  | cosh  | tanh  |
| Inverse Hyperbolic | asinh | acosh | atanh |
```

All have the following signature:

```lisp
Function: (<fn> x &key out broadcast)
```

`atan` additionally has the following signature,  mimicking the optional `x` argument of `cl:atan`:

```lisp
Function: (<fn> y x &key out broadcast)
```

### Exponentiation

```lisp
Function: (expt base power &key broadcast out)
Function: (exp x &key broadcast out) # base e exponentiation
```

### Natural Logarithm

This has two signatures, mimicking the optional `base` argument of `cl:log`:

```lisp
Function: (log x &key broadcast out)
Function: (log x y &key broadcast out)
```

The first signature computes the natural logarithm of `x`, while the second signature uses `y` 

### In-place Operations

All of the above have in-place equivalents obtained by appending a '!' to the function name. These do not require supplying the `out` or `broadcast` argument. Instead, the first argument is treated as `out` and modified in-place; `broadcast` is taken as `nil`.

```lisp
CL-USER> (let ((a (numericals:rand 3 :type 'single-float)))
           (print a)
           (print (numericals:sin a))
           (print a))

#(0.46031213 0.51501644 0.61946905)
#(0.44422776 0.49254915 0.58060294)
#(0.46031213 0.51501644 0.61946905)
#(0.46031213 0.51501644 0.61946905)
CL-USER> (let ((a (numericals:rand 3 :type 'single-float)))
           (print a)
           (print (numericals:sin! a))
           (print a))

#(0.6814344 0.97704315 0.50856435)
#(0.6299077 0.8288467 0.48692378)
#(0.6299077 0.8288467 0.48692378)
#(0.6299077 0.8288467 0.48692378)
```

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

Package: `numericals/random` or `dense-numericals/random`

Like Linear Algebrain, the functions in these packages too use the [Eigen](https://eigen.tuxfamily.org/index.php?title=Main_Page) library of C++. In actuality, a [lite C interface](https://github.com/digikar99/ceigen_lite/) is used.

Each of the random number generator can return either a scalar floating point number, or an array of floating point numbers with the given `shape` or `size`.

### seed

To make random number generation reproducible, the `seed` function is provided:

```
Function: (seed unsigned-byte-64)
```

This allows the following piece of code to always generate the same results:

```lisp
(progn
  (numericals/random:seed 42)
  (numericals/random:gaussian))
```

Users may then average the performance of their algorithms against a variety of seeds.

### gaussian

```lisp
Function: (gaussian &key loc mean scale std shape size type out)
```

Returns a scalar or an array of shape `shape` (or `size`) filled with random numbers drawn from a gaussian/normal distribution centered at `loc` (or `mean`) and standard deviation `scale` (or `std`).

If `shape` (or `size`) is `nil` (default) and `out` is `nil`, then only a scalar is returned.

The following are analogous pairs of arguments. Supply only one of these.

- `loc` and `mean`
- `scale` and `std`
- `size` and `shape`

For more information and examples, see: https://numpy.org/doc/stable/reference/random/generated/numpy.random.normal.html

### chisquare

```lisp
Function: (chisquare &key size shape out type (ndof 1))
```

Returns a scalar or an array of shape `shape` (or `size`) filled with random numbers drawn from a chisquare distribution with `ndof` as the degrees of freedom.

If `shape` (or `size`) is `nil` (default) and `out` is `nil`, then only a scalar is returned.

Exactly one of `size` or `shape` must be supplied; both mean the same thing.

For more information and examples, see:
https://numpy.org/doc/stable/reference/random/generated/numpy.random.Generator.chisquare.html

### beta

```lisp
Function: (beta a b &key size shape out type)
```

Returns a scalar or an array of shape `shape` (or `size`) filled with random numbers drawn from a beta distribution with parameters A (alpha) and B (beta).

If `shape` (or `size`) is `nil` (default) and `out` is `nil`, then only a scalar is returned.

Exactly one of `size` or `shape` must be supplied; both mean the same thing.

For more information and examples, see: https://numpy.org/doc/stable/reference/random/generated/numpy.random.Generator.beta.html

## Statistics

Package: `numericals/statistics` or `dense-numericals/statistics`

### mean

```lisp
Function: (mean array-like &key out axes keep-dims)
```

See https://numpy.org/doc/stable/reference/generated/numpy.mean.html

### variance

```lisp
Function: (variance array-like &key out axes keep-dims (ddof 0))
```

See https://numpy.org/doc/stable/reference/generated/numpy.var.html

### std

```lisp
Function: (std array-like &key out axes keep-dims (ddof 0))
```

See https://numpy.org/doc/stable/reference/generated/numpy.std.html

## magicl

The following two systems provide packages for using `magicl` functions:

- `numericals/magicl`
- and `dense-numericals/magicl`

Functions in the `numericals/magicl` and the `dense-numericals/magicl` package are essentially wrappers around [magicl](https://github.com/quil-lang/magicl) and return `cl:array` and `dense-arrays:array` respectively. This can be helpful for using magicl with other lisp packages such as numcl or lisp-stat.

## tests

Run `(asdf:test-system "numericals")` or `(asdf:test-system "dense-numericals")`. This will load the `"numericals/tests"` or `"dense-numericals/tests"` system respectively and run the tests.
