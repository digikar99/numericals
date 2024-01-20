# API Reference

### Configuration

Package: `numericals` or `dense-numericals`

`numericals` and `dense-numericals` come with a number of dynamically bound configuration variables that are put to use in non-inlined code. These include:

- \*multithreaded-threshold\*
- \*default-float-format\*
- \*inline-with-multithreading\*
- \*array-element-type\*
- \*array-layout\*
- \*broadcast-automatically\*

### Generating and transforming arrays

Package: `numericals` or `dense-numericals`

There are also a number of ways to generate arrays:

- from lists using [asarray](#asarray)
- simple filled arrays using 
    - [zeros](#zeros)
    - [ones](#ones)
    - [rand](#rand)
    - [full](#full)
    - [eye](#eye)
- and their counterparts using
    - [zeros-like](#zeros-like)
    - [ones-like](#ones-like)
    - [rand-like](#rand-like)
    - [full-like](#full-like)
    
You can also use other arrays to generate arrays using:

- [copy](#copy)
- [transpose](#transpose)
- [concat](#concat)
- [reshape](#reshape)

### Element-wise operators

Package: `numericals` or `dense-numericals`

Binary arithmetic operators:

- add
- subtract
- multiply
- divide

Binary logical operators:

- two-arg-<
- two-arg-<=
- two-arg-=
- two-arg-/=
- two-arg->
- two-arg->=

Their n-ary counterparts:

- + - / *
- < <= = /= > >=

Transcendental operators:

- sin asin sinh asinh
- cos acos cosh acosh
- tan atan tanh atanh
- exp expt
- log

Rounding operators:

- abs 
- ffloor
- floor
- fceiling
- ftruncate

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

### Linear algebra operators

Package: `numericals.linalg` or `dense-numericals.linalg`

- inv
- lu
- svd
- vdot
- cholesky
- solve
- eigvals
- qr
- pinv
- eigvecs
- rank
- norm2
- det
- matmul
