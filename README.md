# numericals

THIS PROJECT IS (VERY) UNSTABLE YET. THINGS ARE SUBJECT TO CHANGE.

This project was previously named `sbcl-numericals`. That project now sits unmaintained in the [sbcl-numericals](./sbcl-numericals) directory.

The project renaming reflects an attempt to separate the ANSI standard parts of the codebase
from the SBCL-specific part, so that a portability attempt may be made in the future.

## Background

Curiosity got the better of me one day, and I set out to explore the limits of numerical computing with Common Lisp. I mean - what does speed require? Just memory-locality and SIMD?
SBCL has memory-locality. What about SIMD? Well, the functionality hasn't been "standardized" yet, and there are several attempts. Indeed, 
SBCL needs more documentation - think Emacs! But knowledge exists in people's heads. People are willing to share it. So, this was possible. 

PS: This library began as a [reddit post](https://www.reddit.com/r/lisp/comments/fkfgjn/sbcl_with_simd_how_to_optimize_sseavx2_to_pointer/), that, in turn, was triggered by [this reddit post](https://www.reddit.com/r/lisp/comments/fjmm6y/deep_learning_with_gpus/).

You should probably use the latest [SBCL (get from git)](https://github.com/sbcl/sbcl), at least SBCL-2.0.4. The build is fairly easy: `sh make.sh && sh run-sbcl.sh # or install.sh`.

## The Plan

The plan is to make the interface as simple and intuitive as possible. But also enable
high performance using SIMD. For the full realm of SIMD possibilities, see [Introduction
to Intel Advanced Vector Extensions](https://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions).

The "intended to be working" list of operations sits in [src/package.lisp](./src/package.lisp).

## TODO (Contributing)

The current list of tasks include:

Quality-based:

- Setting up tests
- Ensuring correctness of already-done operations for (simple-array single-float)

Feature/Quantity-based:

- [SIMD] Ensuring functionality for fixnums and double-floats
- [SIMD] Implementing comparison operators: translating between 1 and 0 of the non-lisp world
to `t` and `nil` of the lisp world; perhaps, adding a parameter that enables or disables
this translation
- [SIMD] Speeding up aref using SIMD or otherwise: we are 5-20x slower than numpy. A part of
the reason is because numpy provides array slices, and I do not know the equivalent for common
lisp based systems.
- [SIMD] Speeding up `concatenate` for axis>0.
- Implementing package (perhaps not based on SIMD) for non-SBCL systems

## Benchmarks






<div id='benchmark'>
  <p>SBCL is faster than NUMPY by (horizontal indicates array sizes; vertical indicates various operations):
  </p>
  <table>
<tr>
  <th>Non-broadcast array operations
  </th>
<th>100
</th>
<th>10000
</th>
<th>1000000
</th>
<th>100000000
</th>
</tr>
<tr>
  <td>+
  </td>
<td>1.09x
</td>
<td>1.09x
</td>
<td>1.06x
</td>
<td>1.00x
</td>
</tr>
<tr>
  <td>-
  </td>
<td>1.21x
</td>
<td>1.17x
</td>
<td>1.02x
</td>
<td>1.01x
</td>
</tr>
<tr>
  <td>*
  </td>
<td>1.15x
</td>
<td>1.17x
</td>
<td>0.99x
</td>
<td>0.99x
</td>
</tr>
<tr>
  <td>/
  </td>
<td>1.26x
</td>
<td>1.31x
</td>
<td>0.99x
</td>
<td>1.00x
</td>
</tr>
  </table>
</div>

<!-- The above div would be filled by :numericals/tests when *write-to-readme* is T. -->

## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
  - Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki) as well as [numcl](https://github.com/numcl/numcl).
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!

