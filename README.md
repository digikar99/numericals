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

The plan is to enable number crunching using this, coupled with py4cl/2. For "light" array
manipulation, we stay within lisp. While for heavy manipulation - deep learning - we offload
to the existing python ecosystem. This should eliminate the ~10000 op/sec limitations of py4cl/2.

SIMD is rich. See [Introduction
to Intel Advanced Vector Extensions](https://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions) for the full realm of possibilities.

## The Juicy Bits (Done)

The following operations are in the "done" bucket for `single-float`s. [tests](./tests/) have been set up for the appropriate ones amongst these.
So, none of the following should \*not\* work. If some thing doesn't work, [file an issue](https://github.com/digikar99/numericals/issues).

Prerequisites: SBCL 2.0.4+.

Operation list:

- `+`
- `-`
- `*`
- `/`
- aref (unoptimized; hard to optimize; use map-outer if possible)
- concatenate (unoptimized for axis != 0)
- map-outer (speed untested)
- zeros 
- ones
- empty (same as zeros)
- asarray (unoptimized and not done for arrays inside nested lists)
- astype (unoptimized)
- shape (unoptimized)

- with-simd-operations [macro]
- with-inline [macro]
- with-array / `with-arrays*` [macro]
- with-constant / with-constants [macro]
- maybe-form-not-constant-error
- def-array [macro]
- numericals-array-element-type
- `*type*`
- `*lookup-type-at-compile-time*`

## TODO (Contributing)

The current list of tasks along with I-feel-to-be difficulty include:

- [SIMD Easy] Ensuring functionality for fixnums and double-floats 
- [SIMD Medium] Implementing comparison operators: translating between 1 and 0 of the non-lisp world
to `t` and `nil` of the lisp world; perhaps, adding a parameter that enables or disables
this translation
- [SIMD Hard] Speeding up aref using SIMD or otherwise: we are 5-20x slower than numpy. A part of
the reason is because numpy provides array slices, and I do not know the equivalent for common
lisp based systems.
- [SIMD Medium] Speeding up `concatenate` for axis>0.
- [SIMD Easy] Determining and Implementing Trigonometric functions 
- [Easy] Implementing package (perhaps not based on SIMD) for non-SBCL systems
- [Easy] Adding tests for not-yet-tested things
- [Medium] Adding more compiler macros and checks for greater efficiency
- [Hard] Implementing `asarray` for arrays nested inside lists of lists reasonably efficiently

## Benchmarks


<div id='benchmark'>
  <p>SBCL is faster than NUMPY by (horizontal indicates array sizes; vertical indicates various operations): 
  </p>
  <table>
<tr>
  <th>Allocation array operations (seems like SBCL allocates arrays
during compilation time)
  </th>
<th>10
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
  <td>ONES
  </td>
<td>20.71x
</td>
<td>20.66x
</td>
<td>35.97x
</td>
<td>∞x
</td>
<td>∞x
</td>
</tr>
<tr>
  <td>ZEROS
  </td>
<td>7.73x
</td>
<td>7.74x
</td>
<td>19.62x
</td>
<td>∞x
</td>
<td>∞x
</td>
</tr>
<tr>
  <td>EMPTY
  </td>
<td>7.66x
</td>
<td>7.63x
</td>
<td>8.42x
</td>
<td>2.46x
</td>
<td>∞x
</td>
</tr>
<tr>
  <th>Non-broadcast array operations
  </th>
<th>10
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
<td>41.75x
</td>
<td>28.30x
</td>
<td>1.54x
</td>
<td>0.99x
</td>
<td>0.99x
</td>
</tr>
<tr>
  <td>-
  </td>
<td>41.59x
</td>
<td>29.05x
</td>
<td>1.48x
</td>
<td>1.03x
</td>
<td>0.98x
</td>
</tr>
<tr>
  <td>*
  </td>
<td>42.39x
</td>
<td>28.27x
</td>
<td>1.57x
</td>
<td>1.02x
</td>
<td>0.97x
</td>
</tr>
<tr>
  <td>/
  </td>
<td>44.99x
</td>
<td>21.98x
</td>
<td>1.60x
</td>
<td>1.09x
</td>
<td>1.02x
</td>
</tr>
<tr>
  <th>Broadcast array operations (warning: can vary quite a bit depending
on actual array dimensions)
  </th>
<th>10
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
<td>1.22x
</td>
<td>1.67x
</td>
<td>1.42x
</td>
<td>1.24x
</td>
<td>0.63x
</td>
</tr>
<tr>
  <td>-
  </td>
<td>1.60x
</td>
<td>1.76x
</td>
<td>1.38x
</td>
<td>1.22x
</td>
<td>0.63x
</td>
</tr>
<tr>
  <td>*
  </td>
<td>1.61x
</td>
<td>1.73x
</td>
<td>1.14x
</td>
<td>0.95x
</td>
<td>0.53x
</td>
</tr>
<tr>
  <td>/
  </td>
<td>3.01x
</td>
<td>3.01x
</td>
<td>1.37x
</td>
<td>1.40x
</td>
<td>0.70x
</td>
</tr>
<tr>
  <th>Concatenate (currently unoptimized for axis != 0; 
as such this can be slower than numpy by a factor of 50)
  </th>
<th>10
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
  <td>Axis 0
  </td>
<td>2.98x
</td>
<td>2.96x
</td>
<td>1.29x
</td>
<td>0.75x
</td>
<td>0.71x
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

