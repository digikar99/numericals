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


To enable fast numpy-like view-based slicing, we have provided a drop-in replacement for common lisp arrays. Besides being used like usual specialized arrays, these include an additional `strides` slot that enables the slicing.

Also broadcasting:

```lisp
(nu:+ (nu:asarray '((1 2 3)))
      (nu:asarray '((1) (2))))
; #2A((2.0 3.0 4.0)
;  (3.0 4.0 5.0)
; )
```

## What about others?

I don't know. There are [many others](https://www.cliki.net/linear%20algebra).

## The Plan

The plan is to enable number crunching using this, coupled with py4cl/2. For "light" array
manipulation, we stay within lisp. While for heavy manipulation - deep learning - we offload
to the existing python ecosystem. This should eliminate the ~10000 op/sec limitations of py4cl/2.

SIMD is rich. See [Introduction
to Intel Advanced Vector Extensions](https://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions) for the full realm of possibilities.

## The Juicy Bits (Done)

The following operations are in the "done" bucket for `single-float`s. [tests](./tests/) have been set up for the appropriate ones amongst these.
So, none of the following should \*not\* work. If something doesn't work, [file an issue](https://github.com/digikar99/numericals/issues).

Prerequisites: SBCL 2.0.5.

Operation list:

- `+`
- `-`
- `*`
- `/`
- sqrt
- aref
- concatenate (unoptimized for axis != 0)
- map-outer (speed untested)
- zeros 
- ones
- empty (same as zeros)
- asarray (unoptimized and not done for arrays inside nested lists)
- astype (unoptimized)
- shape (unoptimized)

- weop [macro]
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
- [SIMD Medium] Speeding up `concatenate` for axis>0.
- [SIMD Easy] Determining and Implementing Trigonometric functions
- [SIMD Easy] Implementing bit-wise boolean operators
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
<td>7.52x
</td>
<td>4.91x
</td>
<td>0.47x
</td>
<td>0.27x
</td>
<td>0.28x
</td>
</tr>
<tr>
  <td>ZEROS
  </td>
<td>3.14x
</td>
<td>1.98x
</td>
<td>0.16x
</td>
<td>0.21x
</td>
<td>0.00x
</td>
</tr>
<tr>
  <td>EMPTY
  </td>
<td>3.64x
</td>
<td>2.89x
</td>
<td>0.09x
</td>
<td>0.00x
</td>
<td>0.00x
</td>
</tr>
<tr>
  <th>Non-broadcast array operations (If you know you do not need broadcast, you might want to try using WEOP that is specialized for element-wise operations)
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
<td>0.75x
</td>
<td>0.69x
</td>
<td>0.55x
</td>
<td>0.79x
</td>
<td>0.84x
</td>
</tr>
<tr>
  <td>-
  </td>
<td>0.91x
</td>
<td>0.72x
</td>
<td>0.52x
</td>
<td>0.76x
</td>
<td>0.83x
</td>
</tr>
<tr>
  <td>*
  </td>
<td>0.90x
</td>
<td>0.69x
</td>
<td>0.57x
</td>
<td>0.78x
</td>
<td>0.85x
</td>
</tr>
<tr>
  <td>/
  </td>
<td>0.76x
</td>
<td>0.71x
</td>
<td>0.63x
</td>
<td>0.77x
</td>
<td>0.83x
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
<td>0.63x
</td>
<td>0.48x
</td>
<td>0.17x
</td>
<td>1.16x
</td>
<td>0.62x
</td>
</tr>
<tr>
  <td>-
  </td>
<td>0.72x
</td>
<td>0.49x
</td>
<td>0.17x
</td>
<td>1.15x
</td>
<td>0.59x
</td>
</tr>
<tr>
  <td>*
  </td>
<td>0.71x
</td>
<td>0.49x
</td>
<td>0.18x
</td>
<td>1.07x
</td>
<td>0.57x
</td>
</tr>
<tr>
  <td>/
  </td>
<td>0.72x
</td>
<td>0.49x
</td>
<td>0.19x
</td>
<td>1.14x
</td>
<td>0.59x
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
<td>1.26x
</td>
<td>1.74x
</td>
<td>0.69x
</td>
<td>0.67x
</td>
<td>0.49x
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

