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

`numericals` / `dense-numericals` brings you the performance of proto-NumPy with the debugging and development abilities of Common Lisp.

- `numericals` works with `cl:array` making it trivial to interface with rest of the lisp ecosystem
  - `dense-numericals` works with [dense-arrays:array](https://github.com/digikar99/dense-arrays)  provid a numpy-like array object for common lisp. `dense-arrays` itself is fairly extensible, and trivial extensions are provided for [static-vectors](https://github.com/sionescu/static-vectors) as well as [cl-cuda](https://github.com/takagi/cl-cuda)*.

*Currently `dense-numericals` only works with `cl:vector` as the backend storage. Create an [issue](https://github.com/digikar99/numericals/issues) if you'd like dense-numericals to be extended to `static-vectors` or `cl-cuda`.


## Where next?

- [What we mean by writing fast code fast?](./common-lisp-and-numericals)
- [Install numericals or dense-numericals](./install)
- [Compare numericals against other libraries](./comparison)
- [See the manual](./manual)
- [History of numericals](./history)
- [Old documentation page](./old-index.html)

## Limitations

`numericals` / `dense-numericals` do not intend to offer a one-stop solution to the numerical computation ecosystem in common lisp. Even planning to do so is delusionary and should be considered a severe case of NIH syndrome. To that extent, we widely adopt:

- [BMAS](https://github.com/digikar99/bmas) backed by [SLEEF](https://sleef.org/),
- and more recently, [Eigen](https://eigen.tuxfamily.org/) backed by [BLAS](https://www.netlib.org/blas/) and [LAPACK](https://www.netlib.org/lapack/)

This is further coupled with multithreading using [lparallel](https://lparallel.org/) and the C-library builtin [OpenMP](https://www.openmp.org/).

As further testament to the *profoundly found elsewhere* attitude adopted here, we have

- `numericals` working with `cl:array` that can be used in conjunction with other libraries
- two systems `numericals/magicl` and `dense-numericals/magicl` that are essentially wrappers around [magicl](https://github.com/quil-lang/magicl)

Thus, we intend to provide facilities for writing fast code fast, but at the same time, we want to make use of the existing facilities wherever appropriate.

Note that optimizing code written using numericals and dense-numericals requires that the implementations support CLTL2, either natively, or through [cl-environments](https://github.com/alex-gutev/cl-environments). Currently, [a bug in variable-information on CCL](https://github.com/Clozure/ccl/issues/421) may prevent optimization. Type propagation requires that users write code using [peltadot](https://gitlab.com/digikar/peltadot).

## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
- Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki) as well as [numcl](https://github.com/numcl/numcl).
- All the [SLEEF](https://github.com/shibatch/sleef) contributors
- All the contributors of [c2ffi](https://github.com/rpav/c2ffi/) and [cl-autowrap](https://github.com/rpav/cl-autowrap)
- [u/moon-chilled's sassy comment](https://www.reddit.com/r/lisp/comments/wei81o/comment/iitttgb/?utm_source=share&utm_medium=web2x&context=3) (Is that the term?)
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!
