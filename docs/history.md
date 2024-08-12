# History

Curiosity got the better of me one day, and I set out to explore the limits of numerical computing with Common Lisp. I mean - what does speed require? Just memory-locality and SIMD?
SBCL has memory-locality. What about SIMD? Well, the functionality hasn't been "standardized" yet, and there are several attempts. Indeed,
SBCL needs more documentation - think Emacs! But knowledge exists in people's heads. People are willing to share it. So, this was possible.

PS: This library began as a [reddit post](https://www.reddit.com/r/lisp/comments/fkfgjn/sbcl_with_simd_how_to_optimize_sseavx2_to_pointer/), that, in turn, was triggered by [this reddit post](https://www.reddit.com/r/lisp/comments/fjmm6y/deep_learning_with_gpus/).

You should probably use the latest [SBCL (get from git)](https://github.com/sbcl/sbcl), at least SBCL-2.0.9. The build is fairly easy: `sh make.sh && sh run-sbcl.sh # or install.sh` or pick a binary from [here](http://www.sbcl.org/platform-table.html).

## Project Predecessors

- [sbcl-numericals](./sbcl-numericals)
- [numericals-2020.08](https://github.com/digikar99/numericals/releases/tag/2020.08)

The project renaming reflects an attempt to separate the portable parts of the codebase
from the SBCL-specific part, so that a portability attempt may be made in the future.
