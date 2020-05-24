For a full lispy capability, we need primitive SIMD operations that can operate on any number of arguments.

A good abstraction entails being able to separate compiler specific code from ansi-standard common lisp code.

The ansi-standard CL code should `use` an appropriate package based on conditional-reader-macros.


