# sbcl-numericals


## Background

Curiosity got the better of me one day, and I set out to explore the limits of numerical computing with Common Lisp. I mean - what does speed require? Just memory-locality and SIMD?
SBCL has memory-locality. What about SIMD? Well, the functionality hasn't been "standardized" yet, and there are several attempts. Indeed, 
SBCL needs more documentation - think Emacs! But knowledge exists in people's heads. People are willing to share it. So, this was possible. 

PS: This library began as a [reddit post](https://www.reddit.com/r/lisp/comments/fkfgjn/sbcl_with_simd_how_to_optimize_sseavx2_to_pointer/), that, in turn, was triggered by [this reddit post](https://www.reddit.com/r/lisp/comments/fjmm6y/deep_learning_with_gpus/).

## So far...

We only have the operations `s+ s- s* s/` working on single float simple array,
and the corresponding `d+ d- d* d/` working on double float simple arrays; all using SIMD, not tested
for arrays without 4/8 multiple sizes. See [sbcl-numericals.lisp](src/sbcl-numericals.lisp).
There are no tests yet either.

## How does this compare to other numerical libraries?

For one, we are currently very limited in features. For the full realm of possibilities, see [Introduction
to Intel Advanced Vector Extensions](https://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions).

However, because, numpy still uses SSE, and we are using AVX, we do happen to be about 3 times
faster in these basic operations. In theory, a speed-up of less than 2 is expected, since
the amount of human resources that go into something like numpy is potentially 10s of times
greater than the resources spent on SBCL and this library; and so, numpy would be expected to
be faster.

### 256-long vector

```py
a = np.random.random((256)) 
b = np.random.random((256)) 
c = np.zeros((256))
def foo(num): 
  start = time.time() 
  for i in range(num): 
    np.multiply(a, b, out = c) 
  return time.time() - start 
print(foo(1000000))
# 1.0689892768859863
```

Python is the bottleneck I'd guess. See the two other cases below.

```lisp
(defparameter double-list (loop for i below 256
                             collect (+ i 0.1d0)))
(defparameter arr-a (make-array '(256) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-b (make-array '(256) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-c (make-array '(256) :initial-element 0.0d0
                                :element-type 'double-float))
(time (loop for i below 1000000
         do (d* arr-a arr-b arr-c)))
;; Evaluation took:
;;   0.137 seconds of real time
;;   0.137595 seconds of total run time (0.137595 user, 0.000000 system)
;;   100.73% CPU
;;   303,799,698 processor cycles
;;   63,995,904 bytes consed
```

### 1024x1024

```py
a = np.random.random((1024, 1024)) 
b = np.random.random((1024, 1024)) 
c = np.zeros((1024, 1024))
def foo(num): 
  start = time.time() 
  for i in range(num): 
    np.multiply(a, b, out = c) 
  return time.time() - start 
print(foo(10000))
# 13.556719779968262
```

Twice as fast - a bit better than expectations.

```lisp
(defparameter double-list (loop for i below 1024
                             collect (loop for j below 1024
                                        collect (+ i j 0.1d0))))
(defparameter arr-a (make-array '(1024 1024) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-b (make-array '(1024 1024) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-c (make-array '(1024 1024) :initial-element 0.0d0
                                :element-type 'double-float))

(time (loop for i below 10000
         do (d* arr-a arr-b arr-c)))
;; Evaluation took:
;;   6.713 seconds of real time
;;   6.712866 seconds of total run time (6.712866 user, 0.000000 system)
;;   100.00% CPU
;;   14,822,035,318 processor cycles
;;   1,277,952 bytes consed
```

### 1024x1024x32

```py
a = np.random.random((1024, 1024, 32)) 
b = np.random.random((1024, 1024, 32)) 
c = np.zeros((1024, 1024, 32))
def foo(num): 
  start = time.time() 
  for i in range(num): 
    np.multiply(a, b, out = c) 
  return time.time() - start 
print(foo(100))
# 5.072538375854492
```

This one's within the expected 'under 2 times' faster bracket.

```lisp
(defparameter double-list (loop for i below 1024
                             collect (loop for j below 1024
                                        collect (loop for k below 32
                                                   collect (+ i j k 0.1d0)))))
(defparameter arr-a (make-array '(1024 1024 32) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-b (make-array '(1024 1024 32) :initial-contents double-list
                                :element-type 'double-float))
(defparameter arr-c (make-array '(1024 1024 32) :initial-element 0.0d0
                                :element-type 'double-float))
(time (loop for i below 100
         do (d* arr-a arr-b arr-c)))
;; Evaluation took:
;;   3.115 seconds of real time
;;   3.115278 seconds of total run time (3.115278 user, 0.000000 system)
;;   100.00% CPU
;;   6,878,583,088 processor cycles
;;   32,768 bytes consed
```


## Acknowledgements

- Everyone who has contributed to SBCL.
- [u/love5an](https://www.reddit.com/user/love5an/) and [u/neil-lindquist](https://www.reddit.com/user/neil-lindquist/) for the required hand-holding and the [gist](https://gist.github.com/Lovesan/660866b96a2632b900359333a251cc1c).
  - Paul Khuong for [some](https://pvk.ca/Blog/2013/06/05/fresh-in-sbcl-1-dot-1-8-sse-intrinsics/) [blog posts](https://pvk.ca/Blog/2014/08/16/how-to-define-new-intrinsics-in-sbcl/).
- [guicho271828](https://github.com/guicho271828) for attempting an [SBCL Wiki](https://github.com/guicho271828/sbcl-wiki/wiki).
- It's possible that I could have forgotten to mention somebody - so... yeah... happy number crunching!

