# Installation

Two main asdf systems are provided:

- `numericals` which works with `cl:array`,
- and `dense-numericals` which works with [dense-arrays](https://github.com/digikar99/dense-arrays)

Load either or both of them according to your needs.

## Without quicklisp client: ocicl

Install ocicl as per the instructions [here](https://github.com/ocicl/ocicl).

Once ocicl is installed, setup, and the runtime loaded into the lisp image, simply:

```lisp
(asdf:load-system "numericals")
;;; OR
(asdf:load-system "dense-numericals")
```

## Using quicklisp client

[quicklisp](https://www.quicklisp.org/beta/) is a defacto package manager for Common Lisp. All the below methods rely on the availability of the quicklisp client. The instructions to install it can be found [here](https://www.quicklisp.org/beta/#installation). Other useful resources to get started with quicklisp include:

- [quicklisp - Common Lisp Libraries](https://common-lisp-libraries.readthedocs.io/quicklisp/#getting-started)
- [Installing quicklisp - The Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/getting-started.html#install-quicklisp)

Once the quicklisp client is installed, proceed with one of the below methods to install `numericals` or `dense-numericals` or both.

### Through quicklisp dist alone

Simply quickload the required asdf system. 

```lisp
(ql:quickload "numericals")
;;; OR
(ql:quickload "dense-numericals")
```

Depending on when you are reading this, the quicklisp versions could be a bit older than what you might wish for. If you want to use recent feature updates or bug fixes, you might want to use ultralisp or download-dependencies.

### Using ultralisp dist

If you are trying [Ultralisp](https://ultralisp.org/) for the first time:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```

If you have been using ultralisp, simply update its dist:

```lisp
(ql:update-dist "ultralisp")
```

Once `(ql:quickload "numericals")` or `(ql:quickload "dense-numericals")` is successful; use inside your own package using `:mix` option of `uiop:define-package`, or [package-local-nicknames](https://common-lisp-libraries.readthedocs.io/#libraries).

### Using download-dependencies

First clone the [github repository of numericals](https://github.com/digikar99/numericals) to where quicklisp can find.

```sh
cd /path/to/quicklisp/local-projects/ # This is usually $HOME/quicklisp/local-projects
git clone https://github.com/digikar99/numericals
```

[download-dependencies](https://github.com/digikar99/download-dependencies) is a simple tool to download the dependencies of any given project. In order to use it, clone also the `download-dependencies` asdf system into your quicklisp local projects.

```sh
git clone https://github.com/digikar99/download-dependencies
```

Then create the directory where you want to download the dependencies for `numericals` or `dense-numericals`.

```sh
mkdir -p /path/to/dependencies
```

Finally, `quickload` and download the dependencies:

```lisp
(ql:quickload "download-dependencies")
(let ((download-dependencies:*dependencies-home* #P"/path/to/dependencies/"))
  (download-dependencies:ensure-system "numericals"))
```

Finally, you will need to instruct the quicklisp client to look into this path. After that, you can simply use `quickload`:

```lisp
(push #P"/path/to/dependencies" ql:*local-project-directories*)
(ql:quickload "numericals")
;;; OR
(ql:quickload "dense-numericals")
```

## Using clpm

TODO

## Using ros

TODO

- Needs a way to install packages outside github.
