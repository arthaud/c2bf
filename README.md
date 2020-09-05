c2bf
====

**c2bf** is a compiler from a subset of C to brainfuck.

Dependencies
------------

You will need `ocamlbuild` and `ocamlfind`.

#### Ubuntu

To install the dependencies on Ubuntu, run the following commands:
```
$ sudo apt install opam ocamlbuild
$ opam init
$ eval $(opam config env)
$ opam install ocamlfind
```

Build
-----

Just run `make build` to build c2bf.

How to use
----------

usage: `./c2bf.native [options] <C source-file>`.
The brainfuck output is in `a.bf`.

I recommend you beef [http://kiyuko.org/software/beef](http://kiyuko.org/software/beef) as a brainfuck interpreter.

See `./c2bf.native --help` for options.

Examples
--------

There are examples in the directory `tests`. You should try them !

Why ? This is useless !
-----------------------

Because it's fun, and I like having fun.

