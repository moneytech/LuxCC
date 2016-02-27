## LuxCC

LuxCC is a small compiler for a subset of the C programming language. This subset is basically C89 (without floating-point numbers and some obsolete or uncommon features) plus a few C99 features.

The compiler comes along with other tools that complete the development toolchain (assemblers, linkers, and VMs).

LuxCC is able to compile itself and also other non-trivial programs (see the tests for examples).

Currently supported targets are:

* i386
* x86_64
* LuxVM (32 & 64 bits)
* MIPS32
* ARM

Note that this is a one-man project developed mainly for educational purposes (and fun!). It is not intended to replace any existing compiler/tool.

## Quickstart

Build everything

    make

Install (this is optional)

    make install

Do a quick test (run execution tests for the LuxVM target only)

    make test

Test everything (run all tests for all targets)

    make fulltest

## Documentation

See `doc/index.html` for in-depth information on usage, internals, and more.

## Contact

<lugon399@gmail.com>
