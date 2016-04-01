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

## Quickstart

Build everything

    make

Install (this is optional)

    make install

Do a quick test (run execution tests for the LuxVM target only)

    make test

Do all tests for the host architecture (assumed to be i386 or x86_64)

    make fulltest1

Test everything (run all tests for all supported architectures)

    make fulltest2

Note that in order to run tests for an architecture different than the host one, you will need to install QEMU user mode and also get a dynamic linker (aka loader) for the corresponding architecture (see the documentation for details).

## Source roadmap

| Directory/File | Contents |
| --- | --- |
| `src/*.(c/h)` | Core compiler |
| `src/luxld` | x86-(32/64), MIPS, and ARM linker |
| `src/luxx86` | x86-(32/64) assembler |
| `src/luxmips` | MIPS assembler |
| `src/luxarm` | ARM assembler |
| `src/luxvm` | Lux VM and assembler and linker for it |
| `src/luxdvr` | Compiler driver and .conf files |
| `src/*_cgen` | Code generators |
| `src/tools` | Testing tool |
| `src/lib` | The standard C library |
| `src/tests` | Test programs |


## Documentation

See `doc/index.html` for in-depth information on usage, internals, and more.