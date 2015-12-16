## LuxCC

LuxCC is a small compiler for a subset of the C programming language. This subset is basically C89 (without floating-point numbers and some obsolete or uncommon features) plus a few C99 features.

The compiler comes along with other tools that complete the development toolchain (assemblers, linkers, and VMs).

LuxCC is able to compile itself and also other non-trivial programs (see the tests for examples).

Currently supported targets are:

* i386
* x86_64
* LuxVM (32 & 64 bits)

This is a one-man project developed mainly for educational purposes (and fun!). It is not intended to replace any existing compiler/tool.

See the documentation at `doc/index.html` for more in-depth information (usage, internals, etc).
