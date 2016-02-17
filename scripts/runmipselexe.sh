#!/bin/bash

# assume default installation paths
mips-linux-gnu-qemu ~/mgc/embedded/codebench/mips-linux-gnu/libc/el/lib/ld.so.1 --library-path ~/mgc/embedded/codebench/mips-linux-gnu/libc/el/lib:~/mgc/embedded/codebench/mips-linux-gnu/libc/el/usr/lib $1
