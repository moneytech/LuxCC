#!/bin/bash

if [ -d "src/lib/mips" ] ; then
	qemu-mipsel src/lib/mips/ld.so.1 --library-path src/lib/mips $1
else
	qemu-mipsel /usr/local/lib/luxcc/mips/ld.so.1 --library-path /usr/local/lib/luxcc/mips $1
fi
