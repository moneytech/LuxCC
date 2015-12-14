#!/bin/bash
CC=src/luxdvr/luxdvr
CFLAGS="-m$1 -q"
TESTDIR=`dirname $0`

rm -f $TESTDIR/aes.output
if ! $CC $CFLAGS $TESTDIR/test.c $TESTDIR/aes.c -o $TESTDIR/out1 &>/dev/null ; then
	echo "Failed to compile Tiny AES128"
	exit 1
fi
$TESTDIR/out1 >$TESTDIR/aes.output
rm -f $TESTDIR/out1

if ! cmp -s $TESTDIR/aes.output $TESTDIR/aes.expect ; then
	echo "Tiny AES128 failed!"
	exit 1
else
	echo "Tiny AES128 succeeded!"
	exit 0
fi
