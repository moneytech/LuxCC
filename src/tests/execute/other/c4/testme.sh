#!/bin/bash
CC=src/luxdvr/luxdvr
CFLAGS="-m$1 -q -static"
TESTDIR=`dirname $0`

$CC $CFLAGS $TESTDIR/c4.c -o $TESTDIR/c4 &>/dev/null
rm -f $TESTDIR/c4.output
$TESTDIR/c4 $TESTDIR/hello.c >$TESTDIR/c4.output
rm -f $TESTDIR/c4

if ! cmp -s $TESTDIR/c4.output $TESTDIR/c4.expect ; then
	echo "C4 compiler failed!"
	exit 1
else
	echo "C4 compiler succeeded!"
	exit 0
fi
