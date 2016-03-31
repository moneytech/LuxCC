#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

rm -f $TESTDIR/trex.output
if ! $CC $CFLAGS $TESTDIR/test.c $TESTDIR/trex.c -o $TESTDIR/out1 &>/dev/null ; then
	echo "Failed to compile T-Rex"
	exit 1
fi
$TESTDIR/out1 >$TESTDIR/trex.output
rm -f $TESTDIR/out1

if ! cmp -s $TESTDIR/trex.output $TESTDIR/trex.expect ; then
	echo "T-Rex failed!"
	exit 1
else
	echo "T-Rex succeeded!"
	exit 0
fi
