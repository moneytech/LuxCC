#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

rm -f $TESTDIR/aes.output
if ! $CC $TESTDIR/test.c $TESTDIR/aes.c -o $TESTDIR/out1 &>/dev/null ; then
	echo "Failed to compile Tiny AES128"
	exit 1
fi
$TESTDIR/out1 >$TESTDIR/aes.output
rm -f $TESTDIR/out1

if ! cmp -s $TESTDIR/aes.output $TESTDIR/aes.expect ; then
	echo "Tiny AES128 failed!"
	exit 1
elif [ ! "$LUX_QUIET" = "1" ] ; then
	echo "Tiny AES128 succeeded!"
fi
exit 0
