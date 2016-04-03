#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

$CC $CFLAGS $TESTDIR/c4.c -o $TESTDIR/c4 &>/dev/null
rm -f $TESTDIR/c4.output
$TESTDIR/c4 $TESTDIR/hello.c >$TESTDIR/c4.output
rm -f $TESTDIR/c4

if ! cmp -s $TESTDIR/c4.output $TESTDIR/c4.expect ; then
	echo "C4 compiler failed!"
	exit 1
elif [ ! "$LUX_QUIET" = "1" ] ; then
	echo "C4 compiler succeeded!"
fi
exit 0
