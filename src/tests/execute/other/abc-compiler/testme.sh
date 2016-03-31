#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

$CC $TESTDIR/b0.c $TESTDIR/b1.c -o $TESTDIR/b &>/dev/null
$TESTDIR/abc -c $TESTDIR/brt.s $TESTDIR/lib.b &>/dev/null

rm -f $TESTDIR/abc.output
for file in $TESTDIR/examples/*.b ; do
	$TESTDIR/abc -o $TESTDIR/out1 $file
	$TESTDIR/out1 >>$TESTDIR/abc.output
done
rm -f $TESTDIR/out1

if ! cmp -s $TESTDIR/abc.output $TESTDIR/abc.expect ; then
	echo "B compiler failed!"
	exit 1
else
	echo "B compiler succeeded!"
	exit 0
fi
