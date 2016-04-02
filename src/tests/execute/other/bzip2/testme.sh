#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

if ! $CC $TESTDIR/bzip2.c -o $TESTDIR/bzip2 &>/dev/null ; then
	echo "Failed to compile bzip2"
	exit 1
fi

# compress and decompress the source
$TESTDIR/bzip2 -c $TESTDIR/bzip2.c >$TESTDIR/out.bz2
$TESTDIR/bzip2 -d -c $TESTDIR/out.bz2 >$TESTDIR/_bzip2.c
cmp -s $TESTDIR/bzip2.c $TESTDIR/_bzip2.c
RES=$?

# clean
rm -f $TESTDIR/out.bz2 $TESTDIR/_bzip2.c $TESTDIR/bzip2

# compare
if [ "$RES" = "0" ] ; then
	echo "bzip2 succeeded!"
	exit 0
else
	echo "bzip2 failed!"
	exit 1
fi
