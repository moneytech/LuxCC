#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`

fail_counter=0

for file in $TESTDIR/c/*.c ; do
	if ! $CC $file -o $TESTDIR/out1 &>/dev/null ; then
		echo "Failed to compile $file"
		let fail_counter=fail_counter+1
		continue
	fi

	$TESTDIR/out1 >"${file%.*}.output"

	if ! cmp -s "${file%.*}.output" "${file%.*}.expect" ; then
		echo "Simple crypto failed with $file"
		let fail_counter=fail_counter+1
	fi
done
rm -f $TESTDIR/out1

if [ "$fail_counter" != "0" ] ; then
	echo "Simple crypto failed!"
	exit 1
elif [ ! "$LUX_QUIET" = "1" ] ; then
	echo "Simple crypto succeeded!"
fi
exit 0
