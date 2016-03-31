#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`
OBJS=""

for file in $TESTDIR/src/*.c ; do
	if ! $CC -c $file -o "${file%.*}.o" &>/dev/null ; then
		echo "Crypto: failed to compile $file"
		exit 1
	else
		OBJS="$OBJS ${file%.*}.o"
	fi
done

ar rcs $TESTDIR/libcrypto.a $OBJS

rm -f $TESTDIR/crypto.output
for file in $TESTDIR/test/*.c ; do
	if ! $CC -i $TESTDIR/src $file $TESTDIR/libcrypto.a -o $TESTDIR/out1 &>/dev/null ; then
		echo "Crypto: failed to compile $file"
		continue
	fi
	$TESTDIR/out1 >>$TESTDIR/crypto.output
done
rm -f $TESTDIR/out1

if ! cmp -s $TESTDIR/crypto.output $TESTDIR/crypto.expect ; then
	echo "Crypto failed!"
	exit 1
else
	echo "Crypto succeeded!"
	exit 0
fi
