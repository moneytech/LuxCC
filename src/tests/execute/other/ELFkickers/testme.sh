#!/bin/bash
CC="src/luxdvr/luxdvr -q $1"
TESTDIR=`dirname $0`
FAILEDONCE="0"

#
# build the library
#
if ! $CC -c $TESTDIR/elfrw/*.c &>/dev/null ; then
	echo "Failed to compile elfrw"
	exit 1
fi
ar rcs $TESTDIR/libelfrw.a $TESTDIR/elfrw/*.o

#
# elfls
#
if ! $CC -i$TESTDIR/elfrw $TESTDIR/elfls/elfls.c -o $TESTDIR/out1 $TESTDIR/libelfrw.a &>/dev/null ; then
	echo "Failed to compile elfls"
else
	$TESTDIR/out1 -d $TESTDIR/test_elf >$TESTDIR/elfls.output
	if ! cmp -s $TESTDIR/elfls.output $TESTDIR/elfls.expect ; then
		echo "elfls failed!"
		FAILEDONCE="1"
	elif [ ! "$LUX_QUIET" = "1" ] ; then
		echo "elfls succeeded!"
	fi
fi
rm -f $TESTDIR/elfls.output

#
# ebfc
#
if ! $CC -c -i$TESTDIR/elfrw $TESTDIR/ebfc/*.c &>/dev/null ; then
	echo "Failed to compile ebfc"
else
	ar crs  $TESTDIR/ebfc/libelfparts.a \
	        $TESTDIR/ebfc/elfparts.o \
		$TESTDIR/ebfc/ehdr.o \
		$TESTDIR/ebfc/phdrtab.o \
		$TESTDIR/ebfc/shdrtab.o \
		$TESTDIR/ebfc/progbits.o \
		$TESTDIR/ebfc/strtab.o \
		$TESTDIR/ebfc/symtab.o \
		$TESTDIR/ebfc/hash.o \
		$TESTDIR/ebfc/rel.o \
		$TESTDIR/ebfc/got.o \
		$TESTDIR/ebfc/dynamic.o
	
	if ! $CC -i$TESTDIR/elfrw $TESTDIR/ebfc/ebfc.o $TESTDIR/ebfc/brainfuck.o $TESTDIR/ebfc/libelfparts.a -o $TESTDIR/out1 ; then
		echo "Failed to link ebfc"
	fi

	for file in $TESTDIR/ebfc/bf/*.b ; do
		$TESTDIR/out1 -o $TESTDIR/out2 $file
		$TESTDIR/out2 >>$TESTDIR/ebfc.output		
	done

	if ! cmp -s $TESTDIR/ebfc.output $TESTDIR/ebfc.expect ; then
		echo "ebfc failed!"
		FAILEDONCE="1"
	elif [ ! "$LUX_QUIET" = "1" ] ; then
		echo "ebfc succeeded!"
	fi
fi
rm -f $TESTDIR/ebfc/*.o $TESTDIR/ebfc/libelfparts.a $TESTDIR/ebfc.output $TESTDIR/out2

#
# objres
#
if ! $CC -i$TESTDIR/elfrw $TESTDIR/objres/objres.c -o $TESTDIR/out1 $TESTDIR/libelfrw.a &>/dev/null ; then
	echo "Failed to compile objres"
else
	# test with its own readme
	$TESTDIR/out1 -o $TESTDIR/objres.output $TESTDIR/objres/README.txt
	if ! cmp -s $TESTDIR/objres.output $TESTDIR/objres.expect ; then
		echo "objres failed!"
		FAILEDONCE="1"
	elif [ ! "$LUX_QUIET" = "1" ] ; then
		echo "objres succeeded!"
	fi
fi
rm -f $TESTDIR/objres.output $TESTDIR/objres.output.h

#
# rebind
#
if ! $CC -i$TESTDIR/elfrw $TESTDIR/rebind/rebind.c -o $TESTDIR/out1 $TESTDIR/libelfrw.a &>/dev/null ; then
	echo "Failed to compile rebind"
else
	# backup test_elf
	cp $TESTDIR/test_elf $TESTDIR/test_elf2
	# change _init's visibility from default to hidden
	$TESTDIR/out1 --visibility=hidden $TESTDIR/test_elf _init
	if ! readelf -s $TESTDIR/test_elf | grep -q "GLOBAL HIDDEN    11 _init" ; then
		echo "rebind failed!"
		FAILEDONCE="1"
	elif [ ! "$LUX_QUIET" = "1" ] ; then
		echo "rebind succeeded!"
	fi
	# restore test_elf
	mv $TESTDIR/test_elf2 $TESTDIR/test_elf
fi

#
# clean
#
rm -f $TESTDIR/out1 $TESTDIR/libelfrw.a $TESTDIR/elfrw/*.o

