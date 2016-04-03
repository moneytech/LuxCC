#!/bin/bash
CC=src/luxcc
TESTS_PATH=src/tests/compile
if uname -i | grep -q "i386"; then
	CC="$CC -q -mvm32"
else
	CC="$CC -q -mvm64"
fi

fail_counter=0
fail_files=""
pass_counter=0

echo "== Compilation tests begin... =="

for file in $(find $TESTS_PATH/ | grep '\.c') ; do
	if [ ! "$LUX_QUIET" = "1" ] ; then
		echo $file
	fi
	
	$CC $file >/dev/null

	if [ "$?" != "0" ] ; then
		echo "failed: $file"
		let fail_counter=fail_counter+1
		fail_files="$fail_files $file"		
	else
		let pass_counter=pass_counter+1
	fi
done

echo "== Compilation tests results: PASS: $pass_counter, FAIL: $fail_counter =="
