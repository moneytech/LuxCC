#!/bin/bash
CC1=src/luxcc
CFLAGS="-q -m$1"
TESTS_PATH=src/tests/compile

fail_counter=0
fail_files=""
pass_counter=0

for file in $(find $TESTS_PATH/ | grep '\.c') ; do
	echo $file
	
	$CC1 $CFLAGS $file &>/dev/null

	if [ "$?" != "0" ] ; then
		echo "failed: $file"
		let fail_counter=fail_counter+1
		fail_files="$fail_files $file"		
	else
		let pass_counter=pass_counter+1
	fi
done

echo "passes: $pass_counter"
echo "fails: $fail_counter"
