COMPILER=src/luxcc
ASSEMBLER=src/luxvm/luxvmas
LINKER=src/luxvm/luxvmld
LIBC=src/lib/libc.o
OUTPROG=luxcc1.vme
if uname -i | grep -q "i386"; then
	COMPILER="$COMPILER -q -mvm32"
	ASSEMBLER="$ASSEMBLER -vm32"
	LINKER="$LINKER -vm32"
	RUNC=src/lib/crt32.o
else
	COMPILER="$COMPILER -q -mvm64"
	ASSEMBLER="$ASSEMBLER -vm64"
	LINKER="$LINKER -vm64"
	RUNC=src/lib/crt64.o
fi

fail_counter=0
pass_counter=0
object_files=""

for file in $(find src/tests/self/ | grep '\.c') ; do
	echo $file

	# compile	
	$COMPILER $file -o "${file%.*}.s" 2>/dev/null
	if [ "$?" != "0" ] ; then
		echo "Compiler failed with file $file"
		let fail_counter=fail_counter+1
		continue
	fi

	# assemble
	$ASSEMBLER "${file%.*}.s" -o "${file%.*}.o" 2>/dev/null
	if [ "$?" != "0" ] ; then
		echo "Assembler failed with file $file"
		let fail_counter=fail_counter+1
		continue
	fi
	object_files="$object_files ${file%.*}.o"

	let pass_counter=pass_counter+1
done

# link
if [ "$fail_counter" = "0" ]; then
	$LINKER -o src/tests/self/$OUTPROG $RUNC $object_files $LIBC
	exit $?
fi

exit 1
