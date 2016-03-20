#COMPILER="scripts/runmipselexe.sh src/tests/self/luxcc1 -q -mmips"
COMPILER="qemu-mipsel src/tests/self/luxcc1 -q -mmips"
ASSEMBLER=src/luxmips/luxasmips
LINKER="mipsel-linux-gnu-ld -melf32ltsmip"
RUNC="src/lib/obj/mips/crt0.o src/lib/obj/mips/luxmemcpy.o src/lib/obj/mips/liblux.o"
LIBC="src/lib/obj/mips/libc.a"
OUTPROG=luxcc2

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
