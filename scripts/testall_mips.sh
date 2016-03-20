#!/bin/bash

echo "============================="
echo " START MIPS TESTS"
echo "============================="

# assume default installation paths
if [ ! -d "/usr/mipsel-linux-gnu/" ] ; then
	echo "Cannot find MIPS cross libraries"
elif ! which qemu-mipsel >/dev/null ; then
	echo "Cannot find qemu-mipsel"
else
	scripts/self_mips.sh &&
	scripts/test_exe_mips.sh
fi

echo "============================="
echo " END MIPS TESTS"
echo "============================="
