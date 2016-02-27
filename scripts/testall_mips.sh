#!/bin/bash

echo "============================="
echo " START MIPS TESTS"
echo "============================="

if which qemu-mipsel >/dev/null ; then
	scripts/self_mips.sh &&
	scripts/test_exe_mips.sh
else
	echo "Cannot find qemu-mipsel"
fi

echo "============================="
echo " END MIPS TESTS"
echo "============================="
