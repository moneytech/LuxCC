#!/bin/bash

echo "============================="
echo " START ARM TESTS"
echo "============================="

# assume default installation paths
if [ ! -d "/usr/arm-linux-gnueabi/" ] ; then
	echo "Cannot find ARM cross libraries"
elif ! which qemu-arm >/dev/null ; then
	echo "Cannot find qemu-arm"
else
	scripts/self_arm.sh &&
	scripts/test_exe_arm.sh
fi

echo "============================="
echo " END ARM TESTS"
echo "============================="
