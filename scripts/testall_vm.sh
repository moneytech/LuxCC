#!/bin/bash

if uname -i | grep -q "i386"; then
	CURRTAR="VM32"
else
	CURRTAR="VM64"
fi
echo "============================="
echo " START $CURRTAR TESTS"
echo "============================="

scripts/self_vm.sh &&
scripts/test_exe_vm.sh &&
scripts/test_com_vm.sh

echo "============================="
echo " END $CURRTAR TESTS"
echo "============================="
