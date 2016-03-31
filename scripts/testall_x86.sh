#!/bin/bash

echo "============================="
echo " START i386 TESTS"
echo "============================="

if /bin/bash scripts/self_x86.sh ; then
	mv src/luxcc src/luxcc_tmp
	cp src/tests/self/luxcc2.out src/luxcc
	scripts/test_exe_x86.sh && scripts/test_com_x86.sh
	mv src/luxcc_tmp src/luxcc
fi

echo "============================="
echo " END i386 TESTS"
echo "============================="
