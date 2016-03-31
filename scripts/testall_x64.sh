#!/bin/bash

echo "============================="
echo " START x86_64 TESTS"
echo "============================="

if /bin/bash scripts/self_x64.sh ; then
	mv src/luxcc src/luxcc_tmp
	cp src/tests/self/luxcc2.out src/luxcc
	scripts/test_exe_x64.sh && scripts/test_com_x64.sh
	mv src/luxcc_tmp src/luxcc
fi

echo "============================="
echo " END x86_64 TESTS"
echo "============================="
