#!/bin/bash

echo "============================="
echo " START i386 TESTS"
echo "============================="

scripts/self_x86.sh &&
cp src/tests/self/luxcc2.out src/luxcc &&
scripts/test_exe_x86.sh &&
scripts/test_com_x86.sh

echo "============================="
echo " END i386 TESTS"
echo "============================="
