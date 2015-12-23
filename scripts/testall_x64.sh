#!/bin/bash

echo "============================="
echo " START x86_64 TESTS"
echo "============================="

scripts/self_x64.sh &&
cp src/tests/self/luxcc2.out src/luxcc &&
scripts/test_exe_x64.sh &&
scripts/test_com_x64.sh

echo "============================="
echo " END x86_64 TESTS"
echo "============================="
