#!/bin/bash

echo "============================="
echo " START MIPS TESTS"
echo "============================="

# assume default installation paths
if [ ! -d "$HOME/mgc/embedded/codebench/mips-linux-gnu/" ] ; then
	echo "cross-development environment not found."
else
	scripts/self_mips.sh &&
	scripts/test_exe_mips.sh
fi

echo "============================="
echo " END MIPS TESTS"
echo "============================="
