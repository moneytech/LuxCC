#!/bin/bash

if uname -i | grep -q "i386"; then
	/bin/bash scripts/testall_x86.sh
else
	/bin/bash scripts/testall_x64.sh
fi
/bin/bash scripts/testall_vm.sh
src/tools/tester src/tests/analyze/*.c
