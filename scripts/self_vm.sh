#!/bin/bash

/bin/bash scripts/self_copy.sh

# phase 1
if ! /bin/bash scripts/self1_vm.sh ; then
	echo "Phase 1 failed!"
	exit 1
else
	echo "Phase 1 succeeded!"
fi

# phase 2
if ! /bin/bash scripts/self2_vm.sh ; then
	echo "Phase 2 failed!"
	exit 1
else
	echo "Phase 2 succeeded!"
fi

# compare binaries
if cmp -s src/tests/self/luxcc1.vme src/tests/self/luxcc2.vme ; then
	echo "PASSED!"
	exit 0
else
	echo "FAILED!"
	exit 1
fi
