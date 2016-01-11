#!/bin/bash

/bin/bash scripts/self_copy.sh

echo "== Self-compilation test begins... =="

# phase 1
if ! /bin/bash scripts/self1_mips.sh ; then
	echo "Phase 1 failed!"
	exit 1
else
	echo "Phase 1 succeeded!"
fi

# phase 2
if ! /bin/bash scripts/self2_mips.sh ; then
	echo "Phase 2 failed!"
	exit 1
else
	echo "Phase 2 succeeded!"
fi

# compare binaries
if cmp -s src/tests/self/luxcc1 src/tests/self/luxcc2 ; then
	echo "== Self-compilation test succeeded! =="
	exit 0
else
	echo "== Self-compilation test failed! =="
	exit 1
fi
