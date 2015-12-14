#!/bin/bash

for file in $(find src/tests/self/ | grep '\.c') ; do
	cmp "${file%.*}.s" "${file%.*}.s2"
done
