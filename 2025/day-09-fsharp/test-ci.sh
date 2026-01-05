#!/bin/bash
set -uo pipefail
cd "$(dirname "$0")" || exit 9

if ! ./build-ci.sh; then
	echo "Build failed"
	exit 3
fi

if ! ./test.sh; then
	echo "Unit tests failed"
	exit 4
fi

output=$(./bin/Release/net9.0/aoc input.txt)
echo
echo "${output}"

expected1=4746238001
result1=$(echo "${output}" | tail -2 | head -1)
if [ "${result1}" != "${expected1}" ]; then
	echo "Expected: »${expected1}«"
	echo "Received: »${result1}«"
	exit 1
fi

expected2=1552139370
result2=$(echo "${output}" | tail -1)
if [ "${result2}" != "${expected2}" ]; then
    echo "Expected: »${expected2}«"
    echo "Received: »${result2}«"
    exit 2
fi
