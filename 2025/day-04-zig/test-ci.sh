#!/bin/bash
set -uo pipefail
cd "$(dirname "$0")" || exit 1

if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

# Printing to stderr is way easier in Zig, so main output will be there.
output=$(./run.sh 2>&1)
echo "${output}"

expected1=1551
result1=$(echo "${output}" | head -1)
if [ "${result1}" != "${expected1}" ]; then
    echo "Expected: »${expected1}«"
    echo "Received: »${result1}«"
    exit 1
fi

expected2=9784
result2=$(echo "${output}" | tail -1)
if [ "${result2}" != "${expected2}" ]; then
    echo "Expected: »${expected2}«"
    echo "Received: »${result2}«"
    exit 2
fi
