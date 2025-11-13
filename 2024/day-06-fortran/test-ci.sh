#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if ! ./test.sh >/dev/null; then
    echo "Unit tests failed"
    exit 3
fi

output=$(./run.sh)

expected1=4973
result1=$(echo "${output}" | tail -2 | head -1)
if [ "${result1}" != "${expected1}" ]; then
    echo "Expected: »${expected1}«"
    echo "Received: »${result1}«"
    exit 1
fi

expected2=1482
result2=$(echo "${output}" | tail -1)
if [ "${result2}" != "${expected2}" ]; then
    echo "Expected: »${expected2}«"
    echo "Received: »${result2}«"
    exit 2
fi
