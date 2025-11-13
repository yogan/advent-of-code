#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


if ! ./test.sh; then
    echo "Unit tests failed!"
    exit 1
fi

output=$(./run.sh)
result1=$(echo "${output}" | head -1)
result2=$(echo "${output}" | tail -1)
expected1="Part 1: 74"
expected2="Part 2: 1795"

if [ "${result1}" != "${expected1}" ]; then
    echo "Expected '${expected1}', got '${result1}'"
    exit 1
fi

if [ "${result2}" != "${expected2}" ]; then
    echo "Expected '${expected2}', got '${result2}'"
    exit 1
fi
