#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

output=$(./run.sh)

expected1=1197984
result1=$(echo "${output}" | head -1)

if [ "${result1}" != "${expected1}" ]; then
    echo "Expected: »${expected1}«"
    echo "Received: »${result1}«"
    exit 1
fi

expected2=23387399
result2=$(echo "${output}" | tail -1)
if [ "${result2}" != "${expected2}" ]; then
    echo "Expected: »${expected2}«"
    echo "Received: »${result2}«"
    exit 2
fi
