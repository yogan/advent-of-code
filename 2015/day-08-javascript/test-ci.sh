#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if ! ./test.sh; then
    echo "Test failed"
    exit 3
fi

output=$(./run.sh)
result1=$(echo "$output" | tail -2 | head -1)
result2=$(echo "$output" | tail -1)
expected1="1371"
expected2="2117"

if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

if [ "$result2" != "$expected2" ]; then
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 2
fi
