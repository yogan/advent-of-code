#!/bin/sh
if ! zig build test; then
    echo "Unit tests failed!"
    exit 3
fi

output=$(./run.sh)
result1=$(echo "$output" | head -1)
result2=$(echo "$output" | tail -1)
expected1="Part 1: 2065338"
expected2="Part 2: 34934171"

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
