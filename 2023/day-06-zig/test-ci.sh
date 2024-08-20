#!/bin/sh
if ! zig build test; then
    echo "Unit tests failed!"
    exit 3
fi

output=$(./run.sh)

expected1=2065338
result1=$(echo "$output" | head -1)
if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

expected2=34934171
result2=$(echo "$output" | tail -1)
if [ "$result2" != "$expected2" ]; then
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 2
fi
