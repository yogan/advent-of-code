#!/bin/sh
if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

output=$(./run.sh)

expected1=233050
result1=$(echo "$output" | head -1)

if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

expected2=276661131175807
result2=$(echo "$output" | tail -1)
if [ "$result2" != "$expected2" ]; then
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 2
fi
