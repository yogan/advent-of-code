#!/bin/sh
output=$(./run.sh)

expected1=917
result1=$(echo "$output" | head -1)
if [ "$result1" != "$expected1" ]; then
    echo "Part 1 failed"
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

expected2=1649
result2=$(echo "$output" | tail -1)
if [ "$result2" != "$expected2" ]; then
    echo "Part 2 failed"
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 1
fi
