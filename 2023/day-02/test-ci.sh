#!/bin/sh

if ! ./test.sh > /dev/null 2> /dev/null ; then
    echo "Unit tests failed!"
    exit 1
fi

output=$(./run.sh)
result1=$(echo "$output" | head -1)
result2=$(echo "$output" | tail -1)
expected1="Part 1: 2076"
expected2="Part 2: 70950"

if [ "$result1" != "$expected1" ]; then
    echo "Expected '$expected1', got '$result1'"
    exit 1
fi

if [ "$result2" != "$expected2" ]; then
    echo "Expected '$expected2', got '$result2'"
    exit 1
fi
