#!/bin/bash
output_sample=$(./sample.sh)
result1_sample=$(echo "$output_sample" | head -1)
result2_sample=$(echo "$output_sample" | tail -1)
expected1_sample="Part 1: 4361 (sample)"
expected2_sample="Part 2: 467835 (sample)"

if [ "$result1_sample" != "$expected1_sample" ]; then
    echo "FAILED (sample part 1)"
    echo "Expected: »$expected1_sample«"
    echo "Got:      »$result1_sample«"
    exit 1
fi

if [ "$result2_sample" != "$expected2_sample" ]; then
    echo "FAILED (sample part 2)"
    echo "Expected: »$expected2_sample«"
    echo "Got:      »$result2_sample«"
    exit 1
fi

output=$(./run.sh)
result1=$(echo "$output" | head -1)
result2=$(echo "$output" | tail -1)
expected1="Part 1: 507214"
expected2="Part 2: 72553319"

if [ "$result1" != "$expected1" ]; then
    echo "FAILED (part 1)"
    echo "Expected: »$expected1«"
    echo "Got:      »$result1«"
    exit 1
fi

if [ "$result2" != "$expected2" ]; then
    echo "FAILED (part 2)"
    echo "Expected: »$expected2«"
    echo "Got:      »$result2«"
    exit 1
fi
