#!/bin/bash
set -euo pipefail

# Run unit tests
echo "Running Nim tests..."
./test.sh

# Run the solution and check output
echo "Running Nim solution..."
output=$(./run.sh 2>&1)

# Filter out warnings/hints and extract just the output numbers
part1_result=$(echo "$output" | grep -E "^[0-9]+$" | head -n1)
part2_result=$(echo "$output" | grep -E "^[0-9]+$" | tail -n1)

# Expected values for day 9
expected_part1="6243"
expected_part2="2630"

# Validate output contains expected results
if [[ "$part1_result" == "$expected_part1" ]] && [[ "$part2_result" == "$expected_part2" ]]; then
    echo "✓ Day 9 Nim solution produces correct output"
    echo "Part 1: $part1_result"
    echo "Part 2: $part2_result"
else
    echo "✗ Day 9 Nim solution output mismatch"
    echo "Expected: Part 1: $expected_part1, Part 2: $expected_part2"
    echo "Actual: Part 1: $part1_result, Part 2: $part2_result"
    exit 1
fi