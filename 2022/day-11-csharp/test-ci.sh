#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


# Run unit tests
echo "Running C# tests..."
./test.sh

# Run the solution and check output
echo "Running C# solution..."
output=$(./run.sh)

# Extract part results
part1_result=$(echo "$output" | grep "Part 1:" | sed 's/.*Part 1: \([0-9]\+\).*/\1/')
part2_result=$(echo "$output" | grep "Part 2:" | sed 's/.*Part 2: \([0-9]\+\).*/\1/')

# Expected values for day 11
expected_part1="117624"
expected_part2="16792940265"

# Validate output contains expected results
if [[ "$part1_result" == "$expected_part1" ]] && [[ "$part2_result" == "$expected_part2" ]]; then
    echo "✓ Day 11 C# solution produces correct output"
    echo "Part 1: $part1_result"
    echo "Part 2: $part2_result"
else
    echo "✗ Day 11 C# solution output mismatch"
    echo "Expected Part 1: $expected_part1, Part 2: $expected_part2"
    echo "Actual Part 1: $part1_result, Part 2: $part2_result"
    exit 1
fi
