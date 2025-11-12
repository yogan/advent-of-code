#!/bin/bash
set -euo pipefail

# Run unit tests
echo "Running Lua tests..."
./test.sh

# Run the solution and check output
echo "Running Lua solution..."
output=$(./run.sh)

# Extract part 1 result (the number after "Part 1:")
part1_result=$(echo "$output" | grep "Part 1:" | sed 's/.*Part 1:[^0-9]*\([0-9]\+\).*/\1/')

# Expected values for day 10
expected_part1="11820"
# Part 2 is ASCII art - just check first line
expected_part2_pattern="####.###....##.###..###..#..#..##..#..#."

# Validate output contains expected results
if [[ "$part1_result" == "$expected_part1" ]] && [[ "$output" == *"$expected_part2_pattern"* ]]; then
    echo "✓ Day 10 Lua solution produces correct output"
    echo "Part 1: $part1_result"
    echo "Part 2: ASCII art verified (contains expected pattern)"
else
    echo "✗ Day 10 Lua solution output mismatch"
    echo "Expected Part 1: $expected_part1"
    echo "Actual Part 1: $part1_result"
    echo "Expected Part 2 to contain: $expected_part2_pattern"
    echo "Full output:"
    echo "$output"
    exit 1
fi
