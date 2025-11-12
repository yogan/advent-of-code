#!/bin/sh
set -e

# Run the solution and capture output
output=$(python3 day13.py)

# Extract results
part1=$(echo "$output" | grep "Part 1:" | sed 's/Part 1: //')
part2=$(echo "$output" | grep "Part 2:" | sed 's/Part 2: //')

# Validate results
if [ "$part1" != "5208" ]; then
    echo "ERROR: Part 1 expected 5208, got $part1"
    exit 1
fi

if [ "$part2" != "25792" ]; then
    echo "ERROR: Part 2 expected 25792, got $part2"
    exit 1
fi

echo "All tests passed!"