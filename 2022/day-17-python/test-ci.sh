#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


cd "$(dirname "$0")"

output=$(python3 day17.py input.txt 2>/dev/null)

part1=$(echo "$output" | grep "Part 1:" | sed 's/Part 1: //' | tr -d ' ')
part2=$(echo "$output" | grep "Part 2:" | sed 's/Part 2: //' | tr -d ' ')

echo "Part 1: $part1"
echo "Part 2: $part2"

expected_part1="3175"
expected_part2="1555113636385"

if [[ "$part1" == "$expected_part1" && "$part2" == "$expected_part2" ]]; then
    echo "✓ All tests passed"
    exit 0
else
    echo "✗ Test failed"
    echo "Expected: Part 1: $expected_part1, Part 2: $expected_part2"
    echo "Got: Part 1: $part1, Part 2: $part2"
    exit 1
fi