#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


cd "$(dirname "$0")"

# Compile if needed
if [ ! -f day25 ] || [ day25.hs -nt day25 ]; then
    ghc -o day25 day25.hs > /dev/null 2>&1
fi

output=$(./day25 input.txt 2>/dev/null)

part1=$(echo "$output" | grep "Part 1:" | sed 's/Part 1: //')

echo "Part 1: $part1"

expected_part1="2-2--02=1---1200=0-1"

if [[ "$part1" == "$expected_part1" ]]; then
    echo "✓ All tests passed"
    exit 0
else
    echo "✗ Test failed"
    echo "Expected: Part 1: $expected_part1"
    echo "Got: Part 1: $part1"
    exit 1
fi