#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

perl day04.pl day04.in | grep -v "^#\|^ok\|^\.\.\|^Seeded\|^1\.\."

output=$(perl day04.pl day04.in | grep "^Part")

expected1="Part 1: 567"
result1=$(echo "$output" | head -1)
if [ "$result1" != "$expected1" ]; then
    echo "Part 1 failed"
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

expected2="Part 2: 907"
result2=$(echo "$output" | tail -1)
if [ "$result2" != "$expected2" ]; then
    echo "Part 2 failed"
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 2
fi