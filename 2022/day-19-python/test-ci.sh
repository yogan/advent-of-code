#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1


cd "$(dirname "$0")"

[ -f "venv/bin/activate" ] && . venv/bin/activate

# Note: This solution takes about 25 seconds to run
output=$(python3 day19.py input.txt 2>/dev/null)

if [ $? -eq 124 ]; then
    echo "✗ Solution timed out after 120 seconds"
    exit 1
fi

part1=$(echo "${output}" | grep "Part 1:" | sed 's/Part 1: //' | tr -d ' ')
part2=$(echo "${output}" | grep "Part 2:" | sed 's/Part 2: //' | tr -d ' ')

echo "Part 1: ${part1}"
echo "Part 2: ${part2}"

expected_part1="2301"
expected_part2="10336"

if [[ "${part1}" == "${expected_part1}" && "${part2}" == "${expected_part2}" ]]; then
    echo "✓ All tests passed"
    exit 0
else
    echo "✗ Test failed"
    echo "Expected: Part 1: ${expected_part1}, Part 2: ${expected_part2}"
    echo "Got: Part 1: ${part1}, Part 2: ${part2}"
    exit 1
fi
