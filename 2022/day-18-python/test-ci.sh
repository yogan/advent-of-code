#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

[ -f "venv/bin/activate" ] && . venv/bin/activate

# Run the solution and capture output
output=$(python3 day18.py input.txt)

# Extract results
part1=$(echo "${output}" | grep "Part 1:" | sed 's/Part 1: //' | sed 's/ *$//')
part2=$(echo "${output}" | grep "Part 2:" | sed 's/Part 2: //' | sed 's/ *$//')

# Validate results
if [ "${part1}" != "4244" ]; then
    echo "ERROR: Part 1 expected 4244, got ${part1}"
    exit 1
fi

if [ "${part2}" != "2460" ]; then
    echo "ERROR: Part 2 expected 2460, got ${part2}"
    exit 1
fi

echo "All tests passed!"
