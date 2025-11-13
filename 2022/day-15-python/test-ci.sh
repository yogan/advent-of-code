#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

set -e

[ -f "venv/bin/activate" ] && . venv/bin/activate

# Run the solution and capture output
output=$(python3 day15.py)

# Extract results
part1=$(echo "${output}" | grep "Part 1:" | sed 's/Part 1: //' | sed 's/ *$//')
part2=$(echo "${output}" | grep "Part 2:" | sed 's/Part 2: //' | sed 's/ *$//')

# Validate results
if [ "${part1}" != "5832528" ]; then
    echo "ERROR: Part 1 expected 5832528, got ${part1}"
    exit 1
fi

if [ "${part2}" != "13360899249595" ]; then
    echo "ERROR: Part 2 expected 13360899249595, got ${part2}"
    exit 1
fi

echo "All tests passed!"
