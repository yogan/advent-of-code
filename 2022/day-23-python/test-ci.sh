#!/bin/bash
set -e
cd "$(dirname "$0")"

[ -f "venv/bin/activate" ] && source venv/bin/activate

# Run the solution and capture output
output=$(python3 day23.py input.txt)

# Extract results
part1=$(echo "$output" | grep "Part 1:" | sed 's/Part 1: //' | sed 's/ *$//')
part2=$(echo "$output" | grep "Part 2:" | sed 's/Part 2: //' | sed 's/ *$//')

# Validate results
if [ "$part1" != "3970" ]; then
    echo "ERROR: Part 1 expected 3970, got $part1"
    exit 1
fi

if [ "$part2" != "923" ]; then
    echo "ERROR: Part 2 expected 923, got $part2"
    exit 1
fi

echo "All tests passed!"