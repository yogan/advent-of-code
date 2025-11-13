#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# Clean previous builds
rm -rf de/

# Compile Java .
javac -d . src/main/java/de/zogan/aoc2022/Day07.java

# Run the solution and check output
output=$(java de.zogan.aoc2022.Day07)

# Expected values for day 7
expected_part1="Part 1: 1611443"
expected_part2="Part 2: 2086088"

# Validate output contains expected results
if [[ "${output}" == *"${expected_part1}"* ]] && [[ "${output}" == *"${expected_part2}"* ]]; then
    echo "✓ Day 7 Java solution produces correct output"
    echo "${output}"
else
    echo "✗ Day 7 Java solution output mismatch"
    echo "Expected: ${expected_part1} and ${expected_part2}"
    echo "Actual: ${output}"
    exit 1
fi