#!/bin/sh
GIT_ROOT=$(git rev-parse --show-toplevel)
"${GIT_ROOT}"/scripts/run-vim.sh

result1=$(head -1 out)
result2=$(tail -1 out)
expected1="255"
expected2="55"

if [ "$result1" != "$expected1" ]; then
    echo "Part 1 failed"
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

if [ "$result2" != "$expected2" ]; then
    echo "Part 2 failed"
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 1
fi
