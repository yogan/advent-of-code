#!/bin/sh
../../../scripts/run-vim.sh > /dev/null 2>&1

expected1=44216
result1=$(head -1 out)
if [ "$result1" != "$expected1" ]; then
    echo "Part 1 failed"
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

# expected2=?
# result2=$(tail -1 out)
# if [ "$result2" != "$expected2" ]; then
#     echo "Part 2 failed"
#     echo "Expected: »$expected2«"
#     echo "Received: »$result2«"
#     exit 1
# fi
