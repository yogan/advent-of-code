#!/bin/sh
if ! ./test.sh; then
    echo "Test failed"
    exit 3
fi

output=$(./run.sh | grep -v "Compil\|Running")
result1=$(echo "$output" | tail -2 | head -1)
# result2=$(echo "$output" | tail -1)
expected1="14545"
# expected2="4978"

if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

# if [ "$result2" != "$expected2" ]; then
#     echo "Expected: »$expected2«"
#     echo "Received: »$result2«"
#     exit 2
# fi
