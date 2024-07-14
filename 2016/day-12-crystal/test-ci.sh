#!/bin/sh
if ! ./test.sh; then
    echo "Unit tests failed!"
    exit 3
fi

output=$(./run.sh)
result1=$(echo "$output" | head -1)
# result2=$(echo "$output" | tail -1)
expected1="318009"
# expected2="?"

if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Got:      »$result1«"
    exit 1
fi

# if [ "$result2" != "$expected2" ]; then
#     echo "Expected: »$expected2«"
#     echo "Got:      »$result2«"
#     exit 2
# fi
