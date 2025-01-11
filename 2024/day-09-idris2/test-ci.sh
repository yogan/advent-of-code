#!/bin/sh
if ! ./test.sh | grep -vi "building\|compiling"; then
    echo "Unit tests failed"
    exit 3
fi

output=$(./run.sh | grep -vi "building\|compiling")

expected1=6448989155953
result1=$(echo "$output" | tail -2 | head -1)
if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

# expected2=6476642796832
# result2=$(echo "$output" | tail -1)
# if [ "$result2" != "$expected2" ]; then
#     echo "Expected: »$expected2«"
#     echo "Received: »$result2«"
#     exit 2
# fi
