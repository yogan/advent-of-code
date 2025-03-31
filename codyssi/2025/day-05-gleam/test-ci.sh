#!/bin/sh
if ! ./test.sh; then
    echo "Unit tests failed"
    exit 4
fi

expected1=692
# expected2=
# expected3=

output=$(./run.sh | grep -v "Compil\|Running")
result1=$(echo "$output" | tail -3 | head -1)
# result2=$(echo "$output" | tail -2 | head -1)
# result3=$(echo "$output" | tail -1)

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

# if [ "$result3" != "$expected3" ]; then
#     echo "Expected: »$expected3«"
#     echo "Received: »$result3«"
#     exit 3
# fi
