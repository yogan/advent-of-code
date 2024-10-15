#!/bin/sh
if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

BINARY="./bin/Release/net9.0/aoc"

if [ ! -f "$BINARY" ]; then
    echo "Binary not found: $BINARY"
    echo "This either means that build-ci.sh was not run, or that it failed."
    exit 4
fi

output=$($BINARY input.txt)

result1=$(echo "$output" | tail -2 | head -1)
result2=$(echo "$output" | tail -1)

expected1=DURLDRRDRD
if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi

expected2=650
result2=$(echo "$output" | tail -1)
if [ "$result2" != "$expected2" ]; then
    echo "Expected: »$expected2«"
    echo "Received: »$result2«"
    exit 2
fi
