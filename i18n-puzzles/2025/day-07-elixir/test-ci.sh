#!/bin/sh
mix compile

if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

expected=32152346

output=$(./run.sh)
result=$(echo "$output" | head -1)

if [ "$result" != "$expected" ]; then
    echo "Expected: »$expected«"
    echo "Received: »$result«"
    exit 1
fi
