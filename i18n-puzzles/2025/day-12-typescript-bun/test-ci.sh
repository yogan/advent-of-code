#!/bin/sh
if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

expected=2720138976393300

output=$(./run.sh)
if [ "$output" != "$expected" ]; then
    echo "Expected: »$expected«"
    echo "Received: »$output«"
    exit 1
fi
