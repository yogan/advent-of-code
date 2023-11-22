#!/bin/sh
result=$(./fuehre-aus.sh | tail -1)
expected="Das Gesamtvolumen beträgt 14538"

if [ "$result" = "$expected" ]; then
    echo "Test passed"
    exit 0
else
    echo "Test failed"
    echo "Expected: $expected"
    echo "Got: $result"
    exit 1
fi
