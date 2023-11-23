#!/bin/sh
result=$(./fuehre-aus.sh | tail -1)
expected="Es werden 1598415 Quadratfuß Geschenkpapier benötigt."

if [ "$result" = "$expected" ]; then
    echo "Test passed"
    exit 0
else
    echo "Test failed"
    echo "Expected: $expected"
    echo "Got: $result"
    exit 1
fi
