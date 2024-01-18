#!/bin/sh
./run.sh > /dev/null 2>&1

result1=$(cat 1.out)
expected1="70509"

if [ "$result1" != "$expected1" ]; then
    echo "Expected: »$expected1«"
    echo "Received: »$result1«"
    exit 1
fi
