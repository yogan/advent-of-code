#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

expected=816

output=$(./run.sh)
if [ "$output" != "$expected" ]; then
    echo "Expected: »$expected«"
    echo "Received: »$output«"
    exit 1
fi
