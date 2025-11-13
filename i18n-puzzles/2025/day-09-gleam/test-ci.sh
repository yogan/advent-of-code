#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if ! ./test.sh; then
    echo "Unit tests failed"
    exit 3
fi

expected="Amelia Amoura Hugo Jack Jakob Junior Mateo"

output=$(./run.sh | grep -v "Compil\|Running")
result=$(echo "$output" | tail -2 | head -1)

if [ "$result" != "$expected" ]; then
    echo "Expected: »$expected«"
    echo "Received: »$result«"
    exit 1
fi
