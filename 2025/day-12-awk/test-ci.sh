#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

output=$(./run.sh)
echo "${output}"

expected=550
result=$(echo "${output}" | head -1)

if [ "${result}" != "${expected}" ]; then
    echo "Expected: »${expected}«"
    echo "Received: »${result}«"
    exit 1
fi
