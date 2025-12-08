#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

filename="input.txt"
if [ "${1:-}" != "" ]; then
    filename=$1
fi
nim compile --run --verbosity:0 aoc "${filename}"
