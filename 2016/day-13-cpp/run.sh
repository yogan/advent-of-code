#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if (cd build && cmake --build .); then
    clear
    ./build/aoc "$@"
else
    exit 1
fi
