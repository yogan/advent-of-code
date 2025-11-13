#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

idris2 --build aoc-tests.ipkg && ./build/exec/aoc-tests
