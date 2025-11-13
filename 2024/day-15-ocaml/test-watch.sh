#!/bin/bash
set -euo pipefail
BIN=_build/default/test/test_aoc.exe
echo "${BIN}" | entr -cc ./test.sh
