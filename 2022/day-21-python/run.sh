#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample.txt"
fi

# No pypy here, using match which requires Python >= 3.10.
# pypy3 in Ubuntu 22.04 is only at 3.8
python3 day*.py "$input"
