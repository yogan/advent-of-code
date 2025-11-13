#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample.txt"
fi

if command -v pypy3 >/dev/null 2>&1; then
    pypy3 day*.py "$input" "$*"
else
    # echo to stderr
    echo "pypy3 not found, using python3 instead" >&2
    python3 day*.py "$input" "$*"
fi
