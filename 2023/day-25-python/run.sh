#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC1091
[ -f "venv/bin/activate" ] && . venv/bin/activate

if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample.txt"
fi

pypy3 day*.py "${input}" "$@"
