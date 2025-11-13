#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC1091
[ -f "venv/bin/activate" ] && . venv/bin/activate

pypy3 aoc.py "$@"
