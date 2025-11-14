#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

[ -f "venv/bin/activate" ] && . venv/bin/activate

if [ $# -gt 0 ]; then
    pypy3 ec.py "$@"
else
    pypy3 ec.py
fi
