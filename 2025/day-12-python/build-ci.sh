#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")" || exit 1

if [ -f requirements.txt ]; then
    pypy3 -m venv venv
    . venv/bin/activate
    pypy3 -m pip install -r requirements.txt
fi
