#!/bin/sh

# Save some time in CI: build/venv is only needed for visualization (PIL+tqdm)
[ -n "$GITHUB_RUN_ID" ] && return

if [ -f requirements.txt ]; then
    pypy3 -m venv venv
    # shellcheck disable=SC1091
    . venv/bin/activate
    pypy3 -m pip install -r requirements.txt
fi
