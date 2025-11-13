#!/bin/sh
if [ -f requirements.txt ]; then
    pypy3 -m venv venv
    # shellcheck disable=SC1091
    . venv/bin/activate
    pypy3 -m pip install -r requirements.txt
fi