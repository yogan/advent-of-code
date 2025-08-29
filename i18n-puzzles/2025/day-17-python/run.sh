#!/bin/sh
# shellcheck disable=SC1091
[ -f "venv/bin/activate" ] && . venv/bin/activate

pypy3 i18n.py "$@"
