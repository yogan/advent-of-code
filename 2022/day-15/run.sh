#!/bin/sh
if command -v pypy3 >/dev/null 2>&1; then
    pypy3 day15.py
else
    echo "pypy3 not found, using python3 instead (slow!)"
    python3 day15.py
fi
