#!/bin/sh
. venv/bin/activate

if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample.txt"
fi

pypy3 day*.py "$input" "$@"
