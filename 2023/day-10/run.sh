#!/bin/sh
if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    input="sample$1.txt"
fi

pypy3 day*.py "$input"
