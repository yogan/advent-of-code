#!/bin/sh
if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
else
    # input="sample.txt"
    # input="sample2.txt"
    input="sample3.txt"
fi

pypy3 day*.py "$input"
