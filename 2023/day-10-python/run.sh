#!/bin/sh
if [ "$(basename "$0")" = "run.sh" ]; then
    input="input.txt"
elif [ "$(basename "$0")" = "reddit.sh" ]; then
    input="reddit.txt"
else
    input="sample$1.txt"
fi

pypy3 day*.py "$input"
