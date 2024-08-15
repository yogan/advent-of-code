#!/bin/sh
filename="input.txt"
if [ "$1" != "" ]; then
    filename=$1
fi

if (cd build && cmake --build .); then
    # clear
    ./build/aoc "$filename"
else
    exit 1
fi
