#!/bin/sh
filename="input.txt"
if [ "$1" != "" ]; then
    filename=$1
fi
idris2 --build aoc.ipkg && ./build/exec/aoc "$filename"
