#!/bin/sh
filename="input.txt"
if [ "$1" != "" ]; then
    filename=$1
fi
./build-ci.sh
dune exec aoc "$filename"
