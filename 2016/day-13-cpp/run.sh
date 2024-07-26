#!/bin/sh
if (cd build && cmake --build .); then
    clear
    ./build/aoc "$@"
else
    exit 1
fi
