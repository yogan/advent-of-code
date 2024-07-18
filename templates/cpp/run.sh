#!/bin/sh
cd build || exit 1
if cmake --build .; then
    clear
    ./aoc
else
    exit 1
fi
