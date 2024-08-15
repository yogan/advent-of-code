#!/bin/sh
filename="input.txt"
if [ "$1" != "" ]; then
    filename=$1
fi
zig build run -- "$filename"
