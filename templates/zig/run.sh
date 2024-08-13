#!/bin/sh
if [ "$(basename "$0")" = "run.sh" ]; then
    zig build run -- input.txt
else
    zig build run -- sample.txt
fi
