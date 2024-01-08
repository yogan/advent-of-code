#!/bin/sh
find src/ -name '*.zig' -or -name '*.txt' | entr -cc ./run.sh
