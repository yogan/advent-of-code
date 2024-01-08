#!/bin/sh
find src/ -name '*.zig' -or -name '*.txt' | entr -cc ./test.sh
