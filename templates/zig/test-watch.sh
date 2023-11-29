#!/bin/sh
find src/ -name '*.zig' | entr -c ./test.sh
