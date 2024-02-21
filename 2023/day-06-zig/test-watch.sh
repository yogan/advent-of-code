#!/bin/sh
find src/ -name '*.zig' | entr -cc ./test.sh
