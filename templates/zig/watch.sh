#!/bin/sh
find src/ -name '*.zig' | entr -c ./run.sh
