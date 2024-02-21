#!/bin/sh
find src/ -name '*.zig' | entr -cc ./run.sh
