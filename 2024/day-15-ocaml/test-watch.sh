#!/bin/sh
BIN=_build/default/test/test_aoc.exe
echo $BIN | entr -cc ./test.sh
