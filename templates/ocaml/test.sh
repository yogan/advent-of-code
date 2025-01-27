#!/bin/sh
BIN=_build/default/test/test_aoc.exe
if [ ! -f $BIN ]; then
    echo "Test binary $BIN not found, run ./build-ci.sh or ./run.sh first."
    exit 1
fi
while [ ! -x $BIN ]; do
    sleep 0.1
done
./$BIN
