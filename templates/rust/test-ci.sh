#!/bin/sh
./test.sh >/dev/null 2>&1 \
    && ./sample.sh >/dev/null 2>&1 \
    && ./run.sh >/dev/null 2>&1
