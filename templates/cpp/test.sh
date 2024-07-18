#!/bin/sh
cd build || exit 1
if cmake --build .; then
    ./tests/tests
else
    exit 1
fi
