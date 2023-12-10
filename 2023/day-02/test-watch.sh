#!/bin/sh
find . -name '*.clj' -or -name '*.txt' | entr -cc ./test.sh
