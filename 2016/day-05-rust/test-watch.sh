#!/bin/sh
find . -name '*.rs' -or -name '*.txt' | entr -cc ./test.sh
