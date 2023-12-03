#!/bin/sh
find . -name '*.clj' | entr -c ./test.sh
