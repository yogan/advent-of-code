#!/bin/sh
find . -name '*.clj' | entr -cc ./test.sh
