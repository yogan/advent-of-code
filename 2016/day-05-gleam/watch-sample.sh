#!/bin/sh
find . -name '*.gleam' -or -name '*.txt' | entr -cc ./sample.sh
