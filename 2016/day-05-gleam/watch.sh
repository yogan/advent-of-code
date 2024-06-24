#!/bin/sh
find . -name '*.gleam' -or -name '*.txt' | entr -cc ./run.sh
