#!/bin/sh
find . -name '*.py' -or -name '*.txt' | entr -cc ./run.sh
