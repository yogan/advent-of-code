#!/bin/sh
find . -name '*.cr' -or -name '*.txt' | entr -cc ./run.sh
