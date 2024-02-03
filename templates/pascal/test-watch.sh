#!/bin/sh
find . -name '*.pas' -or -name '*.txt' | entr -cc ./test.sh
