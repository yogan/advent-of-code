#!/bin/sh
find . -name '*.pas' -or -name '*.txt' | entr -cc ./run.sh
