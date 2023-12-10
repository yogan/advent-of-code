#!/bin/sh
find . -name '*.ddp' -or -name '*.txt' | entr -cc ./fuehre-aus.sh
