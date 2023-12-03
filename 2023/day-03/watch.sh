#!/bin/sh
find . -name '*.py' | entr -c ./run.sh
