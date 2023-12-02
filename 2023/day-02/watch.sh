#!/bin/sh
find . -name '*.clj' | entr -c ./run.sh
