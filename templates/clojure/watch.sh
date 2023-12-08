#!/bin/sh
find . -name '*.clj' | entr -cc ./run.sh
