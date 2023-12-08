#!/bin/sh
find . -name '*.py' | entr -cc ./sample.sh
