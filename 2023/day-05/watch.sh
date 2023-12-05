#!/bin/sh
find . -name '*.py' | entr -cc ./run.sh
