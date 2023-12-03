#!/bin/sh
find . -name '*.fish' | entr -c ./test.fish
