#!/bin/sh
find . -name '*.fish' | entr ./test.fish
