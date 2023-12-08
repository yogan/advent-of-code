#!/usr/bin/env fish
find . -name '*.fish' | entr -cc ./test.fish
