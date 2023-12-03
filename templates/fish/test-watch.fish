#!/usr/bin/env fish
find . -name '*.fish' | entr -c ./test.fish
