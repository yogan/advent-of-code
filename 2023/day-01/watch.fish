#!/usr/bin/env fish
find . -name '*.fish' -or -name '*.txt' | entr -cc ./run.fish
