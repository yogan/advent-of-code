#!/usr/bin/env fish
find . -name '*.fish' | entr -cc ./run.fish
