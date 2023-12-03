#!/bin/sh
find . -name '*.fish' | entr -c ./run.fish
