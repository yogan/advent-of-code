#!/bin/sh
find . -name '*.fish' | entr ./run.fish
