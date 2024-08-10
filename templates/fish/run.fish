#!/usr/bin/env fish
if [ "$(basename $(status -f))" = "run.fish" ]
    ./aoc.fish <input.txt
else
    ./aoc.fish <sample.txt
end
