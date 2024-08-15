#!/usr/bin/env fish
set filename "input.txt"
if test (count $argv) -ge 1
    set filename $argv[1]
end
./aoc.fish <"$filename"
