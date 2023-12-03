#!/usr/bin/env fish
argparse --name=aoc 'p/part=!_validate_int --min 1 --max 2' -- $argv
or return

function calc_volume
    set l $argv[1]
    set w $argv[2]
    set h $argv[3]
    echo (math $l '*' $w '*' $h)
end

function calc_surface_area
    set l $argv[1]
    set w $argv[2]
    set h $argv[3]
    echo (math 2 '*' $l '*' $w + 2 '*' $w '*' $h + 2 '*' $h '*' $l)
end

set sum 0

while read -la line
    set dims (string split "x" $line)

    if test $_flag_part -eq 1
        set volume (calc_volume $dims[1] $dims[2] $dims[3])
        set sum (math $sum + $volume)
    else
        set surface_area (calc_surface_area $dims[1] $dims[2] $dims[3])
        set sum (math $sum + $surface_area)
    end
end

echo "Part $_flag_part: $sum"
