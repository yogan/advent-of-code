#!/usr/bin/env fish
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

set p1 0
set p2 0

while read -la line
    set dims (string split "x" $line)

    set volume (calc_volume $dims[1] $dims[2] $dims[3])
    set p1 (math $p1 + $volume)

    set surface_area (calc_surface_area $dims[1] $dims[2] $dims[3])
    set p2 (math $p2 + $surface_area)
end

echo $p1
echo $p2
