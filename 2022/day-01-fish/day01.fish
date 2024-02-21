#!/usr/bin/env fish
argparse --name=day01 'p/part=!_validate_int --min 1 --max 2' -- $argv
or return

while read -la line
    if test -z $line
        set --append sums $calories
        set calories 0
    else
        set calories $(math $calories + $line)
    end
end

set --append sums $calories

if test $_flag_part -eq 1
    set elves 1
else
    set elves 3
end

set top_sums $(for sum in $sums
    echo $sum
end | sort -n | tail -$elves
)

echo $(math $(string join '+' $top_sums))
