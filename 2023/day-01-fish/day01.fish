#!/usr/bin/env fish
argparse --name=day01 'p/part=!_validate_int --min 1 --max 2' -- $argv
or return

set sum 0

while read -la line
    if test $_flag_part -eq 1
        set regex "\d"
        set back_regex "\d"
    else
        set regex "one|two|three|four|five|six|seven|eight|nine"
        set back_regex (echo $regex | rev)
        set regex "\d|$regex"
        set back_regex "\d|$back_regex"
    end

    set matches (string match --all --regex "$regex" "$line" \
        | string split ' ')
    set digit1 $matches[1]

    set back_matches (echo $line | rev \
        | string match --all --regex "$back_regex" \
        | string split ' ')
    set digit2 (echo $back_matches[1] | rev)

    set digits "$digit1$digit2"

    if test $_flag_part -eq 2
        set digits (string replace --all --regex 'one'   1 $digits)
        set digits (string replace --all --regex 'two'   2 $digits)
        set digits (string replace --all --regex 'three' 3 $digits)
        set digits (string replace --all --regex 'four'  4 $digits)
        set digits (string replace --all --regex 'five'  5 $digits)
        set digits (string replace --all --regex 'six'   6 $digits)
        set digits (string replace --all --regex 'seven' 7 $digits)
        set digits (string replace --all --regex 'eight' 8 $digits)
        set digits (string replace --all --regex 'nine'  9 $digits)
    end

    set sum (math $sum + $digits)
end

echo "Part $_flag_part: $sum"
