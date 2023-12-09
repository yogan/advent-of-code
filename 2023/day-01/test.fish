#!/usr/bin/env fish
set expected_part_1 "Part 1: 54304"
set expected_part_2 "Part 2: 54418"

set INPUT input.txt

set result_part_1 $(./day01.fish --part=1 <$INPUT)
set result_part_2 $(./day01.fish --part=2 <$INPUT)

function validate -a part expected result
    echo -n "Part $part: " 1>&2
    if test $result = $expected
        echo "OK (»$result«)" 1>&2
    else
        echo "Wrong answer!"
        echo "Expected: »$expected«"
        echo "Got:      »$result«"
        exit 1
    end
end

validate 1 $expected_part_1 $result_part_1
validate 2 $expected_part_2 $result_part_2
