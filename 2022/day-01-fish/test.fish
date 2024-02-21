#!/usr/bin/env fish
set expected_part_1 24000
set expected_part_2 45000

set INPUT day01.sample

set result_part_1 $(./day01.fish --part=1 <$INPUT)
set result_part_2 $(./day01.fish --part=2 <$INPUT)

function validate -a day expected result
    echo -n "Day $day: "
    if not string match --quiet --regex '^\d+$' $result
        echo "Result was not an integer (got: \"$result\")"
        exit 1
    end

    if test $result -eq $expected
        echo "Correct answer! ($result)"
    else
        echo "Wrong answer (expected $expected but got $result)"
        exit 1
    end
end

validate 1 $expected_part_1 $result_part_1
validate 2 $expected_part_2 $result_part_2
