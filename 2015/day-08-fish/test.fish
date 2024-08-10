#!/usr/bin/env fish
set expected 1371 2117
set output (./run.fish)
set status 0

function validate -a part
    echo -n "Part $part: " 1>&2
    if test $output[$part] = $expected[$part]
        echo "OK (»$output[$part]«)" 1>&2
    else
        echo "Wrong answer!"
        echo "Expected: »$expected[$part]«"
        echo "Got:      »$output[$part]«"
        set status (math $status + $part)
    end
end

validate 1
validate 2
exit $status
