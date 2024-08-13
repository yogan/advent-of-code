#!/usr/bin/env fish
set expected 14545 4978
set output (./run.fish)
set code 0

function validate -a part
    echo -n "Part $part: " 1>&2
    if test $output[$part] = $expected[$part]
        echo "OK (»$output[$part]«)" 1>&2
    else
        echo "Wrong answer!"
        echo "Expected: »$expected[$part]«"
        echo "Received: »$output[$part]«"
        set code (math $code + $part)
    end
end

validate 1
# validate 2
exit $code
