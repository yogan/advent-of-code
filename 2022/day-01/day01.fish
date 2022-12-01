#!/usr/bin/env fish
while read -la line
    if test -z $line
        set --append sums $calories
        set calories 0
    else
        set calories $(math $calories + $line)
    end
end

set --append sums $calories

echo $(math max $(string join ',' $sums))