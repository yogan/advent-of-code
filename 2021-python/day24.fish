#!/usr/bin/env fish

# set's see which lines are actually different for each digit:
for line in (seq 18)
    echo
    echo Line $line
    for infile in inputs/24/*txt.*
        sed -n {$line}p $infile
    end
end

echo

# turns out: lines 5, 6, and 16
for infile in inputs/24/*txt.*
    echo
    echo $infile
    sed -n -e 5p -e 6p -e 16p $infile
end