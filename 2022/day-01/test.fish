#!/usr/bin/env fish
set expected 24000
set result $(./day01.fish <day01.sample)

if not string match --quiet --regex '^\d+$' $result
    echo "Result was not an integer (got: \"$result\")."
    exit 1
end

if test $result -eq $expected
    echo "Correct answer! ($result)"
else
    echo "Wrong answer (expected $expected but got $result)."
    exit 1
end
