#!/usr/bin/env fish
set p1_len 0
set p2_len 0
set total  0

while read -la line
    set line_len (string length $line)
    set line_raw (string sub -s 2 -l (math $line_len - 1) $line) # strip quotes
    set p1       (string unescape $line_raw)
    set p2       (string escape   $line)
    set p1_len   (math $p1_len + (string length $p1))
    set p2_len   (math $p2_len + (string length $p2) + 2) # +2 for quotes
    set total    (math $total  + (string length $line))
end

echo (math $total - $p1_len)
echo (math $p2_len - $total)
