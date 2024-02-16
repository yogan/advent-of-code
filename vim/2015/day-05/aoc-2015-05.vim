:r input.txt|1d
:v/[aeiou].*[aeiou].*[aeiou]/d
:v/\(.\)\1/d
:g/ab\|cd\|pq\|xy/d
:$s/.*/\=line('$')
:$d a
:%d
:r input.txt|1d
:v/\(..\).*\1/d
:v/\(.\).\1/d
:$s/.*/\=line('$')
:1,$-d
:pu! a
:x! out
