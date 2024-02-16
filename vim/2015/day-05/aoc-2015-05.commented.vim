" ----------------------------------- part 1 -----------------------------------

" read input and remove blank first line
:r input.txt|1d

" delete lines without at least three vowels
:v/[aeiou].*[aeiou].*[aeiou]/d

" delete lines without at least one letter that appears twice in a row
:v/\(.\)\1/d

" delete lines with the strings ab, cd, pq, or xy
:g/ab\|cd\|pq\|xy/d

" replace last line with its line number (= number of nice strings)
:$s/.*/\=line('$')

" delete result into register a
:$d a

" clear buffer
:%d

" ----------------------------------- part 2 -----------------------------------

" read input and remove blank first line
:r input.txt|1d

" delete lines without letter pairs that appear twice
:v/\(..\).*\1/d

" delete lines without a letter that repeats with exactly one letter in between
:v/\(.\).\1/d

" replace last line with its line number (= number of nice strings)
:$s/.*/\=line('$')

" clear remaining string lines
:1,$-d

" ---------------------------------- output ------------------------------------

" restore part 1 result from register a (! for above current line)
:pu! a

" kthxbye
:x! out
