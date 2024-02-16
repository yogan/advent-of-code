" ----------------------------------- input ------------------------------------
" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" delete lines without at least three vowels
:v/[aeiou].*[aeiou].*[aeiou]/d

" delete lines without at least one letter that appears twice in a row
:v/\(.\)\1/d

" delete lines with the strings ab, cd, pq, or xy
:g/ab\|cd\|pq\|xy/d

" replace last line with its line number (= number of nice strings)
:$s/.*/\=line('$')

" delete remaining lines
:1,$-d

" ---------------------------------- output ------------------------------------

" kthxbye
:x! out
