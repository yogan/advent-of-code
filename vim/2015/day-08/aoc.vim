" get total length of input lines by joining them into a single line
:r input.txt|1d
:norm vipgJ
:let total = col('$') - 1

" part 1:
" - eval all lines to get rid of escape sequences
" - build a sum expression of the line lengths
" - substract that sum from the total length
:r input.txt|1d
:silent! %s/\(.*\)/\=len(eval(submatch(0))) . '+'
:norm vipgJ$x
:let p1 = total - eval(getline('.'))

" part 2:
" - encode all lines with even more escape sequences
" - build a sum expression of the line lengths (+ 2 for missing quotes)
" - substract the total length from that sum
:r input.txt|1d
:silent! %s/\\/\\\\/g
:silent! %s/"/\\"/g
:silent! %s/\(.*\)/\=len(submatch(0)) + 2 . '+'
:norm vipgJ$x
:s/\(.*\)/\=eval(submatch(0)) - total

" kthxbye
:pu! =p1
:x! out
