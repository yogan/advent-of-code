" read input and remove blank first line
:r input.txt|1d

" add extra blank lines between input lines
:silent! %s/$/\r

" converting spaces to line breaks
:silent! %s/\s\+/\r/g

" go to top (:1 would work as well)
:norm gg

" select one paragraph (vip), sort paragraph numerically
:let p1 = ':exe ":norm vip:sort! n\<cr>"'
" we end at start of paragraph

" only keep top and bottom line of paragraph (largest / smallest)
:let p2 = ':norm jV}kkd\'

" join the remaining two lines together with a minus sign in between
:let p3 = ':exe ":norm kA-\<esc>J"'

" evaluate the expression to get the difference
:let p4 = ':s/.*/\=eval(submatch(0))'

" move to next paragraph
:let p5 = ':norm j'

" create macro (with enter keys between each command to execute it)
:let cr = "\<cr>"
:let @m = p1 . cr . p2 . cr . p3 . cr . p4 . cr . p5 . cr

" execute the macro for all lines (it stops at end of buffer)
:norm 999@m

" delete empty lines
:g/^$/d

" add plus at the end of each line
:silent! %s/$/+

" get rid of trailing + in last line, join all lines
:norm $xvipJ

" eval the sum
:s/.*/\=eval(submatch(0))

" kthxbye
:x! out
