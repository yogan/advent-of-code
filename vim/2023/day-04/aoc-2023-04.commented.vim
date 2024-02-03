" ----------------------------------- input ------------------------------------
" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" remove leading "Card XY: "
:%s/^.*: \+/
" remove the | and normalize duplicate spaces to single space
:%s/ [| ]\+/ /g
" put each number on a new line and add empty line between card blocks
:%s/ \|$/\r/g

" go to first line
:1

" sort the numbers in each block
:let @q = "vip:sort!n\<cr>gvJjj"
:norm 193@q

" search for numbers that appear twice consecutively and replace them with X
:%s/\v<(\d+) \1/X/g

" remove lines without winning numbers (no X in there)
:v/X/d

" keep only the Xs (this is enough to see how many winning numbers we have)
:%s/[^X]//g

" score of the card is 2 ^ (w-1), with w being the number of matches (Xs)
" we can use substitute with an expression (\=) to calculate that:
" col('$') is line length + 1, so pow(2, col('$')-2) is 2 ^ (w-1)
:%s/.*/\=float2nr(pow(2, col('$')-2))

" add a + to the end of each line, expect the last one (range 1,$-)
:1,$-s/$/+

" evaluate the sum via expression register
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ---------------------------------- output ------------------------------------
:x! out
