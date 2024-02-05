" read input and remove blank first line
:r input.txt|1d

" cards = number of lines
:let cards = line('$')

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
:exe ":norm " . cards . "@q"

" remove blank lines
:g/^$/d

" search for numbers that appear twice consecutively and replace them with X
:%s/\v<(\d+) \1/X/g

" keep only the Xs (this is enough to see how many winning numbers we have)
:%s/[^X]//g

" replace Xs with length of the line (col('$') is one char after the end, so -1)
:%s/.*/\=col('$')-1

" yank intermediate result into register b
:%y b

" score of the card is 2 ^ (w-1), with w being the number of matches
:%s/.*/\=float2nr(pow(2, submatch(0)-1))

" add a + to the end of each line, expect the last one (range 1,$-)
:1,$-s/$/+

" evaluate the sum via expression register
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" restore intermediate result for part 2 from register b
:put b

" yank result for part 1 into register a
:1d a

" set mark m at beginning of buffer
:ma m

" We now have a block with the number of winning numbers for each card.
" Below that, separated by a blank line, we add a block to calculate the number
" of copies of each card. We start out with values of 1 for each card.
:exe "norm Go\<cr>1\<esc>dd" . cards . "p"

" - go to mark m
" - yank number of winning numbers into register c
" - set mark in next line
" - move down to counting block
" - yank current number of copies into register d
:let getregs = "'m\"cywjmm" . cards . "j\"dyw"

" simulate a loop by repeating a macro "c times
" we need the if, as 0@y will execute the macro once, instead of not at all
:let var = ":let c = getreg('c')\<cr>"
:let loop = ':exe "if c > 0 \n exe \"norm \" . c . \"@y\" \n endif"' . "\<cr>"

" this is the loop body:
" - go down one line (to next card)
" - add the amount of copies of the winning card ("d) to the current amount
:let @y = "j:s/.*/\\=submatch(0)+getreg('d')/\<cr>\<esc>"

" move back
:let back = ":exe \"norm \" . c . \"k\"\<cr>j"

" build the whole macro with the loop
:let @u = getregs . var . loop . back

" execute for all cards
:exe ":norm " . cards . "@u"

" remove upper block (number of winning numbers), leaving only the card counts
" + 1 to include the blank line
:exe "1," . (cards + 1) . "d"

" add a + to the end of each line, expect the last one (range 1,$-)
:1,$-s/$/+

" evaluate the sum via expression register (reuse macro @s from above)
:norm @s

" restore result for part 1 from register a
:norm "aP

" kthxbye
:x! out
