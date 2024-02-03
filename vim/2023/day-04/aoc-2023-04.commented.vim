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

" I'm too lazy to find out how to calculate n^x in Vim, and we have a maximum
" of 10 winning numbers, we'll use the equivalent of a lookup table: a bunch of
" substitutions
:%s/XXXXXXXXXX/512+
:%s/XXXXXXXXX/256+
:%s/XXXXXXXX/128+
:%s/XXXXXXX/64+
:%s/XXXXXX/32+
:%s/XXXXX/16+
:%s/XXXX/8+
:%s/XXX/4+
:%s/XX/2+
:%s/X/1+

" remove last +
:$s/+$/

" evaluate the sum via expression register
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ---------------------------------- output ------------------------------------
:x! out
