" ----------------------------------- input ------------------------------------
" read input and remove blank first line
:r input.txt|1d

" add a border with . around the input to avoid edge cases:
" - add . to the beginning and end of each line
:%s/.*/.\0./

" - copy first line twice to the start of the buffer
:1|norm yy2P

" - replace all characters with . in those two lines (1,2s/././g)
" - move first line to end of buffer (1m$)
" - put cursor on first line (1)
:1,2s/././g|1m$|1

" ----------------------------------- part 1 -----------------------------------

" disable search wrapping so that we don't jump from end to start of buffer
:set nowrapscan

" set mark i at first input line
:norm mi

" add three blank lines above input grid and set mark j at their start
:let spc = "O\<esc>mjyy2p"

" Find the first number in the input and select it with a 1 char border around
" - :exe "norm /\\d\<cr>" is what we need, requires escaping straight from hell
" - <c-v> start visual block selection
" - iw = select inner word
" - jl = grow end of selection one to the right and one down
" - o = toggle to start of selection
" - hk = grow start of selection one to the left and one up
:let sel = ":exe \"norm /\\\\d\\\<lt>cr>\"\<cr>\<c-v>iwjlohk"

" - yank block into register a ("ay), move to mark j ('j), paste (p)
" - join three lines together without spaces (3gJ)
:let cpy = "\"ay'jp3gJ"

" gv = re-select last visual selection
" jlohk = shrink selection back to the inner number
" r. = replace digits with .
:let clr = "gvjlohkr."

" jump back to beginning of input (mark i)
:let pos = "'i"

" let the magic happen
:let @q = spc . sel . cpy . clr . pos
:norm 2000@q

" delete input grid (range ,$ is current line to end of file)
:,$d

" delete all lines with only numbers and dots
:g/\v^(\.|\d)*$/d

" keep only digits, add trailing + for summing
:%s/\v^\D*(\d+)\D*$/\1+/

" remove trailing + from last line, join together
:norm $xvipgJ

" evaluate the sum via expression register
:let @s = "0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ---------------------------------- output ------------------------------------
:x! out
