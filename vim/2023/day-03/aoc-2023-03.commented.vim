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

" yank prepared input grid into register i for re-use in part 2
:%y i

" ----------------------------------- part 1 -----------------------------------

" disable search wrapping so that we don't jump from end to start of buffer
:set nowrapscan

" set mark i at first input line
:norm mi

" add three blank lines above input grid and set mark j at their start
:let spc = "O\<esc>mjyy2p"

" Find the first number in the input and select it with a 1 char border around
" - :exe "norm /\\d\<cr>" is what we need, requires escaping straight from hell
" - silent! is used to suppress E385: search hit BOTTOM when we reached the end
" - <c-v> start visual block selection
" - iw = select inner word
" - jl = grow end of selection one to the right and one down
" - o = toggle to start of selection
" - hk = grow start of selection one to the left and one up
:let sel = ":exe \"silent! norm /\\\\d\\\<lt>cr>\"\<cr>\<c-v>iwjlohk"

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

" evaluate sum in line and store for later output
:let part1 = eval(getline("."))

" ----------------------------------- part 2 -----------------------------------

" restore extended input grid from register i, remove sum line from part 1
:pu i|1d

:function Part2()
:   let sum = 0
:
:   while search('\*') " no 'W' flag, as nowrapscan is already set above
"       set mark m, so that we can always return to the * in the center
:       normal mm
:
:       let numbers = []
:
:       for move in ['kh', 'k', 'kl', 'h', 'l', 'jh', 'j', 'jl']
:           exe "norm `m" . move
:           let [l, c] = [line('.'), col('.')]
:           if getline(l)[c-1] =~ '\d'
"               select whole number, yank to register b
:               normal viw"by
"               get start and end column of visual selection
:               let [start, end] = [getpos("'<")[2], getpos("'>")[2]]
:               let number_with_pos = [l, start, end, str2nr(@b)]
"               we might select the same number multiple times, so:
:               if index(numbers, number_with_pos) == -1
:                   call add(numbers, number_with_pos)
:               endif
:           endif
:       endfor
:
"       return to the * in the center, so that search will move to the next
:       normal `m
:
:       if len(numbers) == 2
:           let sum += numbers[0][3] * numbers[1][3]
:       endif
:   endwhile
:
"   clear buffer and print result
:   %d
:   put =sum
:endfunction

:call Part2()

" ---------------------------------- output ------------------------------------

" restore part 1 result, remove blank first line
:pu! =part1|1d

:x! out
