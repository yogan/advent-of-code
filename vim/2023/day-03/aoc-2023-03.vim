:r input.txt|1d
:%s/.*/.\0./
:1|norm yy2P
:1,2s/././g|1m$|1
:%y i
:set nowrapscan
:norm mi
:let spc = "O\<esc>mjyy2p"
:let sel = ":exe \"silent! norm /\\\\d\\\<lt>cr>\"\<cr>\<c-v>iwjlohk"
:let cpy = "\"ay'jp3gJ"
:let clr = "gvjlohkr."
:let pos = "'i"
:let @q = spc . sel . cpy . clr . pos
:norm 2000@q
:,$d
:g/\v^(\.|\d)*$/d
:%s/\v^\D*(\d+)\D*$/\1+/
:norm $xvipgJ
:let part1 = eval(getline("."))
:pu i|1d
:function Part2()
:   let sum = 0
:   while search('\*') " no 'W' flag, as nowrapscan is already set above
:       normal mm
:       let numbers = []
:       for move in ['kh', 'k', 'kl', 'h', 'l', 'jh', 'j', 'jl']
:           exe "norm `m" . move
:           let [l, c] = [line('.'), col('.')]
:           if getline(l)[c-1] =~ '\d'
:               normal viw"by
:               let [start, end] = [getpos("'<")[2], getpos("'>")[2]]
:               let number_with_pos = [l, start, end, str2nr(@b)]
:               if index(numbers, number_with_pos) == -1
:                   call add(numbers, number_with_pos)
:               endif
:           endif
:       endfor
:       normal `m
:       if len(numbers) == 2
:           let sum += numbers[0][3] * numbers[1][3]
:       endif
:   endwhile
:   %d
:   put =sum
:endfunction
:call Part2()
:pu! =part1|1d
:x! out
