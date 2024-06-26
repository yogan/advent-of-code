:r input.txt|1d
:function ProcessColumn()
:   exe "norm \<C-v>Gd"
:   let chars = split(getreg('"'), '\n')
:   let counts = {}
:   let max = 0
:   let res = '?'
:   for c in chars
:       let counts[c] = get(counts, c, 0) + 1
:       if counts[c] > max
:           let max = counts[c]
:           let res = c
:       endif
:   endfor
:   return res
:endfunction
:function Part1()
:   let msg = ''
:   for _ in range(col("$") - 1)
:       let msg .= ProcessColumn()
:   endfor
:   return msg
:endfunction
:let p1 = Part1()
:pu! =p1
:silent 2,$d
:x! out
