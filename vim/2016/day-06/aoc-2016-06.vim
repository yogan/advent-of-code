:r input.txt|1d
:function ProcessColumn()
:   exe "norm \<C-v>Gd"
:   let chars = split(getreg('"'), '\n')
:   let counts = {}
:   let max = 0
:   let min = 999999
:   let p1 = '?'
:   let p2 = '?'
:   for c in chars
:       let counts[c] = get(counts, c, 0) + 1
:   endfor
:   for c in keys(counts)
:       if counts[c] > max
:           let max = counts[c]
:           let p1 = c
:       endif
:       if counts[c] < min
:           let min = counts[c]
:           let p2 = c
:       endif
:   endfor
:   return [p1, p2]
:endfunction
:function Decode()
:   let part1 = ''
:   let part2 = ''
:   for _ in range(col("$") - 1)
:       let chars = ProcessColumn()
:       let part1 .= chars[0]
:       let part2 .= chars[1]
:   endfor
:   return [part1, part2]
:endfunction
:let res = Decode()
:s/.*/\=res[0] . "\r" . res[1]
:3,$d
:x! out
