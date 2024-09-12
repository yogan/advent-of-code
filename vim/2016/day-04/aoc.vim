" ----------------------------------- part 1 -----------------------------------

:function Check(name, id, checksum)
:   let counts = {}
:   for char in split(substitute(a:name, '-', '', 'g'), '\zs')
:       let counts[char] = get(counts, char, 0) + 1
:   endfor
:   let counts = sort(items(counts), {a, b -> a[1] == b[1] ? a[0] > b[0] : a[1] < b[1]})
:   let checksum = ''
:   for i in range(0, 4)
:       let checksum .= counts[i][0]
:   endfor
:   if checksum == a:checksum
:       return a:id
:   else
:       return 0
:   endif
:endfunction

:r input.txt|1d
:silent %s/\v(.*)-(\d+)\[(.*)\]/\=Check(submatch(1), submatch(2), submatch(3)) . '+'
:norm vipgJ
:s/\(.*\)+/\=eval(submatch(1))
:g/./d a

" ----------------------------------- part 2 -----------------------------------

:function Decrypt(name, id)
:   let result = ''
:   for char in split(a:name, '\zs')
:       if char == '-'
:           let result .= ' '
:       else
:           let result .= nr2char(((char2nr(char) - 97 + a:id) % 26) + 97)
:       endif
:   endfor
:   return result
:endfunction

:r input.txt|1d
:silent %s/\v(.*)-(\d+)\[(.*)\]/\=Decrypt(submatch(1), submatch(2)) . '|' . submatch(2)
:silent v/northpole/d
:norm df|

" ----------------------------------- output -----------------------------------

:pu! a|x! out
