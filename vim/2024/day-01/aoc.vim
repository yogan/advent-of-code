" ----------------------------------- input ------------------------------------
" Read input in twice, with a blank line in between.
:r input.txt|:0r input.txt

" ----------------------------------- part 1 -----------------------------------
" In the first paragraph, keep only the numbers of the first column.
" In the second paragraph, keep only the numbers of the second column.
:sil '{,'}s/ .*$/
:norm 2j
:sil '{,'}s/.* /

" Sort both columns numerically. We have to watch out not to include the blank
" line in the paragraph range ('{,'}), which is what the +1 / -1 is for.
:'{+1,'}sort n
:norm 2k
:'{,'}-1sort n

" Add a minus to the end of the first paragraph.
:sil '{,'}-1s/$/-

" Use column mode to move the second column to the right of the first column.
:norm 2j
:exe "norm \<C-v>G$xgg$p"

" Drop empty lines.
:g/^\s*$/d

" Evaluate the distances and add a trailing + to prepare summing it all up.
:sil %s/.*/\=abs(eval(submatch(0))).'+'

" Join lines together, delete trailing +, evaluate the sum.
:norm vipgJ$x
:s/.*/\=eval(submatch(0))

" Delete part 1 result to register a.
:norm "add

" ----------------------------------- part 2 -----------------------------------

:function Part2()
:   r input.txt | 1d | let lines = getline(1, '$') | 1,$d
:   let left = {} | let right = {}
:   for line in lines
:       let parts = split(line)
:       let l = parts[0] | let r = parts[1]
:       if !has_key(left, l)
:           let left[l] = 0
:       endif
:       let left[l] += 1
:       if !has_key(right, r)
:           let right[r] = 0
:       endif
:       let right[r] += 1
:   endfor
:   let result = 0
:   for num in keys(left)
:       if has_key(right, num)
:           let result += num * left[num] * right[num]
:       endif
:   endfor
:   return result
:endfunction

:pu =Part2()

" ----------------------------------- output -----------------------------------
" Paste part 1 result from register a, clear blank top line.
:pu! a|1d
:x! out
