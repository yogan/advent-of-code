" ----------------------------------- input ------------------------------------
" Read input in twice, with a blank line in between.
:r input.txt|:0r input.txt
" :r sample.txt|:0r sample.txt

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

" ----------------------------------- output -----------------------------------
:x! out
