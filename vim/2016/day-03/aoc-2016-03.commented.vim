" ----------------------------------- input ------------------------------------

" read input and remove blank first line
:r input.txt|1d

" ----------------------------------- part 1 -----------------------------------

" remove all leading spaces by shifting left
:norm <G

" split the lines by spaces to prepare sorting, move to top
:%s/ \+/\r/g|1

" define a macro that sorts three lines numerically and joins them, so that they
" form an expression in the form a + b - c, where c is the largest side
:let @q = "Vjj:sort n\<cr>ji+\<esc>A-\<esc>k3Jj"

" run macro on the whole input
:norm 9999@q

:%s/.*/\=eval(submatch(0))

" only keep positive numbers, those are possible triangles
:g/^0\|-/d

" line number of the last line is the result
:s/.*/\=line(".")

" get rid of all other lines
:norm kdgg

" ----------------------------------- output -----------------------------------

" kthxbye
:x! out
