:r input.txt|1d
:norm <G
:%s/ \+/\r/g|1
:let @q = "Vjj:sort n\<cr>ji+\<esc>A-\<esc>k3Jj"
:norm 9999@q
:%s/.*/\=eval(submatch(0))
:g/^0\|-/d
:s/.*/\=line(".")
:norm kdgg
:r input.txt|1d a
:exe "norm 0\<C-v>GE\"xd\<C-v>GEd"
:$pu x|pu
:norm <gg
:norm 9999@q
:%s/.*/\=eval(submatch(0))
:g/^0\|-/d
:$|s/.*/\=line(".")
:norm kdgg
:pu! a
:x! out
