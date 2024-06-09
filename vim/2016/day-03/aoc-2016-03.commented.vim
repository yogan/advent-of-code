"
"        ▐▄▄▄.▄▄ ·  ▄▄· ▄▄▄   ▄▄▄· ·▄▄▄▄▄▄▄▄ ▄▄·  ▄▄▄· • ▌ ▄ ·.  ▄▄▄·
"         ·██▐█ ▀. ▐█ ▌▪▀▄ █·▐█ ▀█ ▐▄▄·•██  ▐█ ▌▪▐█ ▀█ ·██ ▐███▪▐█ ▄█
"       ▪▄ ██▄▀▀▀█▄██ ▄▄▐▀▀▄ ▄█▀▀█ ██▪  ▐█.▪██ ▄▄▄█▀▀█ ▐█ ▌▐▌▐█· ██▀·
"       ▐▌▐█▌▐█▄▪▐█▐███▌▐█•█▌▐█ ▪▐▌██▌. ▐█▌·▐███▌▐█ ▪▐▌██ ██▌▐█▌▐█▪·•
"        ▀▀▀• ▀▀▀▀ ·▀▀▀ .▀  ▀ ▀  ▀ ▀▀▀  ▀▀▀ ·▀▀▀  ▀  ▀ ▀▀  █▪▀▀▀.▀
"          Shout out to the wonderful people at JSCraftCamp 2024!

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

" evaluate the expressions on all lines
:%s/.*/\=eval(submatch(0))

" only keep positive numbers, those are possible triangles
:g/^0\|-/d

" line number of the last line is the result
:s/.*/\=line(".")

" get rid of all other lines
:norm kdgg

" ----------------------------------- part 2 -----------------------------------

" read input again, delete first line (result of part 1) into register a
:r input.txt|1d a

" use visual block mode to delete first and second column into registers x and "
:exe "norm 0\<C-v>GE\"xd\<C-v>GEd"

" paste columns back at the end (first put needs $ to paste at the end;
" this leaves the cursor at the end, so we don't need a $ for the second put)
:$pu x|pu

" remove leading spaces again, but this time we are at the bottom, so gg instead
" of G to move up
:norm <gg

" now it's part 1 again, so apply macro q to get the expressions, evaluate them,
" keep lines with positive numbers, and count remaining lines
:norm 9999@q
:%s/.*/\=eval(submatch(0))
:g/^0\|-/d
:$|s/.*/\=line(".")
:norm kdgg

" ----------------------------------- output -----------------------------------

" paste of part 1 from register a above current line
:pu! a

" kthxbye
:x! out
