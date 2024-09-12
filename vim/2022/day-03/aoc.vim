" ----------------------------------- input ------------------------------------
" read input data, delete blank first line
:r input.txt|1d

" store input in register i for part 2
:norm "iyG

" ----------------------------------- part 1 -----------------------------------

" move to middle of line and break into two lines
:let brk = ':exe "norm " . strlen(getline(".")) / 2 . "|a\<cr>\<esc>"'
" yank first half into default register
:let ynk = "\<cr>ky$j"
" remove characters from default register in current line
:let rch = ':exe "s/[^" . getreg(' . "'\"'" . ') . "]//g"'
" delete first half
:let dl1 = "\<cr>kdd"
" convert letter to priority number, incl. trailing + for summing later
" regex takes care that we could have multiple of the same latter in the line
" (only first one is converted, rest is removed)
:let pr1 = ":s/^\\([a-z]\\).*$/\\=char2nr(submatch(1)) - char2nr('a') +  1 . '+'/e\<cr>"
:let pr2 = ":s/^\\([A-Z]\\).*$/\\=char2nr(submatch(1)) - char2nr('A') + 27 . '+'/e\<cr>"

" do above for all input lines (300)
:let @q = brk . ynk . rch . dl1 . pr1 . pr2 . "j"
:norm 300@q

" remove trailing +, join lines, evaluate sum via expression register
" double <cr> to add an empty blank line to separate part 1 and 2
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<cr>\<esc>"
:norm @s

" ----------------------------------- part 2 -----------------------------------

" paste input back into buffer
:norm "ip

" yank first group into default register, move to second group
:let yk1 = "y$j"
" yank remaining characters of group 2 into default register, move to group 3
:let yk2 = "\<cr>y$j"
" delete group 1 and 2 lines
:let dl2 = "\<cr>kdk"

" do above, plus priority conversion (pr1/pr2) from part 1, for all input lines
" the "remove characters" substitution (rch) from part 1 is also reused
:let @q = yk1 . rch . yk2 . rch . dl2 . pr1 . pr2 . "j"
:norm 300@q

" repeat macro s from part 1 to sum up all numbers
:norm @s

" ----------------------------------- output -----------------------------------

" delete empty lines
:g/^$/d

" write result to output file
:x! out
