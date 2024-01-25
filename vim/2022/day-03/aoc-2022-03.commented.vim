" read input data, delete blank first line
:r input.txt|1d

" move to middle of line and break into two lines
:let a = ':exe "norm " . strlen(getline(".")) / 2 . "|a\<cr>\<esc>"'
" yank first half into default register
:let b = "\<cr>ky$j"
" keep only characters matching first half chars in seconds half
:let c = ':exe "s/^.*\\([" . getreg(' . "'\"'" . ') . "]\\).*$/\\1/ge"'
" delete first half
:let d = "\<cr>kdd"
" convert letter to priority number, incl. trailing + for summing later
:let e = ":s/[a-z]/\\=char2nr(submatch(0)) - char2nr('a') +  1 . '+'/e\<cr>"
:let f = ":s/[A-Z]/\\=char2nr(submatch(0)) - char2nr('A') + 27 . '+'/e\<cr>"

" do above for all input lines (300)
:let @q = a . b . c . d . e . f . "j"
:norm 300@q

" remove trailing +, join lines, evaluate sum via expression register
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" write result to output file
:x! out
