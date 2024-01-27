:r input.txt|1d
:let d1 = ':s/\D*\(\d\).*/\1/'
:let d2 = ':s/.*\(\d\).*/\1+/'
:let @q = "yyp" . d2 . "\<cr>k" . d1 . "\<cr>gJj"
:norm 1000@q
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out
