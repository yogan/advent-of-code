:r input.txt|1d
:let a = ':exe "norm " . strlen(getline(".")) / 2 . "|a\<cr>\<esc>"'
:let b = "\<cr>ky$j"
:let c = ':exe "s/^.*\\([" . getreg(' . "'\"'" . ') . "]\\).*$/\\1/ge"'
:let d = "\<cr>kdd"
:let e = ":s/[a-z]/\\=char2nr(submatch(0)) - char2nr('a') +  1 . '+'/e\<cr>"
:let f = ":s/[A-Z]/\\=char2nr(submatch(0)) - char2nr('A') + 27 . '+'/e\<cr>"
:let @q = a . b . c . d . e . f . "j"
:norm 300@q
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out
