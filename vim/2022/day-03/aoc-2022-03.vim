:r input.txt|1d
:norm "iyG
:let brk = ':exe "norm " . strlen(getline(".")) / 2 . "|a\<cr>\<esc>"'
:let ynk = "\<cr>ky$j"
:let rch = ':exe "s/[^" . getreg(' . "'\"'" . ') . "]//g"'
:let dl1 = "\<cr>kdd"
:let pr1 = ":s/^\\([a-z]\\).*$/\\=char2nr(submatch(1)) - char2nr('a') +  1 . '+'/e\<cr>"
:let pr2 = ":s/^\\([A-Z]\\).*$/\\=char2nr(submatch(1)) - char2nr('A') + 27 . '+'/e\<cr>"
:let @q = brk . ynk . rch . dl1 . pr1 . pr2 . "j"
:norm 300@q
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<cr>\<esc>"
:norm @s
:norm "ip
:let yk1 = "y$j"
:let yk2 = "\<cr>y$j"
:let dl2 = "\<cr>kdk"
:let @q = yk1 . rch . yk2 . rch . dl2 . pr1 . pr2 . "j"
:norm 300@q
:norm @s
:g/^$/d
:x! out
