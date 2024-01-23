:r input.txt
:let @q = "vip:j\<cr>j"
:norm 999@q
:g/^$/d
:%s/ /+/g
:norm gg0
:let @v = "c$\<C-r>=\<C-r>\"\<cr>\<Esc>0j"
:norm 999@v
:%!sort -rn
:4,$d
:norm ggyypJJ
:s/ /+/g
:norm @v
:x! out
