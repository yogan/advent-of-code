:r input.txt
:%s/[AX]/1/g
:%s/[BY]/2/g
:%s/[CZ]/3/g
:%s/\([123]\) \1/3+\1+/e
:%s/1 2/6+2+/e
:%s/1 3/3+/e
:%s/2 1/1+/e
:%s/2 3/6+3+/e
:%s/3 1/6+1+/e
:%s/3 2/2+/e
:norm vipgJ$x0
:let @v = "c$\<C-r>=\<C-r>\"\<cr>"
:norm @v
:1d|:$d
:x! out
