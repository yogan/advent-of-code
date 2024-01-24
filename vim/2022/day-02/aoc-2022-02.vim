:r input.txt
:1d
:norm vip"ay
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
:let @v = "vipgJ$x0c$\<C-r>=\<C-r>\"\<cr>"
:norm @v
:norm "ap
:%s/A X/3+/g
:%s/A Y/4+/g
:%s/A Z/8+/g
:%s/B X/1+/g
:%s/B Y/5+/g
:%s/B Z/9+/g
:%s/C X/2+/g
:%s/C Y/6+/g
:%s/C Z/7+/g
:norm @v
:g/^$/d
:x! out
