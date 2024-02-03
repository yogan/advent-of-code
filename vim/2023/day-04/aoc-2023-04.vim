:r input.txt|1d
:%s/^.*: \+/
:%s/ [| ]\+/ /g
:%s/ \|$/\r/g
:1
:let @q = "vip:sort!n\<cr>gvJjj"
:norm 193@q
:%s/\v<(\d+) \1/X/g
:v/X/d
:%s/[^X]//g
:%s/.*/\=float2nr(pow(2, col('$')-2))
:1,$-s/$/+
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out
