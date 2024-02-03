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
:%s/XXXXXXXXXX/512+
:%s/XXXXXXXXX/256+
:%s/XXXXXXXX/128+
:%s/XXXXXXX/64+
:%s/XXXXXX/32+
:%s/XXXXX/16+
:%s/XXXX/8+
:%s/XXX/4+
:%s/XX/2+
:%s/X/1+
:$s/+$/
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out
