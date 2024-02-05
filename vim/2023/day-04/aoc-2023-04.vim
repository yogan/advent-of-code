:r input.txt|1d
:let cards = line('$')
:%s/^.*: \+/
:%s/ [| ]\+/ /g
:%s/ \|$/\r/g
:1
:let @q = "vip:sort!n\<cr>gvJjj"
:exe ":norm " . cards . "@q"
:g/^$/d
:%s/\v<(\d+) \1/X/g
:%s/[^X]//g
:%s/.*/\=col('$')-1
:%y b
:%s/.*/\=float2nr(pow(2, submatch(0)-1))
:1,$-s/$/+
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:put b
:1d a
:ma m
:exe "norm Go\<cr>1\<esc>dd" . cards . "p"
:let getregs = "'m\"cywjmm" . cards . "j\"dyw"
:let var = ":let c = getreg('c')\<cr>"
:let loop = ':exe "if c > 0 \n exe \"norm \" . c . \"@y\" \n endif"' . "\<cr>"
:let @y = "j:s/.*/\\=submatch(0)+getreg('d')/\<cr>\<esc>"
:let back = ":exe \"norm \" . c . \"k\"\<cr>j"
:let @u = getregs . var . loop . back
:exe ":norm " . cards . "@u"
:exe "1," . (cards + 1) . "d"
:1,$-s/$/+
:norm @s
:norm "aP
:x! out
