:r input.txt|1d
:silent! %s/$/\r
:silent! %s/\s\+/\r/g
:norm gg
:let p1 = ':exe ":norm vip:sort! n\<cr>"'
:let p2 = ':norm jV}kkd\'
:let p3 = ':exe ":norm kA-\<esc>J"'
:let p4 = ':s/.*/\=eval(submatch(0))'
:let p5 = ':norm j'
:let cr = "\<cr>"
:let @m = p1 . cr . p2 . cr . p3 . cr . p4 . cr . p5 . cr
:norm 999@m
:g/^$/d
:silent! %s/$/+
:norm $xvipJ
:s/.*/\=eval(submatch(0))
:x! out
