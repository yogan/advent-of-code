:r input.txt|1d
:let l = col('$') - 1
:norm yy2pgJ
:exe ":norm Vr \<esc>"
:exe "norm yy" . (2*l) . "p"
:norm 0gg
:s/v/jr./g
:s/\^/kr./g
:s/</hr./g
:s/>/lr./g
:norm "my$
:exe "norm " . (l+1) . "j" . l . "lr."
:norm @m
:1d
:v/\./d
:%s/ //g
:norm vipgJ
:s/.*/\=col('$') - 1/
:normal "add
:r input.txt|1d
:norm yyp
:1s/\v(.)./\1/g
:2s/\v.(.)/\1/g
:let l = l / 2
:norm yy2pgJ
:exe ":norm Vr \<esc>"
:exe "norm yy" . (2*l) . "p"
:norm 0gg
:%s/v/jr./g
:%s/\^/kr./g
:%s/</hr./g
:%s/>/lr./g
:norm gg"my$
:norm j"ny$
:exe "norm " . (l+1) . "j" . l . "lr."
:norm mc@m
:norm `c@n
:1,2d
:v/\./d
:%s/ //g
:norm vipgJ
:s/.*/\=col('$') - 1/
:normal "aP
:x! out
