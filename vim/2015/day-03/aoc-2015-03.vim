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
:x! out
