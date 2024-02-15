" read input and remove blank first line
:r input.txt|1d

" length of input string
:let l = col('$') - 1

" create line with 2*l spaces
:norm yy2pgJ
:exe ":norm Vr \<esc>"

" copy that line 2*l times to create empty field to draw into
:exe "norm yy" . (2*l) . "p"

:norm 0gg

" convert directions to Vim commands that move around and draw dots
:s/v/jr./g
:s/\^/kr./g
:s/</hr./g
:s/>/lr./g

" store commands into register m
:norm "my$

" go to middle of the empty field and draw dot at start position
:exe "norm " . (l+1) . "j" . l . "lr."

" execute commands from register m
:norm @m

" remove first line (converted input)
:1d

" keep only lines with dots
:v/\./d

" NOTE: this is a nice place to stop, the created map looks quite beautiful

" remove spaces and join all the dots into one line
:%s/ //g
:norm vipgJ

" replace whole line with its length
:s/.*/\=col('$') - 1/

" kthxbye
:x! out
