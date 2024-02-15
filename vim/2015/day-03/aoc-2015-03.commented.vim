" ----------------------------------- input ------------------------------------
" read input and remove blank first line
:r input.txt|1d

" length of input string
:let l = col('$') - 1

" ----------------------------------- part 1 -----------------------------------

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

" store result into register a (leaving a clear buffer for part 2)
:normal "add

" ----------------------------------- part 2 -----------------------------------

" read input and remove blank first line
:r input.txt|1d

" duplicate line
:norm yyp

" in the first line, leave only the odd commands for Santa
:1s/\v(.)./\1/g

" in the seconds line, leave only the even commands for Robo-Santa
:2s/\v.(.)/\1/g

" we can use a grid with half the side length now, since the inputs are shorter
:let l = l / 2

" create line with 2*l spaces
:norm yy2pgJ
:exe ":norm Vr \<esc>"

" copy that line 2*l times to create empty field to draw into
:exe "norm yy" . (2*l) . "p"

:norm 0gg

" like in part 1, convert directions to Vim commands (but for both lines - %)
:%s/v/jr./g
:%s/\^/kr./g
:%s/</hr./g
:%s/>/lr./g

" store commands for Santa in register m, commands for Robo-Santa in register n
:norm gg"my$
:norm j"ny$

" go to middle of the empty field and draw dot at start position
:exe "norm " . (l+1) . "j" . l . "lr."

" - set mark c to the start position
" - execute commands for Santa from register m
:norm mc@m

" - return to mark c (start position)
" - execute commands for Robo-Santa from register n
:norm `c@n

" remove first two lines (converted input)
:1,2d

" keep only lines with dots
:v/\./d

" NOTE: this is a nice place to stop, the created map looks quite beautiful

" remove spaces and join all the dots into one line
:%s/ //g
:norm vipgJ

" replace whole line with its length
:s/.*/\=col('$') - 1/

" ---------------------------------- output ------------------------------------

" restore part 1 result from register a
:normal "aP

" kthxbye
:x! out
