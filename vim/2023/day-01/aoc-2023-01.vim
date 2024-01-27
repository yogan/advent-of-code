:norm yyP
:r input.txt
:norm VGykP
:let d1 = ':s/\v.{-}(\d).*/\1/'
:let d2 = ':s/\v.*(\d).*/\1+/'
:let @q = "yyp" . d2 . "\<cr>k" . d1 . "\<cr>gJj"
:norm 1000@q
:let @s = "k$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:norm 3j
:let dw1 = ':s/\v.{-}(\d|one|two|three|four|five|six|seven|eight|nine).*/\1/'
:let dw2 = ':s/\v.*(\d|one|two|three|four|five|six|seven|eight|nine).*/\1+/'
:let @q = "yyp" . dw2 . "\<cr>k" . dw1 . "\<cr>gJj"
:norm 1000@q
:%s/one/1/g
:%s/two/2/g
:%s/three/3/g
:%s/four/4/g
:%s/five/5/g
:%s/six/6/g
:%s/seven/7/g
:%s/eight/8/g
:%s/nine/9/g
:norm G@s
:g/^$/d
:x! out
