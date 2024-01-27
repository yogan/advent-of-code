" ----------------------------------- input ------------------------------------
" duplicate empty line, so we hava an empty trailing line
" this lets us use macro @s for both parts
:norm yyP

" read data from input file
:r input.txt

" duplicate input lines, so that we can first do part 1 and then part 2
" this will leave a blank line between the two parts, and the cursor on line 1
:norm VGykP

" ----------------------------------- part 1 -----------------------------------
" \v is very magic mode (no more \ before (, ), {, }, etc.)
" .{-} is a non-greedy match, so the (\d) matches the first digit in the line
" .* is a greedy match, so the (\d) matches the last digit in the line
:let d1 = ':s/\v.{-}(\d).*/\1/'
:let d2 = ':s/\v.*(\d).*/\1+/'

" yyp - duplicate line
"  d2 - reduce second line to last digit
"   k - move up
"  d1 - reduce first line to last digit, incl. trailing + for summing later
"  gJ - join lines without spaces
"   j - move down to next input line
:let @q = "yyp" . d2 . "\<cr>k" . d1 . "\<cr>gJj"

" execute macro 1000 times (number of lines in input file)
:norm 1000@q

" remove trailing +, join lines, evaluate sum via expression register
:let @s = "k$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ----------------------------------- part 2 -----------------------------------
" move to beginning of part 2 block
:norm 3j

" same trick from part 1, but regexen include the number words now
:let dw1 = ':s/\v.{-}(\d|one|two|three|four|five|six|seven|eight|nine).*/\1/'
:let dw2 = ':s/\v.*(\d|one|two|three|four|five|six|seven|eight|nine).*/\1+/'
:let @q = "yyp" . dw2 . "\<cr>k" . dw1 . "\<cr>gJj"
:norm 1000@q

" replace the words with digits
:%s/one/1/g
:%s/two/2/g
:%s/three/3/g
:%s/four/4/g
:%s/five/5/g
:%s/six/6/g
:%s/seven/7/g
:%s/eight/8/g
:%s/nine/9/g

" move to bottom, execute summing macro
:norm G@s

" ----------------------------------- output -----------------------------------
" delete empty lines
:g/^$/d

" write results to output file
:x! out
