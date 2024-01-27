" read data from input file, remove empty first line
:r input.txt|1d

:let d1 = ':s/\D*\(\d\).*/\1/'
:let d2 = ':s/.*\(\d\).*/\1+/'

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
:let @s = "$xvipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" write results to output file
:x! out
