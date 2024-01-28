" ----------------------------------- input ------------------------------------
:r input.txt

" ----------------------------------- part 1 -----------------------------------
" extract game id and add leading empty line and trailing +
:%s/\vGame (\d+)/\r\1+/g

" put color counts on separate lines like "1 red" / "2 green" / "3 blue"
:%s/[:,;] /\r/g

" store current buffer (%) in register t for part 2
:%y t

" replace the cube counts with OK or FAIL depending on the count value
:%s/\v(\d+) red/\=submatch(1)   > 12 ? "FAIL" : "OK"
:%s/\v(\d+) green/\=submatch(1) > 13 ? "FAIL" : "OK"
:%s/\v(\d+) blue/\=submatch(1)  > 14 ? "FAIL" : "OK"

" join game id and OK/FAIL lines to single line, for all games
:let @q = "vipJ2k"
:norm 100@q

" a single FAIL makes the game impossible, so we can remove it
" while at it, we also remove empty lines (^$)
:g/FAIL\|^$/d

" get rid of the leftover trailing OK OK OK… after the +
:%s/+.*/+/

" we end up on the very last line, so we can remove the trailing + there
:s/+//

" join all lines, so we have a sum in a single line
:norm vipgJ

" evaluate the sum via expression register
:let @s = "0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ----------------------------------- part 2 -----------------------------------
" paste intermediate result from register t
:norm "tp

" remove game id lines (the only ones with a + in them)
:g/+/d

" swap count and color name ("12 red" -> "red 12")
" having the color name at the start allows to group by color via sort
:%s/\v(\d+) (.+)/\2 \1/

" move to line 2 (after part 1 result) so that macro can be repeated downward
:2

" group blue/green/red lines together in each game
:let @g = ")jvip:sort\<cr>"
:norm 100@g

" add and empty line before each color group
:%s/\v(blue \d+)\n(green)/\1\r\r\2/
:%s/\v(green \d+)\n(red)/\1\r\r\2/

" we end up in the last section (red) of the last game, so we build a macro that
" moves back upward
" vip = select current color section
" :sort!n = sort numerically, max. number ends up in first line
" yyp = hack for color sections with only 1 line
" vipJ = join current color section
" 2k = move up 2 lines to get to the next color section
:let @m = "vip:sort!n\<cr>yypvipJ2k"

" wrap the macro in a another macro that repeats it for the 3 colors (3@m),
" removes the blank lines between blue/green/red (3jddjdd),
" and moves up to the next game (5k)
:let @n = ":norm 3@m3jddjdd5k\<cr>"

" repeat the madness for all 100 games
:norm 100@n

" we just joined the sorted color sections, so they look like this:
"     green 4 green 4 green 3 green 1
" time to clean up the mess
" we just need the first number in the line, and add a * to later multiply
:%s/\v%(blue|green) (\d+).*/\1*/
" red is the last color, so we can place the + for summing the powers here
:%s/\vred (\d+).*/\1+/

" merge everything into a single line that looks like this: 1*2*3+4*5*6+…
:norm $xVggjgJ

" reuse evaluation from part 1 to get result
:norm @s

" ---------------------------------- output ------------------------------------
:x! out
