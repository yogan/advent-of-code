" ----------------------------------- input ------------------------------------
:r input.txt

" ----------------------------------- part 1 -----------------------------------
" clever substitutions to break games into separate lines:
" - first line is game id incl. trailing + for later summing
" - following lines are initially like "1 red" / "2 green" / "3 blue", etc.
" - … and then replaced with OK or FAIL depending on the count value
:%s/\vGame (\d+)/\r\1+/g
:%s/[:,;] /\r/g
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

" join all lines and evaluate sum via expression register
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s

" ---------------------------------- output ------------------------------------
:x! out
