:r input.txt
:%s/\vGame (\d+)/\r\1+/g
:%s/[:,;] /\r/g
:%y t
:%s/\v(\d+) red/\=submatch(1)   > 12 ? "FAIL" : "OK"
:%s/\v(\d+) green/\=submatch(1) > 13 ? "FAIL" : "OK"
:%s/\v(\d+) blue/\=submatch(1)  > 14 ? "FAIL" : "OK"
:let @q = "vipJ2k"
:norm 100@q
:g/FAIL\|^$/d
:%s/+.*/+/
:s/+//
:norm vipgJ
:let @s = "0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:norm "tp
:g/+/d
:%s/\v(\d+) (.+)/\2 \1/
:2
:let @g = ")jvip:sort\<cr>"
:norm 100@g
:%s/\v(blue \d+)\n(green)/\1\r\r\2/
:%s/\v(green \d+)\n(red)/\1\r\r\2/
:let @m = "vip:sort!n\<cr>yypvipJ2k"
:let @n = ":norm 3@m3jddjdd5k\<cr>"
:norm 100@n
:%s/\v%(blue|green) (\d+).*/\1*/
:%s/\vred (\d+).*/\1+/
:norm $xVggjgJ
:norm @s
:x! out
