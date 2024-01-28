:r input.txt
:%s/\vGame (\d+)/\r\1+/g
:%s/[:,;] /\r/g
:%s/\v(\d+) red/\=submatch(1)   > 12 ? "FAIL" : "OK"
:%s/\v(\d+) green/\=submatch(1) > 13 ? "FAIL" : "OK"
:%s/\v(\d+) blue/\=submatch(1)  > 14 ? "FAIL" : "OK"
:let @q = "vipJ2k"
:norm 100@q
:g/FAIL\|^$/d
:%s/+.*/+/
:s/+//
:let @s = "vipgJ0c$\<C-r>=\<C-r>\"\<cr>\<esc>"
:norm @s
:x! out
