:r input.txt|1d
:norm vipgJ
:let total = col('$') - 1
:r input.txt|1d
:silent! %s/\(.*\)/\=len(eval(submatch(0))) . '+'
:norm vipgJ$x
:let p1 = total - eval(getline('.'))
:r input.txt|1d
:silent! %s/\\/\\\\/g
:silent! %s/"/\\"/g
:silent! %s/\(.*\)/\=len(submatch(0)) + 2 . '+'
:norm vipgJ$x
:s/\(.*\)/\=eval(submatch(0)) - total
:pu! =p1
:x! out
