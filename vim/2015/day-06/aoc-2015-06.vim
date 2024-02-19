:r input.txt|1d
:let l = line('$')
:%s/\v^(.*) (\d+),(\d+) through (\d+),(\d+)$/\="`m" . (submatch(2) - 1) . "j" . (submatch(3) - 1) . "l\<c-v>" . (submatch(4) - submatch(2)) . "j" . (submatch(5) - submatch(3)) . "l" . (submatch(1) == "turn on" ? "rX" : (submatch(1) == "turn off" ? "rx" : "~"))
:%s/\v([^0-9])0[jl]/\1/g
:exe "norm Go\<cr>\<esc>1000ix\<esc>yy999p(mm"
:let @q = "gg\"ad$dd@a"
:exe "norm " . l . "@q"
:1d
:norm vipgJ
:s/x//g
:s/.*/\=col('$') - 1
:x! out
