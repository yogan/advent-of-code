" ----------------------------------- part 1 -----------------------------------

" read input and remove blank first line
:r input.txt|1d

" remember number of inputs
:let l = line('$')

" transform instructions to Vim motions; input looks like this:
" (turn on|turn off|toggle) x_start,ystart through x_end,y_end
" - move x_start - 1 down from (0,0) with j
" - move y_start - 1 right from (0,0) with l
" - start a visual block mode, then:
"   - move x_end - x_start lines down with j
"   - move y_end - y_start chars right with l
" - perform the operation on the block selection:
"   - turn on  -> rX
"   - turn off -> rx
"   - toggle   -> ~
:%s/\v^(.*) (\d+),(\d+) through (\d+),(\d+)$/\="`m" . (submatch(2) - 1) . "j" . (submatch(3) - 1) . "l\<c-v>" . (submatch(4) - submatch(2)) . "j" . (submatch(5) - submatch(3)) . "l" . (submatch(1) == "turn on" ? "rX" : (submatch(1) == "turn off" ? "rx" : "~"))

" we end up with a few 0 movements ("0j", "0l")
" those are not valid Vim motions, so we remove them
:%s/\v([^0-9])0[jl]/\1/g

" create 1000x1000 field of x, set mark m at beginning (0,0)
:exe "norm Go\<cr>\<esc>1000ix\<esc>yy999p(mm"

" treat each transformed line as Vim commands
" - go to first line (gg)
" - yank line into register a ("ad$)
" - delete the whole line
" - run line a macro (@a)
:let @q = "gg\"ad$dd@a"
:exe "norm " . l . "@q"

" remove leftover separator line, now at top (movement are gone)
:1d

" join all x/X into single line
:norm vipgJ

" remove lights that are turned off (x), keeping only Xs
:s/x//g

" replace line by its length, giving the number of turned on lights
:s/.*/\=col('$') - 1

" ---------------------------------- output ------------------------------------

" kthxbye
:x! out
